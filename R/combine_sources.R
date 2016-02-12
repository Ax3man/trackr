check_parallel <- function(parallel) {
  if (parallel) {
    if (!requireNamespace('multidplyr', quietly = TRUE)) {
      message('Can only use parallel if the multidplyr package is installed.
Reverting to non-parallel processing.
trackr currently uses a fork of multidplyr by fugufisch.
Install using devtools::install_github(\'fugufisch/multidplyr\')')
      parallel <- FALSE
    }
  }
  return(invisible(NULL))
}

#' Integrate data from multiple tracking sources
#'
#' This function will attempt to combine data from trials that have been tracked
#' by multiple trackers ('sources'). This is useful if one want to leverage the
#' individual strengths of multiple trackers (e.g. combine orientation and
#' ellipse data from Ctrax with resolved identities from idTracker). It can also
#' be used to exert quality control, by excluding observations that trackers
#' don't agree on.
#'
#' All variables that are supplied by both trackers will be used to match
#' identities between sources. The error will be calculated and added to the $tr
#' table, along with the mean values for the two sources. All other variables
#' will be maintained. In some cases, precedence may be given when a source
#' clearly is better suited for that variable (e.g. when combining idTracker
#' with Ctrax, identities will always be based on idTracker.)
#'
#' The methods tend to be designed for reasonable speeeds, and may use a lot
#' of memory.
#'
#' Animal names _will_ be lost.
#'
#' @param tracks1 Tracks object to be combined.
#' @param tracks2 The other tracks object to be combined.
#' @param err_cutoff Hard limit for error (squared distance), that is deemed
#'   unreliable as a match. The default is 25 ^ 2, which means the centroids
#'   estimated are allowed to be 25 pixels apart.
#' @param report Whether a small report should be printed.
#'
#' @return A tracks object.
#' @export
combine_sources <- function(tracks1, tracks2, err_cutoff = 25 ^ 2,
                            report = TRUE, parallel = TRUE) {
  check_parallel(parallel)
  # Check compatibility
  if (tracks1$params$frame_rate != tracks2$params$frame_rate)
    stop('Frame rates do not match.', call. = FALSE)
  else
    frame_rate <- tracks1$params$frame_rate

  if (any(tracks1$params$resolution != tracks2$params$resolution))
    stop('Resolutions do not match.', call. = FALSE)
  else
    resolution <- tracks1$params$resolution

  if (any(tracks1$params$bounds != tracks2$params$bounds))
    stop('Bounds do not match.', call. = FALSE)
  else
    bounds <- tracks1$params$bounds

  # Find correct combine.
  sources <- c(tracks1$params$source, tracks2$params$source)
  if (any(sources == 'Ctrax') & any(sources == 'idTracker'))
    tr <- combine_Ctrax_idTracker(tracks1, tracks2, err_cutoff, report,
                                  parallel)
  else
    stop('Can currently only combine Ctrax and idTracker sources.',
         call. = FALSE)

  tracks <- as_tracks(tr, frame_rate, resolution[1])
  tracks$params$resolution <- resolution
  tracks$params$source <- sources
  tracks$params$bounds <- bounds
  return(tracks)
}

combine_Ctrax_idTracker <- function(tracks1, tracks2, err_cutoff, report,
                                    parallel) {
  # This second version will assume that ctrax is golden, it's just the id's
  # that are off.

  # Input handling -------------------------------------------------------------
  ctrax <- switch(tracks1$param$source,
                  'Ctrax' = tracks1,
                  'idTracker' = tracks2)
  ct <- ctrax$tr
  idtracker <- switch(tracks2$param$source,
                      'Ctrax' = tracks1,
                      'idTracker' = tracks2)
  id <- idtracker$tr

  # Before we do anything, we make sure we only use _reliable_ idtracker id's
  id <- dplyr::filter_(id, ~prob_id == 1)

  if (parallel) {
    res <- multidplyr::partition(ct, trial, animal)
    # In order to do efficient parallelisation, we should be careful to only
    # assign the needed parts of the idtracker data to each node
    cl <- res$cluster
    reg_trials <- lapply(multidplyr::cluster_get(cl, res$name),
                         function(x) names(table(x$trial)[table(x$trial) > 0]))
    id_cl <- lapply(reg_trials, function(trs) dplyr::filter(id, trial %in% trs))
    multidplyr::cluster_assign_each(cl, 'id', id_cl)
    multidplyr::cluster_assign_value(cl, 'err_cutoff', err_cutoff)
    multidplyr::cluster_assign_value(cl, 'match_ids', match_ids)
  } else {
    res <- dplyr::group_by_(ct, ~trial, ~animal)
  }
  cat('Matching id\'s...\n')
  res <- dplyr::do(res, match_ids(.))
  if (parallel)
    res <- dplyr::collect(res)
  res <- dplyr::ungroup(res)
  res <- dplyr::select_(res, ~trial, ~animal.y, ~frame, ~X.x, ~Y.x,
                        ~major_axis, ~minor_axis, ~orientation, ~combine_err)
  names(res)[c(2, 4:5)] <- c('animal', 'X', 'Y')
  # All that is left is to filter out frames above cut-off and duplicate matches
  res <- dplyr::filter_(res, ~!is.na(animal))

  res <- dplyr::group_by_(res, ~trial, ~animal, ~frame)
  if (attributes(res)$biggest_group_size > 1) {
    if (parallel) {
      res <- multidplyr::partition(res, trial, animal, frame)
      multidplyr::cluster_assign_value(multidplyr::get_default_cluster(),
                                       'err_cutoff', err_cutoff)
    }
    cat('Finding duplicates...\n')
    res_ok <- dplyr::filter(res, n() == 1)
    res_dups <- dplyr::filter(res, n() > 1)
    cat(nrow(res_dups), 'duplicate detections found. Merging...\n')
    res_dups <-
      dplyr::summarize_(res_dups,
                        X = ~mean(X[combine_err < err_cutoff]),
                        Y = ~mean(Y[combine_err < err_cutoff]),
                        major_axis =
                          ~mean(major_axis[combine_err < err_cutoff]),
                        minor_axis =
                          ~mean(minor_axis[combine_err < err_cutoff]),
                        orientation =
                          ~trackr:::mean_direction(
                            orientation[combine_err < err_cutoff]),
                        combine_error =
                          ~mean(combine_err[combine_err < err_cutoff]))
    if (parallel) {
      res_ok <- dplyr::collect(res_ok)
      res_dups <- dplyr::collect(res_dups)
      dplyr::stop_cluster()
    } else {
      res <- dplyr::ungroup(res)
    }
    res <- dplyr::bind_rows(res_ok, res_dups)
  }
  res <- dplyr::arrange_(res, ~trial, ~animal, ~frame)
  return(res)
}

match_ids <- function(Ct) {
  d <- dplyr::left_join(Ct, id, by = c('trial', 'frame'))
  d$combine_err <- with(d, abs(X.x - X.y) ^ 2 + abs(Y.x - Y.y) ^ 2)
  # Quickly detect spurious ctrax detections
  if (all(is.na(d$combine_err)) ||
      min(d$combine_err, na.rm = TRUE) > err_cutoff)
    return(NA)
  # Find best matches
  d <- dplyr::group_by_(d, ~frame)
  d <- dplyr::slice_(d, ~ifelse(all(is.na(combine_err)),
                                1,
                                which.min(combine_err)))
  d$animal.y[d$combine_err > err_cutoff] <- NA
  d <- dplyr::ungroup(d)
  # When no match found, check if next and previous id are equal, if so assign
  # that id, if not keep NA's. We do this per consecutive run of NA frames.
  min_frame <- min(d$frame, na.rm = TRUE)
  max_frame <- max(d$frame, na.rm = TRUE)
  na_frames <- d$frame[which(is.na(d$animal.y))]
  Breaks <- c(0, which(diff(na_frames) != 1), length(na_frames))
  runs <- lapply(seq(length(Breaks) - 1),
                 function(i) na_frames[(Breaks[i] + 1):Breaks[i + 1]])
  if (length(runs) == 0 || (length(runs) == 1 && is.na(runs)))
    return(d)

  for (i in seq_along(runs)) {
    frames <- runs[[i]]
    min_fr <- min(frames)
    max_fr <- max(frames)
    prev_id <- d$animal.y[d$frame == min_fr - 1]
    next_id <- d$animal.y[d$frame == max_fr + 1]

    if (length(prev_id) + length(next_id) == 1) {
      if (length(prev_id) == 0)
        d[i, 'animal.y'] <- next_id
      if (length(next_id) == 0)
        d[i, 'animal.y'] <- prev_id
    } else {
      if (prev_id == next_id)
        d$animal.y[d$frame %in% frames] <- next_id
    }
  }
  return(d)
}
