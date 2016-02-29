#' Create a tracks object from a data.frame.
#'
#' @param tr Data.frame with tracking data (from read in functions).
#' @param frame_rate Frames per second.
#' @param resolution Provide standard name for resolution (e.g. "720") or actual
#'    resolution in pixels (e.g. "1920x1080"). Stored for later use, also
#'    used as default tracking window (boundary).
#' @param meta_data Optionally supply a data.frame with meta data about the
#'   trials, such as treatments. Can include a column named \code{trial} that
#'   needs to match \code{levels(tr$trial)}. Otherwise this column is created,
#'   using \code{levels(tr$trial)}, and identical ordering is assumed.
#' @param px_per_cm Length scaling factor, optional.
#' @param minimal When TRUE, the minimum amount of components (see list below)
#'   are including in the object. When FALSE, the group, pairs and trial
#'   components will already be generated.
#' @return A tracks object.
#'
#' @section Object structure:
#'
#' The tracks object is a \code{list} with the following tables:
#' \describe{
#'   \item{\code{tr}}{\code{party_df} organized by trial, animal, frame.
#'   Useful for storing individual variables such as location, speed,
#'   orientation, distance to object etc.}
#'   \item{\code{group}}{\code{tbl_df} organized by trial, frame. Useful for
#'   storing group level variables, such as polarization, centrality, group
#'   size, mean neighbour distance etc.}
#'   \item{\code{pairs}}{\code{party_df} organized by trial, pair of animals,
#'   frame. Useful for storing social variables, such as distance, relative
#'   orientation, vector correlation etc.}
#'   \item{\code{location}}{\code{tbl_df} organized by trial, xbin, ybin.
#'   Useful for aggregate data for spatial heatmaps etc.}
#'   \item{\code{meta_data}}{\code{tbl_df} organized by trial. Contains
#'   information about the trial, such as group origin, treatment, time of
#'   testing etc.}
#'   \item{\code{animal}}{\code{tbl_df} organized by trial and animal,
#'   containing animal level measurements (nested in trial), such as average
#'   speed.}
#'   \item{\code{trial}}{\code{tbl_df} organized by trial, containing trial
#'   level measurements, such as average speed.}
#'   \item{\code{params}}{A \code{list} with general experiment parameters, such
#'   as frame_rate, scale, resolution etc.}
#'   \item{\code{pr}}{A \code{list} with vectors indicating what calculated
#'   variables are present in the parallelized tables, for internal purposes.}
#' }
#'  \code{tracks}, \code{meta_data}, \code{params} and \code{pr} are always
#'  present, others may not be. This is to preserve memory space when they are
#'  not needed. Order is not garantueed, refer by name if you need to access
#'  them.
#'
#'  Methods for common generics may be available.
#'
#' @section Parallel computing:
#'
#' Trackr utilizes parallel computing as implemented by the multidplyr package.
#' This means both the $tr and $pairs tables are always split by trial, and kept
#' on seperate nodes as so called 'party_df's'. The other tables are typically
#' small enough not to warrant the use of a cluster. If necessary, the tables
#' that are split can be combined again using \code{dplyr::collect()}.
#'
#' @seealso \code{\link{read_idTracker}}, \code{\link{read_Ctrax}}
#' @export
as_tracks <- function(tr, frame_rate, resolution, meta_data = NULL,
                      px_per_cm = NULL, minimal = TRUE) {
  # Input handling -------------------------------------------------------------
  if (!is.data.frame(tr))
    stop('tr has to be a data.frame.', call. = FALSE)
  if (!is.numeric(frame_rate) | length(frame_rate) > 1)
    stop('frame_rate has to be a numeric vector of length 1.', call. = FALSE)

  res <- as.character(resolution)
  Source <- attributes(tr)$source

  # Define bounds (default) #---------------------------------------------------
  if (res %in% c('480', '720', '1080')) {
    ress <- list('480' = c(640, 480),
                 '720' = c(1280, 720),
                 '1080' = c(1920, 1080))
    res <- as.numeric(ress[[res]])
  } else {
    res <- as.numeric(strsplit(res, split = '[xX]')[[1]])
  }
  bounds <- matrix(c(0, 0, 0, res[2], res[1], res[2], res[1], 0), 2,
                   dimnames = list(c('x', 'y'), c('ll', 'ul', 'ur', 'lr')))

  # Check or create meta_data table --------------------------------------------
  if (is.null(meta_data)) {
    if (is.null(tr$trial)) {
      meta_data <- dplyr::data_frame(trial = factor(1))
    } else {
      meta_data <- dplyr::data_frame(trial = levels(tr$trial))
    }
  } else {
    if (is.null(meta_data$trial)) {
      if (nrow(meta_data) != length(levels(tr$trial)))
        stop('Number of rows in meta_data is not equal to the number of
             different trials in tr.', call. = FALSE)
      meta_data$trial <- levels(tr$trial)
    } else {
      if (!setequal(levels(tr$trial), levels(tr$trial)))
        stop('The factor levels of trial in meta_data and tr do not match.',
             call. = FALSE)
      # if we arrive here we just leave meta_data as supplied.
    }
  }

  # Parallel -------------------------------------------------------------------
  pr_tr <- names(tr)
  tr <- multidplyr::partition(tr, trial)

  # Build object and return ----------------------------------------------------
  tracks <- list(tr = tr,
                 meta_data = meta_data,
                 params = list(frame_rate = frame_rate,
                               resolution = res,
                               bounds = bounds,
                               px_per_cm = px_per_cm,
                               source = Source),
                 pr = list(tr = pr_tr))
  tracks$params <- tracks$params[!sapply(tracks$params, is.null)]

  if (minimal) {
    attributes(tracks)$class <- 'tracks'
    return(tracks)
  } else {
    expand_tracks(tracks)
  }
}

#' Add missing components to a tracks object.
#'
#' This function will add the group, pairs and trial components to a track
#' object, if they are currently not included. Note that the pairs component
#' will be increase the size by a lot, if there is more than a few animals per
#' trial.
#'
#' Note that this function is similar to running \code{as_tracks(tracks$tr, ...)
#' }, but safer (i.e. no loss of data).
#'
#' If group, pairs and trial are all false, it will return tracks.
#'
#' @param tracks A tracks object.
#' @param group If TRUE, group will be checked for and added if missing.
#' @param pairs If TRUE, pairs will be checked for and added if missing.
#' @param animal If TRUE, animal will be checked for and added if missing.
#' @param trial If TRUE, trial will be checked for and added if missing.
#'
#' @return A tracks object.
#' @export
#'
#' @examples expand_tracks(guppies)
expand_tracks <- function(tracks,
                          group = TRUE,
                          pairs = TRUE,
                          animal = TRUE,
                          trial = TRUE) {
  Group <- Pairs <- Animal <- Trial <- NULL
  # Build group object ---------------------------------------------------------
  if (group & is.null(tracks$group)) {
    Group <- dplyr::group_by_(tracks$tr, ~frame)
    Group <- dplyr::summarize(Group)
    Group <- dplyr::collect(Group)
  }

  # Build pairs object (becomes very large with many animals) ------------------
  if (pairs & is.null(tracks$pairs)) {
    # Might have to look for a faster way (without groups and using nesting)
    Pairs <- dplyr::mutate_(tracks$tr, .dots = list(animal2 = ~animal))
    Pairs <- dplyr::collect(Pairs)
    Pairs <- tidyr::expand_(Pairs, dots = list(~frame, ~animal, ~animal2))
    Pairs <- dplyr::rename_(Pairs, .dots = list(animal1 = ~animal))
    tracks$pr$pairs <- names(Pairs)
    Pairs <- multidplyr::partition(Pairs, trial, cluster = tracks$tr$cluster)
    Pairs <- dplyr::filter_(Pairs, ~animal1 != animal2)
  }

  #Build animal object ---------------------------------------------------------
  if (animal & is.null(tracks$animal)) {
    Animal <- dplyr::group_by_(tracks$tr, ~animal)
    Animal <- dplyr::summarize(Animal)
    Animal <- dplyr::collect(Animal)
  }

  #Build trial object ----------------------------------------------------------
  if (trial & is.null(tracks$trial)) {
    Trial <- dplyr::summarize(tracks$tr)
    Trial <- dplyr::collect(Trial)
  }

  # Build object and return ----------------------------------------------------
  tracks <- c(tracks,
              list(group = Group),
              list(pairs = Pairs),
              list(animal = Animal),
              list(trial = Trial))
  attributes(tracks)$class <- 'tracks'

  return(tracks)
}

#' @describeIn as_tracks Test if object is of class tracks.
#' @param x An object.
#' @export
is.tracks <- function(x) {
  is(x, 'tracks')
}

#' Report track completeness per trial
#'
#' The report gives two statistics:
#' - complete: which is simply the percentage of frame by animal by trial
#'   combinations for which the vars are there.
#' - per frame: which checks for what percentage of frames all animals are
#' complete. This is useful for social parameters, for which all individuals are
#' needed.
#'
#' If some frames were poorly tracked, the two parameters will be similar. If
#' there are large differences, then there is many frames in which a portion of
#' the individuals gets.
#'
#' @section Note:
#' This is only useful if their are persistent id's across the video. If there
#' are any spurious detections, this will lead to very low scores.
#'
#' @param tracks A tracks object
#' @param vars The variables in the $tr table that need to be present to count
#'   towards completeness.
#' @param lower_limit If a percentage is below this limit, the cell in the table
#'   will be marked with a *.
#'
#' @return Prints a table, silently returns the table too.
#' @export
check_complete <- function(tracks, vars = c('X', 'Y'), lower_limit = 95) {
  tr <- tracks$tr
  tr <- do.call(dplyr::select_,c(list(tr), c('trial', 'frame', 'animal', vars)))
  multidplyr::cluster_assign_value(tr$cluster, 'vars', vars)
  # We left_join a data.frame with the complete range of frames for every animal
  tr <- dplyr::do(tr,
                  dplyr::left_join(
                    data.frame(trial = .$trial[1],
                               frame = min(.$frame):max(.$frame),
                               animal =
                                 rep(unique(.$animal),
                                     max(.$frame) - min(.$frame) + 1)),
                    ., by = c('frame', 'animal', 'trial')))
  # Count the NA's in vars
  tab <- dplyr::do(tr,
                   complete = nrow(.[complete.cases(.), vars]) / nrow(.))
  tab <- dplyr::collect(tab)
  tab <- dplyr::arrange(tab, trial)
  tr <- dplyr::group_by_(tr, ~frame)

  f <- function(dat, var) {
    dots <- setNames(list(lazyeval::interp(~anyNA(v), v = as.name(var))), 'na')
    dplyr::summarize_(dat, .dots = dots)
  }
  l <- lapply(vars, f, dat = tr)
  l <- lapply(l, dplyr::collect)
  l <- dplyr::bind_cols(l[[1]][, c('trial', 'frame')],
                        dplyr::bind_cols(lapply(l, '[', , 'na')))
  names(l) <- 1:ncol(l)
  l <- split(l, l$`1`)
  l <- lapply(l, '[', , c(-1, -2))
  l <- lapply(l, rowSums)
  l <- unlist(lapply(l, function(x) sum(x == 0) / length(x)))
  # Prepare a nice table
  tab2 <- data.frame(tab[, 1], unname(unlist(tab[, 2])), unname(l))
  names(tab2) <- c('Trial', 'Complete', 'Per frame')
  tab3 <- tab2
  tab3[, 2:3] <- round(tab3[, 2:3] * 100, 2)
  tab5 <- tab4 <- tab3
  tab4[, 2:3] <- apply(tab3[, 2:3], 2, paste0, '%')
  for (i in 2:3) tab4[, i] <- ifelse(tab3[, i] > lower_limit, tab3[, i],
                                     paste('*', tab3[, i]))
  for (i in 2:3) tab5[, i] <- ifelse(tab3[, i] > lower_limit, tab3[, i],
                                     paste(tab3[, i], '*'))
  if (requireNamespace('knitr', quietly = TRUE))
    print(knitr::kable(tab5))
  else
    print(tab4)
  return(invisible(tab2))
}

#' Save tracks object
#'
#' This function allows for the writing of a tracks object to disk. The base
#' version of \code{save } cannot be used, since the tr and pairs tables need to
#' be pulled of the cluster.
#'
#' Name will
#'
#' @param tracks A tracks object
#' @param file File name.
#'
#' @return invisible(NULL)
#' @seealso load_tracks
#' @export
save_tracks <- function(tracks, file) {
  tracks$tr <- dplyr::collect(tracks$tr)
  if (!is.null(tracks$pairs)) {
    tracks$pairs <- dplyr::collect(tracks$pairs)
  }
  save(tracks, file = file)
  return(invisible(NULL))
}

#' Load tracks object
#'
#' This function allows for the reading of a tracks object from disk. The base
#' version of \code{load } cannot be used, since the tr and pairs tables need to
#' be reloaded onto a cluster.
#'
#' In contrast to regular \code{load} in \code{base}, you should assign the
#' result.
#'
#' @param tracks A tracks object
#' @param file File name.
#'
#' @return A tracks object.
#' @seealso save_tracks
#' @export
load_tracks <- function(file) {
  tmp_env <- new.env()
  load(file, tmp_env)
  tracks <- get(ls(tmp_env), envir = tmp_env)
  rm(tmp_env)

  tracks$tr <- multidplyr::partition(tracks$tr, trial)
  if (!is.null(tracks$pairs)) {
    tracks$pairs <- multidplyr::partition(tracks$pairs, trial,
                                          cluster = tracks$tr$cluster)
  }
  return(tracks)
}