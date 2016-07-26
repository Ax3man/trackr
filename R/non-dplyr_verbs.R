#' Thin the stored frame rate of a tracks object
#'
#' This function reduces the effective frame rate by dropping every n frame,
#' recoding the frame variable in $tr, $soc and $group and adjusts the
#' frame_rate parameter. Useful for reducing the size of a tracks object. Supply
#' either n or new_frame_rate.
#'
#' @param tracks Tracks object.
#' @param n Thinning interval, every n frames is dropped.
#' @param new_frame_rate New frame rate.
#' @param drop When FALSE, an error will produced if data is found that was
#'   aggregated over frames. If TRUE, that data will be dropped.
#'
#' @return Tracks object.
#' @export
thin_frame_rate <- function(tracks, n = NULL, new_frame_rate = NULL,
                            drop = FALSE) {
  # Input checking -------------------------------------------------------------
  if ((is.null(n) & is.null(new_frame_rate)) |
      (!is.null(n) & !is.null(new_frame_rate)))
    stop('Supply either n or new_frame_rate.', call. = FALSE)
  if (!is.tracks(tracks))
    stop('tracks should be a tracks object.', call. = FALSE)
  if (!is.null(new_frame_rate) & !is.numeric(new_frame_rate) |
      length(new_frame_rate) > 1)
    stop('new_frame_rate should be a numeric vector of length one.',
         call. = FALSE)
  if (!is.null(new_frame_rate))
    n <- tracks$params$frame_rate / new_frame_rate
  if (abs(n - round(n)) > .Machine$double.eps ^ 0.5)
    stop('new_frame_rate should be a factor of the old one, and n should be and
         integer number.')
  # Select new frames ----------------------------------------------------------
  range. <- dplyr::summarise_(tracks$tr, min = ~min(frame), max = ~max(frame))
  range. <- dplyr::collect(range.)
  range. <- c(min(range.$min), max(range.$max))
  frames <- seq.int(range.[1] + n - 1, range.[2], n)
  tracks <- filter_(tracks, drop = ~drop,
                    .dots = lazyeval::interp(~frame %in% x, x = frames))
  # Recalculate frame nrs ------------------------------------------------------
  tracks$tr$frame <- tracks$tr$frame / n
  if (!is.null(tracks$group))
    tracks$group$frame <- tracks$group$frame / n
  if (!is.null(tracks$soc))
    tracks$soc$frame <- tracks$soc$frame / n
  tracks$params$frame_rate <- new_frame_rate

  return(tracks)
}

#' Retrieve track sections based on conditions from multiple tables
#'
#' Similar to \code{filter}, but does not filter on each table seperately, but
#' filters frames for which all conditions from seperate tables apply. That is,
#' one can filter frames based on a variable in e.g. the \code{tr} table and
#' \code{tr}, \code{soc} and \code{group} will be filtered for the frames where
#' that condition is \code{TRUE}.
#'
#' It returns a list with tracks tables as well as a summary table of sequences.
#' The first is named \code{tracks}, tables are grouped appropriately. The
#' second list enty is named \code{sections} and provides an overview of the
#' track sections that were found. It is grouped by trial, with a \code{start},
#' \code{stop} and \code{length} column.
#'
#' The function supports a tolerance level which allows for combining sequences
#' that are close together.
#'
#' Any variables used as conditions will be looked up in the \code{tr},
#' \code{soc} and \code{group} tables and applied when present.
#'
#' @section Example use case:
#'
#' If one wants to select all sequences of frames where
#' animals are chasing each other (to plot them, for example), one could filter
#' for a high mean speed of the pair, and a small pairwise distance.
#'
#' @param tracks A tracks object.
#' @param ... Conditions.
#' @param tol Combine sequences that are \code{tol} frames apart.
#' @param pad Add padding of this many frames around each section. This allows
#'   for capturing context around sections of interest.
#' @param add_times When \code{TRUE}, three columns will be added to the
#'   sections table that give human readable times, instead of only frame
#'   numbers.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse")
#'   for details.
#'
#' @return A tbl_df.
#' @export
find_sections <- function(tracks, ..., tol = 1, pad = 0, add_times = TRUE) {
  find_sections_(tracks, tol = tol, pad = pad, add_times = add_times,
                 .dots = lazyeval::lazy_dots(...))
}

#' @describeIn find_sections Retrieve the timestamps for track section based on
#'   conditions.
#' @export
find_sections_ <- function(tracks, ..., tol = 1, pad = 0, add_times = TRUE,
                           .dots) {
  # Setup ----------------------------------------------------------------------
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  tables <- find_conds_in_tables(list(tr = tracks$tr, soc = tracks$soc,
                                      group = tracks$group, pr = tracks$pr),
                                 conds)
  if (pad >= tol) {
    stop('pad is equal or larger than tol. This can cause sections with overlapping frames, which is not allowed.',
            .call = FALSE)
  }
  present <- names(tracks)[(names(tracks) %in% c('tr', 'soc', 'group'))]
  frame_rate <- tracks$params$frame_rate
  cl <- tracks$tr$cluster
  # Filter tracks tables on conds ----------------------------------------------
  frames <- tracks <- tracks[present]
  for (i in seq_along(conds)) {
    frames[tables[[i]]] <- lapply(frames[tables[[i]]], dplyr::filter_,
                                  .dots = conds[i])
  }
  frames <- lapply(frames, dplyr::collect)
  # Find common frames ---------------------------------------------------------
  if (length(frames) == 1) {
    frames <- unlist(frames)
  } else {
    if (length(frames) == 2) {
      frames <- dplyr::inner_join(frames[[1]], frames[[2]],
                                  by = c('trial', 'frame'))
    } else {
      frames <- dplyr::inner_join(
        dplyr::inner_join(frames[[1]], frames[[2]], by = c('trial', 'frame')),
        frames[[3]],
        by = c('trial', 'frame'))
    }
  }
  frames <- dplyr::ungroup(frames)
  frames <- dplyr::select_(frames, ~trial, ~frame)
  frames <- dplyr::distinct(frames)
  frames <- dplyr::arrange_(frames, ~trial, ~frame)
  frames <- add_section_numbers(frames, tol)
  frames <- dplyr::select_(frames, ~trial, ~frame, ~section)
  frames <- dplyr::group_by_(frames, ~trial, ~section)
  frames <- dplyr::do_(frames, ~pad_frames(., pad))
  frames <- dplyr::distinct_(frames, .dots = list(~trial, ~frame))
  ref <- frames <- add_section_numbers(frames, tol)
  frames <- dplyr::group_by_(frames, ~trial, ~section)
  # browser()
  frames <- dplyr::summarize_(frames,
                              start = ~min(frame),
                              end = ~max(frame),
                              length = ~end - start + 1)
  # Filter all tables on common those frames -----------------------------------
  fr <- split(frames, frames$trial)
  fr <- lapply(fr,
               function(.) {
                 unlist(mapply(`:`, .$start, .$end, SIMPLIFY = FALSE))
               } )
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'fr', fr)
  for (i in seq_along(tracks)) {
    tracks[[i]] <- lapply(1:length(fr),
                          function(j) {
                            multidplyr::cluster_assign_value(cl, 'j', j)
                            dots <- list(lazyeval::interp(~trial == x,
                                                          x = names(fr)[j]),
                                         lazyeval::interp(~frame %in% fr[[j]]))
                            dplyr::filter_(tracks[[i]], .dots = dots)
                          } )
  }
  multidplyr::cluster_rm(cl, c('fr', 'j'))
  tracks <- lapply(tracks, lapply, dplyr::collect)
  tracks <- lapply(tracks, function(.) dplyr::bind_rows(.[!is.null(.)]))
  tracks <- add_sections_by_reference(tracks, ref, pad)
  # Prepare nice output with grouped tables and human readable times -----------
  tracks$tr <- dplyr::group_by_(tracks$tr, ~trial, ~section, ~animal)
  if (!is.null(tracks$soc)) {
    tracks$soc <- dplyr::group_by_(tracks$soc, ~trial, ~section, ~animal1,
                                   ~animal2)
  }
  if (!is.null(tracks$group)) {
    tracks$group <- dplyr::group_by_(tracks$group, ~trial, ~section)
  }
  if (add_times) {
    get_time <- function(x) {
      as.character(round(frames_to_times(x, frame_rate)))
    }
    frames <- dplyr::mutate_(frames,
                             start_t = ~get_time(start),
                             end_t = ~get_time(end),
                             length_t = ~get_time(length))
  }
  return(list(tracks = tracks, sections = frames))
}

pad_frames <- function(tbl, pad) {
  dplyr::bind_rows(
    data.frame(trial = tbl$trial[1],
               frame = (tbl$frame[1] - pad):(tbl$frame[1] - 1),
               section = tbl$section[1]),
    tbl,
    data.frame(trial = tbl$trial[1],
               frame = (tbl$frame[nrow(tbl)] + 1):(tbl$frame[nrow(tbl)] + pad),
               section = tbl$section[1])
  )
}

add_section_numbers <- function(tbl, tol) {
  tbl <- dplyr::group_by_(tbl, ~trial)
  tbl <- dplyr::mutate_(tbl,
                        dif = ~ifelse(row_number() == 1,
                                      Inf,
                                      frame - dplyr::lag(frame, 1) - 1),
                        gap = ~ifelse(dif > tol, TRUE, FALSE))
  tbl <- dplyr::filter_(tbl, ~!is.na(gap))
  tbl <- dplyr::mutate_(tbl, section = ~cumsum(gap))
  dplyr::select_(tbl, ~-dif, ~-gap)
}

add_sections_by_reference <- function(tracks, ref, pad) {
  tracks <- lapply(tracks, dplyr::left_join, ref, by = c('trial', 'frame'))
  # Now we need to deal with padding
  find_nearest_section <- function(v) {
    leads <- apply(sapply(1:pad, function(x) dplyr::lead(v, x)),
                   1, function(y) y[which.min(is.na(y))])
    lags <- apply(sapply(1:pad, function(x) dplyr::lag(v, x)),
                  1, function(y) y[which.min(is.na(y))])
    ifelse(!is.na(v), v, ifelse(!is.na(leads), leads, lags))
  }

  tracks <- lapply(tracks, dplyr::group_by_, ~trial)
  tracks <- lapply(tracks, function(tr) {
    dplyr::mutate_(tr, section = ~find_nearest_section(section))
  } )
  return(tracks)
}

#' Summarize track sections.
#'
#' Process the output of \code{find_sections} by summarizing each tracks
#' portion, and joing that to the sections table.
#'
#' Multiple rows per section can be created, as grouping is preserved in the
#' summary. By default, \code{tr} is grouped by animal and \code{soc} is grouped
#' by pair. Adjust grouping beforehand if other results are required.
#'
#' @param sections Ouput from \code{find_sections} (a list of length 2).
#' @param ... Summary statements.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse")
#'   for details.
#'
#' @return A tbl_df.
#' @export
summarise_sections <- function(sections, ...) {
  summarise_sections_(sections, .dots = lazyeval::lazy_dots(...))
}

#' @describeIn summarise_sections Retrieve the timestamps for track section
#'   based on conditions.
#' @export
summarize_sections <- function(sections, ...) {
  summarise_sections_(sections, .dots = lazyeval::lazy_dots(...))
}

#' @describeIn summarise_sections Retrieve the timestamps for track section
#'   based on conditions.
#' @export
summarize_sections_ <- function(sections, ..., .dots) {
  summarise_sections_(sections, ..., .dots)
}

#' @describeIn summarise_sections Retrieve the timestamps for track section
#'   based on conditions.
#' @export
summarise_sections_ <- function(sections, ..., .dots) {
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  tracks <- sections$tracks
  sections <- sections$sections
  conds_tables <- find_conds_in_tables(tracks, conds)
  tables <- unique(unlist(conds_tables))
  tracks <- tracks[tables]

  for (i in seq_along(tracks)) {
    tracks[[i]] <- dplyr::summarize_(
      tracks[[i]], .dots = conds[which(conds_tables == tables[i])])
  }
  # Join all results
  tracks <- Reduce(
    function(...) dplyr::full_join(..., by = c('trial', 'section')), tracks)
  tracks$section <- as.numeric(tracks$section)
  sections <- dplyr::full_join(sections, tracks, by = c('trial', 'section'))
  sections <- dplyr::arrange_(sections, ~trial, ~section)
  return(sections)
}

#' Collapse multiple identies in fewer.
#'
#' This function allows one to assign several duplicated identities over time
#' to a fewer number of actual animals. This is useful when tracking software
#' does not keep track of consisent identities, and it occasionally loses and
#' rediscovers animals. When more identities exist within a frame than there is
#' animals, those frames are lost.
#'
#' @param tracks A \code{tracks} object.
#' @param n The number of animals. Currently only 1 is supported.
#'
#' @return A \code{tracks} object.
#' @export
collapse_identities <- function(tracks, n = 1) {
  tracks <- tracks[c('tr', 'meta_data', 'params', 'pr')]
  class(tracks) <- 'tracks'
  if (n != 1) {
    stop('Only n = 1 supported for now.', call. = FALSE)
  }
  r <- dplyr::summarize_(tracks$tr, start = ~min(frame), end = ~max(frame))
  r <- dplyr::collect(r)
  r <- dplyr::arrange_(r, ~trial, ~start)
  r <- invisible(dplyr::mutate_(r,
                      end_overlap = ~end - dplyr::lead(start) > 0,
                      start_new = ~dplyr::if_else(dplyr::lag(end_overlap),
                                                  dplyr::lag(end), start),
                      r = ~row_number(),
                      start_new = ~dplyr::if_else(r == 1, start,
                                                  start_new),
                      end_new = ~dplyr::if_else(end_overlap,
                                                dplyr::lead(start), end)))
  r <- dplyr::select_(r, ~-start, ~-end, ~-end_overlap, ~-r)
  r <- dplyr::filter_(r, ~start_new < end_new)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'r', r)

  multidplyr::cluster_eval_(
    tracks$tr$cluster,
    lazyeval::interp(quote(
      assign(x, dplyr::left_join(x2, r))),
      x = tracks$tr$name, x2 = as.name(tracks$tr$name)))
  multidplyr::cluster_rm(tracks$tr$cluster, 'r')

  multidplyr::cluster_eval_(
    tracks$tr$cluster,
    lazyeval::interp(quote(
      assign(x, dplyr::filter_(x2, ~frame > start_new, ~frame < end_new))),
      x = tracks$tr$name, x2 = as.name(tracks$tr$name)))

  multidplyr::cluster_eval_(
    tracks$tr$cluster,
    lazyeval::interp(quote(
      assign(x, dplyr::select_(x2, ~-start_new, ~-end_new))),
      x = tracks$tr$name, x2 = as.name(tracks$tr$name)))

  tracks$tr <- dplyr::mutate_(tracks$tr, animal = ~factor(1))

  return(tracks)
}
