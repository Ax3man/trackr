#' Thin the stored frame rate of a tracks object
#'
#' This function reduces the effective frame rate by dropping every n frame,
#' recoding the frame variable in $tr, $pairs and $group and adjusts the
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
  if (!is.null(tracks$pairs))
    tracks$pairs$frame <- tracks$pairs$frame / n
  tracks$params$frame_rate <- new_frame_rate

  return(tracks)
}

#' Retrieve the timestamps for track sections based on conditions.
#'
#' Similar to \code{filter}, but returns a table of sequences grouped by trial,
#' with a \code{start}, \code{stop} and \code{length} column. It supports a
#' tolerance level which allows for combining sequences that are close together
#' (the default of 1 is zero tolerance).
#'
#' Any variables used as conditions will be looked up in the \code{tr},
#' \code{pairs} and \code{group} tables and applied when present. Non-existing
#' variables will not produce an error.
#'
#' Seperate conditions on different variables with a \code{,}, not \code{&}.
#' Combine several conditions on the same variable with \code{&}.
#'
#' Example use case: If one wants to select all sequences of frames where
#' animals are chasing each other (to plot them, for example), one could filter
#' for a high mean speed of the pair, and a small pairwise distance.
#'
#' @param tracks A tracks object.
#' @param ... Conditions.
#' @param tol Combine sequences that are \code{tol} frames apart.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse")
#'   for details.
#'
#' @return A tbl_df.
#' @export
find_sections <- function(tracks, ..., tol = 0) {
  find_track_sections_(tracks, tol = tol, .dots = lazyeval::lazy_dots(...))
}

#' @describeIn summarize_sections Retrieve the timestamps for track section
#'   based on conditions.
#' @export
find_sections_ <- function(tracks, ..., tol = 1, .dots) {
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  vars <- sapply(strsplit(names(conds), ' '), '[', 1)
  present <- names(tracks)[(names(tracks) %in% c('tr', 'pairs', 'group'))]

  if (length(vars) > length(unique(vars))) {
    stop("Combine different conditions for the same variable with '&', instead
       of using seperate ... arguments.")
  }

  # Apply filter to all tables -------------------------------------------------
  frames <- lapply(tracks[present], function(d, conds, vars) {
    var_d <- switch(class(d)[1],
                    'party_df' = get_party_df_names(d),
                    'tbl_df' = names(d),
                    return(d))

    conds2 <- conds[which(vars %in% var_d)]
    if (length(conds2) > 0) {
      d <- dplyr::filter_(d, .dots = conds2)
    }
    if ('party_df' %in% class(d)) {
      d <- dplyr::collect(d)
    }
    dplyr::select_(d, ~trial, ~frame)
  }, conds = conds, vars = vars)

  # Find common frames ---------------------------------------------------------
  if (length(present) == 1) {
    frames <- unlist(frames)
  } else {
    if (length(present) == 2) {
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
  frames <- dplyr::group_by_(frames, ~trial)

  frames <- dplyr::mutate_(frames,
                           dif = ~ifelse(row_number() == 1,
                                         Inf,
                                         frame - dplyr::lag(frame, 1)),
                           gap = ~ifelse(dif > tol, TRUE, FALSE))
  frames <- dplyr::filter_(frames, ~!is.na(gap))
  frames <- dplyr::mutate_(frames, seq = ~cumsum(gap))
  frames <- dplyr::select_(frames, ~trial, ~frame, ~seq)
  frames <- dplyr::group_by_(frames, ~trial, ~seq)
  frames <- dplyr::summarize_(frames,
                              start = ~min(frame),
                              end = ~max(frame),
                              length = ~end - start + 1)
}

#' Summarize track sections.
#'
#' Similar to \code{filter}, but returns a table of sequences grouped by trial,
#' with a \code{start}, \code{stop} and \code{length} column. It supports a
#' tolerance level which allows for combining sequences that are close together
#' (the default of 1 is zero tolerance).
#'
#' Any variables used as conditions will be looked up in the \code{tr},
#' \code{pairs} and \code{group} tables and applied when present. Non-existing
#' variables will not produce an error.
#'
#' Seperate conditions on different variables with a \code{,}, not \code{&}.
#' Combine several conditions on the same variable with \code{&}.
#'
#' Example use case: If one has selected all sequences of frames where animals
#' are chasing each other, one can calculate the mean speed for each of the
#' chase sequences.
#'
#' @param tracks A table with track sections (output from
#'   \code{find_track_sections}).
#' @param tracks A tracks object.
#' @param ... Summary statements.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse")
#'   for details.
#'
#' @return A tbl_df.
#' @export
summarize_sections <- function(sections, tracks, ...) {
  summarize_sections_(sections, tracks, .dots = lazyeval::lazy_dots(...))
}

#' @describeIn summarize_sections Retrieve the timestamps for track section
#'   based on conditions.
#' @export
summarize_sections_ <- function(sections, tracks, ..., .dots) {
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  vars <- sapply(strsplit(names(conds), ' '), '[', 1)
  present <- names(tracks)[(names(tracks) %in% c('tr', 'pairs', 'group'))]
}


