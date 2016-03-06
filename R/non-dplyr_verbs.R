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
