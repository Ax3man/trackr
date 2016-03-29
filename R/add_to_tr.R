#' Calculate distance.
#'
#' @param x X coordinates.
#' @param y Y coordinates.
#' @param f Always use 'f' or 'frame' (equivalent). Used to order by frame
#'   before calculation.
#'
#' @return A vector of distances in px.
#' @export
distance <- function(x, y, f) {
  sqrt(change(x, f) ^ 2 + change(y, f) ^ 2)
}

#' Calculate speed.
#'
#' @inheritParams distance
#' @return A vector of accelerations in px per frame.
#' @export
speed <- function(x, y, f) {
  change(distance(y, x, f), f)
}

#' Calculate acceleration.
#'
#' @inheritParams distance
#' @return A vector of accelerations in px per frame^2.
#' @export
acceleration <- function(x, y, f) {
  change(change(distance(y, x, f), f), f)
}

#' Calculate turning angle.
#'
#' This adds the turning angle of the path, that is, based on coordinates, not
#' orientation.
#' @inheritParams distance
#' @export
turn <- function(x, y, f) {
  ifelse(
    f - dplyr::lag(f, order_by = f) == 1,
    angle_diff(angle(dplyr::lag(x, order_by = f),
                     dplyr::lag(y, order_by = f), x, y),
               angle(x, y, dplyr::lead(x, order_by = f),
                     dplyr::lead(y, order_by = f))),
    NA)
}

#' Safely calculate the change over time for a variable.
#'
#' Use this convenience function within calls to mutate_.tracks.
#'
#' @param x A vector.
#' @param n Time lag in frames.
#'
#' @return A vector.
#' @export
change <- function(x, f, n = 1) {
  ifelse(f - dplyr::lag(f, n = n, order_by = f) == n,
         x - dplyr::lag(x, n = n, order_by = f),
         NA)
}

