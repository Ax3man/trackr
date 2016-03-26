# Functions that add variables to the $tr table of a tracks object

#' Add speed to tracks object.
#'
#' @param tracks A tracks object.
#'
#' @return tracks object
#' @export
add_speed <- function(tracks) {
  if ('speed' %in% tracks$pr$tr) {
    return(tracks)
  }
  frame_rate <- tracks$params$frame_rate
  scale <- ifelse(is.null(tracks$params$scale), 1, tracks$params$scale)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'frame_rate', frame_rate)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'scale', scale)

  tracks$tr <- dplyr::group_by_(tracks$tr, ~animal)
  tracks$tr <- dplyr::mutate_(tracks$tr,
                              prev_X = ~dplyr::lag(X, order_by = frame),
                              prev_Y = ~dplyr::lag(Y, order_by = frame),
                              prev_frame = ~dplyr::lag(frame, order_by = frame))
  tracks$tr <- dplyr::mutate_(
    tracks$tr,
    speed = ~ifelse(frame - prev_frame == 1,
                    sqrt((X - prev_X) ^ 2 + (Y - prev_Y) ^ 2) / frame_rate * scale,
                    NA)) # we use order_by since arrange doesnt work on party_df
  tracks$tr <- dplyr::select_(tracks$tr, ~-prev_X, ~-prev_Y, ~-prev_frame)
  tracks$tr <- dplyr::group_by_(tracks$tr, ~trial)

  multidplyr::cluster_rm(tracks$tr$cluster, c('frame_rate', 'scale'))

  tracks$pr$tr <- c(tracks$pr$tr, 'speed')
  return(tracks)
}

#' Add acceleration to tracks object.
#'
#' @param tracks A tracks object.
#'
#' @return tracks object
#' @export
add_acceleration <- function(tracks) {
  if ('acceleration' %in% tracks$pr$tr) {
    return(tracks)
  }
  if (!('speed' %in% tracks$pr$tr)) {
    message('Adding speed to tracks object first.')
    tracks <- add_speed(tracks)
  }
  tracks <- add_diff_to_track(tracks, 'speed', 'acceleration')
  tracks$pr$tr <- c(tracks$pr$tr, 'acceleration')
  return(tracks)
}

#' Add turning angle to tracks object.
#'
#' This adds the turning angle of the path, that is, based on coordinates, not
#' orientation.
#'
#' @param tracks A tracks object.
#'
#' @return tracks object
#' @export
add_turn <- function(tracks) {
  if ('turn' %in% tracks$pr$tr)
    return(tracks)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'angle_diff', angle_diff)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'angle', angle)
  tracks$tr <- dplyr::group_by_(tracks$tr, ~trial, ~animal)
  tracks$tr <- dplyr::mutate_(
    tracks$tr,
    'turn' = ~ifelse(
      frame - dplyr::lag(frame, order_by = frame) == 1,
      angle_diff(angle(dplyr::lag(X, order_by = frame),
                       dplyr::lag(Y, order_by = frame), X, Y),
                 angle(X, Y, dplyr::lead(X, order_by = frame),
                       dplyr::lead(Y, order_by = frame))),
      NA))
  tracks$tr <- dplyr::group_by_(tracks$tr, ~trial)
  multidplyr::cluster_rm(tracks$tr$cluster, c('angle_diff', 'angle'))

  tracks$pr$tr <- c(tracks$pr$tr, 'turn')
  return(tracks)
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
change <- function(x, frame, n = 1) {
  ifelse(frame - dplyr::lag(frame, n = n, order_by = frame) == n,
         x - dplyr::lag(x, n = n, order_by = frame),
         NA)
}

