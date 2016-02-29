# Functions that add variables to the $tr table of a tracks object

#' Internal function to add a new calculated variable calculation to a tracks
#' object.
#'
#' This function will take .calc and apply it to the tr portion of the tracks
#' object, feeding it portions of tr grouped by trial and animal. It will then
#' summarize this new variable to the other parts of the tracks object using
#' .summary (typically this would be mean, or median etc.).
#'
#' @section Important Note:
#'  .calc should return something of the form:
#'      \code{dplyr::tbl_df(data.frame(d, var = x))} and that name var should be
#'  passed to this function as the first argument in .names.
#'
#' @param tracks A tracks object.
#' @param .calc A function of the form function(tracks, ...), which returns a
#'   a data frame. Should return tr plus any new columns necessary.
#' @param ... Any extra arguments to supply to .calc.
#'
#' @return tracks object
#' @importFrom dplyr '%>%'
add_to_tr <- function(tracks, .calc, ...) {
  tracks$tr <- tracks$tr %>%
    dplyr::group_by_(~trial, ~animal) %>%
    dplyr::do_(~.calc(., ...)) %>%
    dplyr::ungroup(.)
  return(tracks)
}

#' Add speed to tracks object
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

  multidplyr::cluster_rm(tracks$tr$cluster, c('frame_rate', 'scale'))

  tracks$pr$tr <- c(tracks$pr$tr, 'speed')
  return(tracks)
}

#' Add acceleration to tracks object
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

#' Add turning angle to tracks object
#'
#' This adds the turning angle of the path, that is, based on coordinates, not
#' orientation.
#'
#' @param tracks A tracks object.
#'
#' @return tracks object
#' @export
#'
#' @examples add_turn(guppies)
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

  multidplyr::cluster_rm(tracks$tr$cluster, c('angle_diff', 'angle'))

  tracks$pr$tr <- c(tracks$pr$tr, 'turn')
  return(tracks)
}

add_diff_to_track <- function(tracks, var, name) {
  tracks$tr <- dplyr::group_by_(tracks$tr, ~trial, ~animal)
  tracks$tr <- dplyr::mutate_(
    tracks$tr,
    .dots = setNames(list(lazyeval::interp(~x - dplyr::lag(x, order_by = frame),
                                           x = as.name(var))), name))
  return(tracks)
}

