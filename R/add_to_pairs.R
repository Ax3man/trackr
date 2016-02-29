# Functions that add variables to the $pairs table of a tracks object.

#' Add pairwise distance.
#'
#' Add the distance between centroids for the pair of animals for each entry in
#' the $pairs table.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object
#' @export
add_pair_dist <- function(tracks) {
  if ('dist' %in% tracks$pr$pairs) {
    return(tracks)
  }
  tr <- dplyr::select_(tracks$tr, ~trial, ~animal, ~frame, ~X, ~Y)
  tr <- dplyr::collect(tr)

  cl <- tracks$pairs$cluster
  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$pairs$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))
  tracks$pairs <- dplyr::rename(tracks$pairs, X1 = X, Y1 = Y)
  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))
  tracks$pairs <- dplyr::rename_(tracks$pairs, X2 = ~X, Y2 = ~Y)
  tracks$pairs <- dplyr::mutate_(tracks$pairs,
                                 dist = ~sqrt((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2))
  tracks$pairs <- dplyr::select_(tracks$pairs, ~-X1, ~-Y1, ~-X2, ~-Y2)

  tracks$pr$pairs <- c(tracks$pr$pairs, 'dist')
  return(tracks)
}

#' Add change in distance between pairs.
#'
#' Add the change in distance between the pair of animals for each entry in the
#' $pairs table. Negative values mean the animals are moving closer to each
#' other, postitive values means they are moving apart.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
add_pair_dist_velocity <- function(tracks) {
  if ('dist_velocity' %in% tracks$pr$pairs) {
    return(tracks)
  }
  if (!('dist' %in% tracks$pr$pairs)) {
    message('Adding distance to pairs table first.')
    tracks <- add_pair_dist(tracks)
  }
  tracks$pr$pairs <- c(tracks$pr$pairs, 'dist_velocity')
  add_diff_to_pairs(tracks, 'dist', 'dist_velocity')
}

#' Add acceleration in distance between pairs.
#'
#' Add the change in the change in distance between the pair of animals for each
#' entry in the $pairs table. Positive values mean the animals are increasing
#' the speed at which their distance in changing.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
add_pair_dist_acceleration <- function(tracks) {
  if ('dist_acceleration' %in% tracks$pr$pairs) {
    return(tracks)
  }
  if (!('dist_velocity' %in% tracks$pr$pairs)) {
    message('Adding distance velocity to pairs table first.')
    tracks <- add_pair_dist_velocity(tracks)
  }
  tracks$pr$pairs <- c(tracks$pr$pairs, 'dist_acceleration')
  add_diff_to_pairs(tracks, 'dist_velocity', 'dist_acceleration')
}

#' Add pairwise distance based on tip to ellipse.
#'
#' Add the distance between the pair of animals for each entry in the $pairs
#' table. This distance is from the mouth of fish 1 (tip of the ellipse) to
#' an approximately closest point on the ellipse for fish 2.
#'
#' It does not get the closest point at the moment, as this calculation is quite
#' complicated, but does a numerical approximation based on n points along the
#' ellipse.
#'
#' @param tracks A tracks object.
#' @param n How many
#'
#' @return A tracks object
#' @export
add_pair_nip_dist <- function(tracks, n = 20) {
  if ('nip_dist' %in% tracks$pr$pairs) {
    return(tracks)
  }
  tr <- dplyr::select_(tracks$tr, ~trial, ~animal, ~frame, ~X, ~Y, ~orientation,
                       ~minor_axis, ~major_axis)
  # tr <- dplyr::mutate_(tr, minor_axis = ~minor_axis / 2,
  #                      major_axis = ~major_axis / 2)
  tr <- dplyr::collect(tr)

  cl <- tracks$pairs$cluster
  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$pairs$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)
  multidplyr::cluster_assign_value(cl, 'find_closest_point_on_ellipse',
                                   find_closest_point_on_ellipse)
  multidplyr::cluster_assign_value(cl, 'n', n)

  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))
  tracks$pairs <- dplyr::rename_(tracks$pairs, X1 = ~X, Y1 = ~Y,
                                 orientation1 = ~orientation,
                                 minor_axis1 = ~minor_axis,
                                 major_axis1 = ~major_axis)
  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))
  tracks$pairs <- dplyr::rename_(tracks$pairs, X2 = ~X, Y2 = ~Y,
                                 orientation2 = ~orientation,
                                 minor_axis2 = ~minor_axis,
                                 major_axis2 = ~major_axis)
  tracks$pairs <- dplyr::mutate_(
    tracks$pairs,
    head_X = ~major_axis1 * cos(orientation1) + X1,
    head_Y = ~major_axis1 * sin(orientation1) + Y1,
    closest_X = ~find_closest_point_on_ellipse(head_X, head_Y, X2, Y2,
                                               major_axis2, minor_axis2,
                                               orientation2, n = n, ret = 'x'),
    closest_Y = ~find_closest_point_on_ellipse(head_X, head_Y, X2, Y2,
                                               major_axis2, minor_axis2,
                                               orientation2, n = n, ret = 'y'),
    nip_dist = ~sqrt((head_X - closest_X) ^ 2 + (head_Y - closest_Y) ^ 2))
  tracks$pairs <- dplyr::select_(tracks$pairs, ~-X1, ~-Y1, ~-orientation1,
                                 ~-minor_axis1, ~-major_axis1, ~-X2, ~-Y2,
                                 ~-orientation2,  ~-minor_axis2, ~-major_axis2,
                                 ~-head_X, ~-head_Y, ~-closest_X, ~-closest_Y)

  multidplyr::cluster_rm(cl, c('tr', 'find_closest_point_on_ellipse', 'n'))

  tracks$pr$pairs <- c(tracks$pr$pairs, 'nip_dist')
  return(tracks)
}

#' Add change in pairwise distance based on tip to ellipse.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
#'
#' @seealso add_pair_nip_dist, add_pair_dist_velocity
add_pair_nip_dist_velocity <- function(tracks) {
  if ('nip_dist_velocity' %in% tracks$pr$pairs) {
    return(tracks)
  }
  if (!('nip_dist' %in% tracks$pr$pairs)) {
    message('Adding nip distance to pairs table first.')
    tracks <- add_pair_dist(tracks)
  }
  tracks$pr$pairs <- c(tracks$pr$pairs, 'nip_dist_velocity')
  add_diff_to_pairs(tracks, 'nip_dist', 'nip_dist_velocity')
}

add_diff_to_pairs <- function(tracks, var, name) {
  tracks$pairs <- dplyr::group_by_(tracks$pairs, ~animal1, ~animal2)
  tracks$pairs <- dplyr::mutate_(
    tracks$pairs,
    .dots = setNames(list(lazyeval::interp(~x - dplyr::lag(x, order_by = frame),
                                           x = as.name(var))), name))
  return(tracks)
}
