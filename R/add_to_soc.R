# Functions that add variables to the $soc table of a tracks object.

#' Add pairwise distance.
#'
#' Add the distance between centroids for the pair of animals for each entry in
#' the $soc table.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object
#' @export
add_pair_dist <- function(tracks) {
  if ('dist' %in% tracks$pr$soc) {
    return(tracks)
  }
  tracks <- join_tr_to_soc(tracks, list(~X, ~Y))

  tracks$soc <- dplyr::mutate_(tracks$soc,
                               pair_dist = ~sqrt((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2))
  tracks$soc <- dplyr::select_(tracks$soc, ~-X1, ~-Y1, ~-X2, ~-Y2)

  tracks$pr$soc <- c(tracks$pr$soc, 'pair_dist')
  return(tracks)
}

#' Add change in distance between pairs.
#'
#' Add the change in distance between the pair of animals for each entry in the
#' $soc table. Negative values mean the animals are moving closer to each
#' other, postitive values means they are moving apart.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
add_pair_dist_velocity <- function(tracks) {
  if ('pair_dist_velocity' %in% tracks$pr$soc) {
    return(tracks)
  }
  if (!('pair_dist' %in% tracks$pr$soc)) {
    message('Adding distance to soc table first.')
    tracks <- add_pair_dist(tracks)
  }
  tracks$pr$soc <- c(tracks$pr$soc, 'pair_dist_velocity')
  add_diff_to_pairs(tracks, 'pair_dist', 'pair_dist_velocity')
}

#' Add acceleration in distance between pairs
#'
#' Add the change in the change in distance between the pair of animals for each
#' entry in the $soc table. Positive values mean the animals are increasing
#' the speed at which their distance in changing.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
add_pair_dist_acceleration <- function(tracks) {
  if ('pair_dist_acceleration' %in% tracks$pr$soc) {
    return(tracks)
  }
  if (!('pair_dist_velocity' %in% tracks$pr$soc)) {
    message('Adding distance velocity to soc table first.')
    tracks <- add_pair_dist_velocity(tracks)
  }
  tracks$pr$soc <- c(tracks$pr$soc, 'pair_dist_acceleration')
  add_diff_to_pairs(tracks, 'pair_dist_velocity', 'pair_dist_acceleration')
}

#' Add pairwise distance based on tip to ellipse.
#'
#' Add the distance between the pair of animals for each entry in the $soc
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
add_nip_dist <- function(tracks, n = 20) {
  if ('nip_dist' %in% tracks$pr$soc) {
    return(tracks)
  }
  tracks <- join_tr_to_soc(tracks, list(~X, ~Y, ~orientation, ~minor_axis,
                                        ~major_axis))

  cl <- tracks$soc$cluster
  multidplyr::cluster_assign_value(cl, 'find_closest_point_on_ellipse',
                                   find_closest_point_on_ellipse)
  multidplyr::cluster_assign_value(cl, 'n', n)

  tracks$soc <- dplyr::mutate_(
    tracks$soc,
    head_X = ~major_axis1 * cos(orientation1) + X1,
    head_Y = ~major_axis1 * sin(orientation1) + Y1,
    closest_X = ~find_closest_point_on_ellipse(head_X, head_Y, X2, Y2,
                                               major_axis2, minor_axis2,
                                               orientation2, n = n, ret = 'x'),
    closest_Y = ~find_closest_point_on_ellipse(head_X, head_Y, X2, Y2,
                                               major_axis2, minor_axis2,
                                               orientation2, n = n, ret = 'y'),
    nip_dist = ~sqrt((head_X - closest_X) ^ 2 + (head_Y - closest_Y) ^ 2))
  tracks$soc <- dplyr::select_(tracks$soc, ~-X1, ~-Y1, ~-orientation1,
                               ~-minor_axis1, ~-major_axis1, ~-X2, ~-Y2,
                               ~-orientation2,  ~-minor_axis2, ~-major_axis2,
                               ~-head_X, ~-head_Y, ~-closest_X, ~-closest_Y)

  multidplyr::cluster_rm(cl, c('find_closest_point_on_ellipse', 'n'))

  tracks$pr$soc <- c(tracks$pr$soc, 'nip_dist')
  return(tracks)
}

#' Add change in pairwise distance based on tip to ellipse.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object.
#' @export
#'
#' @seealso add_nip_dist, add_pair_dist_velocity
add_nip_dist_velocity <- function(tracks) {
  if ('nip_dist_velocity' %in% tracks$pr$soc) {
    return(tracks)
  }
  if (!('nip_dist' %in% tracks$pr$soc)) {
    message('Adding nip distance to pairs table first.')
    tracks <- add_pair_dist(tracks)
  }
  tracks$pr$soc <- c(tracks$pr$soc, 'nip_dist_velocity')
  add_diff_to_pairs(tracks, 'nip_dist', 'nip_dist_velocity')
}

add_diff_to_pairs <- function(tracks, var, name) {
  tracks$soc <- dplyr::group_by_(tracks$soc, ~animal1, ~animal2)
  tracks$soc <- dplyr::mutate_(
    tracks$soc,
    .dots = setNames(list(lazyeval::interp(~x - dplyr::lag(x, order_by = frame),
                                           x = as.name(var))), name))
  return(tracks)
}
