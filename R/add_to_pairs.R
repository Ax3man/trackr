#' Add pairwise distance
#'
#' Add the distance between the pair of animals for each entry in the $pairs
#' table.
#'
#' @param tracks A tracks object.
#'
#' @return A tracks object
#' @export
add_pair_dist <- function(tracks) {
  if (!is.null(tracks$pairs$dist))
    return(tracks)
  tr <- tracks$tr[, c('trial', 'animal', 'frame', 'X', 'Y')]
  pairs <- dplyr::left_join(tracks$pairs, tr,
                            by = c('trial', 'frame', 'animal1' = 'animal'))
  names(pairs)[c(ncol(pairs) - 1, ncol(pairs))] <- c('X1', 'Y1')
  pairs <- dplyr::left_join(pairs, tr,
                            by = c('trial', 'frame', 'animal2' = 'animal'))
  names(pairs)[c(ncol(pairs) - 1, ncol(pairs))] <- c('X2', 'Y2')
  pairs$dist <- with(pairs, sqrt((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2))
  tracks$pairs <- pairs[, -which(names(pairs) %in% c('X1', 'Y1', 'X2', 'Y2'))]

  return(tracks)
}

#' Add change in distance between pairs
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
  if (!is.null(tracks$pairs$dist_velocity))
    return(tracks)
  if (is.null(tracks$pairs$dist)) {
    message('Adding distance to pairs table first.')
    tracks <- add_pair_dist(tracks)
  }
  add_diff_to_pairs(tracks, 'dist', 'dist_velocity')
}

#' Add acceleration in distance between pairs
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
  if (!is.null(tracks$pairs$dist_acceleration))
    return(tracks)
  if (is.null(tracks$pairs$dist_velocity)) {
    message('Adding distance to pairs table first.')
    tracks <- add_pair_dist_velocity(tracks)
  }
  add_diff_to_pairs(tracks, 'dist_velocity', 'dist_acceleration')
}

add_diff_to_pairs <- function(tracks, var, name) {
  tracks$pairs <- dplyr::group_by_(tracks$pairs, ~trial, ~animal1, ~animal2)
  tracks$pairs <- dplyr::arrange_(tracks$pairs, ~frame)
  tracks$pairs <- dplyr::mutate_(tracks$pairs, .dots =
                     setNames(list(
                       lazyeval::interp(~lag(x) - x, x = as.name(var))), name))
  return(tracks)
}
