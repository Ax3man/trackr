#' Add select variables from the tr table to the soc table.
#'
#' Use this function if you need access to track variables while computing
#' social variables. Each variable will be split into two columns, for fish1 and
#' fish2, with the numbers 1 and 2 amended. E.g. joining X and Y will generate
#' four new columns in \code{soc}, X1, X2, Y1, Y2.
#'
#' @param tracks A tracks object.
#' @param ... Which variables to join.
#'
#' @return A tracks object.
#' @export
join_tr_to_soc <- function(tracks, ...) {
  join_tr_to_soc_(tracks, .dots = lazyeval::lazy_dots(...))
}

join_tr_to_soc_ <- function(tracks, ..., .dots) {
  select <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  cl <- tracks$tr$cluster

  tr <- dplyr::select_(tracks$tr, .dots = c(~trial, ~animal, ~frame, select))
  tr <- dplyr::collect(tr)

  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$soc$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))

  Names1 <- setNames(names(select), paste0(names(select), 1))
  tracks$soc <- dplyr::rename_(tracks$soc, .dots = Names1)

  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))
  multidplyr::cluster_rm(cl, '.tr')
  Names2 <- setNames(names(select), paste0(names(select), 2))
  tracks$soc <- dplyr::rename_(tracks$soc, .dots = Names2)

  tracks$pr$soc <- c(tracks$pr$soc, names(Names1), names(Names2))
  return(tracks)
}

#' Calculate pairwise distance.
#'
#' Calculate the distance between centroids for a pair of animals.
#'
#' @param x1 x-coordinate for fish 1.
#' @param x2 x-coordinate for fish 2.
#' @param y1 y-coordinate for fish 1.
#' @param y2 y-coordinate for fish 2.
#'
#' @return A vector of distances in pixels.
#' @export
pair_dist <- function(x1, x2, y1, y2) {
  sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}
..pair_dist.. <- ~pair_dist(X1, X2, Y1, Y2)

#' Calculate change in distance between pairs.
#'
#' Calculate the change in distance between the pair of animals. Negative values
#' mean the animals are moving closer to each other, postitive values means they
#' are moving apart.
#'
#' @inheritParams pair_dist
#' @inheritParams distance
#'
#' @return A vector of velocities in pixels per frame.
#' @export
pair_dist_velocity <- function(x1, x2, y1, y2, f) {
  change(pair_dist(x1, x2, y1, y2), f)
}
..pair_dist_velocity.. <- ~pair_dist_velocity(X1, X2, Y1, Y2, frame)

#' Calculate accereleration in distance between pairs.
#'
#' Calculate the change in relative velocity between the pair of animals.
#' Negative values mean the animals are decreasing the rate at which their
#' distance changes, postitive values means an increase.
#'
#' @inheritParams pair_dist
#' @inheritParams distance
#'
#' @return A vector of accelerations in pixels per frame^2.
#' @export
pair_dist_acceleration <- function(x1, x2, y1, y2, f) {
  change(change(pair_dist(x1, x2, y1, y2), f), f)
}
..pair_dist_acceleration.. <- ~pair_dist_acceleration(X1, X2, Y1, Y2, frame)

#' Add pairwise distance based on tip to ellipse.
#'
#' Add the distance between the pair of animals for each entry in the $soc
#' table. This distance is from the mouth of fish 1 (tip of the ellipse) to
#' an approximately closest point on the ellipse for fish 2.
#'
#' It does not get the closest point, as this calculation is quite involved, but
#' does a numerical approximation based on n points along the ellipse.
#'
#' @param tracks A tracks object.
#' @param n How many points along the ellipse are used for the approximation.
#'
#' @return A tracks object
#' @export
nip_dist <- function(x1, x2, y1, y2, minor_axis1, minor_axis2, major_axis1,
                         major_axis2, orientation1, orientation2, n = 20) {
  # Vector of angles that need to be tested.
  theta <- seq(0, 2 * pi, length.out = n + 1)
  # Find the head position of fish1.
  head_X <- major_axis1 * cos(orientation1) + x1
  head_Y <- major_axis1 * sin(orientation1) + y1
  # Create matrices of all x and y coordinates on the ellipse (at origin)
  x <- major_axis2 %o% cos(theta)
  y <- minor_axis2 %o% sin(theta)
  # Rotate around origin and add offset
  x <- x * cos(orientation2) + y * sin(orientation2) + x2
  y <- y * cos(orientation2) + y * sin(orientation2) + y2

  # Find the minimal distances
  res <- apply((x - head_X) ^ 2 + (y - head_Y) ^ 2, 1, min)
  sqrt(res)
}
..nip_dist.. <- ~nip_dist(X1, X2, Y1, Y2, minor_axis1, minor_axis2, major_axis1,
                          major_axis2, orientation1, orientation2)

#' Calculate change in pairwise distance based on tip to ellipse.
#'
#' @inheritParams nip_dist
#' @inheritParams dist
#'
#' @return A vector of velocities in px per frame.
#' @export
nip_dist_velocity <- function(x1, x2, y1, y2, minor_axis1, minor_axis2,
                              major_axis1, major_axis2, orientation1,
                              orientation2, n = 20, f) {
  change(nip_dist(x1, x2, y1, y2, minor_axis1, minor_axis2, major_axis1,
                  major_axis2, orientation1, orientation2), f)
}
..nip_dist_velocity.. <-
  ~nip_dist_velocity(X1, X2, Y1, Y2, minor_axis1, minor_axis2, major_axis1,
                     major_axis2, orientation1, orientation2, f = frame)
