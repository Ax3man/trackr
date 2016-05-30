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
  if (is.null(tracks$soc)) {
    stop('soc table was not found or is empty.', call. = FALSE)
  }
  cl <- tracks$tr$cluster

  tr <- dplyr::select_(tracks$tr, .dots = c(~trial, ~animal, ~frame, select))
  tr <- dplyr::collect(tr)

  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$soc$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  Names1 <- setNames(names(select), paste0(names(select), 1))
  Names2 <- setNames(names(select), paste0(names(select), 2))
  if (any(tracks$pr$soc %in% c(Names1, Names2))) {
    tracks$soc <- dplyr::select_(tracks$soc,
                                 lazyeval::interp(~-one_of(x), x = c(Names1, Names2)))
  }
  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))

  tracks$soc <- dplyr::rename_(tracks$soc, .dots = Names1)

  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))
  multidplyr::cluster_rm(cl, '.tr')
  tracks$soc <- dplyr::rename_(tracks$soc, .dots = Names2)

  tracks$pr$soc <- c(tracks$pr$soc, names(Names1), names(Names2))
  return(tracks)
}

#' Conveniently add common new variables to the \code{soc} table.
#'
#' These functions are designed for use within \code{summarise_}, and will
#' compute common parameters for pairs of animals. By default, these functions
#' will use the expected variable names, as are default in \code{tracks}
#' objects, but they can be overridden.
#'
#' Many of these functions rely on parameters that originate from the \code{tr}
#' table, such as \code{X1} or \code{orientation2}. This usually means you have
#' to add those variable to the \code{soc} table first, by calling
#' \code{join_tr_to_soc}.
#'
#' @param X1 X-coordinate for animal 1.
#' @param X2 X-coordinate for animal 2.
#' @param Y1 Y-coordinate for animal 1.
#' @param Y2 Y-coordinate for animal 2.
#' @param minor_axis1 Minor axis (*b*) of the ellipse fit of animal 1.
#' @param minor_axis2 Minor axis (*b*) of the ellipse fit of animal 2.
#' @param major_axis1 Major axis (*a*) of the ellipse fit of animal 1.
#' @param major_axis2 Major axis (*a*) of the ellipse fit of animal 2.
#' @param orientation1 Angle of ellipse fit with the x-axis for animal 1.
#' @param orientation2 Angle of ellipse fit with the x-axis for animal 2.
#' @param heading1 Angle animal 1 is heading based on previous and coming
#'   coordinates.
#' @param heading2 Angle animal 2 is heading based on previous and coming
#'   coordinates.
#' @param n Number of point used for numerical approximation.
#'
#' @name mutate_soc
#' @seealso mutate_tr mutate_.tracks
NULL

#' @rdname mutate_soc
#' @export
pair_dist <- function(X1 = X1, X2 = X2, Y1 = Y1, Y2 = Y2) {
  sqrt((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2)
}

#' @rdname mutate_soc
#' @export
nip_dist <- function(X1 = X1, X2 = X2, Y1 = Y1, Y2 = Y2,
                     minor_axis1 = minor_axis1, minor_axis2 = minor_axis2,
                     major_axis1 = major_axis1, major_axis2 = major_axis2,
                     orientation1 = orientation1, orientation2 = orientation2,
                     n = 20) {
  # Vector of angles that need to be tested.
  theta <- seq(0, 2 * pi, length.out = n + 1)
  # Find the head position of fish1.
  head_X <- major_axis1 * cos(orientation1) + X1
  head_Y <- major_axis1 * sin(orientation1) + Y1
  # Create matrices of all x and y coordinates on the ellipse (at origin)
  x <- major_axis2 %o% cos(theta)
  y <- minor_axis2 %o% sin(theta)
  # Rotate around origin and add offset
  x <- x * cos(orientation2) + y * sin(orientation2) + X2
  y <- y * cos(orientation2) + y * sin(orientation2) + Y2

  # Find the minimal distances
  res <- apply((x - head_X) ^ 2 + (y - head_Y) ^ 2, 1, min)
  sqrt(res)
}

#' @rdname mutate_soc
#' @export
orientation_diff <- function(orientation1 = orientation1,
                             orientation2 = orientation2) {
  abs(angle_diff(orientation1, orientation2))
}

#' @rdname mutate_soc
#' @export
heading_diff <- function(heading1 = heading1, heading2 = heading2) {
  abs(angle_diff(heading1, heading2))
}
