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
#'
#' @examples
#' Guppies <- guppies %>%
#'   as_tracks(30, 1080) %>%
#'   expand_tracks()
#'
#' join_tr_to_soc(Guppies, X, Y)
join_tr_to_soc <- function(tracks, ...) {
  join_tr_to_soc_(tracks, .dots = lazyeval::lazy_dots(...))
}

join_tr_to_soc_ <- function(tracks, ..., .dots) {
  select <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (is.null(tracks$soc)) {
    stop('soc table was not found or is empty.', call. = FALSE)
  }
  cl <- tracks$soc$cluster

  tr <- dplyr::select_(tracks$tr, .dots = c(~trial, ~animal, ~frame, select))
  tr <- dplyr::collect(tr)

  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$soc$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  Names1 <- stats::setNames(names(select), paste0(names(select), 1))
  Names2 <- stats::setNames(names(select), paste0(names(select), 2))
  if (any(tracks$pr$soc %in% c(names(Names1), names(Names2)))) {
    tracks$soc <- dplyr::select_(tracks$soc,
                                 lazyeval::interp(~-one_of(x),
                                                  x = c(names(Names1),
                                                        names(Names2))))
    tracks$pr$soc <- tracks$pr$soc[!(tracks$pr$soc %in%
                                       c(names(Names1), names(Names2)))]
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
#' These functions typically rely on parameters that originate from the
#' \code{tr} table, such as \code{X1} or \code{orientation2}. If so, that means
#' you have to add those variable to the \code{soc} table first, by calling
#' \code{\link{join_tr_to_soc}}.
#'
#' @section Overview of functions:
#'
#' The following functions are currently available for easy use in calls to
#' \code{\link[=mutate_.tracks]{mutate}}.
#' \describe{
#'   \item{\code{pair_dist}}{Calculates the distance between the centroids of
#'   animal1 and animal2. You need to make x and y coordinates available.}
#'   \item{\code{nip_dist}}{When ellipse fits are available, calculates the
#'   distance from the front of the ellipse of animal1 (presumably the head or
#'   mouth) and the closest point on the ellipse of animal2. This function uses
#'   a numerical approximation based on n points along the ellipse of animal2.
#'   You need to make x and y coordinates, orientations, and minor and major
#'   axis sizes available.} \item{\code{orientation_diff}}{Difference in
#'   orientations. You need to make orientations available.}
#'   \item{\code{heading_diff}}{Difference in headings. Calculate heading for
#'   each animal first (using \code{mutate(tracks, heading = heading())}), then
#'   add the heading to the \code{soc} table (using \code{join_tr_to_soc(tracks,
#'   heading)}). Use \code{heading_diff2} when basing it off x and y
#'   coordinates. You need to make headings available.}
#'   \item{\code{heading_diff2}}{Difference in headings, based on x and y
#'   coordinates. You need to make x and y coordinates available.}
#'   \item{\code{leader}}{Find whether animal1 is in front (TRUE), or in behind
#'   (FALSE) of animal 2, based on their mean heading. You need to make x and y
#'   coordinates available.}
#' }
#'
#' @param x1 X-coordinate for animal 1.
#' @param x2 X-coordinate for animal 2.
#' @param y1 Y-coordinate for animal 1.
#' @param y2 Y-coordinate for animal 2.
#' @param minor_axis1 Minor axis (*b*) of the ellipse fit of animal 1.
#' @param minor_axis2 Minor axis (*b*) of the ellipse fit of animal 2.
#' @param major_axis1 Major axis (*a*) of the ellipse fit of animal 1.
#' @param major_axis2 Major axis (*a*) of the ellipse fit of animal 2.
#' @param orientation1 Angle of ellipse fit with the x-axis for animal 1.
#' @param orientation2 Angle of ellipse fit with the x-axis for animal 2.
#' @param heading1 Angle animal 1 is heading based on previous and current
#'   coordinates.
#' @param heading2 Angle animal 2 is heading based on previous and current
#'   coordinates.
#' @param n Number of point used for numerical approximation.
#' @param order_by Used for making sure only subsequent frames are used for
#'   time dependent calculatens. You probably want to leave this at default.
#'
#' @name mutate_soc
#' @seealso mutate_tr mutate_.tracks
NULL

#' @rdname mutate_soc
#' @export
pair_dist <- function(x1 = X1, y1 = Y1, x2 = X2, y2 = Y2) {
  sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

#' @rdname mutate_soc
#' @export
nip_dist <- function(x1 = X1, y1 = Y1, x2 = X2, y2 = Y2,
                     minor_axis1 = minor_axis1, minor_axis2 = minor_axis2,
                     major_axis1 = major_axis1, major_axis2 = major_axis2,
                     orientation1 = orientation1, orientation2 = orientation2,
                     n = 20) {
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

#' @rdname mutate_soc
#' @export
heading_diff2 <- function(x1 = X1, y1 = Y1, x2 = X2, y2 = Y2, order_by = frame) {
  hd1 <- heading(x1, y1, order_by)
  hd2 <- heading(x2, y2, order_by)
  abs(angle_diff(hd1, hd2))
}

#' @rdname mutate_soc
#' @export
leader <- function(x1 = X1, y1 = Y1, x2 = X2, y2 = Y2, order_by = frame) {
  hd1 <- heading(x1, y1, order_by)
  hd2 <- heading(y2, y2, order_by)

  mhd <- apply(cbind(hd1, hd2), 1, mean_angle)
  mx <- rowMeans(cbind(x1, x2))
  my <- rowMeans(cbind(y1, y2))

  a <- -tan(mhd)
  b <- my - a * mx

  y_check <- a * x1 + b

  r <- ifelse(mhd > 0, y1 > y_check, y1 < y_check)
  r[mhd == 0 | is.na(mhd)] <- NA
  return(r)
}
