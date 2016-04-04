#' Common functions to deal with angles.
#'
#' These functions make it easy to calculate things about orientation and
#' headings, while keeping the angles within -pi to pi.
#'
#' @param x1 X-coordinate for poinr 1.
#' @param x2 X-coordinate for poinr 2.
#' @param y1 Y-coordinate for poinr 1.
#' @param y2 Y-coordinate for poinr 2.
#' @param a1 Angle 1.
#' @param a2 Angle 2.
#' @param x A vector of angles.
#'
#' @name trig
NULL


#' @rdname trig
#' @export
angle <- function(x1, y1, x2, y2) atan2(y2 - y1, x2 - x1)

#' @rdname trig
#' @export
angle_diff <- function(a1, a2) atan2(sin(a2 - a1), cos(a2 - a1))

#' @rdname trig
#' @export
mean_angle <- function(x) {
  if (all(is.na(x)))
    return(NA)
  atan2(sum(sin(x), na.rm = T), sum(cos(x), na.rm = T))
}

res_vec <- function(angles, lengths = NULL) {
  if (all(is.na(angles))) {
    return(NA)
  }
  if (is.null(lengths)) {
    lengths <- rep(1, length(angles))
  }
  stopifnot(length(angles) == length(lengths))
  sqrt(sum(lengths * sin(angles), na.rm = T) ^ 2 +
         sum(lengths * cos(angles), na.rm = T) ^ 2)
}

find_smallest_surrounding_rect <- function(points) {
  # Authored by William A. Huber (http://gis.stackexchange.com/a/22934) and
  # @Bangyou (http://gis.stackexchange.com/a/174577).
  if (!requireNamespace('geometry', quietly = TRUE))
    stop("Package \'geometry\' needs to be installed to use this function.")
  a2 <- geometry::convhulln(points, options = 'FA')
  e <- points[a2$hull[, 2], ] - points[a2$hull[, 1], ]
  norms <- apply(e, 1, function(x) sqrt(x %*% x))
  v <- diag(1 / norms) %*% as.matrix(e)
  w <- cbind(-v[, 2], v[, 1])

  vertices <- as.matrix((points) [a2$hull, 1:2])
  minmax <- function(x) c(min(x), max(x))
  x <- apply(vertices %*% t(v), 2, minmax)
  y <- apply(vertices %*% t(w), 2, minmax)
  areas <- (y[1, ] - y[2, ]) * (x[1, ] - x[2, ])
  k <- which.min(areas)

  rect <- cbind(x[c(1, 2, 2, 1, 1), k], y[c(1, 1, 2, 2, 1), k]) %*%
    rbind(v[k, ], w[k, ])
  rect <- as.data.frame(rect)
  names(rect) <- c('x', 'y')

  return(rect)
}
