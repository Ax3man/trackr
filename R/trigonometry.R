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

find_smallest_enclosing_rect <- function(points) {
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

#' Find smalles enclosing circle.
#'
#' Algorithm by Sven Skyum, from http://dx.doi.org/10.7146/dpb.v19i314.6704.
#'
#' @param S A matrix or data.frame of point coordinates with the first column
#'   being x and the second being y.
#'
#' @return A vector of lenght three, center x and y, and radius.
#' @export
find_smallest_enclosing_circle <- function(S) {
  S <- as.matrix(S[chull(S), ])
  if (nrow(S) == 1) {
    return(c(x = S[1, 1], y = S[1, 2], r = NA))
  }
  finished <- FALSE
  while (!finished) {
    if (nrow(S) == 1) {
      finished <- TRUE
    }
    S2 <- rbind(S[nrow(S), ], S, S[1, ])
    radii <- sapply(1:nrow(S), function(i) {
      find_circle_from_three_points(S2[i:(i + 2), ])[3]
    } )
    angles <- sapply(1:nrow(S), function(i) {
      angle_diff(angle(S2[i, 1], S2[i, 2], S2[i + 1, 1], S2[i + 1, 2]),
                 angle(S2[i + 1, 1], S2[i + 1, 2], S2[i + 2, 1], S2[i + 2, 2]))
    } )
    index <- order(radii, angles, decreasing = TRUE)[1]
    angle <- angles[index]
    if (angle <= -pi / 2 | angle >= pi / 2) {
      if (nrow(S) == 2) {
        circle <- find_circle_from_two_points(S)
      } else {
        circle <- find_circle_from_three_points(S2[index:(index + 2), ])
      }
      finished <- TRUE
    } else {
      S <- S[-index, ]
    }
  }
  names(circle) <- c('x', 'y', 'r')
  return(circle)
}


find_circle_from_two_points <- function(points) {
  cx <- mean(points[, 1])
  cy <- mean(points[, 2])
  radius <- sqrt((cx - points[1, 1]) ^ 2 + (cy - points[1, 2]) ^ 2)
  return(c(x = cx, y = cy, r = radius))
}

find_circle_from_three_points <- function(points) {
  x1 <- points[1, 1]; x2 <- points[2, 1]; x3 <- points[3, 1]
  y1 <- points[1, 2]; y2 <- points[2, 2]; y3 <- points[3, 2]
  mA <- (y2 - y1) / (x2 - x1)
  mB <- (y3 - y2) / (x3 - x2)
  cx <- (mA * mB * (y1 - y3) + mB * (x1 + x2) - mA * (x2 + x3)) / (2 * (mB - mA))
  cy <- ((x1 + x2) / 2 - cx) / mA + (y1 + y2) / 2
  radius <- sqrt((cx - x1) ^ 2 + (cy - y1) ^ 2)
  return(c(x = cx, y = cy, r = radius))
}

check_if_in_circle <- function(points, x, y, r) {
  (points[, 1] - x) ^ 2 + (points[, 2] - y) ^ 2 < r ^ 2
}
