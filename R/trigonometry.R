angle <- function(x1, y1, x2, y2) atan2(y2 - y1, x2 - x1)

angle_diff <- function(a1, a2) atan2(sin(a2 - a1), cos(a2 - a1))

mean_direction <- function(x) {
  if (all(is.na(x)))
    return(NA)
  atan2(sum(sin(x), na.rm = T), sum(cos(x), na.rm = T))
}

res.vec <- function(angles, lengths = NULL) {
  if (all(is.na(angles)))
    return(NA)
  if (is.null(lengths))
    lengths <- rep(1, length(angles))
  stopifnot(length(angles) == length(lengths))
  sqrt(sum(lengths * sin(angles), na.rm = T) ^ 2 +
         sum(lengths * cos(angles), na.rm = T) ^ 2)
}

find_smallest_surrounding_rect <- function(points) {
  # Authored by @whuber (http://gis.stackexchange.com/a/22934) and @Bangyou
  # (http://gis.stackexchange.com/a/174577).
  if (!requireNamespace('geometry', quietly = TRUE))
    stop("Package \'geometry\' needs to be installed to use this function.")
  # Find the convex hull
  a2 <- geometry::convhulln(points, options = 'FA')
  # Edge directions
  e <- points[a2$hull[, 2], ] - points[a2$hull[, 1], ]
  # Edge lengths
  norms <- apply(e, 1, function(x) sqrt(x %*% x))
  # Unit edge directions
  v <- diag(1 / norms) %*% as.matrix(e)
  # Normal directions to the edges
  w <- cbind(-v[, 2], v[, 1])

  # Find the minimal area surrounding rectangle
  vertices <- as.matrix((points) [a2$hull, 1:2])  # Convex hull vertices
  minmax <- function(x) c(min(x), max(x))         # Computes min and max
  x <- apply(vertices %*% t(v), 2, minmax)        # Extremes along edges
  y <- apply(vertices %*% t(w), 2, minmax)        # Extremes normal to edges
  areas <- (y[1, ] - y[2, ]) * (x[1, ] - x[2, ])            # Areas
  k <- which.min(areas)                           # Index of the best edge

  # Form a rectangle from the extremes of the best edge
  rect <- cbind(x[c(1, 2, 2, 1, 1), k], y[c(1, 1, 2, 2, 1), k]) %*%
    rbind(v[k, ], w[k, ])

  rect <- as.data.frame(rect)
  names(rect) <- c('x', 'y')

  return(rect)
}

find_closest_point_on_ellipse <- function(px, py, ex, ey, a, b, angle, n = 100, ret = 'x') {
  # This is a numerical approximation based on n evenly spaced points
  # (vectorized with loop)
  theta <- seq(0, 2 * pi, length.out = n + 1)
  res <- rep(NA, length(px))
  for (i in seq_along(px)) {
    x <- a[i] * cos(theta)
    y <- b[i] * sin(theta)
    # Rotate around origin and offset
    x <- x * cos(angle[i]) + y * sin(angle[i]) + ex[i]
    y <- y * cos(angle[i]) + y * sin(angle[i]) + ey[i]
    m <- which.min((x - px[i]) ^ 2 + (y - py[i]) ^ 2)
    if (length(m) == 0) {
      res[i] <- NA
      next
    }
    res[i] <- switch(ret, x = x[m], y = y[m])
  }
  res
}
