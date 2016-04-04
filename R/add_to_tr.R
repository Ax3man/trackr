#' Conveniently add common new variables to the \code{soc} table.
#'
#' These functions are designed for use within \code{summarise_}, and will
#' compute common derived parameters for tracks. By default, these functions
#' will use the expected variable names, as are default in \code{tracks}
#' objects, but they can be overridden.
#'
#' @param x X-coordinate.
#' @param y Y-coordinate.
#' @param order_by This parameter controls the ordering of \code{dplyr::lag} and
#'   \code{dplyr::lead} and it is strongly advised to leave it at it's default
#'   (\code{frame}).
#'
#' @name mutate_tr
#' @seealso mutate_soc mutate_.tracks
NULL

#' @rdname mutate_tr
#' @export
change <- function(x, order_by = frame, n = 1) {
  x <- deparse(substitute(x))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::change_(x = ', x, ', order_by = ', order_by, ', n = ', n, ')')
}

#' @rdname mutate_tr
#' @export
change_ <- function(x, order_by, n = 1) {
  ifelse(order_by - dplyr::lag(order_by, n = n, order_by = order_by) == n,
         x - dplyr::lag(x, n = n, order_by = order_by),
         NA)
}

#' @rdname mutate_tr
#' @export
distance <- function(x = X, y = Y, order_by = frame) {
  X <- deparse(substitute(X))
  Y <- deparse(substitute(Y))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::distance_(x = ', X, ', y = ', Y, ', order_by = ', order_by, ')')
}

#' @rdname mutate_tr
#' @export
distance_ <- function(x, y, order_by) {
  sqrt(change_(x, order_by) ^ 2 + change_(y, order_by) ^ 2)
}

#' @rdname mutate_tr
#' @export
speed <- function(x = X, y = Y, order_by = frame) {
  X <- deparse(substitute(X))
  Y <- deparse(substitute(Y))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::speed_(x = ', X, ', y = ', Y, ', order_by = ', order_by, ')')
}
#' @rdname mutate_tr
#' @export
speed_ <- function(x, y, order_by) {
  change_(distance_(y, x, order_by), order_by)
}

#' @rdname mutate_tr
#' @export
acceleration <- function(x = X, y = Y, order_by = frame) {
  X <- deparse(substitute(X))
  Y <- deparse(substitute(Y))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::acceleration_(x = ', X, ', y = ', Y, ', order_by = ', order_by, ')')
}

#' @rdname mutate_tr
#' @export
acceleration_ <- function(x, y, order_by) {
  change_(change_(distance_(y, x, order_by), order_by), order_by)
}

#' @rdname mutate_tr
#' @export
heading <- function(x = X, y = Y, order_by = frame) {
  X <- deparse(substitute(X))
  Y <- deparse(substitute(Y))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::heading_(x = ', X, ', y = ', Y, ', order_by = ', order_by, ')')
}

#' @rdname mutate_tr
#' @export
heading_ <- function(x, y, order_by) {
  ifelse(
    order_by - dplyr::lag(order_by, order_by = order_by) == 1,
    angle(dplyr::lag(x, order_by = order_by),
          dplyr::lag(y, order_by = order_by),
          x,
          y),
    NA)
}

#' @rdname mutate_tr
#' @export
turn <- function(x = X, y = Y, order_by = frame) {
  X <- deparse(substitute(X))
  Y <- deparse(substitute(Y))
  order_by <- deparse(substitute(order_by))
  paste0('trackr::turn_(x = ', X, ', y = ', Y, ', order_by = ', order_by, ')')
}

#' @rdname mutate_tr
#' @export
turn_ <- function(x, y, order_by) {
  change_(heading_(x, y, order_by), order_by)
}
