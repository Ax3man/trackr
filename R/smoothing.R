#' Smooth a vector using a rolling mean.
#'
#' By default, smoothing is done by averaging over a Gaussian kernel.
#'
#' @param x A vector to smooth.
#' @param window The size of the window to smooth over (ignored if weights is
#'   non-\code{NULL}).
#' @param weights A vector of weights.
#'
#' @return A numeric vector.
#' @export
sm <- function(x, window = 5, weights = dnorm(seq(-3, 3, length.out = 9))) {
  if (is.null(weights)) {
    l <- window
  } else {
    l <- length(weights)
  }
  l <- (l - 1) / 2
  c(rep(NA, l), RcppRoll::roll_mean(x, window, weights), rep(NA, l))
}

#' #' Roll a function over a tracks variable.
#' #'
#' #' This is mainly useful for smoothing a variable over time.
#' #'
#' #' For the common operations of mean, median, min, max, sum, prod, sd and var,
#' #' fast implementations from the \code{RcppRoll} package are used.
#' #'
#' #' By default a normal kernel is applied. Note that \code{weights} takes
#' #' priority over \code{window}.
#' #'
#' #' @param var Which variable to roll over.
#' #' @param fun Which function to use.
#' #' @param window Set to \code{NULL} if it has to be ignored. Ignored if weights
#' #'   is non-\code{NULL}.
#' #' @param weights Alternatively to window, you can supply a set of weights.
#' #'   By defualt uses normal weights.
#' #'
#' #' @return A numeric vector.
#' roll <- function(var, fun = mean, window = 5,
#'                  weights = dnorm(seq(-3, 3, length.out = 20))) {
#'   fun <- match.fun(fun)
#'
#'   m <- NULL # have to a loop here as sapply doesn't work??
#'   for (f in c('mean', 'median', 'min', 'max', 'sum', 'prod', 'sd', 'var'))
#'     m <- c(m, isTRUE(all.equal(fun, match.fun(f))))
#'   if (sum(m) == 1) {
#'     roll_fast(var, fun, window, weights, hit = m)
#'   } else {
#'     roll_slow(var, fun, window)
#'   }
#' }
#'
#' roll_fast <- function(var, fun, window, weights, hit) {
#'   # This line is to load the namespace of RcppRoll without upsetting R CMD check.
#'   dump <- RcppRoll::roll_mean
#'   rm(dump)
#'   fun <- getAnywhere(paste0('roll_',
#'                             c('mean', 'median', 'min', 'max', 'sum', 'prod',
#'                               'sd', 'var')[hit]))$objs[[1]]
#'   return(function())
#'
#'   return(tracks)
#' }
