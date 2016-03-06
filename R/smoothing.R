#' Roll a function over a tracks variable.
#'
#' This is mainly useful for smoothing a variable over time.
#'
#' For the common operations of mean, median, min, max, sum, prod, sd and var,
#' fast implementations from the \code{RcppRoll} package are used.
#'
#' By default a normal kernel is applied. Note that \code{weights} takes
#' priority over \code{window}.
#'
#' @param tracks A tracks object.
#' @param table Which table to look up the variable.
#' @param var Which variable to roll over.
#' @param fun Which function to use.
#' @param window Set to \code{NULL} if it has to be ignored. Ignored if weights
#'   is non-\code{NULL}.
#' @param weights Alternatively to window, you can supply a set of weights.
#'   A good example: \code{dnorm(seq(-3, 3, length.out = 20))}.
#'
#' @return A tracks object.
#' @export
roll <- function(tracks, table = 'tr', var, fun = mean, window = 5,
                 weights = dnorm(seq(-3, 3, length.out = 20))) {
  fun <- match.fun(fun)

  m <- NULL # have to a loop here as sapply doesn't work??
  for (f in c('mean', 'median', 'min', 'max', 'sum', 'prod', 'sd', 'var'))
    m <- c(m, isTRUE(all.equal(fun, match.fun(f))))
  if (sum(m) == 1) {
    roll_fast(tracks, table, var, fun, window, weights, hit = m)
  } else {
    roll_slow(tracks, table, var, fun, window)
  }
}

roll_fast <- function(tracks, table, var, fun, window, weights, hit) {
  # This line is to load the namespace of RcppRoll without upsetting R CMD check.
  dump <- RcppRoll::roll_mean
  fun <- getAnywhere(paste0('roll_',
                            c('mean', 'median', 'min', 'max', 'sum', 'prod',
                              'sd', 'var')[hit]))$objs[[1]]

  if ('party_df' %in% class(tracks[[table]])) {
    multidplyr::cluster_assign_value(tracks[[table]], 'fun', fun)
    multidplyr::cluster_assign_value(tracks[[table]], 'window', window)
    multidplyr::cluster_assign_value(tracks[[table]], 'weights', weights)
  }

  tracks[[table]] <- dplyr::mutate_(
    tracks[[table]],
    .dots = setNames(list(
      lazyeval::interp(~ifelse(frame == dplyr::lag(frame) + 1 &
                                 frame == dplyr::lead(frame) - 1, x, NA),
                       x = as.name(var)),
      ~fun(temp, window, weights, fill = NA)),
      c('temp', paste0(var, '_roll'))))
  tracks[[table]] <- dplyr::select_(tracks[[table]], ~-temp)

  return(tracks)
}

roll_slow <- function(tracks, table, var, fun, window) {
  stop('roll-ing on non-RcppRoll functions is not yet implemented.',
       call. = FALSE)
  # if ('party_df' %in% class(tracks[[table]])) {
  #   multidplyr::cluster_assign_value(tracks[[table]]$cluster, 'var', var)
  #   multidplyr::cluster_assign_value(tracks[[table]]$cluster, 'fun', fun)
  #   multidplyr::cluster_assign_value(tracks[[table]]$cluster, 'window', window)
  # }
  #
  # tracks[[table]] <- dplyr::mutate(
  #   tracks[[table]],
  #   temp = ifelse(frame == dplyr::lag(frame) + 1 &
  #                   frame == dplyr::lead(frame) - 1, speed, NA),
  #   speed_sm = zoo::rollapply(speed_temp, window, fun, fill = NA))
  # tracks[[table]] <- dplyr::select_(tracks[[table]], ~-temp)
  #
  # stop('Frames are not subsequent.', call. = FALSE)
  #
  # if ('party_df' %in% class(tbl)) {
  #   multidplyr::cluster_rm(tbl$cluster, c('var', 'fun', 'window'))
  # }
}
