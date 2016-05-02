#' Calculate pairwise speed correlation lag.
#'
#' Pairwise correlations can be used to determine leader follower relationships.
#' Nagy et al. Nature (464) 2010 used directional correlations, this function
#' allows for speed correlations.
#'
#' This function returns two variables per animal pair per trial;
#' \code{lag} is the delay where the correlation was maximal (positive means),
#' and \code{cor} is the magnitude of the correlation.
#'
#' @param tracks A tracks object.
#' @param range The range of frames for which lags should be calculated, e.g.
#'   100 means from lags from -100 to +100 are evaluated.
#' @param time_bin Size of time bins in frames (optional). If supplied, for each
#'   of these time bins, a seperate lag correlation will be calculated, allowing
#'   you to see if the relationships change over the course of the trials.
#'
#' @return A data.frame.
#' @export
calc_speed_lag <- function(tracks, range = 100, time_bin = NULL) {
  if (!('speed' %in% tracks$pr$tr)) {
    stop('Speed not found in tr table.', call. = FALSE)
  }

  multidplyr::cluster_assign_value(tracks$soc$cluster, 'range', range)
  multidplyr::cluster_assign_value(tracks$soc$cluster, 'find_max_cross_corr',
                                   find_max_cross_corr)
  tracks <- join_tr_to_soc(tracks, list(~speed))

  if (is.null(time_bin)) {
    tracks$soc <- dplyr::mutate_(tracks$soc, time_bin = ~1)
  } else {
    f <- dplyr::collect(dplyr::do(tracks$soc, data.frame(a = range(.$frame))))
    br <- seq(min(f$a), max(f$a), by = time_bin)
    multidplyr::cluster_assign_value(tracks$soc$cluster, 'br', br)
    tracks$soc <- dplyr::mutate_(tracks$soc, time_bin = ~cut(frame, br))
  }
  tracks$soc <- dplyr::group_by_(tracks$soc, ~animal1, ~animal2, ~time_bin)

  res <- dplyr::do_(tracks$soc, ~find_max_cross_corr(.$speed1, .$speed2, range))
  res <- dplyr::filter_(res, !is.na(time_bin))

  res <- dplyr::collect(res)
  if (is.null(time_bin)) {
    res <- dplyr::select_(res, ~-time_bin)
  }
  return(res)
}


#' #' Calculate pairwise vector correlation.
#' #'
#' #' Pairwise correlations can be used to determine leader follower relationships.
#' #' Nagy et al. Nature (464) 2010.
#' #'
#' #' This function returns two variables per animal pair per trial;
#' #' vector_corr_tau is the delay (animal two is the 0 reference), and vector_corr
#' #' is the magnitude of the correlation.
#' #'
#' #' @param tracks A tracks object.
#' #'
#' #' @return A data.frame.
#' #' @export
#' calc_pair_vector_corr <- function(tracks) {
#'   tracks <- join_tr_to_pairs(tracks, list(~X, ~Y))
#'
#'   tracks$pairs <- dplyr::group_by_(tracks$pairs, ~animal1, ~animal2)
#'   tracks$pairs <- dplyr::mutate_(
#'     tracks$pairs,
#'     frame_ok = ~ifelse(frame == dplyr::lag(frame, order_by = frame) + 1, T, F),
#'     vX1 = ~ifelse(frame_ok, abs(X1 - dplyr::lag(X1, order_by = frame)), NA),
#'     vY1 = ~ifelse(frame_ok, abs(Y1 - dplyr::lag(Y1, order_by = frame)), NA),
#'     vX2 = ~ifelse(frame_ok, abs(X2 - dplyr::lag(X2, order_by = frame)), NA),
#'     vY2 = ~ifelse(frame_ok, abs(Y2 - dplyr::lag(Y2, order_by = frame)), NA))
#'
#'   tracks$pairs <- dplyr::mutate_(
#'     tracks$pairs,
#'     vX1 = ~vX1 - mean(vX1, na.rm = TRUE),
#'     vY1 = ~vY1 - mean(vY1, na.rm = TRUE),
#'     vX2 = ~vX2 - mean(vX2, na.rm = TRUE),
#'     vY2 = ~vY2 - mean(vY2, na.rm = TRUE))
#'
#'   multidplyr::cluster_assign_value(tracks$pairs$cluster, 'tau', 5)
#'   tracks$pairs <- dplyr::mutate_(
#'     tracks$pairs,
#'     v_cor = ~(vX1 * lag(vX2, tau) + vY1 * lag(vY2, tau)) /
#'       (sqrt(mean(c(vX1 ^ 2 + vY1 ^ 2), na.rm = TRUE)) *
#'          sqrt(mean(c(vX2 ^ 2, vY2 ^ 2), na.rm = TRUE))))
#'
#'   return(res)
#' }
#'
#' add_diff_to_pairs <- function(tracks, var, name) {
#'   tracks$pairs <- dplyr::group_by_(tracks$pairs, ~animal1, ~animal2)
#'   tracks$pairs <- dplyr::mutate_(
#'     tracks$pairs,
#'     .dots = setNames(list(lazyeval::interp(~x - dplyr::lag(x, order_by = frame),
#'                                            x = as.name(var))), name))
#'   return(tracks)
#' }
