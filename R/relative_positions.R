#' Add the relative positions of each pair
#'
#' @param tracks A tracks object.
#'
#' @return A data.frame with xbin, ybin and n (count).
#' @export
add_relative_positions <- function(tracks) {
  if (!('soc' %in% names(tracks))) {
    tracks <- expand_tracks2(tracks, 'soc')
  }

  tracks %>%
    mutate(h = heading()) %>%
    join_tr_to_soc(X, Y, h) %>%
    magrittr::extract2('soc') %>%
    mutate(rel_X = Map(rotate, x = X2 - X1, y = Y2 - Y1, angle = h1),
           rel_Y = sapply(rel_X, `[`, 2),
           rel_X = sapply(rel_X, `[`, 1))
}

#' Count the relative positions of each pair binned on X and Y
#'
#' @param tracks A tracks object.
#' @param nbins The number of bins you want. The same size bins are used for X and Y.
#'
#' @return A data.frame with xbin, ybin and n (count).
#' @export
bin_relative_positions <- function(tracks, nbins = 100) {
  tmp <- add_relative_positions(tracks)

  lim <- max(abs(range(c(tmp$rel_X, tmp$rel_Y), na.rm = TRUE)))
  br <- seq(-lim, lim, length.out = nbins)
  labs <- na.omit(unlist(Map(function(x, y) mean(c(x, y)), br, dplyr::lead(br))))

  tmp %>%
    mutate(xbin = as.numeric(as.character(cut(rel_X, br, labs))),
           ybin = as.numeric(as.character(cut(rel_Y, br, labs)))) %>%
    dplyr::ungroup() %>%
    dplyr::count(trial, animal1, animal2, xbin, ybin)
}
