get_party_df_names <- function(d) {
  fun <- function(x) names(eval(x))
  multidplyr::cluster_call(d$cluster, fun, as.name(d$name))[[1]]
}

join_tr_to_pairs <- function(tracks, select) {
  # Select is a list of ~variables.
  cl <- tracks$tr$cluster

  tr <- dplyr::select_(tracks$tr, .dots = c(~trial, ~animal, ~frame, select))
  tr <- dplyr::collect(tr)

  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$pairs$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))

  Names <- as.character(select)
  ren1 <- setNames(select, paste0(substring(Names, 2), 1))
  tracks$pairs <- dplyr::rename_(tracks$pairs, .dots = ren1)

  tracks$pairs <- dplyr::do_(tracks$pairs,
                             ~dplyr::left_join(
                               ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))

  ren2 <- setNames(select, paste0(substring(Names, 2), 2))
  tracks$pairs <- dplyr::rename_(tracks$pairs, .dots = ren2)

  return(tracks)
}

find_max_cross_corr <- function(v1, v2, range) {
  cross_corr <- ccf(v1, v2, range, na.action = na.pass, plot = FALSE)
  res <- data.frame(cor = cross_corr$acf[, , 1], lag = cross_corr$lag[, 1, 1])
  res = res[which.max(res$cor), ]
  return(res)
}
