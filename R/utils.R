get_party_df_names <- function(d) {
  fun <- function(x) names(eval(x))
  multidplyr::cluster_call(d$cluster, fun, as.name(d$name))[[1]]
}
