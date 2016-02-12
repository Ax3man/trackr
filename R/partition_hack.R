partition <- function(.data, ..., cluster = multidplyr::get_default_cluster())
{
  dots <- lazyeval::lazy_dots(...)
  partition_(.data, dots, cluster)
}

partition_ <- function(data, groups,
                       cluster = multidplyr::get_default_cluster()) {
  n <- nrow(data)
  m <- length(cluster)

  if (length(groups) == 0) {
    part_id <- sample(floor(m * (seq_len(n) - 1) / n + 1))
    n_groups <- m

    data$PARTITION_ID <- part_id
    data <- dplyr::group_by_(data, ~PARTITION_ID)
    group_vars <- list(quote(PARTITION_ID))
  } else {
    group_vars <- multidplyr:::grouping_vars(groups)

    data <- dplyr::group_by_(data, .dots = groups)
    group_id <- dplyr::group_indices_(data)
    n_groups <- dplyr::n_groups(data)

    groups <- multidplyr:::scramble_rows(dplyr::data_frame(
      id = seq_len(n_groups),
      n = tabulate(group_id, n_groups)
    ))
    groups$part_id <- floor(m * (cumsum(groups$n) - 1) / sum(groups$n) + 1)
    part_id <- groups$part_id[match(group_id, groups$id)]
  }

  idx <- split(seq_len(n), part_id)
  shards <- lapply(idx, function(i) data[i, , drop = FALSE])

  if (length(shards) < length(cluster)) {
    cluster <- cluster[1:length(shards)]
  }

  name <- multidplyr:::random_table_name()
  multidplyr::cluster_assign_each(cluster, name, shards)

  multidplyr:::party_df(name, cluster, group_vars)
}
