#' Selecting a table from a tracks object.
#'
#' @param tracks A tracks object.
#' @param table The name of a table.
#' @param collect Whether the table should be collected from the cluster, if
#'   applicable.
#'
#' @return A \code{tbl_df} or a \code{party_df}.
#' @export
#'
#' @examples
#' Guppies <- as_tracks(guppies, 30, 1080)
#'
#' # Select and collect:
#' Guppies[['tr']]
#'
#' # Only select:
#' Guppies[['tr', FALSE]]
#' Guppies$tr
`[[.tracks` <- function(tracks, table, collect = TRUE) {
  class(tracks) <- 'list'
  if (collect) {
    return(dplyr::collect(tracks[[table]]))
  }
  return(tracks[[table]])
}

#' @export
print.tracks <- function(x, ...) {
  # Precalculations ------------------------------------------------------------
  # frame range
  fr <- dplyr::summarise_(x$tr, min = ~min(frame), max = ~max(frame))
  fr <- dplyr::collect(fr)
  fr <- dplyr::ungroup(fr)
  frame_range <- dplyr::summarise_(fr, min = ~min(min), max = ~max(max))

  # object size (more difficult with party_dfs)
  multidplyr::cluster_assign_value(x$tr$cluster, name = '.name', x$tr$name)
  tr_size <- multidplyr::cluster_eval(x$tr$cluster,
                                      utils::object.size(eval(as.name(.name))))
  tr_size <- sum(unlist(tr_size))
  if (!is.null(x$soc)) {
    multidplyr::cluster_assign_value(x$soc$cluster, name = '.name',
                                     x$soc$name)
    pairs_size <- multidplyr::cluster_eval(x$soc$cluster,
                                           utils::object.size(
                                             eval(as.name(.name))))
    pairs_size <- sum(unlist(pairs_size))
  } else {
    pairs_size <- NA
  }
  tracks_size <- sum(utils::object.size(x), tr_size, pairs_size, na.rm = TRUE)
  class(tracks_size) <- 'object_size'

  # trial names
  trials <- levels(x$trial$trial)

  # Printing of info  ----------------------------------------------------------
  cat('tracks object (see ?tracks):\n')
  if (length(trials) > 1) {
    cat('  Containing data from:\n    Trials:', trials, '\n')
  }
  cat('    Frames:', frame_range[[1]], 'to', frame_range[[2]], '\n\n',
      'With these tables and variables:\n    Table:\t Variables:\n    tr\t\t',
      x$pr$tr, '\n')
  if (!is.null(x$soc)) {
    cat('    soc\t\t', x$pr$soc, '\n')
  }
  if (!is.null(x$group)) {
    cat('    group\t', names(x$group), '\n')
  }
  if (!is.null(x$pair)) {
    cat('    pair\t', names(x$pair), '\n')
  }
  if (!is.null(x$animal)) {
    cat('    animal\t', names(x$animal), '\n')
  }
  cat('    trial\t', names(x$trial), '\n')
  cat('    meta_data\t', names(x$meta_data), '\n\n')

  cat('Global parameters:\n  Frame rate:', x$params$frame_rate,
      'fps.\n  Scale:', x$params$px_per_cm, 'pixels per cm.\n  Source:',
      x$params$source, '\n\n')

  cat('The size of this tracks object is ')
  print(tracks_size, units = "auto")
}

#' @export
#' @rdname plot_tracks
plot.tracks <- function(x, ...) {
  plot_tracks(x, ...)
}
