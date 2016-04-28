#' @export
print.tracks <- function(x, ...) {
  # Precalculations ------------------------------------------------------------
  # frame range
  fr <- dplyr::summarise_(x$tr, ~min(frame), ~max(frame))
  fr <- dplyr::collect(fr)
  frame_range <- list(min = min(fr$`min(frame)`), max = max(fr$`max(frame)`))

  # object size (more difficult with)
  multidplyr::cluster_assign_value(x$tr$cluster, name = '.name', x$tr$name)
  tr_size <- multidplyr::cluster_eval(x$tr$cluster,
                                      object.size(eval(as.name(.name))))
  tr_size <- sum(unlist(tr_size))
  if (!is.null(x$soc)) {
    multidplyr::cluster_assign_value(x$soc$cluster, name = '.name',
                                     x$soc$name)
    pairs_size <- multidplyr::cluster_eval(x$soc$cluster,
                                           object.size(eval(as.name(.name))))
    pairs_size <- sum(unlist(pairs_size))
  } else {
    pairs_size <- NA
  }
  tracks_size <- sum(object.size(x), tr_size, pairs_size, na.rm = TRUE)
  class(tracks_size) <- 'object_size'

  # var names
  tr_names <- names(multidplyr::cluster_get(x$tr$cluster, x$tr$name)[[1]])
  if (!is.null(x$soc))
    soc_names <- names(multidplyr::cluster_get(x$soc$cluster,
                                               x$soc$name)[[1]])
  # trial names
  trials <- unique(multidplyr::cluster_get(x$tr$cluster,
                                           x$tr$name)[[1]]$trial)

  # Printing of info  ----------------------------------------------------------
  cat('Tracks object conaining the following elements:\n\n')

  cat('Tracks:\n')
  if (length(trials) > 1) {
    cat('  Containing data from:\n')
    cat('    Trials:', trials, '\n')
  }

  cat('    Frames:', frame_range$min, 'to',
      frame_range$max, '\n')
  cat('  With these variables stored:\n')
  cat('    ', tr_names, '\n\n')

  if (!is.null(x$soc)) {
    cat('Social:\n')
    cat('  Has these variables stored:\n')
    cat('  ', soc_names, '\n\n')
  }

  if (!is.null(x$group)) {
    cat('Group:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$group), '\n\n')
  }

  if (!is.null(x$pair)) {
    cat('Pair:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$pair), '\n\n')
  }

  if (!is.null(x$animal)) {
    cat('Animal:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$animal), '\n\n')
  }

  if (!is.null(x$trial)) {
    cat('Trial:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$trial), '\n\n')
  }

  cat('Meta data:\n')
  cat('  Has these variables stored:\n')
  cat('  ', names(x$meta_data), '\n\n')

  cat('Global parameters:\n')
  cat('  Frame rate:', x$params$frame_rate, 'fps.\n')
  if (!is.null(x$params$px_per_cm))
    cat('  Scale:', x$params$px_per_cm, 'pixels per cm.\n')
  if (!is.null(x$params$source))
    cat('  Source: ', x$params$source, '.\n\n', sep = '')

  cat('The size of this tracks object is ')
  print(tracks_size, units = "auto")
}

#' @export
#' @rdname plot_tracks
plot.tracks <- function(x, ...) {
  plot_tracks(x, ...)
}
