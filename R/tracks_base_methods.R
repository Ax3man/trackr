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
  if (!is.null(x$pairs)) {
    multidplyr::cluster_assign_value(x$pairs$cluster, name = '.name',
                                     x$pairs$name)
    pairs_size <- multidplyr::cluster_eval(x$pairs$cluster,
                                           object.size(eval(as.name(.name))))
    pairs_size <- sum(unlist(pairs_size))
  } else {
    pairs_size <- NA
  }
  tracks_size <- sum(object.size(x), tr_size, pairs_size, na.rm = TRUE)
  class(tracks_size) <- 'object_size'

  # var names
  tr_names <- names(multidplyr::cluster_get(x$tr$cluster, x$tr$name)[[1]])
  if(!is.null(x$pairs))
    pairs_names <- names(multidplyr::cluster_get(x$pairs$cluster,
                                                 x$pairs$name)[[1]])

    # Printing of info  ----------------------------------------------------------
    cat('Tracks object conaining the following elements:\n\n')

    cat('Tracks:\n')
    cat('  Containing data from:\n')
    if (!is.null(x$tr$trial))
      cat('    Trials:', levels(x$tr$trial), '\n')
    cat('    Frames:', frame_range$min, 'to',
        frame_range$max, '\n')
    cat('  With these variables stored:\n')
    cat('    ', tr_names, '\n\n')

    if (!is.null(x$group)) {
      cat('Group:\n')
      cat('  Has these variables stored:\n')
      cat('  ', names(x$group), '\n\n')
    }

    if (!is.null(x$pairs)) {
      cat('Pairs:\n')
      cat('  Has these variables stored:\n')
      cat('  ', pairs_names, '\n\n')
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
