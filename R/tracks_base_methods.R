#' @export
print.tracks <- function(x, ...) {
  # Precalculations ------------------------------------------------------------
  # frame range
  fr <- dplyr::summarise_(x$tr, min = ~min(frame), max = ~max(frame))
  fr <- dplyr::ungroup(fr)
  frame_range <- dplyr::summarise_(fr, min = ~min(min), max = ~max(max))

  tracks_size <- utils::object.size(x)
  # trial names
  trials <- levels(x$trial$trial)

  # Printing of info  ----------------------------------------------------------
  cat('tracks object (see ?tracks):\n')
  if (length(trials) > 1) {
    cat('  Containing data from:\n    Trials:', trials, '\n')
  }
  cat('    Frames:', frame_range[[1]], 'to', frame_range[[2]], '\n\n',
      ' With these tables and variables:\n    Table:\t Variables:\n    tr\t\t',
      names(x$tr), '\n')
  if (!is.null(x$soc)) {
    cat('    soc\t\t', names(x$soc), '\n')
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

  cat('  Global parameters:\n    Frame rate:', x$params$frame_rate,
      'fps.\n    Scale:', x$params$px_per_cm, 'pixels per cm.\n    Source:',
      x$params$source, '\n\n')

  cat('The size of this tracks object is ')
  print(tracks_size, units = "auto")
}

#' @export
#' @rdname plot_tracks
plot.tracks <- function(x, ...) {
  plot_tracks(x, ...)
}
