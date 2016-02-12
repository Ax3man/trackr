#' @export
print.tracks <- function(x, ...) {
  cat('Tracks object conaining the following elements:\n\n')

  cat('Tracks:\n')
  cat('  Containing data from:\n')
  if (!is.null(x$tr$trial))
    cat('    Trials:', levels(x$tr$trial), '\n')
  cat('    Frames:', min(x$tr$frame), 'to',
      max(x$tr$frame), '\n')
  cat('  With these variables stored:\n')
  vars <- names(x$tr)[!(names(x$tr) %in%
                              c('trial', 'frame', 'X', 'Y'))]
  cat('    ', vars, '\n\n')

  if (!is.null(x$group)) {
    cat('Group:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$group), '\n\n')
  }

  if (!is.null(x$pairs)) {
    cat('Pairs:\n')
    cat('  Has these variables stored:\n')
    cat('  ', names(x$pairs), '\n\n')
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
  print(object.size(x), units = "auto")
}
