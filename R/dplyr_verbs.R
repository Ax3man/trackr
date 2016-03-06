#' @importFrom dplyr filter
#' @name filter
#' @rdname filter_.tracks
#' @export
NULL

#' @importFrom dplyr filter_
#' @name filter_
#' @rdname filter_.tracks
#' @export
NULL

#' Return a subset of a tracks object with matching conditions.
#'
#' Method for the dplyr verb filter. You can use them as \code{filter()} and
#' \code{filter_()}, as \code{drop} will be passed on accordingly. Allows to
#'   filter a tracks object on the following variables:
#' \itemize{
#'   \item Frame number
#'   \item Trial
#'   \item Animal
#'   \item Animal pair (not implemented)
#'   \item xbin and ybin for spatial data (not implemented)
#' }
#'
#' @param .data A tracks object.
#' @param ... The conditions. Do not use \code{&} to combine different
#'   varialbles. Do use \code{&} to combine different conditions on the same
#'   variable.
#' @param drop Whether to drop conflicting aggregate data. If the tracks object
#'   contains data that was aggregated over a variable that is now being used to
#'   filter, drop must be TRUE or an error will be raised. Have to opt in, since
#'   aggregation may have been expensive.
#' @inheritParams dplyr::filter
#'
#' @return The subsetted tracks object
#'
#' @section Policy on conflicting dependecies:
#'
#'   It will apply the fitering to all the applicable sections of the tracks
#'   object. It will also attempt to check for any dependency problems. This is
#'   crucial in order to maintain internal consistency in the tracks object. For
#'   example, if you select a sequence of frames, any data that was aggregated
#'   over time will now no longer match. If conflicts are found, by default, an
#'   error will be raised. By setting \code{drop = TRUE}, you can allow for any
#'   conflicting data to be deleted from the tracks object.
#'
#' @export
#' @seealso \link[dplyr]{filter}
filter_.tracks <- function(.data, ..., drop = FALSE, .dots) {
  # rename the .data argument (we need it for dplyr consistency).
  tracks <- .data
  rm(.data)
  # collect conditions
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  # check if drop happens to be in the .dots (coming from filter dispatching to
  # filter_), if so then extract it out
  if (any(names(conds) == 'drop')) {
    drop <- lazyeval::lazy_eval(conds$drop)
    conds <- conds[-which(names(conds) == 'drop')]
  }
  # extract which things those conditions apply to
  vars <- sapply(strsplit(names(conds), ' '), '[', 1)
  if (any(!(vars %in% c('frame', 'trial', 'animal', 'time'))))
    warning("Compatability is only checked when filtering on frame, trial or animal.",
            call. = FALSE)
  if (length(vars) > length(unique(vars)))
    stop("Combine different conditions for the same variable with '&', instead
         of using seperate ... arguments.")
  # Find which components of the tracks object are currently present
  present <- names(tracks)[!(names(tracks) %in% c('params', 'pr'))]

  # check for compatibility issues ---------------------------------------------
  if (any(vars == 'frame') & any(present %in% c('location', 'trial'))) {
    if (drop) {
      tracks$location <- NULL
      tracks$trial <- NULL
    } else {
      stop("Found data aggregated over frames. Include drop == TRUE to delete.",
           call. = FALSE)
    }
  }
  if (any(vars == 'animal') &
      any(present %in% c('group', 'pairs', 'location', 'trial'))) {
    if (drop) {
      message("Dropping incompatible data.")
      tracks$group <- NULL
      tracks$pairs <- NULL
      tracks$location <- NULL
      tracks$trial <- NULL
    } else {
      stop("Found data aggregated over animals. Include drop == TRUE to
           delete.",
           call. = FALSE)
    }
  }
  # Update which components of the tracks object are currently present
  present <- names(tracks)[!(names(tracks) %in% c('params', 'pr'))]

  # Apply filter to all tables -------------------------------------------------
  tracks[present] <- lapply(tracks[present], function(d, conds, vars) {
    var_d <- switch(class(d)[1],
                    'party_df' = get_party_df_names(d),
                    'tbl_df' = names(d),
                    return(d))

    conds2 <- conds[which(vars %in% var_d)]
    if (length(conds2) > 0) {
      d <- dplyr::filter_(d, .dots = conds2)
      #d <- droplevels(d)
    }
    d
  }, conds = conds, vars = vars)

  return(tracks)
  }

#' @importFrom dplyr summarise
#' @name summarise
#' @rdname summarise_.tracks
#' @export
NULL

#' @importFrom dplyr summarise_
#' @name summarise_
#' @rdname summarise_.tracks
#' @export
NULL

#' @importFrom dplyr summarize
#' @name summarize
#' @rdname summarise_.tracks
#' @export
NULL

#' @importFrom dplyr summarize_
#' @name summarize_
#' @rdname summarise_.tracks
#' @export
NULL

#' Summarize a tracks variable into higher tables
#'
#' @param .data A tracks object.
#' @param ... Summary expressions, such as \code{mean(speed, na.rm = TRUE)}, or
#'   \code{var_nnd = var(nearest_neighbour_dist, na.rm = TRUE)}.
#' @param .source Which table those variables come from. Will make guesses in
#'   the future.
#' @param .tables To which tables you want to summarize them. Will make guesses
#'   in the future.
#'
#' @return A tracks object.
#' @export
#'
#' @inheritParams dplyr::summarise
#' @seealso dplyr::summarise
#' @importFrom dplyr '%>%'
summarise_.tracks <- function(.data,
                              ...,
                              .source = 'tr',
                              .tables = c('group', 'animal', 'trial'),
                              .dots) {
  # rename the .data argument (we need it for correct passing from the general).
  tracks <- .data
  rm(.data)
  # collect conditions
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  # check if .source or .tables happen to be in the .dots (coming from filter
  # dispatching to filter_), if so then extract them out
  if (any(names(conds) == '.source')) {
    .source <- lazyeval::lazy_eval(conds$.source)
    conds <- conds[-which(names(conds) == '.source')]
  }
  if (any(names(conds) == '.tables')) {
    .source <- lazyeval::lazy_eval(conds$.tables)
    conds <- conds[-which(names(conds) == '.tables')]
  }

  if ('group' %in% .tables & !is.null(tracks$group)) {
    tracks$group <- tracks$tr %>%
      dplyr::group_by_(~trial, ~frame) %>%
      dplyr::summarize_(.dots = conds) %>%
      dplyr::full_join(tracks$group, c('trial', 'frame')) %>%
      dplyr::ungroup(.)
  }

  if ('animal' %in% .tables & !is.null(tracks$animal)) {
    tracks$animal <- tracks$tr %>%
      dplyr::group_by_(~trial, ~animal) %>%
      dplyr::summarize_(.dots = conds) %>%
      dplyr::full_join(tracks$animal, c('trial', 'animal')) %>%
      dplyr::ungroup(.)
  }

  if ('trial' %in% .tables & !is.null(tracks$trial)) {
    tracks$trial <- tracks$tr %>%
      dplyr::group_by_(~trial) %>%
      dplyr::summarize_(.dots = conds) %>%
      dplyr::full_join(tracks$trial, 'trial')
  }
  return(tracks)
}

#' @importFrom dplyr select
#' @name select
#' @rdname select_.tracks
#' @export
NULL

#' @importFrom dplyr select_
#' @name select_
#' @rdname select_.tracks
#' @export
NULL

#' Select tables from a tracks object
#'
#' This function allows for convenient selection of some of the tables in a
#' tracks object, usually to manage space or computation times. It will always
#' retain the tr and meta_data tables, as well as the list of parameters.
#'
#' You can use dplyr's special functions inside of select calls, such as
#' starts_with().
#'
#' @param .data A tracks object
#' @param ... Comma seperated list of tables to select. Use \code{-table} to
#'   drop a table.
#' @inheritParams dplyr::select
#'
#' @return A tracks object.
#' @export
select_.tracks <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(names(.data), dots,
                              include = c('tr', 'meta_data', 'params'))
  .data <- .data[vars]
  class(.data) <- c("tracks",class(.data))
  return(.data)
}
