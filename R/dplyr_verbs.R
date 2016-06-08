#' @importFrom dplyr collect
#' @export
dplyr::collect

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
#' Method for the dplyr verb \code{filter}. Filters the tracks object according
#' to logical filtering statements, in a strict sense. If one of the filtering
#' conditions cannot be be applied to all tables present, it will either give an
#' error (\code{drop = FALSE}) or will delete the tables that can't be filtered
#' from the tracks object (\code{drop = FALSE}), see details. For another
#' strategy, see \code{find_sections} and \code{summarize_sections}.
#'
#' @param .data A tracks object.
#' @param ... The filtering conditions (logical statements).
#' @param drop Whether to drop conflicting aggregate data. If the tracks object
#'   contains data that was aggregated over a variable that is now being used to
#'   filter, drop must be TRUE or an error will be raised. That is, any table
#'   that can not be filtered (i.e. it does not contain the variable used to
#'   filter) will be dropped. Have to opt in, since aggregation may have been
#'   expensive.
#' @param repartition If TRUE, will collect and reassign the \code{tr} and
#'   \code{soc} tables. Useful if worker nodes become empty or unbalanced.
#' @inheritParams dplyr::filter
#'
#' @return The subsetted tracks object.
#'
#' @section Strategy on conflicting dependecies:
#'
#'   It will apply the fitering to all the applicable sections of the tracks
#'   object. It will also attempt to check for any dependency problems. This is
#'   crucial in order to maintain internal consistency in the tracks object. For
#'   example, if you select a sequence of frames, any data that was aggregated
#'   over time (such as in the \code{trial} table) will now no longer match. If
#'   conflicts are found, by default, conflicted tables will be deleted from
#'   the tracks object, with a message. By setting \code{drop = FALSE}, you can
#'   have the function error instead.
#'
#' @export
#' @seealso \link[dplyr]{filter}, \link{find_sections},
#'   \link{summarise_sections}
filter_.tracks <- function(.data, ..., drop = TRUE, .dots,
                           repartition = FALSE) {
  tracks <- .data
  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  # Extract special arguments from ...
  if (any(names(conds) == 'drop')) {
    drop <- lazyeval::lazy_eval(conds$drop)
    conds <- conds[-which(names(conds) == 'drop')]
  }
  if (any(names(conds) == 'repartition')) {
    repartition <- lazyeval::lazy_eval(conds[['repartition']])
    conds <- conds[-which(names(conds) == 'repartition')]
  }
  # extract which things those conditions apply to
  tables <- find_conds_in_tables(tracks, conds)
  to_be_kept <- c(Reduce(intersect, tables), 'pr', 'params', 'meta_data')
  tables <- lapply(tables, function(x) x <- x[x %in% to_be_kept])

  if (!(all(names(tracks) %in% to_be_kept))) {
    if (drop) {
      message('The following tables were dropped:\n',
              paste(names(tracks)[!(names(tracks) %in% to_be_kept)],
                    collapse = ', '))
      tracks <- tracks[to_be_kept]
      class(tracks) <- c('tracks', class(tracks))
    } else {
      stop('This filter cannot be applied to all tables present.
           Set drop to TRUE if you want to drop the tables that can\'t be filtered',
           call. = FALSE)
    }
  }

  for (i in seq_along(conds)) {
    tracks[tables[[i]]] <- lapply(tracks[tables[[i]]], dplyr::filter_,
                                  .dots = conds[i])
  }

  if (repartition) {
    tracks <- repartition(tracks)
  }
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

#' Summarize a tracks variable into higher tables.
#'
#' @param .data A tracks object.
#' @param ... Summary expressions, such as \code{mean(speed, na.rm = TRUE)}, or
#'   \code{var_nnd = var(nearest_neighbour_dist, na.rm = TRUE)}.
#' @param .source Which table those variables come from (\code{tr} or
#'   \code{pairs}).
#' @param .tables To which tables you want to summarize them. By default
#'   \code{group}, \code{animal} and \code{trial}, when present.
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
                              .tables = c('group', 'animal', 'pair', 'trial'),
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
  Source <- switch(.source,
                   'tr' = tracks$tr,
                   'soc' = tracks$pairs)
  vars <- names(conds)

  if ('group' %in% .tables & !is.null(tracks$group)) {
    if (any(names(tracks$group) %in% vars)) {
      tracks$group <- tracks$group[-which(names(tracks$group) %in% vars)]
    }
    Group <- dplyr::group_by_(Source, ~trial, ~frame)
    Group <- dplyr::summarize_(Group, .dots = conds)
    Group <- dplyr::collect(Group)
    tracks$group <- dplyr::full_join(Group, tracks$group, c('trial', 'frame'))
    rm(Group)
    tracks$group <- dplyr::ungroup(tracks$group)
  }

  if ('animal' %in% .tables & !is.null(tracks$animal) & .source == 'tr') {
    if (any(names(tracks$animal) %in% vars)) {
      tracks$animal <- tracks$animal[-which(names(tracks$animal) %in% vars)]
    }
    Animal <- dplyr::group_by_(Source, ~trial, ~animal)
    Animal <- dplyr::summarize_(Animal, .dots = conds)
    Animal <- dplyr::collect(Animal)
    tracks$animal <- dplyr::full_join(Animal, tracks$animal,
                                      c('trial', 'animal'))
    rm(Animal)
    tracks$animal <- dplyr::ungroup(tracks$animal)
  }

  if ('pair' %in% .tables & !is.null(tracks$pair) & .source == 'soc') {
    if (any(names(tracks$pair) %in% vars)) {
      tracks$pair <- tracks$pair[-which(names(tracks$pair) %in% vars)]
    }
    Pair <- dplyr::group_by_(Source, ~trial, ~animal1, ~animal2)
    Pair <- dplyr::summarize_(Pair, .dots = conds)
    Pair <- dplyr::collect(Pair)
    tracks$pair <- dplyr::full_join(Pair, tracks$animal,
                                    c('trial', 'animal1', 'animal2'))
    rm(Pair)
    tracks$pair <- dplyr::ungroup(tracks$pair)
  }

  if ('trial' %in% .tables & !is.null(tracks$trial)) {
    if (any(names(tracks$trial) %in% vars)) {
      tracks$trial <- tracks$trial[-which(names(tracks$trial) %in% vars)]
    }
    Trial <- dplyr::group_by_(Source, ~trial)
    Trial <- dplyr::summarize_(Trial, .dots = conds)
    Trial <- dplyr::collect(Trial)
    tracks$trial <- dplyr::full_join(Trial, tracks$trial, 'trial')
  }
  return(tracks)
}

#' @importFrom dplyr mutate
#' @name mutate
#' @rdname mutate_.tracks
#' @export
NULL

#' @importFrom dplyr mutate_
#' @name mutate_
#' @rdname mutate_.tracks
#' @export
NULL

#' Add new variables to a tracks object.
#'
#' @param .data A tracks object
#' @inheritParams dplyr::mutate
#'
#' @return A tracks object.
#' @export
mutate_.tracks <- function(.data, ..., .dots) {
  tracks <- .data

  conds <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  conds <- add_defaults_to_dots(conds)
  conds <- interp_params(conds, tracks$params)
  tables <- find_conds_in_tables(tracks, conds)

  for (i in seq_along(conds)) {
    .data[tables[[i]]] <- lapply(tracks[tables[[i]]], dplyr::mutate_,
                                 .dots = conds[i])
    if (tables[[i]] %in% names(tracks$pr)) {
      tracks$pr[tables[[i]] == names(tracks$pr)][[1]] <-
        c(tracks$pr[tables[[i]] == names(tracks$pr)][[1]],
          names(conds[i]))
    }
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
#' retain the tr and meta_data tables, as well as internal components.
#'
#' You can drop a table by writing it as a negative.
#'
#' You can use dplyr's special functions inside of select calls, such as
#' \code{starts_with()}.
#'
#' @param .data A tracks object
#' @param ... Comma seperated list of tables to select. Use \code{-table} to
#'   drop a table.
#' @inheritParams dplyr::select
#'
#' @return A tracks object.
#' @export
select_.tracks <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  vars <- dplyr::select_vars_(names(.data), dots,
                              include = c('tr', 'meta_data', 'params', 'pr'))
  .data <- .data[vars]
  .data$pr <- .data$pr[names(.data$pr) %in% vars]
  class(.data) <- c("tracks", class(.data))
  return(.data)
}
