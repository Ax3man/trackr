#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

get_party_df_names <- function(d) {
  fun <- function(x) names(eval(x))
  multidplyr::cluster_call(d$cluster, fun, as.name(d$name))[[1]]
}

remove_empty_shards <- function(tracks) {
  fun <- function(x) nrow(eval(x))
  empty_shards_tr <- any(multidplyr::cluster_call(tracks$tr$cluster, fun,
                                                  as.name(tracks$tr$name))
                         == 0)

  if (!is.null(tracks$soc)) {
    empty_shards_soc <- any(multidplyr::cluster_call(tracks$soc$cluster, fun,
                                                     as.name(tracks$soc$name))
                            == 0)
  } else {
    empty_shards_soc <- FALSE
  }

  if (empty_shards_tr | empty_shards_soc) {
    repartition(tracks)
  }
  return(tracks)
}

repartition <- function(tracks) {
  tracks$tr <- dplyr::collect(tracks$tr)
  if (!is.null(tracks$soc)) {
    tracks$soc <- dplyr::collect(tracks$soc)
  }

  tracks$tr <- multidplyr::partition(tracks$tr, trial)
  if (!is.null(tracks$soc)) {
    tracks$soc <- multidplyr::partition(tracks$soc, trial,
                                        cluster = tracks$tr$cluster)
  }
}

find_conds_in_tables <- function(tracks, conds) {
  vars <- lapply(conds, function(x) all.vars(x$expr))
  tables <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    if (any(vars[[i]] %in% names(conds))) {
      w <- which(vars[[i]] %in% names(conds))
      vars[[i]] <- c(vars[[i]][-w], vars[[vars[[i]][w]]])
    }
  }

  for (i in seq_along(tracks)) {
    tbl <- names(tracks)[i]
    if (tbl %in% c('pr', 'params', 'meta_data')) {
      next
    }
    for (j in seq_along(vars)) {
      if ('party_df' %in% class(tracks[[i]])) {
        if (all(vars[[j]] %in% tracks$pr[[tbl]])) {
          tables[[j]] <- c(tables[[j]], tbl)
        }
      } else {
        if (all(vars[[j]] %in% names(tracks[[tbl]]))) {
          tables[[j]] <- c(tables[[j]], tbl)
        }
      }
    }
  }

  if (any(sapply(tables, function(x) all(is.na(x))))) {
    stop(paste('The following variables(s) caused a problem:\n',
               paste(collapse = ' ',
                     vars[[which(sapply(tables, function(x) all(is.na(x))))]]),
               '\nEither (one of) the variable(s) could not be found,
               or they were not found in the same table.'), call. = FALSE)
  }
  return(tables)
}

find_max_cross_corr <- function(v1, v2, range) {
  cross_corr <- stats::ccf(v1, v2, range, na.action = stats::na.pass,
                           plot = FALSE)
  res <- data.frame(cor = cross_corr$acf[, , 1], lag = cross_corr$lag[, 1, 1])
  res <- res[which.max(res$cor), ]
  return(res)
}

#dots <- lazyeval::all_dots(~speed(), ~acceleration(y = bleh), ~1, ~x,
#                           all_named = TRUE)
add_defaults_to_dots <- function(dots) {
  calls <- lapply(dots, `[[`, 'expr')
  fun_calls <- calls[sapply(calls, class) == 'call']
  funs <- lapply(fun_calls, `[[`, 1)

  matched_calls <- Map(function(x, y) match.call(match.fun(x), y),
                       funs, fun_calls)
  envrs <- lapply(dots, `[[`, 'env')
  calls_list <- lapply(matched_calls, as.list)

  defaults <- lapply(funs, function(.) formals(eval(.)))
  given <- lapply(calls_list, `[`, -1)

  arguments <- Map(function(def, giv) {
    def[names(giv)] <- giv
    return(def)
  }, defaults, given)

  new_calls <- Map(function(x, y) {
    lazyeval::make_call(x, y)},
    funs, arguments)

  dots[sapply(calls, class) == 'call'] <- new_calls
  Map(function(x, y) { x$env <- y; return(x) }, dots, envrs)
}

# dots <- lazyeval::lazy_dots(speed = speed(), av = angular_velocity()) %>%
#   add_defaults_to_dots
# Use lazyeval::interp to fill in any values gives as tracks$params$...
interp_params <- function(dots, params) {
  calls <- lapply(dots, `[[`, 'expr')
  fun_calls <- calls[sapply(calls, class) == 'call']
  funs <- lapply(fun_calls, `[[`, 1)

  matched_calls <- Map(function(x, y) match.call(match.fun(x), y),
                       funs, fun_calls)
  calls_list <- lapply(matched_calls, as.list)
  arguments <- lapply(calls_list, `[`, -1)
  envrs <- lapply(dots, `[[`, 'env')

  with_params <- lapply(arguments, function(x) {
    lapply(x, function(y) {
      y <- as.character(y)
      all(y[1:2] == c('$', 'tracks$params'))
    } )
  } )
  with_params2 <- sapply(with_params, function(x) any(unlist(x)))

  new_arguments <- arguments
  new_arguments[with_params2] <- Map(function(a, w) {
    a[unlist(w)] <- lapply(a[unlist(w)], function(x) as.character(x)[3])
    return(a)
  }, arguments[with_params2], with_params[with_params2])

  new_calls <- Map(function(x, y) {
    lazyeval::make_call(x, y)},
    funs, new_arguments)
  new_calls <- lapply(new_calls, lazyeval::interp, .values = params)

  dots[sapply(calls, class) == 'call'] <- new_calls
  Map(function(x, y) { x$env <- y; return(x) }, dots, envrs)
}

default_summarize_targets <- function(tables) {
  .target <- vector('list', length(tables))
  .target[tables == 'tr'] <- 'animal'
  .target[tables == 'soc'] <- 'pair'
  .target[tables == 'animal'] <- 'trial'
  .target[tables == 'group'] <- 'trial'
  .target[tables == 'pair'] <- 'trial'
  return(.target)
}

#' Convert between frame numbers and human readable time formats.
#'
#' NOTE: see examples for correct usage of \code{times_to_frames}.
#'
#' \code{t2f} and \code{f2t} provide convinient short-hands.
#'
#' @param frames A numerc vector of frame numbers to convert.
#' @param seconds A character vector of times to convert (see examples).
#' @param frame_rate The frame rate of the tracking data in frames per second
#'   (e.g. \code{tracks$params$frame_rate}).
#'
#' @name frame_time
#'
#' @examples
#' # Using frame rate of 1 for clarity of usable formats
#' frames_to_times(1, 1)
#' frames_to_times(120, 1)
#' frames_to_times(3661, 1)
#'
#' times_to_frames('5S', 1)
#' times_to_frames('2M 0S', 1)
#' times_to_frames('1H 1M 1S', 1)
#' times_to_frames('1H1M1S', 1)
#'
#' # Note that the letters are not functional, just spaces work too:
#' times_to_frames('1 1 1', 1)
#' # Which means you can leave out larger denominations, but not smaller ones
#' times_to_frames('10S', 1) == times_to_frames('0M 10S', 1)
#' times_to_frames('2M', 1) == times_to_frames('2M 0S', 1)
NULL

#' @describeIn frame_time Convert frames to human-readable times.
#' @export
frames_to_times <- function(frames, frame_rate) {
  lubridate::seconds_to_period(frames / frame_rate)
}

#' @describeIn frame_time Convert human-readable times to frames.
#' @export
times_to_frames <- function(seconds, frame_rate) {
  date_times <- lubridate::parse_date_time(seconds, c('H!M!S!', 'M!S!', 'S!'))
  secs <- lubridate::seconds(date_times) + lubridate::seconds(62167305600)
  as.numeric(secs * frame_rate)
}

#' @describeIn frame_time Convert human-readable times to frames.
#' @export
f2t <- frames_to_times

#' @describeIn frame_time Convert human-readable times to frames.
#' @export
t2f <- times_to_frames

resolve_time_frame <- function(var, frame_rate) {
  if (is.numeric(var) | is.null(var)) {
    var
  } else {
    times_to_frames(var, frame_rate)
  }
}

time_bin_labels <- function(bins, frame_rate) {
  labels <- frames_to_times(bins, frame_rate)
  labels <- gsub("\\s*\\w*$", "", round(labels))
  labels <- tolower(gsub(" ", "", labels, fixed = TRUE))
  paste(utils::head(labels, -1), 'till', labels[-1])
}
