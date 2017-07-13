#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
    if (tbl %in% c('params', 'meta_data')) {
      next
    }
    for (j in seq_along(vars)) {
      if (all(vars[[j]] %in% names(tracks[[tbl]]))) {
        tables[[j]] <- c(tables[[j]], tbl)
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

# dots <- lazyeval::all_dots(~speed(), ~acceleration(y = bleh), ~1, ~x, ~speed(a) > speed(),
#                           all_named = TRUE)
add_defaults_to_dots <- function(dots) {
  # A recursive function that continues to add defaults to lower and lower levels.
  add_defaults_to_expr <- function(expr) {
    # First, if a call is a symbol or vector, there is nothing left to do but
    # return the value (since it is not a function call).
    if (is.symbol(expr) | is.vector(expr) | class(expr) == "formula") {
      return(expr)
    }
    # If it is a function however, we need to extract it.
    fun <- expr[[1]]
    # If it is a primitive function (like `+`) there are no defaults, and we
    # should not manipulate that call, but we do need to use recursion for cases
    # like a + f(b).
    if (is.primitive(match.fun(fun))) {
      new_expr <- expr
    } else {
      # If we have an actual non-primitve function call, we formally match the
      # call, so abbreviated arguments and order reliance work.
      matched_expr <- match.call(match.fun(fun), expr, expand.dots = TRUE)
      expr_list <- as.list(matched_expr)
      # Then we find the defualt arguments:
      arguments <- formals(eval(fun))
      # And overwrite the defaults for which other values were supplied:
      given <- expr_list[-1]
      arguments[names(given)] <- given
      # And finally build the new call:
      new_expr <- as.call(c(fun, arguments))
    }
    # Then, for all function arguments we run the function recursively.
    new_arguments <- as.list(new_expr)[-1]
    null <- sapply(new_arguments, is.null)
    new_arguments[!null] <- lapply(new_arguments[!null], add_defaults_to_expr)
    new_expr <- as.call(c(fun, new_arguments))
    return(new_expr)
  }
  # For lazy dots supplied, seperate the expression and environments.
  exprs <- lapply(dots, `[[`, 'expr')
  envrs <- lapply(dots, `[[`, 'env')
  # Add the defaults to the expressions.
  new_exprs <- lapply(exprs, add_defaults_to_expr)
  # Add back the correct environments.
  new_calls <- Map(function(x, y) {
    lazyeval::as.lazy(x, y)
  }, new_exprs, envrs)
  return(new_calls)
}

# dots <- lazyeval::lazy_dots(speed = speed(), av = angular_velocity()) %>%
#   add_defaults_to_dots
# Use lazyeval::interp to fill in any values gives as tracks$params$...
interp_params <- function(dots, params) {
  calls <- lapply(dots, `[[`, 'expr')

  fun_calls <- calls[sapply(calls, class) == 'call']
  funs <- lapply(fun_calls, `[[`, 1)
  primitives <- sapply(funs, function(x) is.primitive(match.fun(x)))

  if (all(primitives)) {
    return(dots)
  }

  fun_calls <- fun_calls[!primitives]
  funs <- funs[!primitives]
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
  new_calls <- lapply(new_calls[!primitives], lazyeval::interp,
                                   .values = params)

  dots[sapply(calls, class) == 'call'][!primitives] <- new_calls
  dots[!primitives] <- Map(function(x, y) { x$env <- y; return(x) },
      dots[!primitives], envrs[!primitives])
  return(dots)
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

