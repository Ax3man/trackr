get_party_df_names <- function(d) {
  fun <- function(x) names(eval(x))
  multidplyr::cluster_call(d$cluster, fun, as.name(d$name))[[1]]
}

join_tr_to_soc <- function(tracks, select) {
  # Select is a list of ~variables.
  cl <- tracks$tr$cluster

  tr <- dplyr::select_(tracks$tr, .dots = c(~trial, ~animal, ~frame, select))
  tr <- dplyr::collect(tr)

  reg_trials <- lapply(multidplyr::cluster_get(cl, tracks$soc$name),
                       function(x) names(table(x$trial)[table(x$trial) > 0]))
  tr_cl <- lapply(reg_trials, function(trs) dplyr::filter(tr, trial %in% trs))
  multidplyr::cluster_assign_each(cl, 'tr', tr_cl)

  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal1' = 'animal')))

  Names <- as.character(select)
  ren1 <- setNames(select, paste0(substring(Names, 2), 1))
  tracks$soc <- dplyr::rename_(tracks$soc, .dots = ren1)

  tracks$soc <- dplyr::do_(tracks$soc,
                           ~dplyr::left_join(
                             ., tr, by = c('trial', 'frame', 'animal2' = 'animal')))

  ren2 <- setNames(select, paste0(substring(Names, 2), 2))
  tracks$soc <- dplyr::rename_(tracks$soc, .dots = ren2)

  return(tracks)
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
  return(tracks)
}

find_conds_in_tables <- function(tracks, conds) {
  vars <- lapply(conds, function(x) all.vars(x$expr))
  tables <- rep(NA, length(vars))
  tables[sapply(vars, function(x) all(x %in% tracks$pr$tr))] <- 'tr'
  if (!is.null(tracks$soc)) {
    tables[sapply(vars, function(x) all(x %in% tracks$pr$soc))] <- 'soc'
  }
  if (!is.null(tracks$group)) {
    tables[sapply(vars, function(x) all(x %in% names(tracks$group)))] <- 'group'
  }
  if (any(is.na(tables))) {
    stop(paste('The following variables(s) caused a problem:\n',
               paste(collapse = ' ', vars[[which(is.na(tables))]]),
               '\nEither (one of) the variable(s) could not be found,
               or they were not found in the same table.'), call. = FALSE)
  }
  return(tables)
}

find_max_cross_corr <- function(v1, v2, range) {
  cross_corr <- ccf(v1, v2, range, na.action = na.pass, plot = FALSE)
  res <- data.frame(cor = cross_corr$acf[, , 1], lag = cross_corr$lag[, 1, 1])
  res = res[which.max(res$cor), ]
  return(res)
}

#' Convert between frame numbers and human readable time formats.
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
  secs <- lubridate::seconds(date_times) + lubridate::seconds(62167219200)
  as.numeric(secs * frame_rate)
}

resolve_time_frame <- function(var, frame_rate) {
  if (is.numeric(var) | is.null(var)) {
    var
  } else {
    times_to_frames(var, frame_rate)
  }
}

time_bin_labels <- function(bins, frame_rate) {
  labels <- frames_to_time(bins, frame_rate)
  labels <- gsub("\\s*\\w*$", "", round(labels))
  labels <- tolower(gsub(" ", "", labels, fixed = TRUE))
  paste(head(labels, -1), 'till', labels[-1])
}
