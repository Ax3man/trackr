if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Create a tracks object from a data.frame.
#'
#' @param tr Data.frame with tracking data (from read in functions).
#' @param frame_rate Frames per second.
#' @param resolution Provide standard name for resolution (e.g. "720") or actual
#'    resolution in pixels (e.g. "1920x1080"). Stored for later use, also
#'    used as default tracking window (boundary).
#' @param meta_data Optionally supply a data.frame with meta data about the
#'   trials, such as treatments. Can include a column named \code{trial} that
#'   needs to match \code{levels(tr$trial)}. Otherwise this column is created,
#'   using \code{levels(tr$trial)}, and identical ordering is assumed.
#' @param px_per_cm Length scaling factor, optional.
#' @param minimal When TRUE, the minimum amount of components (see list below)
#'   are including in the object. When FALSE, the group, pairs and trial
#'   components will already be generated.
#' @return A tracks object.
#'
#' @section Object structure:
#'
#' The tracks object is a \code{list} with the following tables:
#' \describe{
#'   \item{\code{tr}}{\code{data.frame} organized by trial, animal, frame.
#'   Useful for storing individual variables such as location, speed,
#'   orientation, distance to object etc.}
#'   \item{\code{group}}{\code{data.frame} organized by trial, frame. Useful for
#'   storing group level variables, such as polarization, centrality, group
#'   size, mean neighbour distance etc.}
#'   \item{\code{pairs}}{\code{data.frame} organized by trial, pair of animals,
#'   frame. Useful for storing social variables, such as distance, relative
#'   orientation, vector correlation etc.}
#'   \item{\code{location}}{\code{data.frame} organized by trial, xbin, ybin.
#'   Useful for aggregate data for spatial heatmaps etc.}
#'   \item{\code{meta_data}}{\code{data.frame} organized by trial. Contains
#'   information about the trial, such as group origin, treatment, time of
#'   testing etc.}
#'   \item{\code{animal}}{\code{data.frame} organized by trial and animal,
#'   containing animal level measurements (nested in trial), such as average
#'   speed.}
#'   \item{\code{trial}}{\code{data.frame} organized by trial, containing trial
#'   level measurements, such as average speed.}
#'   \item{\code{params}}{A \code{list} with general experiment parameters, such
#'   as frame_rate, scale, resolution etc.}
#' }
#'  \code{tracks}, \code{meta_data} and \code{params} are always present, others
#'  may not be. This is to preserve memory space. Order is not garantueed, refer
#'  by name if you need to access them. By definiton, this function create a
#'  tracks objects with the tracks, meta.data and params, but no other entries.
#'
#'  Methods for common generics may be available.
#'
#' @seealso \code{\link{read_idTracker}}
#' @export
as_tracks <- function(tr, frame_rate, resolution, meta_data = NULL,
                      px_per_cm = NULL, minimal = TRUE) {
  # Input handling -------------------------------------------------------------
  if (!is.data.frame(tr))
    stop('tr has to be a data.frame.', call. = FALSE)
  if (!is.numeric(frame_rate) | length(frame_rate) > 1)
    stop('frame_rate has to be a numeric vector of length 1.', call. = FALSE)

  res <- as.character(resolution)

  # Define bounds (default) #---------------------------------------------------
  if (res %in% c('480', '720', '1080')) {
    ress <- list('480' = c(640, 480),
                 '720' = c(1280, 720),
                 '1080' = c(1920, 1080))
    res <- as.numeric(ress[[res]])
  } else {
    res <- as.numeric(strsplit(res, split = '[xX]')[[1]])
  }
  bounds <- matrix(c(0, 0, 0, res[2], res[1], res[2], res[1], 0), 2,
                   dimnames = list(c('x', 'y'), c('ll', 'ul', 'ur', 'lr')))

  # Check or create meta_data table --------------------------------------------
  if (is.null(meta_data)) {
    if (is.null(tr$trial)) {
      meta_data <- dplyr::data_frame(trial = factor(1))
    } else {
      meta_data <- dplyr::data_frame(trial = levels(tr$trial))
    }
  } else {
    if (is.null(meta_data$trial)) {
      if (nrow(meta_data) != length(levels(tr$trial)))
        stop('Number of rows in meta_data is not equal to the number of
             different trials in tr.', call. = FALSE)
      meta_data$trial <- levels(tr$trial)
    } else {
      if (!setequal(levels(tr$trial), levels(tr$trial)))
        stop('The factor levels of trial in meta_data and tr do not match.',
             call. = FALSE)
      # if we arrive here we just leave meta_data as supplied.
    }
  }

  # Build object and return ----------------------------------------------------
  tracks <- list(tr = tr,
                 meta_data = meta_data,
                 params = list(frame_rate = frame_rate,
                               resolution = res,
                               bounds = bounds,
                               px_per_cm = px_per_cm,
                               source = attributes(tr)$source))
  tracks$params <- tracks$params[!sapply(tracks$params, is.null)]

  if (minimal) {
    attributes(tracks)$class <- 'tracks'
    return(tracks)
  } else {
    expand_tracks(tracks)
  }
}

#' Add missing components to a tracks object.
#'
#' This function will add the group, pairs and trial components to a track
#' object, if they are currently not included. Note that the pairs component
#' will be increase the size by a lot, if there is more than a few animals per
#' trial.
#'
#' Note that this function is similar to running \code{as_tracks(tracks$tr, ...)
#' }, but safer (i.e. no loss of data).
#'
#' If group, pairs and trial are all false, it will return tracks.
#'
#' @param tracks A tracks object.
#' @param group If TRUE, group will be checked for and added if missing.
#' @param pairs If TRUE, pairs will be checked for and added if missing.
#' @param animal If TRUE, animal will be checked for and added if missing.
#' @param trial If TRUE, trial will be checked for and added if missing.
#'
#' @return A tracks object.
#' @export
#'
#' @examples expand_tracks(guppies)
expand_tracks <- function(tracks,
                          group = TRUE,
                          pairs = TRUE,
                          animal = TRUE,
                          trial = TRUE) {
  Group <- Pairs <- Animal <- Trial <- NULL
  # Build group object ---------------------------------------------------------
  if (group & is.null(tracks$group)) {
    Group <- dplyr::group_by_(tracks$tr, ~trial, ~frame)
    Group <- dplyr::summarize(Group)
    Group <- dplyr::ungroup(Group)
  }

  # Build pairs object (becomes very large with many animals) ------------------
  if (pairs & is.null(tracks$pairs)) {
    # Might have to look for a faster way (without groups and using nesting)
    Pairs <- dplyr::mutate_(tracks$tr, .dots = list(animal2 = ~animal))
    Pairs <- dplyr::group_by_(Pairs, ~trial)
    Pairs <- tidyr::expand_(Pairs, dots = list(~frame, ~animal, ~animal2))
    Pairs <- dplyr::filter_(Pairs, ~animal != animal2)
    Pairs <- dplyr::rename_(Pairs, .dots = list(animal1 = ~animal))
    Pairs <- dplyr::ungroup(Pairs)
  }

  #Build animal object ---------------------------------------------------------
  if (animal & is.null(tracks$animal)) {
    Animal <- dplyr::group_by_(tracks$tr, ~trial, ~animal)
    Animal <- dplyr::summarize(Animal)
    Animal <- dplyr::ungroup(Animal)
  }

  #Build trial object ----------------------------------------------------------
  if (trial & is.null(tracks$trial)) {
    Trial <- dplyr::group_by_(tracks$tr, ~trial)
    Trial <- dplyr::summarize(Trial)
  }

  # Build object and return ----------------------------------------------------
  tracks <- c(tracks,
              list(group = Group),
              list(pairs = Pairs),
              list(animal = Animal),
              list(trial = Trial))
  attributes(tracks)$class <- 'tracks'

  return(tracks)
}

#' @describeIn as_tracks Test if object is of class tracks.
#' @param x An object.
#' @export
is.tracks <- function(x) {
  is(x, 'tracks')
}

#' Report track completeness per trial
#'
#' The report gives two statistics:
#' - complete: which is simply the percentage of frame by animal by trial
#'   combinations for which the vars are there.
#' - per frame: which checks for what percentage of frames all animals are
#' complete. This is useful for social parameters, for which all individuals are
#' needed.
#'
#' If some frames were poorly tracked, the two parameters will be similar. If
#' there are large differences, then there is many frames in which a portion of
#' the individuals gets.
#'
#' @section Note:
#' This is only useful if their are persistent id's across the video. If there
#' are any spurious detections, this will lead to very low scores.
#'
#' @param tracks A tracks object
#' @param vars The variables in the $tr table that need to be present to count
#'   towards completeness.
#' @param lower_limit If a percentage is below this limit, the cell in the table
#'   will be marked with a *.
#'
#' @return Prints a table, silently returns the table too.
#' @export
check_complete <- function(tracks, vars = c('X', 'Y'), lower_limit = 95,
                           parallel = TRUE) {
  check_parallel(parallel)
  # Write a much faster solution perhaps. This is slow, but flexible.
  tr <- tracks$tr
  tr <- do.call(dplyr::select_,c(list(tr), c('trial', 'frame', 'animal', vars)))
  if (parallel) {
    cl <- multidplyr::create_cluster(min(parallel::detectCores(),
                                         length(levels(tr$trial))) - 1)
    multidplyr::cluster_assign_value(cl, 'vars', vars)
    tr <- multidplyr::partition(tr, trial, cluster = cl)
  } else {
    tr <- dplyr::group_by_(tr, ~trial)
  }
  # We left_join a data.frame with the complete range of frames for every animal
  tr <- dplyr::do(tr,
                  dplyr::left_join(
                    data.frame(trial = .$trial[1],
                               frame = min(.$frame):max(.$frame),
                               animal =
                                 rep(unique(.$animal),
                                     max(.$frame) - min(.$frame) + 1)),
                    ., by = c('frame', 'animal', 'trial')))
  # Count the NA's in vars
  tab <- dplyr::do(tr,
                   complete = nrow(.[complete.cases(.), vars]) / nrow(.))
  if (parallel) {
    tab <- dplyr::collect(tab)
    tab <- dplyr::arrange(tab, trial)
    tr <- dplyr::collect(tr)
    tr <- multidplyr::partition(tr, trial, frame)
  } else {
    tr <- dplyr::group_by_(tr, ~trial, ~frame)
  }
  f <- function(dat, var) {
    dots <- setNames(list(lazyeval::interp(~anyNA(v), v = as.name(var))), 'na')
    dplyr::summarize_(dat, .dots = dots)
  }
  l <- lapply(vars, f, dat = tr)
  if (parallel)
    l <- lapply(l, dplyr::collect)
  l <- dplyr::bind_cols(l[[1]][, c('trial', 'frame')],
                        dplyr::bind_cols(lapply(l, '[', , 'na')))
  names(l) <- 1:ncol(l)
  l <- split(l, l$`1`)
  l <- lapply(l, '[', , c(-1, -2))
  l <- lapply(l, rowSums)
  l <- unlist(lapply(l, function(x) sum(x == 0) / length(x)))
  # Prepare a nice table
  tab2 <- data.frame(tab[, 1], unname(unlist(tab[, 2])), unname(l))
  names(tab2) <- c('Trial', 'Complete', 'Per frame')
  tab3 <- tab2
  tab3[, 2:3] <- round(tab3[, 2:3] * 100, 2)
  tab5 <- tab4 <- tab3
  tab4[, 2:3] <- apply(tab3[, 2:3], 2, paste0, '%')
  for (i in 2:3) tab4[, i] <- ifelse(tab3[, i] > lower_limit, tab3[, i],
                                     paste('*', tab3[, i]))
  for (i in 2:3) tab5[, i] <- ifelse(tab3[, i] > lower_limit, tab3[, i],
                                     paste(tab3[, i], '*'))
  if (requireNamespace('knitr', quietly = TRUE))
    print(knitr::kable(tab5))
  else
    print(tab4)
  return(invisible(tab2))
}
