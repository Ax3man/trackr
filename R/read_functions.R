#' Read idTracker data
#'
#' Loads a txt file containing tracking data from idTracker into R.
#'
#' This function generally assumes that each file is in it's own folder. If not,
#' build a list of file paths beforehand and supply that as file.
#'
#' @param file Either directly supply a path to a file, a list of paths,
#' @param folder Or supply a path to a folder.
#' @param type If folder is supplied, use type to indicate which track type
#'   should be read. Can be either \code{"gaps"} or \code{"nogaps"}.
#' @param multiple If TRUE, will try to find files in the subfolders of folder
#'   and combine them into one long data.frame. Only use when using folder.
#' @param animals Optional vector of names (ordered in the same order as the
#'   tracking id's). Otherwise numbers are used instead.
#'
#' @return A long \code{data.frame} (with class \code{tbl_df} and source
#' \code{idTracker}).
#' @family trackr read functions
#' @export
read_idTracker <- function(file = NULL,
                           folder = NULL,
                           type = "nogaps",
                           multiple = TRUE,
                           animals = NULL) {
  # Input handeling ------------------------------------------------------------
  if ((is.null(file) & is.null(folder)) | (!is.null(file) & !is.null(folder))) {
    stop("Supply either `file` or `folder`.", call. = FALSE)
  }

  if (!is.null(file) & is.character(file)) {
    file <- list(file)
  }

  if (!is.null(folder) & !(type %in% c("gaps", "nogaps"))) {
    stop("No other type than `gaps` or `nogaps` implemented.", call. = FALSE)
  }

  if (!is.null(folder)) {
      if (type == "nogaps") {
        if (multiple) {
          file <- file.path(folder, list.files(folder, 'trajectories_nogaps.txt',
                                               recursive = TRUE))
          if (length(file) == 0)
            stop("No files of this type found in this folder, or its subfolders.",
                 call. = FALSE)
        } else {
          file <- list(file.path(folder, 'trajectories_nogaps.txt'))
        }
      } else {
        if (multiple) {
          file <- file.path(folder, list.files(folder, 'trajectories.txt',
                                               recursive = TRUE))
          if (length(file) == 0)
            stop("No files of this type found in this folder, or its subfolders.",
                 call. = FALSE)
        } else {
          file <- list(file.path(folder, 'trajectories.txt'))
        }
      }
  }

  # Read file ------------------------------------------------------------------
  # We need to skip the first line, since it has one character too many, causing
  # confusion. Doesn't seem to be easily fixable at this point?
  # I use cols(.default) here in case ProbId is 1 for the first 1000 rows, which
  # will cause readr to guess that it is an integer column.
  d <- lapply(file, readr::read_tsv,
              col_names = FALSE,
              col_types = readr::cols(.default = "d"),
              skip = 1)
  # Create the column names
  d <- lapply(d, function(x) {
    new.names <- expand.grid(c('X', 'Y', 'ProbId'), 1:(ncol(x)/3))
    names(x) <- do.call(paste0, as.list(new.names))
    x
  } )
  # Add parent folder as trial names, if applicable
  if (length(file) > 1) {
    folder.names <- strsplit(file, split = '/', fixed = TRUE)
    names(d) <- mapply('[', folder.names, sapply(folder.names, length) - 1)
  }

  # Put in long format ---------------------------------------------------------
  d <- lapply(d, function(x) {
    prob_id. <- unname(do.call(c, x[seq.int(3, ncol(x), 3)]))
    if (is.null(animals))
      animals <- 1:(ncol(x) / 3)
    dplyr::data_frame(
      animal = factor(rep(animals, each = nrow(x))),
      frame = rep(1:nrow(x), ncol(x) / 3),
      X = unname(do.call(c, x[seq.int(1, ncol(x) - 2, 3)])),
      Y = unname(do.call(c, x[seq.int(2, ncol(x) - 1, 3)])),
      prob_id = ifelse(prob_id. >= 0, prob_id., 0),
      reliability = factor(ifelse(prob_id. >= 0,
                                  'ok',
                                  ifelse(prob_id. == -1,
                                         'resegmented',
                                         'unreliable'))))
  } )

  if (length(d) == 1) {
    d <- d[[1]]
  } else {
    d <- dplyr::bind_rows(d, .id = 'trial')
    d$trial <- as.factor(d$trial)
  }

  # Add a source identifier to be used as a parameter later
  attributes(d)$source <- 'idTracker'

  return(d)
}

#' Read Ctrax data
#'
#' Loads a csv or mat file containing tracking data from Ctrax into R.
#'
#' This function always filters out (0, 0) coordinates, as they are a Ctrax bug.
#'
#' @param file Either directly supply a path to a file, a list of paths,
#' @param folder Or supply a path to a folder.
#' @param type If folder is supplied, use type to indicate which track type
#'   should be read. When 'fixed', will try to find files with a 'fixed_'
#'   suffix, when 'raw' those with the suffix will be excluded.
#' @param file_type Whether to use mat files or csv files. Will guess from the
#'   extension if file is used, defaults to 'mat' if folder is used.
#' @param recursive If TRUE, will also try to find files in the subfolders of
#' folder and combine them into one long data.frame. Only use when using folder.
#' @param animals Optional vector of names (ordered in the same order as the
#'   tracking id's). Otherwise file names are used instead.
#'
#' @return A long \code{data.frame} (with class \code{tbl_df} and source
#' \code{idTracker}).
#' @family trackr read functions
#' @export
read_Ctrax <- function(file = NULL,
                       folder = NULL,
                       type = "fixed",
                       file_type = ifelse(is.null(file), "mat",
                                          tools::file_ext(file)),
                       recursive = TRUE,
                       animals = NULL) {
  # Input handeling ------------------------------------------------------------
  if ((is.null(file) & is.null(folder)) | (!is.null(file) & !is.null(folder)))
    stop("Supply either `file` or `folder`.", call. = FALSE)

  if (!(file_type %in% c('csv', 'mat')))
    stop("file_type should be csv or mat.", call. = FALSE)

  if (!(type %in% c('fixed', 'raw')))
    stop('type should be either fixed or raw.')

  # Find the files in folder ---------------------------------------------------
  if (!is.null(folder)) {
    if (type == "fixed") {
      if (recursive) {
        file <- file.path(folder, list.files(folder,
                                             paste0('fixed_\\.*', file_type),
                                             recursive = TRUE))
        if (length(file) == 0)
          stop("No files of this type found in this folder, or its
               subfolders.", call. = FALSE)
      } else {
        file <- file.path(folder, list.files(folder,
                                             paste0('fixed_.\\*', file_type)))
        if (length(file) == 0)
          stop("No files of this type found in this folder.", call. = FALSE)
      }
    }
    if (type == "raw" & recursive) {
      file <- file.path(folder, list.files(folder, paste0('*\\.', file_type),
                                           recursive = TRUE))
      if (length(grep('fixed_', file) > 0))
        file <- file[-grep('fixed_', file)]
      if (length(file) == 0)
        stop("No files of this type found in this folder, or its subfolders.",
             call. = FALSE)
    } else {
      file <- file.path(folder, list.files(folder, paste0('*\\.', file_type)))
      if (length(grep('fixed_', file) > 0))
        file <- file[-grep('fixed_', file)]
      if (length(file) == 0)
        stop("No files of this type found in this folder.", call. = FALSE)
    }
  }
  # Reading data ---------------------------------------------------------------
  d <- switch(file_type,
              csv = read_Ctrax_csv(file, animals),
              mat = read_Ctrax_mat(file, animals))

  # Add file names as trial names, if applicable
  if (length(file) > 1) {
    file.names <- strsplit(basename(file), split = '.', fixed = TRUE)
    file.names <- mapply('[', file.names, -sapply(file.names, length))
    if (any(substr(file.names, 1, 6) == 'fixed_'))
      file.names[substr(file.names, 1, 6) == 'fixed_'] <-
      substr(file.names[substr(file.names, 1, 6) == 'fixed_'], 7, 100L)
    names(d) <- file.names
  }

  if (length(d) == 1) {
    d <- d[[1]]
  } else {
    d <- dplyr::bind_rows(d, .id = 'trial')
    d$trial <- as.factor(d$trial)
  }
  d$animal <- as.factor(d$animal)

  # Get rid of (0, 0) and (1, 1) detections.
  d <- dplyr::filter(d, !(X %in% 0:1) | !(Y %in% 0:1))

  # Add a source identifier to be used as a parameter later
  attributes(d)$source <- 'Ctrax'

  return(d)
}

read_Ctrax_csv <- function(file, animals) {
  # Read file ------------------------------------------------------------------
  d <- lapply(file, readr::read_table, col_names = FALSE)
  # Create the column names
  d <- lapply(d, function(x) {
    new.names <- expand.grid(c('ID', 'X', 'Y', 'major_axis', 'minor_axis',
                               'orientation'), 1:(ncol(x) / 6))
    names(x) <- do.call(paste0, as.list(new.names))
    x
  } )

  # Drop the animal id columns
  d <- lapply(d, function(x) x[-which(substr(names(x), 1, 2) == 'ID')])

  # Put in long format ---------------------------------------------------------
  d <- lapply(d, function(x) {
    if (is.null(animals))
      animals <- 1:(ncol(x) / 5)
    dplyr::data_frame(
      animal = rep(animals, each = nrow(x)),
      frame = rep(1:nrow(x), ncol(x) / 5),
      X = unname(do.call(c, x[seq.int(1, ncol(x) - 4, 5)])),
      Y = unname(do.call(c, x[seq.int(2, ncol(x) - 3, 5)])),
      major_axis = unname(do.call(c, x[seq.int(3, ncol(x) - 2, 5)])),
      minor_axis = unname(do.call(c, x[seq.int(4, ncol(x) - 1, 5)])),
      orientation = unname(do.call(c, x[seq.int(5, ncol(x), 5)])))
  } )
}

read_Ctrax_mat <- function(file, animals) {
  # Read file ------------------------------------------------------------------
  d <- lapply(file, R.matlab::readMat)

  # We have to check wheter it's a 'fixed' file or not, those have different
  # structures for some reason.

  lapply(d, function(x) {
    if (names(x)[1] == "ntargets") {
      if (is.null(animals))
        animals <- as.character(unique(x$identity) + 1)
      xx <- dplyr::data_frame(
        animal = animals[x$identity[, 1] + 1],
        frame = rep.int(seq_along(x$ntargets[, 1]), x$ntargets[, 1]),
        X = x$x.pos[, 1],
        Y = x$y.pos[, 1],
        major_axis = x$maj.ax[, 1],
        minor_axis = x$min.ax[, 1],
        orientation = x$angle[, 1])
      res <- dplyr::arrange_(xx, ~animal, ~frame)
    } else {
      xs <- lapply(1:dim(x$trx)[3], function(i) x$trx[, , i])
      xs2 <- lapply(xs, function(x2) {
        xx <- dplyr::data_frame(
          frame = x2$firstframe:x2$endframe,
          X = x2$x[1, ],
          Y = x2$y[1, ],
          major_axis = x2$a[1, ],
          minor_axis = x2$b[1, ],
          orientation = x2$theta[1, ])
      } )
      res <- dplyr::bind_rows(xs2, .id = 'animal')
      if (!is.null(animals))
        res$animal <- factor(res$animal, labels = animals)
    }
    return(res)
  } )
}
