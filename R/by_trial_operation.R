#' Set a new starting frame
#'
#' Pick a new frame as the starting frame, which will become frame 1. You can
#' use either a single frame number for all trials using \code{value}, or you
#' can supply a different frame number for each trial using \code{tab}.
#'
#' All values below the start frame will be filtered out.
#'
#' @param tracks A tracks object.
#' @param value A single value to set as the start frame.
#' @param tab A data.frame with trial names and start frames. Use \code{frame}
#'   and \code{trial} to choose which variables are used.
#' @param frame The column from \code{tab} that is used as the starting frame.
#'   By default this is \code{'frame'}.
#' @param trial The column from \code{tab} that is used as the trial identifier.
#'   By default this is \code{'trial'}.
#'
#' @return A tracks object.
#' @export
#'
#' @examples
#'   Guppies <- as_tracks(guppies, 30, '1080p')
#'   # Normally this dataset starts from frame 10000:
#'   Guppies$tr
#'   # But we can easily set that to 1 instead:
#'   set_start_frame(Guppies, 10000)$tr
#'   # Or we can use different frame numbers per trial:
#'   frame_table <- data.frame(trial = c('a', 'b'), frame = c(5000, 10000))
#'   set_start_frame(Guppies, tab = frame_table)$tr
#'   # Since some frames were fropper,
#'
set_start_frame <- function(tracks, value = NULL, tab = NULL, frame = 'frame',
                            trial = 'trial') {
  if ((is.null(value) & is.null(tab)) | (!is.null(value) & !is.null(tab))) {
    stop('You have to supply either value or table.')
  }
  if (!is.null(value)) {
    call_ <- list(frame = lazyeval::interp(~frame - x, x = value))
    tracks <- dplyr::mutate_(tracks, .dots = call_)
  } else {
    tracks <- by_trial_operation(tracks, tab = tab, var = frame,
                                 operation = `-`, trial = trial)
  }
  filter(tracks, frame > 0)
}

#' Perform an arbitrary operation with a different constant for each trial.
#'
#' @param tracks
#' @param tab
#' @param operation
#' @param var
#' @param trial
#'
#' @return
#' @export
#'
#' @examples
by_trial_operation <- function(tracks, tab, var, operation = `+`,
                               trial = 'trial') {
  # make tab nice
  tab <- dplyr::select_(tab, var, trial)
  names(tab)[1] <- c('..to_use_var..')

  update_table <- function(d) {
    d <- dplyr::left_join(d, tab, by = 'trial')
    d <- dplyr::mutate_(
      d,
      .dots = setNames(lazyeval::interp(~operation(a, ..to_use_var..), a = as.name(var)), var)
    )
    dplyr::select_(d, ~-..to_use_var..)
  }
  ### TR
  tracks$tr <- update_table(tracks$tr)

  ### SOC
  if (!is.null(tracks$soc)) {
    tracks$soc <- update_table(tracks$soc)
  }
  ### GROUP
  if (!is.null(tracks$group)) {
    tracks$group <- update_table(tracks$group)
  }
  return(tracks)
}
