#' Plot paths
#'
#' Only plots a path between subsequent frames.
#'
#' @param x A tracks object.
#' @param tracks A tracks object.
#' @param color Color specification of the path as a formula (e.g. ~animal).
#' @param facet Facet specification as a formula (e.g. ~trial), passed to
#'   facet_wrap.
#' @param nrow Control number of rows for the facets.
#' @param ncol Control number of columns for the facets.
#' @param ... Pass arguments to \code{plot_tracks}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' plot(guppies)
#' plot(guppies, color = NULL) # No animal coloring
#' plot(guppies, facet = ~1) # No facets
#' plot(guppies, facet = ~trial + animal, ncol = 8) # more complex facetting
plot_tracks <- function(tracks, color = ~animal, facet = ~trial,  nrow = NULL,
                        ncol = NULL) {
  if ((facet == ~trial) & length(levels(tracks$tr$trial)) == 1)
    facet <- ~1

  # We need to add a grouping factor in order to create gaps when there are
  # non-subsequent frames.
  tracks$tr <- dplyr::mutate_(tracks$tr,
                              gap = ~ifelse(frame == 1 + dplyr::lag(frame), 0, 1),
                              .GROUP = ~c(0, cumsum(gap[-1])))

  ggplot2::ggplot(dplyr::collect(tracks$tr),
                  ggplot2::aes_(~X, ~Y, color = color, group = ~.GROUP)) +
    ggplot2::geom_path() +
    ggplot2::coord_equal(xlim = tracks$params$bounds[1, c(1, 3)],
                         ylim = tracks$params$bounds[2, c(1, 3)],
                         expand = FALSE) +
    ggplot2::facet_wrap(facet, nrow, ncol) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

#' Facet paths using time bins.
#'
#' @param tracks A tracks object.
#' @param x Formula for what should be on the x-axis.
#' @param y Formula for what should be on the y-axis.
#' @param time_bins The number of periods time should be divided in.
#' @param color Formula to color the paths.
#' @param mode Either dual, which uses both the rows and columns of the facets
#'   to display time bins, or manual, where you can manually set the formula for
#'   facet_grid.
#' @param nrow Override the number of rows that should be used in facetting.
#'   Only used if mode is dual.
#' @param scales Optional setting to facets. See ?ggplot2::facet_grid and
#'   ?ggplot2::facet_wrap.
#' @param facet Optional facetting, should probably include time_bin. Only used
#'   if mode is set to manual.
#' @param coord_boundary Whether to fix the plot limits to the boundary. Will
#'   attempt to autodetect whether plotting ~X and ~Y.
#'
#' @return A \code{ggplot} object.
#' @export
plot_time_facets <- function(tracks, x = ~X, y = ~Y, time_bins = 4,
                             color = ~animal, mode = 'dual', nrow = NULL,
                             scales = 'free_x', facet = trial ~ time_bin,
                             coord_boundary = NULL) {
  # We need to add a grouping factor in order to create gaps when there are
  # non-subsequent frames.
  tracks$tr <- dplyr::mutate_(tracks$tr,
                              gap = ~ifelse(frame == 1 + lag(frame), 0, 1),
                              .GROUP = ~c(0, cumsum(gap[-1])))
  facet <- switch(mode,
                  manual = ggplot2::facet_grid(facet, scales = scales),
                  dual = ggplot2::facet_wrap(~time_bin, nrow, scales = scales))
  if (is.null(coord_boundary)) {
    if (x == ~X & y == ~Y)
      coord_boundary <- TRUE
    else
      coord_boundary <- FALSE
  }
  pdat <- dplyr::collect(tracks$tr)
  bins <- seq(min(pdat$frame, na.rm = TRUE),
              max(pdat$frame, na.rm = TRUE),
              length.out = time_bins + 1)

  labels <- time_bin_labels(bins, tracks$params$frame_rate)

  pdat$time_bin <- findInterval(pdat$frame, bins, all.inside = TRUE)
  pdat$time_bin <- factor(pdat$time_bin, labels = labels)

  p <- ggplot2::ggplot(pdat, ggplot2::aes_(x, y, color = color,
                                           group = ~.GROUP)) +
    ggplot2::geom_path() +
    facet +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  if (coord_boundary)
    p + ggplot2::coord_equal(xlim = tracks$params$bounds[1, c(1, 3)],
                             ylim = tracks$params$bounds[2, c(1, 3)],
                             expand = FALSE)
  else
    p
}

#' Plot sparklines for several track variables.
#'
#' @param tracks A tracks object.
#' @param trial A character vector indicating from which trial to plot.
#' @param start Start of section to plot (either in frames or time).
#' @param end End of section to plot (either in frames or time).
#' @param vars A character vector indicating which variables get a sparkline.
#'   They will be looked for in the $tr and $pairs tables. Optional. When not
#'   given, will plot all variables available.
#' @param point_events An optional vector of point events to highlight with
#'   vertical dotted lines (either frame numbers or times).
#' @param window The time that should be plotted around the events, i.e. total
#'   size of the window.
#' @param quantiles A vector of length two with the probabilities to be used for
#'   the shade grey quantile boxes.
#'
#'  If frames is left NULL, but point_events and window are provided then a
#'  window around the point_events.
#'
#' @return A \code{ggplot} object.
#' @export
plot_tracks_sparklines <- function(tracks, trial, start = NULL, end = NULL,
                                   vars = NULL, point_events = NULL,
                                   window = 600, quantiles = c(0.025, 0.975)) {
  start <- resolve_time_frame(start, tracks$params$frame_rate)
  end <- resolve_time_frame(end, tracks$params$frame_rate)
  point_events <- resolve_time_frame(point_events, tracks$params$frame_rate)
  window <- resolve_time_frame(window, tracks$params$frame_rate)

  if (is.null(start) | is.null(end)) {
    if (is.null(point_events) | is.null(window)) {
      stop('Provide either start and end, or point_events and window.',
           call. = FALSE)
    }
    start <- min(point_events) - window / 2
    end <- max(point_events) + window / 2
  }

  if (is.null(vars)) {
    vars <- c(tracks$pr$tr, tracks$pr$soc)
  }

  sel <- list(trial = trial, start = start, end = end)
  multidplyr::cluster_assign_value(tracks$tr$cluster, 'sel', sel)
  tracks <- filter_(tracks, ~trial %in% sel[['trial']], drop = TRUE)

  tr <- dplyr::collect(tracks$tr)
  tr <- dplyr::ungroup(tr)
  tr <- dplyr::select_(tr,
                       .dots = c('animal', 'frame', vars[vars %in% names(tr)]))
  tr <- tidyr::gather_(tr, 'var', 'value',
                       names(tr)[!(names(tr) %in% c('animal', 'frame'))])
  tr$animal <- as.character(tr$animal)

  soc <- dplyr::collect(tracks$soc)
  soc <- dplyr::ungroup(soc)
  soc <- dplyr::mutate_(soc, animal = ~paste(animal1, animal2, sep = '-'))
  soc <- dplyr::select_(soc, .dots = c('animal', 'frame',
                                       vars[vars %in% names(soc)]))
  soc <- tidyr::gather_(soc, 'var', 'value',
                        names(soc)[!(names(soc) %in% c('animal', 'frame'))])

  pdat <- dplyr::bind_rows(tr, soc)
  pdat <- dplyr::group_by_(pdat, ~animal, ~var)
  pdat$animal <- factor(pdat$animal, unique(pdat$animal))
  pdat$var <- factor(pdat$var, vars)

  quants <- dplyr::group_by_(pdat, ~var)
  quants <- dplyr::summarise_(quants,
                              quant1 = ~quantile(value, quantiles[1],
                                                 na.rm = TRUE),
                              quant2 = ~quantile(value, quantiles[2],
                                                 na.rm = TRUE))
  quants <- dplyr::right_join(quants, pdat, by = 'var')

  pdat <- dplyr::filter_(pdat, ~frame %in% sel[['start']]:sel[['end']])
  quants <- dplyr::group_by_(quants, ~var, ~animal)
  quants <- dplyr::filter_(quants, ~frame %in% range(pdat$frame))

  mins <- dplyr::slice_(pdat, ~which.min(value))
  maxs <- dplyr::slice_(pdat, ~which.max(value))

  p <- ggplot2::ggplot(pdat,
                       ggplot2::aes_(x = ~frame, y = ~value, color = ~animal,
                                     label = ~signif(value, 3))) +
    ggplot2::facet_grid(var ~ ., scales = "free_y", switch = 'y') +
    ggplot2::geom_ribbon(data = quants,
                         ggplot2::aes_(ymin = ~quant1, max = ~quant2),
                         fill = 'grey90', col = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::geom_point(data = mins, size = 2) +
    ggplot2::geom_point(data = maxs, size = 2) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.position = 'top')
  if (!is.null(point_events)) {
    p <- p + ggplot2::geom_vline(data = data.frame(v = point_events),
                                 ggplot2::aes_(xintercept = ~v), lty = 2)
  }
  return(p)
}

#' Plot lag correlations
#'
#' This function produces a \code{ggplot} that shows how the lag correlations
#' changes over time for each trial. Tries to do clever facetting. Will show
#' all pairs in the data, but if all trials contain only one pair, will only
#' show pair 1,2 (and not 2,1).
#'
#' The y-axis shows time bins (top to bottom), the x-axis shows the lag in
#' frames and the size (area) of the dots denotes strength of the correlation
#' (*r*).
#'
#' @param data The result from a lag correlation function (e.g.
#'   \code{calc_speed_lag}).
#'
#' @return A \code{ggplot} object.
#' @export
plot_lag_cor <- function(data) {
  if (length(levels(data$time_bin)) == 1) {
    stop('This plot only makes sense with multiple time bins.', call. = FALSE)
  }

  data$time_bin <- factor(data$time_bin, rev(levels(data$time_bin)))

  if (length(levels(data$animal1)) == 2) {
    data <- dplyr::filter_(data, ~animal1 == 1)
    if (length(levels(data$trial)) > 1) {
      facet <- ggplot2::facet_wrap(~trial)
    } else {
      facet <- ggplot2::facet_null()
    }
  } else {
    if (length(levels(data$trial)) > 1) {
      facet <- ggplot2::facet_grid(interaction(animal1, animal2) ~ trial)
    } else {
      facet <- ggplot2::facet_null()
    }
  }

  ggplot2::ggplot(data, ggplot2::aes_(x = ~lag, y = ~time_bin)) +
    ggplot2::geom_point(ggplot2::aes_(size = ~cor)) +
    ggplot2::geom_path(ggplot2::aes_(group = ~1)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::theme_bw() +
    facet
}
