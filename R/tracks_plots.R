#' Plot paths
#'
#' Only plots a path between subsequent frames.
#'
#' @param x A tracks object
#' @param color Color specification of the path as a formula (e.g. ~animal).
#' @param facet Facet specification as a formula (e.g. ~trial), passed to
#'   facet_wrap.
#' @param nrow Control number of rows for the facets.
#' @param ncol Control number of columns for the facets.
#' @param ... Ignored.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' plot(guppies)
#' plot(guppies, NULL) # No animal coloring
#' plot(guppies, facet = ~1) # No facets
#' plot(guppies, facet = ~trial + animal, ncol = 8) # more complex facetting
plot.tracks <- function(x, color = ~animal, facet = ~trial,  nrow = NULL,
                        ncol = NULL, ...) {
  tracks <- x
  rm(x)
  if ((facet == ~trial) & length(levels(tracks$tr$trial)) == 1)
    facet <- ~1

  # We need to add a grouping factor in order to create gaps when there are
  # non-subsequent frames.
  tracks$tr <- dplyr::mutate(tracks$tr,
                             gap = ifelse(frame == 1 + lag(frame), 0, 1),
                             .GROUP = c(0, cumsum(gap[-1])))

  ggplot2::ggplot(tracks$tr, ggplot2::aes_(~X, ~Y, color = color,
                                           group = ~.GROUP)) +
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
#' @param facet Optional facetting, should probably include time_bin. Only used
#'   if mode is set to manual.
#' @param coord_boundary Whether to fix the plot limits to the boundary. Will
#'   attempt to autodetect whether plotting ~X and ~Y.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#'   time_facet_plot(filter(guppies, trial == 'd1t3b'))
#'   time_facet_plot(guppies, mode = 'manual')
time_facet_plot <- function(tracks, x = ~X, y = ~Y, time_bins = 4,
                            color = ~animal, mode = 'dual', nrow = NULL,
                            scales = 'free_x', facet = trial ~ time_bin,
                            coord_boundary = NULL) {
  # We need to add a grouping factor in order to create gaps when there are
  # non-subsequent frames.
  tracks$tr <- dplyr::mutate(tracks$tr,
                             gap = ifelse(frame == 1 + lag(frame), 0, 1),
                             .GROUP = c(0, cumsum(gap[-1])))
  facet <- switch(mode,
                  manual = ggplot2::facet_grid(facet, scales = scales),
                  dual = ggplot2::facet_wrap(~time_bin, nrow, scales = scales))
  if (is.null(coord_boundary)) {
    if (x == ~X & y == ~Y)
      coord_boundary <- TRUE
    else
      coord_boundary <- FALSE
  }
  # Add time_bins
  bins <- seq(min(tracks$tr$frame, na.rm = TRUE),
              max(tracks$tr$frame, na.rm = TRUE),
              length.out = time_bins + 1)
  # Make some nice labels
  labels <- lubridate::seconds_to_period(bins / tracks$params$frame_rate)
  labels <- gsub("\\s*\\w*$", "", round(labels))
  labels <- tolower(gsub(" ", "", labels, fixed = TRUE))
  labels <- paste(head(labels, -1), 'till', labels[-1])

  tracks$tr$time_bin <- findInterval(tracks$tr$frame, bins, all.inside = TRUE)
  tracks$tr$time_bin <- factor(tracks$tr$time_bin, labels = labels)


  p <- ggplot2::ggplot(tracks$tr, ggplot2::aes_(x, y, color = color,
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
