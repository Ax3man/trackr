#' Estimate arena from tracks and reallign.
#'
#' @param tracks A tracks object.
#' @param grouping On what level the arena should be estimated.
#' @param shape The shape of the arena. Currently has to be a circle.
#' @param radius The true radius of the circle, used for rescaling the track
#'   coordinates.
#'
#' @return A tracks object.
#' @export
estimate_arena <- function(tracks, grouping = ~trial, shape = 'circle',
                           radius = NULL) {
  if (shape != 'circle') {
    stop('Currently only circular arenas are implemented.')
  }
  if (is.null(grouping) & is.na(grouping)) {
    tr <- dplyr::group_by(tracks$tr)
    tr <- dplyr::select_(tr, ~X, ~Y)
    tr <- dplyr::collect(tr)
    tr <- dplyr::ungroup(tr)
  } else {
    tr <- dplyr::group_by_(tracks$tr, grouping)
  }
  if (shape == 'circle') {
    arena <- dplyr::do_(tr,
                        qq = ~find_smallest_enclosing_circle(.[, c('X', 'Y')]))
    arena <- dplyr::collect(arena)
    arena <- dplyr::bind_cols(arena, as.data.frame(do.call(rbind, arena$qq)))
    arena$qq <- NULL

    multidplyr::cluster_assign_value(tracks$tr$cluster, 'arena', arena)
    multidplyr::cluster_assign_value(tracks$tr$cluster, 'radius', radius)

    if (is.null(grouping) & is.na(grouping)) {
      tracks$tr <- dplyr::mutate_(tracks$tr,
                                  x = arena$x, y = arena$y, r = arena$r)
    } else {
      tracks$tr <- multidplyr::cluster_eval_(
        tracks$tr$cluster,
        lazyeval::interp(quote(dplyr::left_join(name, arena)),
                         name = as.name(tracks$tr$name)))
    }
    tracks$tr <- dplyr::mutate_(tracks$tr,
                                X = lazyeval::interp(~(X - x) * (r / R), R = radius),
                                Y = lazyeval::interp(~(Y - y) * (r / R), R = radius))
    if ('minor_axis' %in% tracks$pr$tr) {
      tracks$tr <- dplyr::mutate_(
        tracks$tr,
        minor_axis = lazyeval::interp(~minor_axis * (r / R), R = radius))
    }
    if ('major_axis' %in% tracks$pr$tr) {
      tracks$tr <- dplyr::mutate_(
        tracks$tr,
        major_axis = lazyeval::interp(~major_axis * (r / R), R = radius))
    }
    tracks$params$bounds <-
      matrix(
        c(-radius, -radius, -radius, radius, radius, radius, radius, -radius), 2,
        dimnames = list(c('x', 'y'), c('ll', 'ul', 'ur', 'lr')))
    tracks$params$arena <- 'circle'
  }
  return(tracks)
}
