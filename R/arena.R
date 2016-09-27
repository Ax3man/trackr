#' Estimate arena from tracks and reallign.
#'
#' @param tracks A tracks object.
#' @param grouping On what level the arena should be estimated.
#' @param shape The shape of the arena. Either a circle or a rectangle.
#' @param radius The true radius of the circle, used for rescaling the track
#'   coordinates.
#' @param height The true height of the rectangle., used for rescaling the track
#'   coordinates.
#' @param width The true width of the rectangle., used for rescaling the track
#'   coordinates.
#'
#' @return A tracks object.
#' @export
estimate_arena <- function(tracks, grouping = ~trial, shape = 'circle',
                           radius = NULL, height = NULL, width = NULL) {
  if (!(shape %in% c('circle', 'rectangle'))) {
    stop('Currently only circular arenas are implemented.')
  }
  if (is.null(grouping)) {
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
    suppressWarnings(
      arena <- dplyr::collect(arena)
    )
    arena <- dplyr::bind_cols(arena, as.data.frame(do.call(rbind, arena$qq)))
    arena$qq <- NULL

    multidplyr::cluster_assign_value(tracks$tr$cluster, 'arena', arena)
    multidplyr::cluster_assign_value(tracks$tr$cluster, 'radius', radius)

    if (is.null(grouping)) {
      tracks$tr <- dplyr::mutate_(tracks$tr,
                                  x = arena$x, y = arena$y, r = arena$r)
    } else {
      multidplyr::cluster_eval_(
        tracks$tr$cluster,
        lazyeval::interp(quote(dplyr::left_join(name, arena)),
                         name = as.name(tracks$tr$name))) %>%
        {
          multidplyr::cluster_assign_each(cluster = tracks$tr$cluster,
                                          name = tracks$tr$name, values = .)
        }
    }
    tracks$tr <- dplyr::mutate_(tracks$tr,
                                X = lazyeval::interp(~(X - x) * (R / r),
                                                     R = radius),
                                Y = lazyeval::interp(~(Y - y) * (R / r),
                                                     R = radius))
    if ('minor_axis' %in% tracks$pr$tr) {
      tracks$tr <- dplyr::mutate_(
        tracks$tr,
        minor_axis = lazyeval::interp(~minor_axis * (R / r), R = radius))
    }
    if ('major_axis' %in% tracks$pr$tr) {
      tracks$tr <- dplyr::mutate_(
        tracks$tr,
        major_axis = lazyeval::interp(~major_axis * (R / r), R = radius))
    }
    tracks$tr <- dplyr::select_(tracks$tr, ~-x, ~-y, ~-r)
    tracks$params$bounds <-
      matrix(
        c(-radius, -radius, -radius, radius, radius, radius, radius, -radius), 2,
        dimnames = list(c('x', 'y'), c('ll', 'ul', 'ur', 'lr')))
    tracks$params$arena <- 'circle'
  }
  if (shape == 'rectangle') {
    if (!is.null(grouping)) {
      stop('No grouping implemented for method `rectangle`')
    }
    tr <- dplyr::collect(tr)
    arena <- find_smallest_enclosing_rect(tr[, c('X', 'Y')])

    tr[, c('X', 'Y')] <- rect_transform(tr[, c('X', 'Y')],
                                        arena[, 'x'], arena[, 'y'],
                                        c(0, width, width, 0),
                                        c(0, 0, height, height))
    a <- angle(arena[1, 'x'], arena[1, 'y'], arena[2, 'x'], arena[2, 'y'])
    tr <- dplyr::mutate_(orientation = ~orienation - a)
    warning('Minor and major axes are not adjusted at this time.')
    tracks$params$bounds <-
      matrix(
        c(0, 0, width, width, 0, height, height, 0), 2, byrow = TRUE,
        dimnames = list(c('x', 'y'), c('ll', 'ul', 'ur', 'lr')))
    tracks$params$arena <- 'rectangle'
  }
  return(tracks)
}
