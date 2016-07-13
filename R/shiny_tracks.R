#' Animate tracks object in shiny app.
#'
#' This function lauches a shiny app that can allows for the viewing of animated
#' tracks. It relies simply on the fast updating of a base plotting window.
#' Performance depends on hardware, but can improved by: decreasing the playback
#' speed, increasing the dilution factor, and by selecting a smaller portion of
#' the tracks using \code{start} and \code{end}.
#'
#' The dilution factor determines how many of the frames are actually animated.
#' For example, when it is set to 3, only at every third frame the plot is
#' updated. The full trial is always shown.
#'
#' @param tracks A tracks object.
#' @param start Starting time or frame.
#' @param end End time or frame.
#' @param ... Other parameters to be passed on to \code{\link[shiny]{runApp}}
#' @export
shiny_tracks <- function(tracks, start = NULL, end = NULL, ...) {
  if (!requireNamespace('shiny')) {
    stop('Install the shiny package to use this function.', call. = FALSE)
  }

  tr <- dplyr::collect(tracks$tr)

  start <- max(start, min(tr$frame))
  end <- min(end, max(tr$frame))
  fps <- tracks$params$frame_rate

  tr <- dplyr::filter_(tr, ~frame %in% start:end)
  LEVELS <- levels(tr$trial)

  steady_ranges <- rbind(range(tr$X, na.rm = TRUE),
                         range(tr$Y, na.rm = TRUE))

  server <- function(input, output) {
    time_step <- shiny::reactive(1000 / fps * input$dilute_in / input$playback_speed)

    output$ui <-
      shiny::renderUI({
        shiny::sliderInput('FR', 'Frame', start, end, start,
                           step = input$dilute_in,
                           animate = shiny::animationOptions(time_step(),
                                                             loop = FALSE))
      } )

    D <- shiny::reactive(
      dplyr::filter_(tr,
                     lazyeval::interp(~trial == TR, TR = input$TR),
                     lazyeval::interp(~frame %in% a:b,
                                      a = input$FR - input$trail_length,
                                      b = input$FR)) %>%
        dplyr::group_by_(~animal)
      )

    output$tr_plot <- shiny::renderPlot( {
      if (input$zoom) {
        graphics::plot(NA, xlim = range(D()$X, na.rm = TRUE),
                       ylim = range(D()$Y, na.rm = TRUE),
                       xlab = '', ylab = '', asp = 1)
      } else {
        graphics::plot(NA, xlim = steady_ranges[1, ], ylim = steady_ranges[2, ],
                       xlab = '', ylab = '', asp = 1)
      }
      if ('Ctrax' %in% tracks$params$source) {
        if (!requireNamespace('plotrix')) {
          stop('Install the plotrix package to plot Ctrax data.', call. = FALSE)
        }
        D() %>% dplyr::slice_(~n()) %>%
        {
          plotrix::draw.ellipse(x = .$X, y = .$Y, a = .$major_axis,
                                b = .$minor_axis, angle = .$orientation,
                                border = .$animal, deg = FALSE, nv = 20,
                                segment = c(0, 2 * pi))
        }
      } else {
        D() %>% dplyr::slice_(~n()) %>%
        {
          graphics::points(x = .$X, y = .$Y, col = .$animal, cex = 3, pch = 16)
        }
      }
      graphics::segments(
        D() %>% dplyr::slice_(~1:(n() - 1)) %>% magrittr::extract2('X'),
        D() %>% dplyr::slice_(~1:(n() - 1)) %>% magrittr::extract2('Y'),
        D() %>% dplyr::slice_(~2:n()) %>% magrittr::extract2('X'),
        D() %>% dplyr::slice_(~2:n()) %>% magrittr::extract2('Y'),
        col = D()$animal)
    }, height = 800, width = 800)
    # { d %>%
    #   ggvis::ggvis(~X, ~Y, fill = ~animal) %>%
    #   ggvis::layer_points(size := 100) } %>%
    # # ggvis::scale_numeric("x", domain = ranges()[1, ]) %>%
    # # ggvis::scale_numeric("y", domain = ranges()[2, ])
    #   ggvis::bind_shiny('tr_plot')
  }

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tags$body(shiny::h1('trackr'),
                  shiny::p('Use dilution to lower the frame rate if playback is choppy.')
        ),
        shiny::selectInput('TR', 'trial', choices = LEVELS),
        shiny::uiOutput('ui'),
        shiny::sliderInput('dilute_in', 'Dilution factor', 1, 10, 2),
        shiny::sliderInput('playback_speed', 'Playback speed', 0.2, 10, 1, 0.2),
        shiny::checkboxInput("zoom", "Zoom to animals", value = TRUE),
        shiny::numericInput('trail_length', 'Trail length', 50, 0)
      ),
      shiny::mainPanel(shiny::plotOutput('tr_plot', width = "100%"))
      #ggvis::ggvisOutput('tr_plot'))
    )
  )
  shiny::runApp(list(ui = ui, server = server), ...)
}
