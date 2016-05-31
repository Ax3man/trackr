shiny_tracks <- function(tracks, min = NULL, max = NULL, ...) {
  tr <- collect(tracks$tr)
  LEVELS <- levels(tr$trial)
  if (is.null(min)) {
    min <- min(tr$frame)
    max <- max(tr$frame)
  }
  fps <- tracks$params$frame_rate
  
  server <- function(input, output) {
    time_dilute <- shiny::reactive(input$dilute_in * input$playback_speed)
    time_step <- shiny::reactive(1000 / fps * time_dilute() / input$playback_speed)
    
    output$ui <-
      shiny::renderUI({
        shiny::sliderInput('FR', 'Frame', min, max, mean(c(min, max)), step = 1,
                           animate = shiny::animationOptions(time_step(),
                                                             loop = FALSE))
      } )
    
    d <- shiny::reactive(
      dplyr::filter_(tr, 
                     lazyeval::interp(~trial == TR, TR = input$TR),
                     lazyeval::interp(~frame == FR, FR = input$FR))
    )
    
    ranges <- rbind(range(tr$X, na.rm = TRUE),
                    range(tr$Y, na.rm = TRUE))
    
    output$tr_plot <- renderPlot(plot(d()$X, d()$Y, col = d()$animal,
                                      pch = 16, cex = 3, xlim = ranges[1, ],
                                      ylim = ranges[2, ]))
    
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
        shiny::selectInput('TR', 'trial', choices = LEVELS),
        shiny::uiOutput('ui'),
        shiny::sliderInput('dilute_in', 'Dilution factor', 1, 10, 4),
        shiny::sliderInput('playback_speed', 'Playback speed', 1, 10, 1)
      ),
      shiny::mainPanel(shiny::plotOutput('tr_plot'))
      #ggvis::ggvisOutput('tr_plot'))
    )
  )
  shiny::runApp(list(ui = ui, server = server), ...)
}
