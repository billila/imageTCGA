#' server: server function for imageTCGA
#'
#' @import shiny
#' @import DT
#' @import dplyr
#' @import bslib
#' @import bsicons
#' @import ggplot2
#' @import viridis
#' @import tidyr
#' @import leaflet
#'
#' @export
build_server <- function(input, output, session) {
  db <- imageTCGA:::db

  # Reactive filtered dataset
  filtered_data <- reactive({
    data <- db
    if (!is.null(input$project) && length(input$project) > 0)
      data <- data %>%
        filter(Project.ID %in% input$project)
    if (!is.null(input$sample_type) && length(input$sample_type) > 0)
      data <- data %>%
        filter(Sample.Type %in% input$sample_type)
    if (!is.null(input$source_site) && length(input$source_site) > 0)
      data <- data %>%
        filter(Source.Site %in% input$source_site)
    if (!is.null(input$state) && length(input$state) > 0)
      data <- data %>%
        filter(state %in% input$state)
    if (input$case_search != "") data <- data %>%
        filter(grepl(input$case_search, Case.ID, ignore.case = TRUE))
    data
  })

  # Render outputs here (e.g., heatmap, map, statistics)
  # Include render functions such as `renderPlot`,
  #`renderText`, `renderDT`, etc.
}
