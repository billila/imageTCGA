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

build_server <- function(input, output, session) {
    db <- imageTCGA:::db


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

    output$table <- render_filtered_table(filtered_data)
    output$heatmap <- render_heatmap_plot(filtered_data)
    output$map <- render_map(filtered_data)

    stats <- generate_summary_statistics(filtered_data)
    output$summary <- renderText({
      paste0("Total records: ", stats$total_records, "\n",
             "Unique cases: ", stats$unique_cases, "\n",
             "Filtered records: ", stats$filtered_records)
    })
}
