#' observers: observers function for imageTCGA
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

reset_filters_observer <- function(input, session) {
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "project", selected = character(0))
    updateSelectInput(session, "sample_type", selected = character(0))
    updateSelectInput(session, "source_site", selected = character(0))
    updateSelectInput(session, "state", selected = character(0))
    updateTextInput(session, "case_search", value = "")
  })
}

generate_code_observer <- function(input, output, session, filtered_data) {
  observeEvent(input$generate_code, {
    current_data <- filtered_data()
    file_ids <- paste(sprintf('"%s"', current_data$File.ID), collapse = ",\n  ")
    code <- sprintf('## Make sure BiocManager is installed
    if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
    library("GenomicDataCommons")
    file_ids <- c(%s)
    lapply(file_ids, gdcdata)', file_ids)
    output$download_code <- renderText({ code })
  })
}
