#' interface: interface visualization for imageTCGA
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

create_project_input <- function(db) {
    selectInput("project", "Project ID",
        choices = unique(db$Project.ID),
        multiple = TRUE)
}

create_sample_type_input <- function(db) {
    selectInput("sample_type", "Sample Type",
        choices = unique(db$Sample.Type),
        multiple = TRUE)
}

create_source_site_input <- function(db) {
    selectInput("source_site", "Source Site",
        choices = unique(db$Source.Site),
        multiple = TRUE)
}

create_state_input <- function(db) {
    selectInput("state", "State",
        choices = unique(db$state),
        multiple = TRUE)
}

create_case_search_input <- function() {
  textInput("case_search", "Search Case ID", "")
}

create_reset_button <- function() {
    actionButton("reset_filters", "Reset Filters",
        class = "btn-warning", style = "margin-top: 10px; width: 100%;")
}

create_generate_code_button <- function() {
    actionButton("generate_code", "Generate R Code",
        class = "btn-primary", style = "margin-top: 10px; width: 100%;")
}
