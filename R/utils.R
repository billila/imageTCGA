#' utils: utility function for imageTCGA
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

generate_r_code <- function(filtered_data) {
  file_ids <- paste(sprintf('"%s"', filtered_data$File.ID), collapse = ",\n  ")
  code <- sprintf('## Make sure BiocManager is installed
    if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
    library("GenomicDataCommons")
    file_ids <- c(%s)
    lapply(file_ids, gdcdata)', file_ids)
  return(code)
}

reset_filters <- function(session) {
  updateSelectInput(session, "project", selected = character(0))
  updateSelectInput(session, "sample_type", selected = character(0))
  updateSelectInput(session, "source_site", selected = character(0))
  updateSelectInput(session, "state", selected = character(0))
  updateTextInput(session, "case_search", value = "")
}
