#' imageTCGAnew: A Shiny application to explore the TCGA Diagnostic Image Database
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
imageTCGAnew <- function() {
  source("R/ui.R")
  source("R/server.R")
  source("R/observers.R")
  db <- imageTCGA:::db
  ui <- build_ui()
  server <- build_server
  app <- shinyApp(ui = ui, server = server)
  return(app)
}

