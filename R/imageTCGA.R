#' imageTCGA: A Shiny application to explore the TCGA Diagnostic Image Database
#'
#' This function launches a Shiny application that allows users to explore and
#' download data from the TCGA Diagnostic Image Database.
#'
#' @import shiny
#' @import dplyr
#' @import bslib
#' @import bsicons
#' @import ggplot2
#' @import viridis
#' @import tidyr
#' @import leaflet
#'
#' @return A Shiny application object
#'
#' @examples
#' if (interactive()) {
#'     BiocHubsShiny()
#' }
#'
#' @export

imageTCGA <- function() {
    # Build UI and server components
    ui <- .build_ui()
    server <- function(input, output, session) {
        #Setup observers and outputs
        .setup_observers(input, output, session)
        .setup_outputs(input, output, session)}

    # Create and return the Shiny app
    shinyApp(ui = ui, server = server)
}
