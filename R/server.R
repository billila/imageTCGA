#' Build the main server for the imageTCGA app
#' @return A Shiny server object
#' @noRd
.build_server <- function(input, output, session) {
    #Setup observers and outputs
    .setup_observers(input, output, session)
    .setup_outputs(input, output, session)
}
