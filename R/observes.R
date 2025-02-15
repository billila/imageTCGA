#' Setup observers for the imageTCGA app
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @noRd
.setup_observers <- function(input, output, session) {
    # Initial setup observer
    observe({
        .reset_filters(session)
        .update_download_code(output, "")
    })

    # Reset filters observer
    observeEvent(input$reset_filters, {
        .reset_filters(session)
        .update_download_code(output, "")
    })

    # Generate code observer
    observeEvent(input$generate_code, {
        filtered <- .filter_data(input)
        code <- .generate_download_code(filtered)
        .update_download_code(output, code)
    })

    # Copy code observer
    observeEvent(input$copy_code, {
        .copy_code_to_clipboard(output)
    })

    # File selection observer
    observe({
        choices <- setNames(
            .filter_data(input)$File.ID,
            .filter_data(input)$File.Name
        )
        updateSelectInput(session, "selected_files", choices = choices)
    })
}

#' Reset all filters to their default state
#' @param session Shiny session object
#' @noRd
.reset_filters <- function(session) {
    updateSelectInput(session, "project", selected = character(0))
    updateSelectInput(session, "sample_type", selected = character(0))
    updateSelectInput(session, "source_site", selected = character(0))
    updateSelectInput(session, "state", selected = character(0))
    updateTextInput(session, "case_search", value = "")
}

#' Update the download code output
#' @param output Shiny output object
#' @param code Character string containing the code
#' @noRd
.update_download_code <- function(output, code) {
    output$download_code <- renderText(code)
}

#' Copy the current download code to clipboard
#' @param output Shiny output object
#' @noRd
.copy_code_to_clipboard <- function(output) {
    code <- isolate(output$download_code())
    clipr::write_clip(code)
    showNotification("Code copied to clipboard!", type = "message")
}
