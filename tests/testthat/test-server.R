test_that(".build_server works correctly", {
    input <- shiny::reactiveValues(dummy = 1)
    output <- shiny::reactiveValues()
    session <- shiny::MockShinySession$new()

    mock_setup_observers <- function(input, output, session) {
        return(NULL)
    }
    mock_setup_outputs <- function(input, output, session) {
        return(NULL)
    }

    assign(".setup_observers", mock_setup_observers, envir = .GlobalEnv)
    assign(".setup_outputs", mock_setup_outputs, envir = .GlobalEnv)

    expect_silent({
        imageTCGA:::.build_server(input, output, session)
    })

    expect_true(exists(".setup_observers", envir = .GlobalEnv))
    expect_true(exists(".setup_outputs", envir = .GlobalEnv))
})
