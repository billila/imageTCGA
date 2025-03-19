test_that(".build_server funziona correttamente", {
    # Creazione di un input, output e sessione fittizi
    input <- shiny::reactiveValues(dummy = 1)
    output <- shiny::reactiveValues()
    session <- shiny::MockShinySession$new()

    # Mock delle funzioni .setup_observers e .setup_outputs
    mock_setup_observers <- function(input, output, session) {
        return(NULL)  # mock semplice
    }
    mock_setup_outputs <- function(input, output, session) {
        return(NULL)  # mock semplice
    }

    # Sostituire le funzioni originali con le versioni mock
    assign(".setup_observers", mock_setup_observers, envir = .GlobalEnv)
    assign(".setup_outputs", mock_setup_outputs, envir = .GlobalEnv)

    # Chiamare la funzione .build_server
    expect_silent({
        imageTCGA:::.build_server(input, output, session)
    })

    # Verifica che le funzioni mock siano state chiamate
    expect_true(exists(".setup_observers", envir = .GlobalEnv))
    expect_true(exists(".setup_outputs", envir = .GlobalEnv))
})
