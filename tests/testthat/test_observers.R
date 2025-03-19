test_that(".setup_observers defines observers", {
    fake_input <- shiny::reactiveValues(dummy = 1)
    fake_output <- shiny::reactiveValues()
    fake_session <- shiny::MockShinySession$new()
    expect_silent(imageTCGA:::.setup_observers(fake_input,
          fake_output, fake_session))
})

test_that(".reset_filters is working", {
    fake_input <- shiny::reactiveValues()
    fake_output <- shiny::reactiveValues()
    fake_session <- shiny::MockShinySession$new()
    expect_silent(imageTCGA:::.reset_filters(fake_session))
})

test_that(".update_download_code updates download_code correctly ", {
    fake_output <- shiny::reactiveValues(download_code = NULL)
    sample_code <- "R code to Download the images"
    expect_type(sample_code, "character")
    expect_s3_class(fake_output, "reactivevalues")
    expect_silent(imageTCGA:::.update_download_code(fake_output, sample_code))
})
