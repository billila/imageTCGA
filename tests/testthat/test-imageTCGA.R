test_that("imageTCGA initializes correctly", {

  app <- imageTCGA::imageTCGA()

  expect_s3_class(app, "shiny.appobj")

  expect_false(is.function(app$ui))

  expect_true(is.function(app$server))
})

test_that(".build_ui constructs the correct UI", {
  ui <- imageTCGA:::.build_ui()

  expect_s3_class(ui, "shiny.tag.list") # Verifica che la UI sia costruita correttamente
})


test_that(".setup_observers defines observers", {
  fake_input <- shiny::reactiveValues(dummy = 1)
  fake_output <- shiny::reactiveValues()

  fake_session <- shiny::MockShinySession$new()

  expect_silent(imageTCGA:::.setup_observers(fake_input, fake_output, fake_session))
})


test_that("imageTCGA handles invalid inputs", {
  app <- imageTCGA::imageTCGA()

  expect_error(
    app$server(NULL, NULL, NULL),
    "unused arguments (NULL, NULL, NULL)",
    fixed = TRUE
  )
})
