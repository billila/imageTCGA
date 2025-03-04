library(shinytest2)

test_that("{shinytest2} recording: imageTCGA", {
  shiny_app <- imageTCGA::imageTCGA()
  app <- AppDriver$new(shiny_app,
                       variant = platform_variant(),
                       name = "imageTCGA",
                       height = 930,
                       width = 1154)
  app$expect_screenshot()
  app$set_inputs(project = "TCGA-OV")
  app$expect_screenshot()
  app$set_inputs(state = "North Carolina")
  app$expect_screenshot()
  app$click("generate_code")
  app$set_window_size(width = 1154, height = 930)
  app$expect_screenshot()
  app$click("reset_filters")
  app$set_window_size(width = 1154, height = 930)
  app$expect_screenshot()
})
