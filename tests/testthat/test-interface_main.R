test_that(".build_ui() constructs the correct UI", {
    ui <- imageTCGA:::.build_ui()
    expect_true(any(grepl("imageTCGA: Diagnostic Image Database Explorer",
                          as.character(ui))))
})

test_that(".build_sidebar_panel() constructs the correct sidebar panel", {
    sidebar <- imageTCGA:::.build_sidebar_panel()
    expect_s3_class(sidebar, "shiny.tag")
    expect_equal(sidebar$name, "div")
    expect_true("well" %in% sidebar$attribs$class)
})

test_that(".build_dotplot_panel() constructs the correct heatmap panel", {
    dotplot_panel <- imageTCGA:::.build_dotplot_panel()
    expect_s3_class(dotplot_panel, "shiny.tag")
    expect_equal(dotplot_panel$name, "div")
    expect_true(any(grepl("Dotplot Parameters", as.character(dotplot_panel))))
})

test_that(".build_main_panel() constructs the correct main panel", {
    main_panel <- imageTCGA:::.build_main_panel()
    expect_s3_class(main_panel, "shiny.tag")
    expect_equal(main_panel$name, "div")
    expect_true("tabbable" %in% main_panel$attribs$class)
})
