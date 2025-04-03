test_that(".build_summary_tab() constructs the correct tab", {
    summary_tab <- imageTCGA:::.build_summary_tab()
    expect_s3_class(summary_tab, "shiny.tag")
    expect_true(grepl("Summary Statistics", summary_tab$attribs$title))
    expect_true(any(grepl("total_records", as.character(summary_tab))))
    expect_true(any(grepl("unique_cases", as.character(summary_tab))))
    expect_true(any(grepl("filtered_records", as.character(summary_tab))))
    expect_true(any(grepl("data_table", as.character(summary_tab))))
    expect_true(any(grepl("download_code", as.character(summary_tab))))
})

test_that(".build_dotplot_tab() constructs the correct Dotplot plot tab", {
    dotplot_tab <- imageTCGA:::.build_dotplot_tab()
    expect_s3_class(dotplot_tab, "shiny.tag")
    expect_true(grepl("Dotplot", dotplot_tab$attribs$title))
    expect_true(any(grepl("dotplot", as.character(dotplot_tab))))
})

test_that(".build_geographic_tab() constructs the correct tab", {
    geographic_tab <- imageTCGA:::.build_geographic_tab()
    expect_s3_class(geographic_tab, "shiny.tag")
    expect_true(grepl("Geographic Distribution", geographic_tab$attribs$title))
    expect_true(any(grepl("map", as.character(geographic_tab))))
    expect_true(any(grepl("num_cities", as.character(geographic_tab))))
    expect_true(any(grepl("num_states", as.character(geographic_tab))))
    expect_true(any(grepl("state_bars", as.character(geographic_tab))))
})

test_that(".build_about_tab() constructs the correct About tab", {
    about_tab <- imageTCGA:::.build_about_tab()
    expect_s3_class(about_tab, "shiny.tag")
    expect_true(grepl("About", about_tab$attribs$title))
    expect_true(any(grepl("Ilaria Billato", as.character(about_tab))))
    expect_true(any(grepl("University of Padova", as.character(about_tab))))
    expect_true(any(grepl("imageTCGA", as.character(about_tab))))
    expect_true(any(grepl("https://github.com/billila/imageTCGA/",
                          as.character(about_tab))))
})
