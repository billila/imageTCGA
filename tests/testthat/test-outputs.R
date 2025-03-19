test_that("render_map works", {
    geo_data <- imageTCGA:::db %>%
        dplyr::filter(state == "Texas") %>%
        group_by(Case.ID, lon, lat, Source.Site, state, samples = 10) %>%
        dplyr::summarize(
            samples = n(),
            cases = n_distinct(.data$Case.ID),
            .groups = 'drop'
        )

    expect_silent({
        map_output <- imageTCGA:::.render_map(geo_data)
        expect_true("leaflet" %in% class(map_output))
    })
})

test_that("render_state_bars works", {
    data <- imageTCGA:::db %>%
        dplyr::filter(state == "Texas") %>%
        dplyr::count(state)

    expect_silent({
        state_bars_output <- imageTCGA:::.render_state_bars(data)
        expect_true("gg" %in% class(state_bars_output))
        expect_true("ggplot" %in% class(state_bars_output))
    })
})

test_that("render_heatmap works", {
    data <- data.frame(
        Var1 = c("A", "B", "C"),
        Var2 = c("X", "Y", "Z"),
        Freq = c(10, 15, 20)
    )

    attr(data, "x_label") <- "Variable 2"
    attr(data, "y_label") <- "Variable 1"

    expect_silent({
        heatmap_output <- imageTCGA:::.render_heatmap(data)
        expect_true("gg" %in% class(heatmap_output))
        expect_true("ggplot" %in% class(heatmap_output))
    })
})

test_that("render_data_table create table", {
    data <- imageTCGA:::db %>%
        dplyr::select(File.ID, File.Name, Data.Type,
                      bcr_patient_uuid, state) %>%
        dplyr::filter(state == "Texas")

    expect_silent({
        data_table_output <- imageTCGA:::.render_data_table(data)
    })
})

