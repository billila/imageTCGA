test_that(".prepare_geo_data correctly aggregates geographic data", {
        db_example <- tibble::tibble(
                Case.ID = c("TCGA-01-0001", "TCGA-02-0001", "TCGA-03-0001"),
                Source.Site = c("MD Anderson Cancer Center",
                                "Johns Hopkins", "MD Anderson Cancer Center"),
                lat = c(29.7604, 34.0522, 29.7604),
                lon = c(-95.3698, -118.2437, -95.3698),
                state = c("Texas", "California", "Texas")
        )

        assign("db", db_example, envir = .GlobalEnv)
        geo_data <- imageTCGA:::.prepare_geo_data(db_example)

        expect_equal(nrow(geo_data), 2)
})

test_that(".prepare_dotplot_data correctly prepares data for the dotplot", {
        db_example <- tibble::tibble(
                Case.ID = c("TCGA-01-0001", "TCGA-02-0001", "TCGA-03-0001"),
                Project.ID = c("TCGA-GBM", "TCGA-GBM", "TCGA-GBM"),
                Sample.Type = c("Primary Tumor", "Primary Tumor",
                                "Metastatic Tumor"),
                Source.Site = c("MD Anderson Cancer Center",
                                "Johns Hopkins", "MD Anderson Cancer Center"),
                state = c("Texas", "California", "Texas"),
                gene = c("BRCA1", "BRCA2", "BRCA1")
        )

        input <- list(dotplot_x = "gene", dotplot_y = "state")

        assign("db", db_example, envir = .GlobalEnv)
        dotplot_data <- imageTCGA:::.prepare_dotplot_data(db_example, input)

        expect_equal(nrow(dotplot_data), 2)
        expect_equal(dotplot_data$Var1[1], "Texas")
        expect_equal(dotplot_data$Var2[1], "BRCA1")
        expect_equal(attr(dotplot_data, "x_label"), "gene")
        expect_equal(attr(dotplot_data, "y_label"), "state")
})

test_that(".generate_download_code correctly creates download code", {
        db_example <- tibble::tibble(
                File.ID = c("file_1", "file_2", "file_3"),
                Case.ID = c("TCGA-01-0001", "TCGA-02-0001", "TCGA-03-0001")
        )

        assign("db", db_example, envir = .GlobalEnv)
        download_code <- imageTCGA:::.generate_download_code(db_example)

        expect_true(grepl("file_ids <- c", download_code))
        expect_true(grepl('"file_1",', download_code))
        expect_true(grepl('"file_2",', download_code))
})

test_that(".get_selected_rows correctly returns selected rows", {
        db_example <- tibble::tibble(
                Case.ID = c("TCGA-01-0001", "TCGA-02-0001", "TCGA-03-0001"),
                Project.ID = c("TCGA-GBM", "TCGA-LUAD", "TCGA-GBM")
        )

        input <- list(data_table_rows_selected = c(1, 3))

        assign("db", db_example, envir = .GlobalEnv)
        selected_rows <- imageTCGA:::.get_selected_rows(input)

        expect_equal(nrow(selected_rows), 2)
})
