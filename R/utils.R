#' Filter the database based on user inputs
#' @param input Shiny input object
#' @return Filtered dataframe
#' @noRd
.filter_data <- function(input) {
    data <- imageTCGA:::db

    if (!is.null(input$project) && length(input$project) > 0) {
        data <- data %>% filter(Project.ID %in% input$project)
    }

    if (!is.null(input$sample_type) && length(input$sample_type) > 0) {
        data <- data %>% filter(Sample.Type %in% input$sample_type)
    }

    if (!is.null(input$source_site) && length(input$source_site) > 0) {
        data <- data %>% filter(Source.Site %in% input$source_site)
    }

    if (!is.null(input$state) && length(input$state) > 0) {
        data <- data %>% filter(state %in% input$state)
    }

    if (!is.null(input$case_search) && input$case_search != "") {
        data <- data %>%
            filter(grepl(input$case_search, Case.ID, ignore.case = TRUE))
    }

    data
}

#' Prepare geographic data for visualization
#' @param data Filtered data
#' @return Processed geographic data
#' @noRd
.prepare_geo_data <- function(data) {
    data %>%
        group_by(Source.Site, lat, lon, state) %>%
        summarize(
            samples = n(),
            cases = n_distinct(Case.ID),
            .groups = 'drop'
        )
}

#' Prepare heatmap data
#' @param data Filtered data
#' @param input Shiny input object
#' @return Processed heatmap data with attributes
#' @noRd
.prepare_heatmap_data <- function(data, input) {
    result <- data %>%
        count(!!sym(input$heatmap_x), !!sym(input$heatmap_y)) %>%
        rename(
            Var1 = !!sym(input$heatmap_y),
            Var2 = !!sym(input$heatmap_x),
            Freq = n
        )

    attr(result, "x_label") <- input$heatmap_x
    attr(result, "y_label") <- input$heatmap_y

    result
}

#' Generate download code based on filtered data
#' @param data Filtered data
#' @return Character string containing R code
#' @noRd
.generate_download_code <- function(data) {
    file_ids <- paste(sprintf('"%s"', data$File.ID), collapse = ",\n  ")

    sprintf('## Make sure BiocManager is installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

## Make sure GenomicDataCommons is installed
# BiocManager::install("GenomicDataCommons")
library("GenomicDataCommons")

# File IDs to download
file_ids <- c(
%s
)

# Download files
lapply(file_ids, gdcdata)', file_ids)
}

#' Get selected rows from data table
#' @param input Shiny input object
#' @return Selected data rows
#' @noRd
.get_selected_rows <- function(input) {
    s <- input$data_table_rows_selected
    if (is.null(s)) return(NULL)
    .filter_data(input)[s, ]
}
