#' Filter the database based on user inputs
#' @importFrom rlang .data
#' @param input Shiny input object
#' @return Filtered dataframe
#' @noRd
.filter_data <- function(input) {
    data <- db_CNA3

    if (!is.null(input$project) && length(input$project) > 0) {
        data <- data %>% filter(.data$Project.ID %in% input$project)
    }

    if (!is.null(input$sample_type) && length(input$sample_type) > 0) {
        data <- data %>% filter(.data$Sample.Type %in% input$sample_type)
    }

    if (!is.null(input$source_site) && length(input$source_site) > 0) {
        data <- data %>% filter(.data$Source.Site %in% input$source_site)
    }

    if (!is.null(input$state) && length(input$state) > 0) {
        data <- data %>% filter(.data$state %in% input$state)
    }

    if (!is.null(input$case_search) && input$case_search != "") {
        data <- data %>%
            filter(grepl(input$case_search, .data$Case.ID, ignore.case = TRUE))
    }

    data
}

#' Prepare geographic data for visualization
#' @param data Filtered data
#' @return Processed geographic data
#' @noRd
.prepare_geo_data <- function(data) {
    data %>%
        group_by(.data$Source.Site, .data$lat, .data$lon, .data$state) %>%
        summarize(
            samples = n(),
            cases = n_distinct(.data$Case.ID),
            .groups = 'drop'
        )
}

#' Prepare dotplot data
#' @param data Filtered data
#' @param input Shiny input object
#' @return Processed dotplot data with attributes
#' @noRd
.prepare_dotplot_data <- function(data, input) {
    result <- data %>%
        count(!!sym(input$dotplot_x), !!sym(input$dotplot_y)) %>%
        rename(
            Var1 = !!sym(input$dotplot_y),
            Var2 = !!sym(input$dotplot_x),
            Freq = n
        )

    attr(result, "x_label") <- input$dotplot_x
    attr(result, "y_label") <- input$dotplot_y

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

if (!require("GenomicDataCommons", quietly = TRUE))
    BiocManager::install("GenomicDataCommons")
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


#' Prepare data for CN signature heatmap (Tao signatures)
#' @param data Filtered data containing CN columns
#' @return Matrix ready for heatmap visualization
#' @noRd
.prepare_cn_heatmap_data <- function(data) {
    # Seleziona solo le colonne CN
    cn_cols <- grep("^CN\\d+$", names(data), value = TRUE)

    if (length(cn_cols) == 0) {
        return(NULL)
    }

    data <- data %>%
        mutate(across(all_of(cn_cols), ~ as.numeric(as.character(.x))))

    # Aggrega per Project.ID
    heatmap_data <- data %>%
        group_by(Project.ID) %>%
        summarise(across(all_of(cn_cols), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
        textshape::column_to_rownames("Project.ID") %>%
        as.matrix()

    # Rimuovi righe con tutti NA
    heatmap_data[rowSums(!is.na(heatmap_data)) > 0, , drop = FALSE]
}

#' Prepare data for Sig signature heatmap (Steel signatures)
#' @param data Filtered data containing Sig columns
#' @return Matrix ready for heatmap visualization
#' @noRd
.prepare_sig_heatmap_data <- function(data) {
    # Seleziona solo le colonne Sig
    sig_cols <- grep("^Sig\\d+$", names(data), value = TRUE)

    if (length(sig_cols) == 0) {
        return(NULL)
    }

    data <- data %>%
        mutate(across(all_of(cn_cols), ~ as.numeric(as.character(.x))))

    # Aggrega per Project.ID
    heatmap_data <- data %>%
        group_by(Project.ID) %>%
        summarise(across(all_of(sig_cols), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
        textshape::column_to_rownames("Project.ID") %>%
        as.matrix()

    # Rimuovi righe con tutti NA
    heatmap_data[rowSums(!is.na(heatmap_data)) > 0, , drop = FALSE]
}

#' Prepare data for CX signature heatmap (Drews signatures)
#' @param data Filtered data containing CX columns
#' @return Matrix ready for heatmap visualization
#' @noRd
.prepare_cx_heatmap_data <- function(data) {
    # Seleziona solo le colonne CX
    cx_cols <- grep("^CX\\d+$", names(data), value = TRUE)

    if (length(cx_cols) == 0) {
        return(NULL)
    }

    data <- data %>%
        mutate(across(all_of(cn_cols), ~ as.numeric(as.character(.x))))

    # Aggrega per Project.ID
    heatmap_data <- data %>%
        group_by(Project.ID) %>%
        summarise(across(all_of(cx_cols), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
        textshape::column_to_rownames("Project.ID") %>%
        as.matrix()

    # Rimuovi righe con tutti NA
    heatmap_data[rowSums(!is.na(heatmap_data)) > 0, , drop = FALSE]
}

#' Render CN signature heatmap
#' @param data Matrix data for heatmap
#' @return A ggplot heatmap object
#' @noRd
.render_cn_heatmap <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(
            ggplot() +
                annotate("text", x = 1, y = 1, label = "No CN signature data available") +
                theme_void()
        )
    }

    heatmap_long <- data %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Project_ID") %>%
        tidyr::pivot_longer(cols = -Project_ID, names_to = "CN_Signature", values_to = "Value")

    ggplot(heatmap_long, aes(x = CN_Signature, y = Project_ID, fill = Value)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_viridis_c(
            option = "D",
            limits = c(0, 1),
            na.value = "grey90",
            name = "CN Score"
        ) +
        theme_minimal() +
        labs(
            x = "Tao Signatures",
            y = "Project.ID"
        ) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "right"
        )
}


#' Render Sig signature heatmap
#' @param data Matrix data for heatmap
#' @return A ggplot heatmap object
#' @noRd
.render_sig_heatmap <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(
            ggplot() +
                annotate("text", x = 1, y = 1, label = "No Sig signature data available") +
                theme_void()
        )
    }

    heatmap_long <- data %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Project_ID") %>%
        tidyr::pivot_longer(cols = -Project_ID, names_to = "Sig_Signature", values_to = "Value")

    ggplot(heatmap_long, aes(x = Sig_Signature, y = Project_ID, fill = Value)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_viridis_c(
            option = "D",
            limits = c(0, 1),
            na.value = "grey90",
            name = "Sig Score"
        ) +
        theme_minimal() +
        labs(
            x = "Steel Signatures",
            y = "Project.ID"
        ) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "right"
        )
}

#' Render CX signature heatmap
#' @param data Matrix data for heatmap
#' @return A ggplot heatmap object
#' @noRd
.render_cx_heatmap <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(
            ggplot() +
                annotate("text", x = 1, y = 1, label = "No CX signature data available") +
                theme_void()
        )
    }

    heatmap_long <- data %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Project_ID") %>%
        tidyr::pivot_longer(cols = -Project_ID, names_to = "CX_Signature", values_to = "Value")

    ggplot(heatmap_long, aes(x = CX_Signature, y = Project_ID, fill = Value)) +
        geom_tile(color = "white", size = 0.1) +
        scale_fill_viridis_c(
            option = "D",
            limits = c(0, 1),
            na.value = "grey90",
            name = "CX Score"
        ) +
        theme_minimal() +
        labs(
            x = "Drews Signatures",
            y = "Project.ID"
        ) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "right"
        )
}
