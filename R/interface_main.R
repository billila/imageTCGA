#' Build the main UI for the imageTCGA app
#' @return A Shiny UI object
#' @noRd
.build_ui <- function() {
    fluidPage(
        theme = bs_theme(bootswatch = "flatly"),
        titlePanel("imageTCGA: Diagnostic Image Database Explorer"),
        fluidRow(
            column(3, .build_sidebar_panel()),
            column(9, .build_main_panel())
        )
    )
}

#' Build the sidebar panel with filters
#' @return A Shiny UI element
#' @noRd
.build_sidebar_panel <- function() {
    wellPanel(
        accordion(
            .build_filters_panel(),
            .build_dotplot_panel()
        )
    )
}

#' Build the filters accordion panel
#' @return A Shiny accordion panel
#' @noRd
.build_filters_panel <- function() {
    accordion_panel(
        "Filters",
        selectInput("project", "Project ID",
            choices = unique(db$Project.ID),
            multiple = TRUE
        ),
        selectInput("sample_type", "Sample Type",
            choices = unique(db$Sample.Type),
            multiple = TRUE
        ),
        selectInput("source_site", "Source Site",
            choices = unique(db$Source.Site),
            multiple = TRUE
        ),
        selectInput("state", "State",
            choices = unique(db$state),
            multiple = TRUE
        ),
        textInput("case_search", "Search Case ID", ""),
        actionButton("reset_filters", "Reset Filters",
            class = "btn-warning",
            style = "margin-top: 10px; width: 100%;"
        ),
        actionButton("generate_code", "Generate R Code",
            class = "btn-primary",
            style = "margin-top: 10px; width: 100%;"
        )
    )
}

#' Build the dotplot parameters panel
#' @return A Shiny accordion panel
#' @noRd
.build_dotplot_panel <- function() {
    accordion_panel(
        "Dotplot Parameters",
        selectInput("dotplot_x", "Dotplot X-axis",
            choices = names(db),
            selected = "Project.ID"
        ),
        selectInput("dotplot_y", "Dotplot Y-axis",
            choices = names(db),
            selected = "Sample.Type"
        )
    )
}

#' Build the main panel with tabs
#' @return A Shiny tabset panel
#' @noRd
.build_main_panel <- function() {
    tabsetPanel(
        .build_summary_tab(),
        .build_dotplot_tab(),
        .build_geographic_tab(),
        .build_about_tab()
    )
}
