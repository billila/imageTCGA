#' ui: ui function for imageTCGA
#'
#' @import shiny
#' @import DT
#' @import dplyr
#' @import bslib
#' @import bsicons
#' @import ggplot2
#' @import viridis
#' @import tidyr
#' @import leaflet
#'
#' @export

build_ui <- function() {
  db <- imageTCGA:::db

  page_sidebar(
    title = "TCGA Diagnostic Image Database Explorer",
    theme = bs_theme(bootswatch = "flatly"),

    sidebar = sidebar(
      accordion(
        accordion_panel(
          "Filters",
          selectInput("project", "Project ID", choices = unique(db$Project.ID), multiple = TRUE),
          selectInput("sample_type", "Sample Type", choices = unique(db$Sample.Type), multiple = TRUE),
          selectInput("source_site", "Source Site", choices = unique(db$Source.Site), multiple = TRUE),
          selectInput("state", "State", choices = unique(db$state), multiple = TRUE),
          textInput("case_search", "Search Case ID", ""),
          actionButton("reset_filters", "Reset Filters", class = "btn-warning", style = "margin-top: 10px; width: 100%;"),
          actionButton("generate_code", "Generate R Code", class = "btn-primary", style = "margin-top: 10px; width: 100%;")
        ),

        accordion_panel(
          "Heatmap Parameters",
          selectInput("heatmap_x", "Heatmap X-axis", choices = names(db), selected = "Project.ID"),
          selectInput("heatmap_y", "Heatmap Y-axis", choices = names(db), selected = "Sample.Type")
        )
      )
    ),

    navset_card_tab(
      nav_panel("Summary Statistics", layout_columns(
        value_box(title = "Total Records", value = textOutput("total_records"), showcase = bs_icon("file-earmark"), theme = "primary"),
        value_box(title = "Filtered Unique Cases", value = textOutput("unique_cases"), showcase = bs_icon("person"), theme = "secondary"),
        value_box(title = "Selected Records", value = textOutput("filtered_records"), showcase = bs_icon("filter"), theme = "success")
      ), card(card_header("Filtered Data Table"), DTOutput("data_table"))),
      nav_panel("HoverNet Features", card(card_header("HoverNet Features"))),
      nav_panel("Prov-GigaPath Features", card(card_header("Prov-GigaPath Features"))),
      nav_panel("Heatmap plot", card(card_header("Distribution Heatmap"), plotOutput("heatmap", height = "600px"))),
      nav_panel("Geographic Distribution", layout_columns(
        card(card_header("Sample Distribution Map"), leafletOutput("map", height = "600px")),
        card(card_header("Distribution Statistics"), layout_columns(
          value_box(title = "Number of Cities", value = textOutput("num_cities"), showcase = bs_icon("buildings"), theme = "primary"),
          value_box(title = "Number of States", value = textOutput("num_states"), showcase = bs_icon("geo-alt"), theme = "secondary")
        ), plotOutput("state_bars", height = "400px"))
      )),
      nav_panel("About", layout_column_wrap(width = 1/2, card(card_header("Author Information"), card_body(
        tags$h4("PhD Student"),
        tags$p(tags$strong("Name: "), "Ilaria Billato", tags$br(), tags$strong("Email: "), "ilaria.billato@phd.unipd.it", tags$br(), tags$strong("Institution: "), "University of Padova")
      )), card(card_header("Project Details"), card_body(
        tags$h4("About this Application"),
        tags$p("This Shiny application was developed to explore the TCGA Diagnostic Image Database with the possibility to extract the R code to download diagnostic images features extract from different tools.", tags$br(), tags$strong("Version: "), "1.0", tags$br(), tags$strong("Last Updated: "), format(Sys.Date(), "%B %d, %Y"), tags$br(), tags$br(), "For more information about the project, please visit: my brain ", tags$a(href = "https://romualdi.bio.unipd.it/", "Romualdi Lab website", target = "_blank"))
      )))
      )
    )

  )
}
