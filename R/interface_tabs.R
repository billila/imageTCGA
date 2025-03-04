#' Build the summary statistics tab
#' @return A Shiny tab panel
#' @noRd
.build_summary_tab <- function() {
    tabPanel(
        "Summary Statistics",
        fluidRow(
            column(4,
                value_box(
                    title = "Total Records",
                    value = textOutput("total_records"),
                    showcase = bs_icon("file-earmark"),
                    theme = "primary",
                    full_width = TRUE
                )
            ),
            column(4,
                value_box(
                    title = "Filtered Unique Cases",
                    value = textOutput("unique_cases"),
                    showcase = bs_icon("person"),
                    theme = "secondary",
                    full_width = TRUE
                )
            ),
            column(4,
                value_box(
                    title = "Selected Records",
                    value = textOutput("filtered_records"),
                    showcase = bs_icon("filter"),
                    theme = "success",
                    full_width = TRUE
                )
            )
        ),
        card(
            card_header("Filtered Data Table"),
            DT::DTOutput("data_table")
        ),
        card(
            card_header("Download R Code"),
            verbatimTextOutput("download_code")
        )
    )
}

#' Build the heatmap tab
#' @return A Shiny tab panel
#' @noRd
.build_heatmap_tab <- function() {
    tabPanel(
        "Heatmap plot",
        card(
            card_header("Distribution Heatmap"),
            plotOutput("heatmap", height = "600px")
        )
    )
}

#' Build the geographic distribution tab
#' @return A Shiny tab panel
#' @noRd
.build_geographic_tab <- function() {
    tabPanel(
        "Geographic Distribution",
        fluidRow(
            column(8,
                card(
                    card_header("Sample Distribution Map"),
                    leafletOutput("map", height = "600px")
                )
            ),
            column(4,
                card(
                    card_header("Distribution Statistics"),
                    value_box(
                        title = "Number of Cities",
                        value = textOutput("num_cities"),
                        showcase = bs_icon("buildings"),
                        theme = "primary",
                        full_width = TRUE
                    ),
                    value_box(
                        title = "Number of States",
                        value = textOutput("num_states"),
                        showcase = bs_icon("geo-alt"),
                        theme = "secondary",
                        full_width = TRUE
                    ),
                    plotOutput("state_bars", height = "400px")
                )
            )
        )
    )
}

#' Build the about tab
#' @return A Shiny tab panel
#' @noRd
.build_about_tab <- function() {
    tabPanel(
        "About",
        fluidRow(
            column(6,
                card(
                    card_header("Author Information"),
                    card_body(tags$p(
                        tags$strong("Mantainer: "), "Ilaria Billato",tags$br(),
                        tags$strong("Email: "),"ilaria.billato@phd.unipd.it",
                        tags$br(),
                        tags$strong("Institution: "), "University of Padova"
                        )
                    )
                )
            ),
            column(6, card(
                    card_header("Project Details"),
                    card_body(tags$h4("imageTCGA"),
                        tags$p(
                            "This Shiny application was developed to explore
                            the TCGA Diagnostic Image Database with the
                            possibility to extract the R code to download
                            diagnostic image with GenomicsDataCommon
                            Bioconductor package.",
                            tags$br(),
                            tags$strong("Package Version: "), "0.99",
                            tags$br(),
                            tags$strong("Bioconductor Version: "), "3.21",
                            tags$br(), tags$strong("Source: "),
                            tags$a(href =
                                "https://github.com/billila/imageTCGA/",
                                "https://github.com/billila/imageTCGA/",
                                target = "_blank"
                            ),
                            tags$br(),tags$strong("Last Updated: "),
                            format(Sys.Date(), "%B %d, %Y"),tags$br(),
                            tags$br(),"For bugs and suggestion please visit:",
                            tags$br(), tags$a(href =
                                "https://github.com/billila/imageTCGA/issues",
                                "https://github.com/billila/imageTCGA/issues",
                                target = "_blank"
                            )
                        )
                    )
                )
            )
        )
    )
}
