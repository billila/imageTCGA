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

#' Build the dotplot tab
#' @return A Shiny tab panel
#' @noRd
.build_dotplot_tab <- function() {
    tabPanel(
        "Dotplot",
        card(
            card_header("Distribution Dotplot"),
            plotOutput("dotplot", height = "600px")
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


#' Build the mutation tab
#' @return A Shiny tab panel
#' @noRd
.build_mutation_tab <- function() {
    tabPanel(
        "Mutation",
        card(
            card_header("Mutation Distribution"),
            plotOutput("dotplot", height = "600px")
        )
    )
}

#' Build the CNA tab
#' @return A Shiny tab panel
#' @noRd
.build_cna_tab <- function() {
    tabPanel(
        "CNA",
        fluidRow(
            column(12,
                   card(
                       card_header("CN Signatures (Tao et al.)"),
                       plotOutput("cn_heatmap", height = "400px")
                   )
            )
        ),
        fluidRow(
            column(12,
                   card(
                       card_header("Mutational Signatures (Steel et al.)"),
                       plotOutput("sig_heatmap", height = "400px")
                   )
            )
        ),
        fluidRow(
            column(12,
                   card(
                       card_header("CX Signatures (Drews et al.)"),
                       plotOutput("cx_heatmap", height = "400px")
                   )
            )
        )
    )
}

#' Build the survival tab
#' @return A Shiny tab panel
#' @noRd
.build_survival_tab <- function() {
    tabPanel(
        "Survival",
        fluidRow(
            column(6,
                   card(
                       card_header("Overall Survival (OS)"),
                       uiOutput("os_km")
                   )
            ),
            column(6,
                   card(
                       card_header("Progression-Free Interval (PFI)"),
                       uiOutput("pfi_km")
                   )
            )
        )
        # ,
        # fluidRow(
        #     column(6,
        #            card(
        #                card_header("Disease-Specific Survival (DSS)"),
        #                uiOutput("dss_km")
        #            )
        #     ),
        #     column(6,
        #            card(
        #                card_header("Recurrence-Free Survival"),
        #                uiOutput("recurrence_km")
        #            )
        #     )
        # )
    )
}
# .build_survival_tab <- function() {
#     tabPanel(
#         "Survival",
#         fluidRow(
#             column(6,
#                    card(
#                        card_header("Overall Survival (OS)"),
#                        plotOutput("os_km", height = "400px")
#                    )
#             ),
#             column(6,
#                    card(
#                        card_header("Progression-Free Interval (PFI)"),
#                        plotOutput("pfi_km", height = "400px")
#                    )
#             )
#         )
#         #,
#         # fluidRow(
#         #     column(6,
#         #            card(
#         #                card_header("Disease-Specific Survival (DSS)"),
#         #                plotOutput("dss_km", height = "400px")
#         #            )
#         #     ),
#         #     column(6,
#         #            card(
#         #                card_header("Recurrence-Free Survival"),
#         #                plotOutput("recurrence_km", height = "400px")
#         #            )
#         #     )
#         # )
#     )
# }

#' Build the hovernet tab
#' @return A Shiny tab panel
#' @noRd
.build_hovernet_tab <- function() {
    tabPanel(
        "HoVer-Net",
        fluidRow(
            column(12,
                   card(
                       card_header("HoVer-Net segmentations"),
                       card_body(
                           div(
                               style = "text-align: center; padding: 50px;",
                               h3("Coming Soon", style = "color: #f39c12; margin-bottom: 30px;"),
                               p(
                                   "This panel will soon display HoVer-Net nuclei segmentation and classification results. Analysis outputs will be available in multiple formats including JSON annotations and H5AD files for seamless integration with downstream analysis workflows and visualization tools.",
                                   style = "font-size: 16px; line-height: 1.6; margin-bottom: 30px; max-width: 800px; margin-left: auto; margin-right: auto;"
                               ),
                               div(
                                   style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 30px; border-left: 4px solid #dc3545;",
                                   h5("Data Download", style = "color: #f39c12; margin-bottom: 15px;"),
                                   p(
                                       "Use the ImageFeatureTCGA package to download and access TCGA imaging data. The package conveniently incorporates imaging features and metadata into existing MultiAssayExperiment instances from curatedTCGAData, providing an integrated framework for multi-omics analysis with imaging data.",
                                       style = "font-size: 14px; line-height: 1.5; margin-bottom: 15px;"
                                   ),
                                   p(
                                       a("Visit ImageFeatureTCGA Repository",
                                         href = "https://github.com/waldronlab/ImageFeatureTCGA",
                                         target = "_blank",
                                         style = "color: #f39c12; font-weight: bold; text-decoration: none;")
                                   )
                               )
                           )
                       )
                   )
            )
        )
    )
}


#' Build the Prov-GigaPath tab
#' @return A Shiny tab panel
#' @noRd
.build_provgigapath_tab <- function() {
    tabPanel(
        "Prov-GigaPath",
        fluidRow(
            column(12,
                   card(
                       card_header("Prov-GigaPath embeddings"),
                       card_body(
                           div(
                               style = "text-align: center; padding: 50px;",
                               h3("Coming Soon", style = "color: #f39c12; margin-bottom: 30px;"),
                               p(
                                   "This panel will soon feature ProvGigaPath analysis results. Data will be available at both tile-level and slide-level resolutions, enabling multi-scale analysis of histopathological images for comprehensive tissue characterization and biomarker discovery.",
                                   style = "font-size: 16px; line-height: 1.6; margin-bottom: 30px; max-width: 800px; margin-left: auto; margin-right: auto;"
                               ),
                               div(
                                   style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 30px; border-left: 4px solid #dc3545;",
                                   h5("Data Download", style = "color: #f39c12; margin-bottom: 15px;"),
                                   p(
                                       "Use the ImageFeatureTCGA package to download and access TCGA imaging data. The package conveniently incorporates imaging features and metadata into existing MultiAssayExperiment instances from curatedTCGAData, providing an integrated framework for multi-omics analysis with imaging data.",
                                       style = "font-size: 14px; line-height: 1.5; margin-bottom: 15px;"
                                   ),
                                   p(
                                       a("Visit ImageFeatureTCGA Repository",
                                         href = "https://github.com/waldronlab/ImageFeatureTCGA",
                                         target = "_blank",
                                         style = "color: #f39c12; font-weight: bold; text-decoration: none;")
                                   )
                               )
                           )
                       )
                   )
            )
        )
    )
}



#' Build the purity tab
#' @return A Shiny tab panel
#' @noRd
.build_purity_tab <- function() {
    tabPanel(
        "Purity",
        card(
            card_header("Purity"),
            plotOutput("dotplot", height = "600px")
        )
    )
}
