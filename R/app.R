#' imageTCGA: A Shiny application to explore the TCGA Diagnostic Image Database
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
imageTCGA <- function() {
  db <- imageTCGA:::db

ui <- page_sidebar(
  title = "TCGA Diagnostic Image Database Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    accordion(
      accordion_panel(
        "Filters",
        selectInput("project", "Project ID",
                    choices = unique(db$Project.ID),
                    multiple = TRUE),

        selectInput("sample_type", "Sample Type",
                    choices = unique(db$Sample.Type),
                    multiple = TRUE),

        selectInput("source_site", "Source Site",
                    choices = unique(db$Source.Site),
                    multiple = TRUE),

        selectInput("state", "State",
                    choices = unique(db$state),
                    multiple = TRUE),

        textInput("case_search", "Search Case ID", ""),

        actionButton("reset_filters", "Reset Filters",
                     class = "btn-warning",
                     style = "margin-top: 10px; width: 100%;"),

        actionButton("generate_code", "Generate R Code",
                     class = "btn-primary",
                     style = "margin-top: 10px; width: 100%;")
      ),

      # accordion_panel(
      #   "Image Selection",
      #   selectInput("selected_files", "Select Images",
      #               choices = NULL,
      #               multiple = TRUE)
      # ),

      accordion_panel(
        "Heatmap Parameters",
        selectInput("heatmap_x", "Heatmap X-axis",
                    choices = names(db),
                    selected = "Project.ID"),

        selectInput("heatmap_y", "Heatmap Y-axis",
                    choices = names(db),
                    selected = "Sample.Type")
      )
    )
  ),

  navset_card_tab(
    nav_panel(
      "Summary Statistics",
      layout_columns(
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          showcase = bs_icon("file-earmark"),
          theme = "primary"
        ),
        value_box(
          title = "Filtered Unique Cases",
          value = textOutput("unique_cases"),
          showcase = bs_icon("person"),
          theme = "secondary"
        ),
        value_box(
          title = "Selected Records",
          value = textOutput("filtered_records"),
          showcase = bs_icon("filter"),
          theme = "success"
          # ),
          # value_box(
          #   title = "Selected Images",
          #   value = textOutput("selected_images_count"),
          #   showcase = bs_icon("images"),
          #   theme = "info"
        )
      ),

      card(
        card_header("Filtered Data Table"),

        DTOutput("data_table")
      ),

      # card(
      #   card_header("Selected Images"),
      #   DTOutput("selected_images_table")
      # ),

      card(
        card_header(
          "Download R Code",
          div(
            style = "float: right;",
            actionButton("copy_code", "", icon = icon("copy"))
          )
        ),
        verbatimTextOutput("download_code")
      )
    ),

    nav_panel(
      "HoverNet Features",
      card(
        card_header("HoverNet Features"),

      )
    ),

    nav_panel(
      "Prov-GigaPath Features",
      card(
        card_header("Prov-GigaPath Features"),

      )
    ),

    nav_panel(
      "Heatmap plot",
      card(
        card_header("Distribution Heatmap"),
        plotOutput("heatmap", height = "600px")
      )
    ),

    nav_panel(
      "Geographic Distribution",
      layout_columns(
        card(
          card_header("Sample Distribution Map"),
          leafletOutput("map", height = "600px")
        ),
        card(
          card_header("Distribution Statistics"),
          layout_columns(
            value_box(
              title = "Number of Cities",
              value = textOutput("num_cities"),
              showcase = bs_icon("buildings"),
              theme = "primary"
            ),
            value_box(
              title = "Number of States",
              value = textOutput("num_states"),
              showcase = bs_icon("geo-alt"),
              theme = "secondary"
            )
          ),
          plotOutput("state_bars", height = "400px")

        )
      )
    ),
    nav_panel(
      "About",
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Author Information"),
          card_body(
            tags$h4("PhD Student"),
            tags$p(
              tags$strong("Name: "), "Ilaria Billato",
              tags$br(),
              tags$strong("Email: "), "ilaria.billato@phd.unipd.it",
              tags$br(),
              tags$strong("Institution: "), "University of Padova"
            )
          )
        ),
        card(
          card_header("Project Details"),
          card_body(
            tags$h4("About this Application"),
            tags$p(
              "This Shiny application was developed to explore the TCGA Diagnostic Image Database with the possibility to extract the R code to download diagnostic images features extract from different tools.",
              tags$br(),
              tags$strong("Version: "), "1.0",
              tags$br(),
              tags$strong("Last Updated: "), format(Sys.Date(), "%B %d, %Y"),
              tags$br(),
              tags$br(),
              "For more information about the project, please visit: my brain ",
              tags$a(href = "https://romualdi.bio.unipd.it/", "Romualdi Lab website", target = "_blank")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive filtered dataset
  filtered_data <- reactive({
    data <- db

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

    if (input$case_search != "") {
      data <- data %>%
        filter(grepl(input$case_search, Case.ID, ignore.case = TRUE))
    }

    data
  })

  # Number of cities
  output$num_cities <- renderText({
    length(unique(filtered_data()$city))
  })

  # Number of states
  output$num_states <- renderText({
    length(unique(filtered_data()$state))
  })

  # Prepare geographic data
  geo_data <- reactive({
    filtered_data() %>%
      group_by(Source.Site, lat, lon, state) %>%
      summarize(
        samples = n(),
        cases = n_distinct(Case.ID),
        .groups = 'drop'
      )
  })

  # Render map
  output$map <- renderLeaflet({
    req(geo_data())

    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = geo_data(),
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(samples) * 3,
        popup = ~paste(
          "<strong>Site:</strong>", Source.Site,
          "<br><strong>State:</strong>", state,
          "<br><strong>Samples:</strong>", samples,
          "<br><strong>Unique Cases:</strong>", cases
        ),
        label = ~Source.Site,
        color = "steelblue",
        fillOpacity = 0.3
      )
  })

  # Render state bar plot
  output$state_bars <- renderPlot({
    req(filtered_data())

    filtered_data() %>%
      count(state) %>%
      ggplot(aes(x = reorder(state, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "State",
        y = "Number of Samples",
        title = "Sample Distribution by State"
      ) +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)
      )
  })

  # Prepare data for heatmap
  heatmap_data <- reactive({
    filtered_data() %>%
      count(!!sym(input$heatmap_x), !!sym(input$heatmap_y)) %>%
      rename(Var1 = !!sym(input$heatmap_y),
             Var2 = !!sym(input$heatmap_x),
             Freq = n)
  })

  # Render heatmap
  output$heatmap <- renderPlot({
    req(heatmap_data())

    ggplot(
      heatmap_data(),
      aes(x = Var2, y = Var1)) +
      geom_point(aes(size = Freq, color = Freq)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_color_viridis(direction = 1) +
      labs(
        x = input$heatmap_x,
        y = input$heatmap_y,
        color = "Number of Records",
        size = "Number of Records"
      ) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
      )
  })

  observe({
    # This will run once when the session starts
    updateSelectInput(session, "project", selected = character(0))
    updateSelectInput(session, "sample_type", selected = character(0))
    updateSelectInput(session, "source_site", selected = character(0))
    updateSelectInput(session, "state", selected = character(0))
    updateTextInput(session, "case_search", value = "")
    output$download_code <- renderText({'## Make sure BiocManager is installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

## Make sure GenomicDataCommons is installed
# BiocManager::install("GenomicDataCommons")
library("GenomicDataCommons")

# File IDs to download
file_ids <- c(
# Insert your file IDs here
  )

# Download files
lapply(file_ids, gdcdata)'
    })
  })


  # code for button in filter panel

  observeEvent(input$reset_filters, {
    updateSelectInput(session, "project", selected = character(0))
    updateSelectInput(session, "sample_type", selected = character(0))
    updateSelectInput(session, "source_site", selected = character(0))
    updateSelectInput(session, "state", selected = character(0))
    updateTextInput(session, "case_search", value = "")
    output$download_code <- renderText({'## Make sure BiocManager is installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

## Make sure GenomicDataCommons is installed
# BiocManager::install("GenomicDataCommons")
library("GenomicDataCommons")

# File IDs to download
file_ids <- c(
# Insert your file IDs here
  )

# Download files
lapply(file_ids, gdcdata)'
    })
  })

  observeEvent(input$generate_code, {
    # Get the current filtered data
    current_data <- filtered_data()

    # Extract file IDs
    file_ids <- paste(sprintf('"%s"', current_data$File.ID), collapse = ",\n  ")

    # Generate the code
    code <- sprintf('## Make sure BiocManager is installed
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

  # output download code
  output$download_code <- renderText({
    code
  })
  })

# copy button
observeEvent(input$copy_code, {
  code <- isolate(output$download_code())
  clipr::write_clip(code)
  showNotification("Code copied to clipboard!", type = "message")
})

# file selection dropdown based on filtered data
observe({
  choices <- setNames(filtered_data()$File.ID, filtered_data()$File.Name)
  updateSelectInput(session, "selected_files", choices = choices)
})

# Combine manually selected rows and sidebar selections
selected_images <- reactive({
  manual_selections <- selected_rows()
  sidebar_selections <- filtered_data() %>%
    filter(File.ID %in% input$selected_files)

  bind_rows(manual_selections, sidebar_selections) %>%
    distinct()
})

# selected images count
# output$selected_images_count <- renderText({
#   nrow(selected_images())
# })

# Display selected images table
output$selected_images_table <- renderDT({
  req(selected_images())
  datatable(
    selected_images() %>%
      select(File.ID, File.Name, Project.ID, Case.ID),
    options = list(pageLength = 5,
                   scrollX = TRUE)
  )
})

# Render the data table
output$data_table <- renderDT({
  data_with_links <- filtered_data() %>%
    mutate(Data.Type = paste0(
      '<a href="https://portal.gdc.cancer.gov/image-viewer/MultipleImageViewerPage?caseId=',
      bcr_patient_uuid,
      '" target="_blank">',
      Data.Type,
      '</a>'
    ))

  datatable(data_with_links,
            escape = FALSE,
            options = list(pageLength = 10,
                           scrollX = TRUE,
                           #css style for selected rows
                           rowCallback = JS(
                             "function(row, data, index) {",
                             "  if ($(row).hasClass('selected')) {",
                             "    $(row).css({'background-color': '#00bc8c', 'steelblue': 'white'});",
                             "  }",
                             "}")),
            selection = 'single')
})

# Statistics outputs
output$total_records <- renderText({
  nrow(db)
})

output$unique_cases <- renderText({
  length(unique(filtered_data()$Case.ID))
})

output$filtered_records <- renderText({
  nrow(filtered_data())
})

# Store selected rows
selected_rows <- reactive({
  s <- input$data_table_rows_selected
  if (is.null(s)) return(NULL)
  filtered_data()[s, ]
})

# Display selected images table
output$selected_images_table <- renderDT({
  req(selected_rows())
  datatable(
    selected_rows() %>% select(File.ID),
    options = list(pageLength = 5,
                   scrollX = TRUE)
  )
})

# Generate download code
#   output$download_code <- renderText({
#     req(selected_rows())
#
#     file_ids <- paste(sprintf('"%s"', selected_images()$File.ID), collapse = ",\n  ")
#
#     sprintf('## Make sure BiocManager is installed
# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager)
#
# ## Make sure GenomicDataCommons is installed
# # BiocManager::install("GenomicDataCommons")
# library(GenomicDataCommons)
#
# # File IDs to download
# file_ids <- c(
#   %s
# )
#
# # Download files
# lapply(file_ids, gdcdata)', file_ids)
#   })
}

shinyApp(ui, server)
}
