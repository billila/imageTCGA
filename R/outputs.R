#' Setup reactive outputs for the imageTCGA app
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @noRd
.setup_outputs <- function(input, output, session) {
    # Statistics outputs
    output$total_records <- renderText({
        nrow(db)
    })

    output$unique_cases <- renderText({
        length(unique(.filter_data(input)$Case.ID))
    })

    output$filtered_records <- renderText({
        nrow(.filter_data(input))
    })

    output$num_cities <- renderText({
        length(unique(.filter_data(input)$city))
    })

    output$num_states <- renderText({
        length(unique(.filter_data(input)$state))
    })

    # Map output
    output$map <- renderLeaflet({
        .render_map(.prepare_geo_data(.filter_data(input)))
    })

    # State bar plot
    output$state_bars <- renderPlot({
        .render_state_bars(.filter_data(input))
    })

    # Heatmap
    output$heatmap <- renderPlot({
        .render_heatmap(.prepare_heatmap_data(.filter_data(input), input))
    })

    # Data table
    output$data_table <- DT::renderDT({
        .render_data_table(.filter_data(input))
    })

    # Selected images table
    output$selected_images_table <- DT::renderDT({
        .render_selected_images_table(.get_selected_rows(input))
    })
}

#' Render the map visualization
#' @param geo_data Processed geographic data
#' @return A leaflet map object
#' @noRd
.render_map <- function(geo_data) {
    req(geo_data)

    leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
            data = geo_data,
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
}

#' Render the state bar plot
#' @param data Filtered data
#' @return A ggplot object
#' @noRd
.render_state_bars <- function(data) {
    req(data)

    data %>%
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
}

#' Render the heatmap
#' @param data Processed heatmap data
#' @return A ggplot object
#' @noRd
.render_heatmap <- function(data) {
    req(data)

    ggplot(data, aes(x = Var2, y = Var1)) +
        geom_point(aes(size = Freq, color = Freq)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        scale_color_viridis(direction = 1) +
        labs(
            x = attr(data, "x_label"),
            y = attr(data, "y_label"),
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
}

#' Render the data table
#' @param data Filtered data
#' @return A DT datatable object
#' @noRd
.render_data_table <- function(data) {
    data_with_links <- data %>%
        mutate(Data.Type = paste0(
            '<a href=
"https://portal.gdc.cancer.gov/image-viewer/MultipleImageViewerPage?caseId=',
            bcr_patient_uuid,
            '" target="_blank">',
            Data.Type,
            '</a>'
        ))

    datatable(
        data_with_links,
        escape = FALSE,
        options = list(
            pageLength = 10,
            scrollX = TRUE,
            rowCallback = JS(
                "function(row, data, index) {",
                "  if ($(row).hasClass('selected')) {",
                "    $(row).css({'background-color':
                '#00bc8c', 'color': 'white'});",
                "  }",
                "}"
            )
        ),
        selection = 'single'
    )
}
