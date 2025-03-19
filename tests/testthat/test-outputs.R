# test_that("setup_outputs crea correttamente gli output", {
#   # Dati di esempio per il test
#   input <- list(
#     # Simula un input di esempio, ad esempio per un filtro specifico
#     city = "Houston",
#     state = "Texas"
#   )
#
#   output <- list()  # Oggetto di output da testare
#   session <- NULL   # Non è necessario per questo test
#
#   # Esegui la funzione .setup_outputs
#   expect_silent({
#     imageTCGA:::.setup_outputs(input, output, session)
#   })
#
#   # Verifica che tutti gli output siano stati creati correttamente
#   expect_true("total_records" %in% names(output))
#   expect_true("unique_cases" %in% names(output))
#   expect_true("filtered_records" %in% names(output))
#   expect_true("num_cities" %in% names(output))
#   expect_true("num_states" %in% names(output))
#   expect_true("map" %in% names(output))
#   expect_true("state_bars" %in% names(output))
#   expect_true("heatmap" %in% names(output))
#   expect_true("data_table" %in% names(output))
#
#   # Verifica che i valori degli output siano del tipo atteso
#   expect_true(is.character(output$total_records()))
#   expect_true(is.character(output$unique_cases()))
#   expect_true(is.character(output$filtered_records()))
#   expect_true(is.character(output$num_cities()))
#   expect_true(is.character(output$num_states()))
#
#   # Verifica che la mappa sia un oggetto leaflet
#   map_output <- output$map()
#   expect_true("leaflet" %in% class(map_output))
#
#   # Verifica che il grafico a barre sia un oggetto ggplot
#   state_bars_output <- output$state_bars()
#   expect_true("gg" %in% class(state_bars_output))
#
#   # Verifica che la heatmap sia un oggetto ggplot
#   heatmap_output <- output$heatmap()
#   expect_true("gg" %in% class(heatmap_output))
#
#   # Verifica che la tabella dei dati sia un oggetto DT
#   data_table_output <- output$data_table()
#   expect_true("DT" %in% class(data_table_output))
# })

test_that("render_map crea correttamente la mappa con i dati del db", {
  # Dati di esempio per la mappa basati su db
  geo_data <- imageTCGA:::db %>%
    dplyr::filter(state == "Texas") %>%
    group_by(Case.ID, lon, lat, Source.Site, state, samples = 10) %>%
    dplyr::summarize(
      samples = n(),
      cases = n_distinct(.data$Case.ID),
      .groups = 'drop'
    )

  # Verifica che la mappa venga creata senza errori
  expect_silent({
    map_output <- imageTCGA:::.render_map(geo_data)
    expect_true("leaflet" %in% class(map_output))
  })
})

test_that("render_state_bars crea correttamente il grafico a barre per stato", {
  # Dati di esempio per il grafico a barre
  data <- imageTCGA:::db %>%
    dplyr::filter(state == "Texas") %>%
    dplyr::count(state)  # Conta il numero di campioni per stato

  # Verifica che il grafico venga creato senza errori
  expect_silent({
    state_bars_output <- imageTCGA:::.render_state_bars(data)
    expect_true("gg" %in% class(state_bars_output))  # Verifica che il risultato sia un oggetto ggplot
    expect_true("ggplot" %in% class(state_bars_output))  # Verifica che il risultato sia un oggetto ggplot
  })
})

test_that("render_heatmap crea correttamente la heatmap", {
  # Dati di esempio per la heatmap
  data <- data.frame(
    Var1 = c("A", "B", "C"),
    Var2 = c("X", "Y", "Z"),
    Freq = c(10, 15, 20)
  )

  # Aggiungiamo gli attributi per le etichette
  attr(data, "x_label") <- "Variable 2"
  attr(data, "y_label") <- "Variable 1"

  # Verifica che la heatmap venga creata senza errori
  expect_silent({
    heatmap_output <- imageTCGA:::.render_heatmap(data)
    expect_true("gg" %in% class(heatmap_output))  # Verifica che il risultato sia un oggetto ggplot
    expect_true("ggplot" %in% class(heatmap_output))  # Verifica che il risultato sia un oggetto ggplot
  })
})



test_that("render_data_table crea correttamente la tabella", {
  # Dati di esempio per la tabella
  data <- imageTCGA:::db %>%
    dplyr::select(File.ID, File.Name, Data.Type, bcr_patient_uuid, state) %>%
    dplyr::filter(state == "Texas")

  # Verifica che la tabella venga creata senza errori
  expect_silent({
    data_table_output <- imageTCGA:::.render_data_table(data)  # Verifica che il risultato sia un oggetto datatable
  })
})

