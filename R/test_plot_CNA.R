load("R/sysdata_CNA.rda")

db_CNA3 <- db_CNA3 %>%
  mutate(Project.ID = as.factor(Project.ID))



merged_CNA_long <- db_CNA3 %>%
  tibble::as_tibble() %>%
  dplyr::select(Case.ID, Project.ID, starts_with("CN")) %>%
  pivot_longer(
    cols = starts_with("CN"),
    names_to = "signature",
    values_to = "value"
  ) %>%
  mutate(
    signature = factor(signature, levels = paste0("CN", 1:21)),
    Project.ID = factor(Project.ID)
  )

merged_CNA_long$value <- as.numeric(merged_CNA_long$value)
data <- merged_CNA_long

.render_CNA_heatmap <- function(data) {
  req(data)
  data <- data %>%
    arrange(Project.ID, Case.ID) %>%
    mutate(Case.ID = factor(Case.ID, levels = unique(Case.ID)))

  ggplot(data, aes(x = signature, y = Case.ID, fill = value)) +
    geom_tile(color = "grey80") +
    #scale_fill_viridis_c(option = "B", na.value = "white") +
    #facet_grid(Project.ID ~ ., scales = "free_y", space = "free_y") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      strip.text.y = element_text(size = 12)  # label per tipo tumorale
    ) +
    labs(
      x = "CNA signature",
      y = "Patients",
      fill = "CN value",
      title = "Copy Number Alterations by Tumor Type (TCGA Project)"
    )
}
