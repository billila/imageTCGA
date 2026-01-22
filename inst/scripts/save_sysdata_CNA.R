# # signature CNA
#
#
# library(dplyr)
# library(tidyr)
# library(viridis)
# library(ggplot2)
# # tao #####################
# tao <- readRDS("/mnt/tcga_images/purity_ploidy_OV/tcga_cn_sigs_CN176_activity (1).rds")
# # relative
#
# tao$relative$sample
# tao$relative$Case.ID <- substr(tao$relative$sample, 1, 12)
#
# tao_summary <- tao$relative |>
#   dplyr::group_by(Case.ID) |>
#   dplyr::summarise(across(everything(), ~ first(.x)))  # o mean(), paste(), ecc.
# merged_CNA <- merge(db, tao_summary, by = "Case.ID", all.x = TRUE)
#
# db_CNA <- merge(
#   db,
#   tao_summary,
#   by = "Case.ID",
#   all.x = TRUE
# )
#
#
# merged_CNA_long <- merged_CNA %>%
#   tibble::as_tibble() %>%
#   dplyr::select(patientID, cluster, starts_with("Sig")) %>%
#   pivot_longer(
#     cols = starts_with("Sig"),
#     names_to = "signature",
#     values_to = "value"
#   )
#
# merged_CNA_long <- merged_CNA_long %>%
#   mutate(signature = factor(signature,
#                             levels = paste0("Sig", 1:20)))
#
# library(ggplot2)
#
# boxplot_tao <- ggplot(merged_CNA_long, aes(x = cluster, y = value, fill = cluster)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
#   facet_wrap(~ signature, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Distribution of Tao signatures across clusters",
#        x = "Cluster", y = "Signature value")
#
# library(rstatix)
#
# stat_tests <- merged_CNA_long %>%
#   group_by(signature) %>%
#   kruskal_test(value ~ cluster) %>%
#   ungroup()
#
# print(stat_tests)
#
#
# library(pheatmap)
#
#
# mean_mat <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE)) %>%
#   pivot_wider(names_from = signature, values_from = mean_val) %>%
#   as.data.frame()
#
# # rownames = cluster
# rownames(mean_mat) <- mean_mat$cluster
# mean_mat$cluster <- NULL
#
# pheatmap(as.matrix(mean_mat),
#          cluster_rows = TRUE, cluster_cols = TRUE,
#          main = "Mean signature values per cluster")
#
#
# mean_df <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
#
#
# heatmap_tao <- ggplot(mean_df, aes(x = signature, y = cluster, fill = mean_val)) +
#   geom_tile(color = "white") +
#   #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(mean_df$mean_val, na.rm = TRUE)) +
#   scale_fill_viridis(option = "viridis", direction = 1) +
#   theme_minimal() +
#   labs(title = "Mean Tao signature values per cluster",
#        x = "Signature",
#        y = "Cluster",
#        fill = "Mean value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
# # steel ###############################
# load("/mnt/tcga_images/purity_ploidy_OV/Steel_raw (1).RData")
#
# X41586_2022_4738_MOESM4_ESM_2_$patientID <- substr(X41586_2022_4738_MOESM4_ESM_2_$Sample, 1, 12)
#
# X41586_2022_4738_MOESM4_ESM_2_$Case.ID <- X41586_2022_4738_MOESM4_ESM_2_$patientID
#
# X41586_2022_4738_MOESM4_ESM_2_summary <- X41586_2022_4738_MOESM4_ESM_2_ |>
#   dplyr::group_by(Case.ID) |>
#   dplyr::summarise(across(everything(), ~ first(.x)))  # o mean(), paste(), ecc.
# db_CNA2 <- merge(db_CNA, X41586_2022_4738_MOESM4_ESM_2_summary, by = "Case.ID", all.x = TRUE)
#
# # merged_CNA <- merge(
# #   db_CNA,
# #   X41586_2022_4738_MOESM4_ESM_2_,
# #   by = "Case.ID",
# #   all.x = TRUE
# # )
#
# merged_CNA <- merged_CNA %>%
#   mutate(across(starts_with("CN"), as.numeric))
#
# merged_CNA_long <- merged_CNA %>%
#   tibble::as_tibble() %>%
#   dplyr::select(patientID, cluster, starts_with("CN")) %>%
#   pivot_longer(
#     cols = starts_with("CN"),
#     names_to = "signature",
#     values_to = "value"
#   )
#
# merged_CNA_long <- merged_CNA_long %>%
#   mutate(signature = factor(signature,
#                             levels = paste0("CN", 1:21)))
#
# library(ggplot2)
#
# boxplot_steel <- ggplot(merged_CNA_long, aes(x = cluster, y = value, fill = cluster)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
#   facet_wrap(~ signature, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Distribution of Steel signatures across clusters",
#        x = "Cluster", y = "Signature value")
#
# library(rstatix)
#
# stat_tests <- merged_CNA_long %>%
#   group_by(signature) %>%
#   kruskal_test(value ~ cluster) %>%
#   ungroup()
#
# print(stat_tests)
#
#
# library(pheatmap)
#
# # calcolo media per cluster-signature
# mean_mat <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE)) %>%
#   pivot_wider(names_from = signature, values_from = mean_val) %>%
#   as.data.frame()
#
# # rownames = cluster
# rownames(mean_mat) <- mean_mat$cluster
# mean_mat$cluster <- NULL
#
# pheatmap(as.matrix(mean_mat),
#          cluster_rows = TRUE, cluster_cols = TRUE,
#          main = "Mean signature values per cluster")
#
#
# mean_df <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
#
#
# heatmap_steel <- ggplot(mean_df, aes(x = signature, y = cluster, fill = mean_val)) +
#   geom_tile(color = "white") +
#   #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(mean_df$mean_val, na.rm = TRUE)) +
#   scale_fill_viridis(option = "viridis", direction = 1) +
#   theme_minimal() +
#   labs(title = "Mean Steel signature values per cluster",
#        x = "Signature",
#        y = "Cluster",
#        fill = "Mean value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
#
#
# ########### drews ###############
# drew <- readRDS("/mnt/tcga_images/purity_ploidy_OV/Signature_Compendium_v5_Cosine-0.74_Activities_THRESH95_NAMESAPRIL21 (1).rds")
# drew <- as.data.frame(drew)
# drew$Case.ID <- rownames(drew)
#
# db_CNA3 <- merge(
#   db_CNA2,
#   drew,
#   by = "Case.ID",
#   all.x = TRUE
# )
#
# remove <- c("sample", "Sample", "patientID")
# db_CNA3 <- db_CNA3[, !colnames(db_CNA3) %in% remove]
#
# save(db_CNA3, file = "sysdata_CNA.rda")
#
# merged_CNA_long <- merged_CNA %>%
#   tibble::as_tibble() %>%
#   dplyr::select(patientID, cluster, starts_with("CX")) %>%
#   pivot_longer(
#     cols = starts_with("CX"),
#     names_to = "signature",
#     values_to = "value"
#   )
#
# merged_CNA_long <- merged_CNA_long %>%
#   mutate(signature = factor(signature,
#                             levels = paste0("CX", 1:17)))
#
# library(ggplot2)
#
# boxplot_drew <- ggplot(merged_CNA_long, aes(x = cluster, y = value, fill = cluster)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
#   facet_wrap(~ signature, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Distribution of Drews signatures across clusters",
#        x = "Cluster", y = "Signature value")
#
# library(rstatix)
#
# stat_tests <- merged_CNA_long %>%
#   group_by(signature) %>%
#   kruskal_test(value ~ cluster) %>%
#   ungroup()
#
# print(stat_tests)
#
#
# library(pheatmap)
#
# # calcolo media per cluster-signature
# mean_mat <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE)) %>%
#   pivot_wider(names_from = signature, values_from = mean_val) %>%
#   as.data.frame()
#
# # rownames = cluster
# rownames(mean_mat) <- mean_mat$cluster
# mean_mat$cluster <- NULL
#
# pheatmap(as.matrix(mean_mat),
#          cluster_rows = TRUE, cluster_cols = TRUE,
#          main = "Mean signature values per cluster")
#
#
# mean_df <- merged_CNA_long %>%
#   group_by(cluster, signature) %>%
#   summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
#
#
# heatmap_drew <- ggplot(mean_df, aes(x = signature, y = cluster, fill = mean_val)) +
#   geom_tile(color = "white") +
#   #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(mean_df$mean_val, na.rm = TRUE)) +
#   scale_fill_viridis(option = "viridis", direction = 1) +
#   theme_minimal() +
#   labs(title = "Mean Drews signature values per cluster",
#        x = "Signature",
#        y = "Cluster",
#        fill = "Mean value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
#
# # plot ################
#
# library(patchwork)
#
# combined_plot <- heatmap_tao / heatmap_steel / heatmap_drew +
#   plot_layout(guides = "collect") & theme(legend.position = "right")
#
# combined_plot
#
# library(cowplot)
#
# combined_plot <- plot_grid(
#   heatmap_tao, heatmap_steel, heatmap_drew,
#   ncol = 1,
#   align = "v",
#   labels = c("A", "B", "C")
# )
#
# png("/mnt/tcga_images/purity_ploidy_OV/case_study_OV/plot/combined_heatmap_CNA.png", width=12, height=8, units = "in", res = 800)
# combined_plot
# dev.off()
#
#
#
# combined_boxplot <- plot_grid(
#   boxplot_tao, boxplot_steel, boxplot_drew,
#   ncol = 1,
#   align = "v",
#   labels = c("A", "B", "C")
# )
#
#
#
#
# png("/mnt/tcga_images/purity_ploidy_OV/case_study_OV/plot/combined_boxplot_CNA.png", width=12, height=16, units = "in", res = 800)
# combined_boxplot
# dev.off()
