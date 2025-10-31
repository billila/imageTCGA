# # add clinical data
#
# library(dplyr)
# library(readr)
#
# clin <- read_csv("/mnt/tcga_images/all_clin_indexed.csv")
# clin2 <- read_csv("/mnt/tcga_images/all_clin_XML.csv")
#
# clin$
# OS <- c("vital_status", "days_to_last_follow_up", "days", "days_to_death")
#
# RFS <- c("days_to_recurrence")
#
# prepare_tcga_survival <- function(clin) {
#   library(dplyr)
#
#   clin %>%
#     mutate(
#       # --- Overall Survival (OS) ---
#       OS.time = ifelse(is.na(days_to_death), days_to_last_follow_up, days_to_death),
#       OS.event = ifelse(vital_status == "Dead", 1, 0),
#
#       # --- Progression-Free Interval (PFI) ---
#       PFI.time = ifelse(!is.na(days_to_recurrence), days_to_recurrence, OS.time),
#       PFI.event = case_when(
#         progression_or_recurrence == "YES" ~ 1,
#         vital_status == "Dead" ~ 1,
#         TRUE ~ 0
#       ),
#
#       # --- Disease Specific Survival (DSS) ---
#       DSS.event = ifelse(grepl("cancer|tumor|neoplasm", tolower(cause_of_death)), 1, 0),
#       DSS.time = OS.time,
#
#       # --- Recurrence-Free Survival (RFS / DFI) ---
#       Recurrence.event = ifelse(progression_or_recurrence == "YES", 1, 0),
#       Recurrence.time = days_to_recurrence,
#
#       Case.ID = bcr_patient_barcode
#     ) %>%
#     select(
#       Case.ID,
#       OS.time, OS.event,
#       DSS.time, DSS.event,
#       PFI.time, PFI.event,
#       Recurrence.time, Recurrence.event
#     )
# }
#
# tcga_survival <- prepare_tcga_survival(clin)
#
#
#
# db_CNA_surv <- merge(
#   db_CNA3,
#   tcga_survival,
#   by = "Case.ID",
#   all.x = TRUE
# )
# db <- db_CNA_surv
# save(db, file = "R/sysdata.rda")
