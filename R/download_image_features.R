#' Download data from U24 Cancer Genomics (HoverNet or ProvGigaPath)
#'
#' This function navigates the data repository structure for HoverNet and
#' ProvGigaPath outputs, allowing selective download by tumor type and data level.
#'
#' @param dataset One of "hovernet" or "provgigapath".
#' @param level For ProvGigaPath, one of "slide_level" or "tile_level". Ignored for HoverNet.
#' @param tumor_types Character vector of TCGA cancer types (e.g., c("TCGA_OV", "TCGA_BRCA")).
#'   If NULL, all available tumor types are included.
#' @param subfolder For HoverNet, one of "h5ad", "json", or "thumb".
#'   If NULL, downloads all subfolders.
#' @param pattern Optional regular expression to match filenames (e.g. "geojson", "tsv", "h5ad").
#' @param dest_dir Local directory for downloads (default = "downloads").
#' @param dry_run Logical; if TRUE, only lists files without downloading.
#'
#' @return Character vector of downloaded (or listed) files.
#' @export
#'
#' @examples
#' \dontrun{
#' # List HoverNet JSON files for OV
#' download_imageTCGA_data("hovernet", tumor_types = "TCGA_OV",
#'                         subfolder = "json", pattern = "geojson", dry_run = TRUE)
#'
#' # Download slide-level ProvGigaPath features for OV and BRCA
#' download_imageTCGA_data("provgigapath", level = "slide_level",
#'                         tumor_types = c("TCGA_OV", "TCGA_BRCA"))
#' }
# download_imageTCGA_data <- function(dataset = c("hovernet", "provgigapath"),
#                                     level = c("slide_level", "tile_level"),
#                                     tumor_types = NULL,
#                                     subfolder = NULL,
#                                     pattern = NULL,
#                                     dest_dir = "downloads",
#                                     dry_run = FALSE) {
#   if (!requireNamespace("rvest", quietly = TRUE)) stop("Please install 'rvest'")
#   if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'")
#
#   dataset <- match.arg(dataset)
#   level <- match.arg(level)
#
#   base_url <- switch(dataset,
#                      "hovernet" = "https://u24-cancer-genomics.seandavi.workers.dev/hovernet/",
#                      "provgigapath" = paste0("https://u24-cancer-genomics.seandavi.workers.dev/provgigapath/", level, "/"))
#
#   message("Scanning repository: ", base_url)
#
#   page <- tryCatch(xml2::read_html(base_url),
#                    error = function(e) stop("Cannot read base URL: ", base_url))
#
#   links <- page |>
#     rvest::html_elements("a") |>
#     rvest::html_attr("href")
#
#   tumor_dirs <- links[grepl("^TCGA_[A-Z]+/?$", links)]
#
#   if (!is.null(tumor_types)) {
#     tumor_dirs <- tumor_dirs[basename(sub("/$", "", tumor_dirs)) %in% tumor_types]
#   }
#
#   all_files <- character()
#
#   for (tumor in tumor_dirs) {
#     tumor_name <- sub("/$", "", basename(tumor))
#     tumor_url <- paste0(base_url, tumor)
#     message("→ Processing ", tumor_name)
#
#     if (dataset == "hovernet") {
#       subfolders <- c("h5ad", "json", "thumb")
#       if (!is.null(subfolder)) subfolders <- subfolder
#
#       for (sf in subfolders) {
#         sub_url <- paste0(tumor_url, sf, "/")
#         sub_dest <- file.path(dest_dir, dataset, tumor_name, sf)
#         if (!dir.exists(sub_dest)) dir.create(sub_dest, recursive = TRUE)
#
#         sub_page <- tryCatch(xml2::read_html(sub_url),
#                              error = function(e) { message("  Skipping ", sub_url); return(NULL) })
#         if (is.null(sub_page)) next
#
#         sub_links <- sub_page |>
#           rvest::html_elements("a") |>
#           rvest::html_attr("href")
#
#         files <- sub_links[!grepl("/$", sub_links)]
#         if (!is.null(pattern)) {
#           files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
#         }
#
#         if (length(files) == 0) next
#
#         full_urls <- paste0(sub_url, files)
#         if (dry_run) {
#           message("  Found ", length(full_urls), " files in ", sf)
#           print(full_urls)
#         } else {
#           for (f in full_urls) {
#             dest_file <- file.path(sub_dest, basename(f))
#             if (!file.exists(dest_file)) {
#               httr::GET(f, httr::write_disk(dest_file, overwrite = TRUE))
#             }
#           }
#         }
#         all_files <- c(all_files, full_urls)
#       }
#     } else if (dataset == "provgigapath") {
#       tumor_url <- paste0(base_url, tumor)
#       tumor_dest <- file.path(dest_dir, dataset, level, tumor_name)
#       if (!dir.exists(tumor_dest)) dir.create(tumor_dest, recursive = TRUE)
#
#       tumor_page <- tryCatch(xml2::read_html(tumor_url),
#                              error = function(e) { message("  Skipping ", tumor_url); return(NULL) })
#       if (is.null(tumor_page)) next
#
#       tumor_links <- tumor_page |>
#         rvest::html_elements("a") |>
#         rvest::html_attr("href")
#       files <- tumor_links[!grepl("/$", tumor_links)]
#       if (!is.null(pattern)) {
#         files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
#       }
#       full_urls <- paste0(tumor_url, files)
#
#       if (dry_run) {
#         message("  Found ", length(full_urls), " files")
#         print(full_urls)
#       } else {
#         for (f in full_urls) {
#           dest_file <- file.path(tumor_dest, basename(f))
#           if (!file.exists(dest_file)) {
#             httr::GET(f, httr::write_disk(dest_file, overwrite = TRUE))
#           }
#         }
#       }
#       all_files <- c(all_files, full_urls)
#     }
#   }
#
#   invisible(all_files)
# }
