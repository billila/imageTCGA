download_imageTCGA_data(
  dataset = "hovernet",
  tumor_types = "TCGA_OV",
  subfolder = "json",
  pattern = "geojson",
  dry_run = TRUE
)

download_imageTCGA_data(
  dataset = "provgigapath",
  level = "slide_level",
  tumor_types = c("TCGA_OV"),
  dest_dir = "/home/ilaria/Downloads/"
)
