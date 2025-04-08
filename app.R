# Launch the ShinyApp (Do not remove this comment)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

options(repos = BiocManager::repositories())

if (!requireNamespace("imageTCGA", quietly = TRUE))
  BiocManager::install("imageTCGA")

imageTCGA2::imageTCGA()
