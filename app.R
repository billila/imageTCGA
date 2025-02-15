# Launch the ShinyApp (Do not remove this comment)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

options(repos = BiocManager::repositories())

# if (!requireNamespace("imageTCGA2", quietly = TRUE))
#   BiocManager::install("imageTCGA2")

imageTCGA::imageTCGA() # add parameters here (if any)
