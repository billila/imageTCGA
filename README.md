# imageTCGA <img align="right" width="170" src="https://raw.githubusercontent.com/billila/imageTCGA/main/vignettes/figures/imageTCGA.png">
`imageTCGA` is an R package designed to provide an interactive Shiny application for exploring the TCGA Diagnostic Image Database. This application allows users to filter and visualize clinical data, geographic distribution, and other relevant statistics related to TCGA diagnostic images.


## Installation and Setup

Follow the steps below to install and load the package.

```r
## Make sure BiocManager is installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

if (!require("imageTCGA", quietly = TRUE))
    BiocManager::install("imageTCGA")
library("imageTCGA")
```

## Run the shiny App
After installing the package, you can run the Shiny application by executing the following command in R:

```r
# run:
imageTCGA::imageTCGA()
```

This command will launch the interactive application in your default web browser.
