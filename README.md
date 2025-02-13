# imageTCGA <img align="right" width="170" src="https://github.com/billila/imageTCGA/vignettes/images/imageTCGA.png">

`imageTCGA` is an R package designed to provide an interactive Shiny application for exploring the TCGA Diagnostic Image Database. This application allows users to filter and visualize clinical data, geographic distribution, and other relevant statistics related to TCGA diagnostic images.


## Installation and Setup

Follow the steps below to install and load the package.

```r
# Step 1: Install devtools if you haven't already
install.packages("devtools")

# Step 2: Load devtools
library("devtools")

# Step 3: Install imageTCGA from GitHub
devtools::install_github("billila/imageTCGA")

# Step 4: Load imageTCGA
library(imageTCGA)
```

## Run the shiny App
After installing the package, you can run the Shiny application by executing the following command in R:

```r
# run:
imageTCGA::imageTCGA()
```

This command will launch the interactive application in your default web browser.
