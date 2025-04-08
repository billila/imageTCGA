
<h1 style="height: 205.141px; display: flex; align-items: center; justify-content: space-between; padding-bottom: 0px;">
imageTCGA <a target="_blank"
        href="https://raw.githubusercontent.com/billila/imageTCGA/devel/vignettes/figures/imageTCGA.png">
<img align="right" width="170"
        src="https://raw.githubusercontent.com/billila/imageTCGA/devel/vignettes/figures/imageTCGA.png"
        style="max-width:100%;" alt=""> </a>
</h1>

# Introduction

`imageTCGA` is an R package designed to provide an interactive Shiny
application for exploring the TCGA Diagnostic Image Database. This
application allows users to filter and visualize metadata, geographic
distribution, and other relevant statistics related to TCGA diagnostic
images.

This package is part of the Multi-omic Integration of Histopathology
Image Analysis working group, which addresses the need for standardized
workflows to integrate histopathology image-derived features with
genomic and transcriptomic analyses in R/Bioconductor. `imageTCGA` lays
the foundation for a comprehensive platform where pre-extracted image
features from Python-based tools from TCGA will be made accessible
within R/Bioconductor data structures, streamlining data integration and
accelerating research in computational pathology and precision oncology.

# Installation

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("imageTCGA")
```

## Setup

Load the package:

``` r
library(imageTCGA)
```

## Run the shiny app

After installing the package, you can run the Shiny application by
executing the following command in R:

``` r
imageTCGA::imageTCGA()
```

This will open the application in your default web browser, where you
can explore 11,765 diagnostic images from 9,640 patients, filtering them
based on various clinical and pathological parameters.

<img src="https://github.com/billila/imageTCGA/blob/devel/vignettes/figures/imageTCGA_shiny.png?raw=true" width="100%" />

# Filtering

The application allows filtering by any of the available columns in the
dataset. For instance, you can filter for a specific tumor type, such as
Ovarian Cancer (107 diagnostic images).

<img src="https://github.com/billila/imageTCGA/blob/devel/vignettes/figures/imageTCGA_shiny_OV.png?raw=true" width="100%" />

# R Code

You can generate R code to download the selected images to your local
machine by clicking the orange “Generate R Code” button. This utilizes
the GenomicDataCommons package.

In the example below, Ovarian Cancer images have been selected:

<img src="https://github.com/billila/imageTCGA/blob/devel/vignettes/figures/imageTCGA_shiny_OV_Rcode.png?raw=true" width="100%" />

# Visualization

## Dotplot

The dot plot visualization allows users to explore gynecological tumors
(BRCA, OV, UCS, UCEC). On the left panel, you can select which variables
to plot on the x-axis and y-axis.

<img src="https://github.com/billila/imageTCGA/blob/devel/vignettes/figures/imageTCGA_shiny_dotplot.png?raw=true" width="100%" />

## Geographic Distribution

The application provides an interactive geographic visualization,
displaying the origin of diagnostic images at the center, country, and
state level.

For example, in the image below, GBM tumors have been selected.
Additionally, summary statistics such as the number of cities and states
are reported alongside a bar plot of the state distribution.

<img src="https://github.com/billila/imageTCGA/blob/devel/vignettes/figures/imageTCGA_shiny_worldmap_GBM.png?raw=true" width="100%" />

# Session Info

<details>
<summary>
Click here for Session Info
</summary>

``` r
sessionInfo()
#> R version 4.4.3 (2025-02-28)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.2 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0 
#> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Rome
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] imageTCGA_0.99.2 shiny_1.10.0    
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6      jsonlite_2.0.0    dplyr_1.1.4       compiler_4.4.3   
#>  [5] bsicons_0.1.2     promises_1.3.2    tidyselect_1.2.1  Rcpp_1.0.14      
#>  [9] leaflet_2.2.2     gridExtra_2.3     tidyr_1.3.1       later_1.4.1      
#> [13] jquerylib_0.1.4   scales_1.3.0      yaml_2.3.10       fastmap_1.2.0    
#> [17] mime_0.13         ggplot2_3.5.1     R6_2.6.1          generics_0.1.3   
#> [21] knitr_1.50        viridis_0.6.5     htmlwidgets_1.6.4 tibble_3.2.1     
#> [25] munsell_0.5.1     bslib_0.9.0       pillar_1.10.2     rlang_1.1.5      
#> [29] cachem_1.1.0      httpuv_1.6.15     xfun_0.52         sass_0.4.9       
#> [33] viridisLite_0.4.2 cli_3.6.4         magrittr_2.0.3    crosstalk_1.2.1  
#> [37] digest_0.6.37     grid_4.4.3        rstudioapi_0.17.1 xtable_1.8-4     
#> [41] lifecycle_1.0.4   vctrs_0.6.5       evaluate_1.0.3    glue_1.8.0       
#> [45] codetools_0.2-20  colorspace_2.1-1  purrr_1.0.4       rmarkdown_2.29   
#> [49] tools_4.4.3       pkgconfig_2.0.3   htmltools_0.5.8.1
```

</details>
