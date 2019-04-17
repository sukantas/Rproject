pkgs_cran <-c (
    "fs",
    
    #import
    "readxl",
    "writexl",
    "odbc",
    "RSQLite",
    
    #tidy
    "tidyverse",
    "lubridate",
    "tidyquant",
    
    #Model
    "tidymodels",
    "umap",
    
    #others
    "devtools"
    
    )
install.packages(pkgs_cran)
