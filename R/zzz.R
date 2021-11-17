# Welcome message
.onLoad <- function(...){
  quietly <- getOption('quietly')
  options(quietly = T)

    invisible(suppressPackageStartupMessages(
      sapply(c("stringi", "stringr",
               "qdapRegex", "readr",
               "tokenizers", "rvest",
               "pryr", "XML", "xml2",
               "lubridate", "data.table",
               "ggplot2", "anytime", "dplyr",
               "network", "quanteda", "ggmap",
               "networkDynamic", "mgsub",
               "dplyr", "ggplot2", "network",
               "stats", "ndtv", "devtools",
               "ggtext"),
             requireNamespace, quietly = TRUE)))

  pkg_info <- "Welcome to WhatsR - A package for parsing and visualizing WhatsApp Chatlogs! \n For the latest version and to file issues, please see: https://github.com/gesiscss/WhatsR"
  packageStartupMessage(pkg_info)
  options(quietly = quietly)
}
