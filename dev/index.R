## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(comment = '##', echo=FALSE)
library(knitr)
library(magrittr)
library(dplyr)
library(tibble)
library(stringr)
library(glue)
library(pander)
panderOptions('knitr.auto.asis', FALSE)


## ----htmlMANpages, echo=FALSE, results = 'asis', cache = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

source("./vignettes/RAQSAPI_exported_functions.R")
RAQSAPIfunctions <- RAQSAPI_functions()
cat("RAQSAPI manual pages \n \n ")
RAQSAPIfunctions[[2]] %>% kable() %>% cat()



