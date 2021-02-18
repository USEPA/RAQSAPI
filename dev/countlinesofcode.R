#a simple script that counts the number of lines of code in the package:RAQSAPI.
#' @name countlinesofcode
#' @description prints out a count of thr number of lines of code in his package
#' @note this function should not be exported
#' @param NA
#' @return NULL
countlinesofcode <- function()
{
  library(dplyr)
  library(magrittr)
  library(stringr)
  list.files(path = getwd(), recursive = TRUE, full.names = TRUE) %>%
      stringr::str_subset(pattern = ".R$|.Rmd$") %>%
      sapply(function(x) x %>% readLines() %>% length()) %>%
      sum() %>%
      print()
}
