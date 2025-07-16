library(tools)
library(dplyr)
library(magrittr)
library(stringr)

Rdfiles <- list.files(recursive = TRUE, pattern = ".Rd$") %>%
  str_subset(pattern = "lifecycle", negate = TRUE) %>%
  str_subset(pattern = "revdep", negate = TRUE)

for(i in Rdfiles)
{
  tools::checkRd(Rd = i, def_enc = TRUE)
}
