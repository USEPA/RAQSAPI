library(dplyr)
library(tibble)
library(rio)
library(magrittr)
library(waldo)
library(glue)

wordlist_file <- "./inst/WORDLIST"

oldvalue <- import(file = wordlist_file, format = "txt") %>%
  tibble()

newvalue <- oldvalue %>%
  unique() %>%
  arrange()

cat("arranging the spelling wordlist. Here is the difference between the old version and the new: \n")
waldo::compare(oldvalue, newvalue) %>%
  print()

if(!all.equal(oldvalue,newvalue))
{
  cat(glue("ovewriting {wordlist_file} with these changes \n "))
  oldvalue %>%
    export(file = wordlist_file, format = "txt")
} else {
  cat("no changes to the wordlist file. \n")
}
