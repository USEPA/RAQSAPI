library(knitr)
library(magrittr)
library(dplyr)
library(tibble)
library(stringr)


RAQSAPI_functions <- function()
{
  setupfunctions <- list("aqs_credentials", "aqs_sign_up")
  listfunctions <- list(
    "aqs_isavailable", "aqs_knownissues", "aqs_counties_by_state", "aqs_sites_by_county", "aqs_classes",
    "aqs_parameters_by_class", "aqs_mas", "aqs_pqaos", "aqs_cbsas", "aqs_states", "aqs_removeheader", "aqs_revisionhistory",
    "aqs_sampledurations", "aqs_fields_by_service"
  )
  serviceshelperfunctions <- list(
    "aqs_services_by_site", "aqs_services_by_county", "aqs_services_by_state", "aqs_services_by_MA", "aqs_services_by_pqao",
    "aqs_services_by_cbsa", "aqs_services_by_box"
  )

  functiontype <- function(functionname)
    {
    case_when(
      str_detect(string = functionname, pattern = "by_site") ~
        "RAQSAPI aggregation by site aggregate functions", str_detect(string = functionname, pattern = "by_county") ~
        "RAQSAPI aggregation by county aggregate functions", str_detect(string = functionname, pattern = "by_state") ~
        "RAQSAPI aggregation by state aggregate functions", str_detect(string = functionname, pattern = "by_cbsa") ~
        "RAQSAPI aggregation by cbsa aggregate functions", str_detect(string = functionname, pattern = "by_pqao") ~
        "RAQSAPI aggregation by pqao aggregate functions", str_detect(string = functionname, pattern = "by_box") ~
        "RAQSAPI aggregation by lat/long bounding box aggregate functions", str_detect(string = functionname,
                                                                                       pattern = "by_MA") ~
        "RAQSAPI aggregation by Monitoring Agency aggregate functions", .default = "misc"
    )
  }

  functiontable <- tibble(
    functionnames = list.files("./man/html") %>%
      str_remove_all(pattern = ".html"),
    relPATH = list.files("./man/html", full.names = TRUE)
  ) %>%
    mutate(functionfamily = functiontype(functionnames))
  functiontable$functionfamily[which(functiontable$functionnames %in% listfunctions)] <- "RAQSAPI list functions"
  functiontable$functionfamily[which(functiontable$functionnames %in% setupfunctions)] <- "RAQSAPI setup functions"
  functiontable$functionfamily[
      which(functiontable$functionnames %in% serviceshelperfunctions)] <- "RAQSAPI services functions"

  functiontable %<>%
    filter(!functionfamily == "services functions") %>%
    filter(!functionnames == "deprecated")

  functionlist <- functiontable %>%
    split(functiontable$functionfamily)
  list(functiontable, functionlist) %>%
    return()
}
