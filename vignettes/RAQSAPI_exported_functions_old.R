library(dplyr)
library(magrittr)
library(purrr)
library(stringi)
library(stringr)
library(waldo)
library(tidyr)
library(tibble)

RAQSAPI_by_site_functions <- c(
  "aqs_annualsummary_by_site",
  "aqs_dailysummary_by_site",
  "aqs_monitors_by_site",
  "aqs_qa_annualperformanceeval_by_site",
  "aqs_qa_annualperformanceevaltransaction_by_site",
  "aqs_qa_blanks_by_site",
  "aqs_qa_collocated_assessments_by_site",
  "aqs_qa_flowrateaudit_by_site",
  "aqs_qa_flowrateverification_by_site",
  "aqs_qa_one_point_qc_by_site",
  "aqs_qa_pep_audit_by_site",
  "aqs_quarterlysummary_by_site",
  "aqs_sampledata_by_site",
  "aqs_transactionsample_by_site"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_county_functions <- c(
  "aqs_annualsummary_by_county",
  "aqs_dailysummary_by_county",
  "aqs_monitors_by_county",
  "aqs_qa_annualperformanceeval_by_county",
  "aqs_qa_annualperformanceevaltransaction_by_county",
  "aqs_qa_blanks_by_county",
  "aqs_qa_collocated_assessments_by_county",
  "aqs_qa_flowrateaudit_by_county",
  "aqs_qa_flowrateverification_by_county",
  "aqs_qa_one_point_qc_by_county",
  "aqs_qa_pep_audit_by_county",
  "aqs_quarterlysummary_by_county",
  "aqs_sampledata_by_county",
  "aqs_sites_by_county",
  "aqs_transactionsample_by_county"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_state_functions <- c(
  "aqs_annualsummary_by_state",
  "aqs_counties_by_state",
  "aqs_dailysummary_by_state",
  "aqs_monitors_by_state",
  "aqs_qa_annualperformanceeval_by_state",
  "aqs_qa_annualperformanceevaltransaction_by_state",
  "aqs_qa_blanks_by_state",
  "aqs_qa_collocated_assessments_by_state",
  "aqs_qa_flowrateaudit_by_state",
  "aqs_qa_flowrateverification_by_state",
  "aqs_qa_one_point_qc_by_state",
  "aqs_qa_pep_audit_by_state",
  "aqs_quarterlysummary_by_state",
  "aqs_sampledata_by_state",
  "aqs_transactionsample_by_state"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_pqao_functions <- c(
  "aqs_qa_annualperformanceeval_by_pqao",
  "aqs_qa_annualperformanceevaltransaction_by_pqao",
  "aqs_qa_blanks_by_pqao",
  "aqs_qa_collocated_assessments_by_pqao",
  "aqs_qa_flowrateaudit_by_pqao",
  "aqs_qa_flowrateverification_by_pqao",
  "aqs_qa_one_point_qc_by_pqao",
  "aqs_qa_pep_audit_by_pqao"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_MA_functions <- c(
  "aqs_qa_annualperformanceeval_by_MA",
  "aqs_qa_annualperformanceevaltransaction_by_MA",
  "aqs_qa_blanks_by_MA",
  "aqs_qa_collocated_assessments_by_MA",
  "aqs_qa_flowrateaudit_by_MA",
  "aqs_qa_flowrateverification_by_MA",
  "aqs_qa_one_point_qc_by_MA",
  "aqs_qa_pep_audit_by_MA",
  "aqs_transactionsample_by_MA"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_box_functions <- c(
  "aqs_annualsummary_by_box",
  "aqs_dailysummary_by_box",
  "aqs_monitors_by_box",
  "aqs_quarterlysummary_by_box",
  "aqs_sampledata_by_box"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_by_cbsa_functions <- c(
  "aqs_annualsummary_by_cbsa",
  "aqs_dailysummary_by_cbsa",
  "aqs_monitors_by_cbsa",
  "aqs_sampledata_by_cbsa",
  "aqs_quarterlysummary_by_cbsa"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_list_functions <- c(
  "aqs_cbsas",
  "aqs_classes",
  "aqs_mas",
  "aqs_parameters_by_class",
  "aqs_pqaos",
  "aqs_sampledurations",
  "aqs_states"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_signup_and_credentials_functions <- c(
  "aqs_credentials",
  "aqs_sign_up"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_metadata_functions <- c(
  "aqs_fields_by_service",
  "aqs_isavailable",
  "aqs_knownissues",
  "aqs_revisionhistory"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

RAQSAPI_misc_functions <- c(
  "aqs_removeheader"
) %>%
  sort() %>%
  tibble() %>%
  rename("FUN" = ".") %>%
  mutate(PATH=glue::glue("../man/{FUN}"))

ALL_EXPORTED_RAQSAPI_FUNCTIONS <- list(
  RAQSAPI_by_site_functions,
  RAQSAPI_by_county_functions,
  RAQSAPI_by_state_functions,
  RAQSAPI_by_pqao_functions,
  RAQSAPI_by_MA_functions,
  RAQSAPI_by_box_functions,
  RAQSAPI_by_cbsa_functions,
  RAQSAPI_list_functions,
  RAQSAPI_signup_and_credentials_functions,
  RAQSAPI_metadata_functions,
  RAQSAPI_misc_functions
)

names(ALL_EXPORTED_RAQSAPI_FUNCTIONS) <- c(
  "RAQSAPI_by_site_functions",
  "RAQSAPI_by_county_functions",
  "RAQSAPI_by_state_functions",
  "RAQSAPI_by_pqao_functions",
  "RAQSAPI_by_MA_functions",
  "RAQSAPI_by_box_functions",
  "RAQSAPI_by_cbsa_functions",
  "RAQSAPI_list_functions",
  "RAQSAPI_signup_and_credentials_functions",
  "RAQSAPI_metadata_functions",
  "RAQSAPI_misc_functions"
)

ALL_EXPORTED_RAQSAPI_FUNCTIONS %>% map(purrr::pluck, "FUN") %>% flatten()


all_fun_list <- ALL_EXPORTED_RAQSAPI_FUNCTIONS %>%
  purrr::list_rbind() %>%
  tibble::deframe() %>%
  sort()

ignoredfiles <- list(
  "aqs_services_by_site",
  "aqs_services_by_county",
  "deprecated",
  "RAQSAPI",
  "aqs_metadata_service"
)

all_manfiles <- list.files(path="./man", pattern="*.Rd", no..=TRUE) %>%
  stringr::str_replace_all(pattern = ".Rd", replacement = "") %>%
  stringr::str_remove_all("aqs_services_by_box") %>%
  stringr::str_remove_all("aqs_services_by_cbsa") %>%
  stringr::str_remove_all("aqs_services_by_MA") %>%
  stringr::str_remove_all("aqs_services_by_pqao") %>%
  stringr::str_remove_all("aqs_services_by_state") %>%
  stringr::str_remove_all("aqs_metadata_service") %>%
  stringr::str_remove_all("aqs_metadata_service") %>%
  stringr::str_remove_all("aqs_services_by_site") %>%
  stringr::str_remove_all("aqs_services_by_county") %>%
  stringr::str_remove_all("AQS_DATAMART_APIv2-S3-class") %>%
  stringr::str_remove_all("deprecated") %>%
  stringr::str_remove_all("RAQSAPI") %>%
  stringi::stri_omit_empty()


checkRAQSAPIfunctionlist <- waldo::compare(all_fun_list, all_manfiles)

RAQSAPI_functions_table <- function()
{
  setupfunctions <- list("aqs_credentials", "aqs_sign_up")
  listfunctions <- list("aqs_isavailable", "aqs_knownissues",
                        "aqs_counties_by_state", "aqs_sites_by_county",
                        "aqs_classes", "aqs_parameters_by_class", "aqs_mas",
                        "aqs_pqaos", "aqs_cbsas", "aqs_states",
                        "aqs_removeheader", "aqs_revisionhistory",
                        "aqs_sampledurations", "aqs_fields_by_service"
  )
  serviceshelperfunctions <- list("aqs_services_by_site",
                                  "aqs_services_by_county",
                                  "aqs_services_by_state",
                                  "aqs_services_by_MA",
                                  "aqs_services_by_pqao",
                                  "aqs_services_by_cbsa",
                                  "aqs_services_by_box"
  )

  functiontype <- function(functionname)
  {
    case_when(
      str_detect(string = functionname, pattern="by_site") ~
        "RAQSAPI aggregation by site aggregate functions",
      str_detect(string = functionname, pattern="by_county") ~
        "RAQSAPI aggregation by county aggregate functions",
      str_detect(string = functionname, pattern="by_state") ~
        "RAQSAPI aggregation by state aggregate functions",
      str_detect(string = functionname, pattern="by_cbsa") ~
        "RAQSAPI aggregation by cbsa aggregate functions",
      str_detect(string = functionname, pattern="by_pqao") ~
        "RAQSAPI aggregation by pqao aggregate functions",
      str_detect(string = functionname, pattern="by_box") ~
        "RAQSAPI aggregation by lat/long bounding box aggregate functions",
      str_detect(string = functionname, pattern="by_MA") ~
        "RAQSAPI aggregation by Monitoring Agency aggregate functions",
      .default = "misc"
    )
  }

  functiontable <- tibble(functionnames = list.files("./man/html") %>%
                            str_remove_all(pattern=".html"),
                          relPATH = list.files("./man/html",
                                               full.names=TRUE)) %>%
    mutate(functionfamily = functiontype(functionnames))
  functiontable$functionfamily[which(functiontable$functionnames %in%
                                       listfunctions)]
                                                     <- "RAQSAPI list functions"
  functiontable$functionfamily[which(functiontable$functionnames %in%
                                       setupfunctions)]
                                                    <- "RAQSAPI setup functions"
  functiontable$functionfamily[which(functiontable$functionnames %in%
                                       serviceshelperfunctions)]
                                                 <- "RAQSAPI services functions"

  functiontable %<>% filter(!functionfamily == "services functions") %>%
    filter(!functionnames == "deprecated")
    return(functiontable)
}

RAQSAPI_functions_list <- function()
{
  functiontable <- RAQSAPI_functions_table()
  functionlist <- functiontable %>% split(functiontable$functionfamily)
  return(functionlist)
}
