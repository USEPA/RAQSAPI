## ----RAQSAPIfun_all, echo = FALSE, comment = NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>" )
invisible(library(magrittr, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(stringr, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(tibble, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(glue, warn.conflicts = FALSE, quietly = TRUE))

#AQAD-33, using list.files works when knitting the vignette but does
#not seem to work when the vignette is knitted as part of a package.
#Instead function names need to be hard coded.
# RAQSAPI_functions <- list.files(path = "./man/", pattern = ".Rd$") %>%
# stringr::str_remove_all(pattern = ".Rd") %>%
# str_replace("ni_infix_operator", replacement = NA_character_) %>%
# str_replace("RAQSAPI", replacement = NA_character_) %>%
# str_replace_all("_services_", replacement = NA_character_) %>%
# na.omit() %>%
# as.character()
RAQSAPI_functions <- c(
"aqs_annualsummary_by_box",  
"aqs_annualsummary_by_cbsa",  
"aqs_annualsummary_by_county",  
"aqs_annualsummary_by_site",
"aqs_annualsummary_by_state",  
"aqs_cbsas",  
"aqs_classes",  
"aqs_counties_by_state",  
"aqs_credentials",  
"aqs_dailysummary_by_box",  
"aqs_dailysummary_by_cbsa",  
"aqs_dailysummary_by_county",  
"aqs_dailysummary_by_site",  
"aqs_dailysummary_by_state",  
"aqs_fields_by_service",  
"aqs_isavailable",  
"aqs_knownissues",  
"aqs_mas",  
"aqs_monitors_by_box",  
"aqs_monitors_by_cbsa",  
"aqs_monitors_by_county",  
"aqs_monitors_by_site",  
"aqs_monitors_by_state",  
"aqs_parameters_by_class",  
"aqs_pqaos",
"aqs_qa_annualperformanceeeval_by_county",
"aqs_qa_annualperformanceeval_by_MA",
"aqs_qa_annualperformanceeval_by_pqao",
"aqs_qa_annualperformanceeval_by_site",
"aqs_qa_annualperformanceeval_by_state",
"aqs_qa_annualperformanceevaltransaction_by_county",
"aqs_qa_annualperformanceevaltransaction_by_MA",
"aqs_qa_annualperformanceevaltransaction_by_pqao",
"aqs_qa_annualperformanceevaltransaction_by_site",
"aqs_qa_annualperformanceevaltransaction_by_state",
"aqs_qa_blanks_by_county",  
"aqs_qa_blanks_by_MA",  
"aqs_qa_blanks_by_pqao",  
"aqs_qa_blanks_by_site",  
"aqs_qa_blanks_by_state",  
"aqs_qa_collocated_assessments_by_county",  
"aqs_qa_collocated_assessments_by_MA",  
"aqs_qa_collocated_assessments_by_pqao",  
"aqs_qa_collocated_assessments_by_site",  
"aqs_qa_collocated_assessments_by_state",  
"aqs_qa_flowrateaudit_by_county",  
"aqs_qa_flowrateaudit_by_MA",  
"aqs_qa_flowrateaudit_by_pqao",  
"aqs_qa_flowrateaudit_by_site",  
"aqs_qa_flowrateaudit_by_state",  
"aqs_qa_flowrateverification_by_county",  
"aqs_qa_flowrateverification_by_MA",  
"aqs_qa_flowrateverification_by_pqao",  
"aqs_qa_flowrateverification_by_site",  
"aqs_qa_flowrateverification_by_state",  
"aqs_qa_one_point_qc_by_county",  
"aqs_qa_one_point_qc_by_MA",  
"aqs_qa_one_point_qc_by_pqao",  
"aqs_qa_one_point_qc_by_site",  
"aqs_qa_one_point_qc_by_state",  
"aqs_qa_pep_audit_by_county",  
"aqs_qa_pep_audit_by_MA",  
"aqs_qa_pep_audit_by_pqao",  
"aqs_qa_pep_audit_by_site",  
"aqs_qa_pep_audit_by_state",
"aqs_quarterlysummary_by_box",
"aqs_quarterlysummary_by_county",
"aqs_quarterlysummary_by_pqao",
"aqs_quarterlysummary_by_site",
"aqs_quarterlysummary_by_state",
"aqs_removeheader",
"aqs_revisionhistory",  
"aqs_sampledata_by_box",  
"aqs_sampledata_by_cbsa",  
"aqs_sampledata_by_county",  
"aqs_sampledata_by_site",  
"aqs_sampledata_by_state",  
"aqs_sampledurations",
"aqs_sign_up",  
"aqs_sites_by_county",  
"aqs_states",
"aqs_transactionsample_by_county",
"aqs_transactionsample_by_site",
"aqs_transactionsample_by_state",
"aqs_transactionsample_by_MA"
)

RAQSAPI_functions %>%
  cat(sep = "  \n")

## ----SIGNUPANDCREDENTIALS, echo = FALSE, comment = NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
signupandcredentials <- paste(".sign_up", ".credentials", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = signupandcredentials) %>%
  cat(sep = "  \n")

## ----METADATAFUNCTIONS, echo = FALSE, comment = NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metadatafunctions <- paste(".available",
                           ".fields_by_service",
                           ".knownissues", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = metadatafunctions) %>%
  cat(sep = "  \n")

## ----LISTFUNCTIONS, echo = FALSE, comment = NA---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
listfunctions <- paste(".states",
                       ".counties_by_state",
                       ".sites_by_county",
                       ".cbsas",
                       ".classes",
                       ".pqaos",
                       ".mas",
                       sep = '|'
                       )

str_subset(string = RAQSAPI_functions, pattern = listfunctions) %>%
  cat(sep = "  \n")

## ----_by_Sitefunctions, echo = FALSE, comment = NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_sitefunctions <- paste("_by_site", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_sitefunctions) %>%
  cat(sep = "  \n")

## ----_by_countyfuncions, echo = FALSE, comment = NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_countyfunctions <- paste("._by_county", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_countyfunctions) %>%
  cat(sep = "  \n")

## ----_by_STATEfunctions, echo = FALSE, comment = NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_STATEfunctions <- paste("._by_state", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_STATEfunctions) %>%
  cat(sep = "  \n")

## ----_by_MAfunctions, echo = FALSE, comment = NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_MAfunctions <- paste("._by_MA", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_MAfunctions) %>%
  cat(sep = "  \n")

## ----bycbsafunctions, echo = FALSE, comment = NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_cbsafunctions <- paste("._by_cbsa", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_cbsafunctions) %>%
  cat(sep = "  \n")

## ----_by_pqaofunctions, echo = FALSE, comment = NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_pqaofunctions <- paste("._by_pqao", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_pqaofunctions) %>%
  cat(sep = "  \n")

## ----_by_BOXfunctions, echo = FALSE, comment = NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_BOXfunctions <- paste("._by_box", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = by_BOXfunctions) %>%
  cat(sep = "  \n")

## ----misc, echo = FALSE, comment = NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
misc_functions <- paste("aqs_removeheader", sep = '|')

str_subset(string = RAQSAPI_functions, pattern = misc_functions) %>%
  cat(sep = "  \n")

