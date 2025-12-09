## ----RAQSAPIfun_all, echo = FALSE, comment = NA---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
invisible(library(magrittr, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(stringr, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(tibble, warn.conflicts = FALSE, quietly = TRUE))
invisible(library(glue, warn.conflicts = FALSE, quietly = TRUE))

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
  "aqs_qa_annualperformanceeval_by_county",
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
  ```
  
  RAQSAPI functions are named according to the service and filter variables that
  are available by the AQS Data Mart API.^[See
  (https://aqs.epa.gov/aqsweb/documents/data_api.html) for full details of the
  Data Mart API]
  
  # Variable descriptions and usage.
  These are all the available variables that can be used with various functions
  exported from the RAQSAPI library listed alphabetically. Not all of these
  variables are used with every function, and not all of these parameters are
  required. See the
  [RAQSAPI functional families](#RAQSAPI functional families) section to
  see which parameters are used with each function.
  
  * AQSobject: a R S3 object that is returned from RAQSAPI aggregate functions
  where return_header is TRUE. An AQS_Data_Mart_APIv2 is a 2 item
  named list in which the first item (\$Header) is a tibble of
  header information from the AQS API and the second item (\$Data)
  is a tibble of the data returned.
  
  * bdate: a R date object which represents the begin date of the data selection.
  Only data on or after this date will be returned.
  
  * cbdate (optional): a R date object which represents the "beginning date of
  last change" that indicates when the data was last
  updated. cbdate is used to filter data based on the
  change date. Only data that changed on or after this date
  will be returned. This is an optional variable which
  defaults to NA_Date. 
  
  * cedate (optional): a R date object which represents the "end date of last
  change" that indicates when the data was last updated.
  cedate is used to filter data based on the change date.
  Only data that changed on or before this date will be
  returned. This is an optional variable which defaults to
  NA_Date.
  
  * countycode: a R character object which represents the 3 digit state FIPS code
  for the county being requested (with leading zero(s)). Refer to
  [aqs_counties_by_state()] for a table of available county
  codes for each state.
  
  * duration (optional): a R character string that represents the parameter
  duration code that limits returned data to a specific
  sample duration. The default value of NA_character_
  will result in no filtering based on duration code.
  Valid durations include actual sample
  durations and not calculated durations such as 8 hour
  CO or O${_3}$ rolling averages, 3/6 day PM averages or
  Pb 3 month rolling averages. Refer to
  [aqs_sampledurations()] for a table of all available
  duration codes.
  
  * edate: a R date object which represents the end date of the data selection.
  Only data on or before this date will be returned.
  
  * email: a R character object which represents the email account that will be
  used to register with the AQS API or change an existing users key. A
  verification email will be sent to the account specified.
  
  * key: the key used in conjunction with the username given to connect to AQS
  Data Mart.
  
  * MA_code: a R character object which represents the 4 digit AQS Monitoring
  Agency code (with leading zeroes).
  
  * maxlat: a R character object that represents the maximum latitude of a
  geographic box. Decimal latitude with north being positive. Only
  data south of this latitude will be returned.
  
  * maxlon: a R character object which represents the maximum longitude of a
  geographic box. Decimal longitude with east being positive. Only
  data west of this longitude will be returned. Note that -80 is less
  than -70.
  
  * minlat: a R character object which represents the minimum latitude of a
  geographic box. Decimal latitude with north being positive.
  Only data north of this latitude will be returned.
  
  * minlon: a R character object which represents the minimum longitude of a
  geographic box. Decimal longitude with east being positive. Only
  data east of this longitude will be returned.
  
  * parameter: a R character list or single character object which represents
  the parameter code of the air pollutant related to the data
  being requested.
  
  * return_header: If FALSE (default) only returns data requested. If TRUE
  returns an AQSAPI_v2 object which is a two item list that
  contains header information returned from the API server
  mostly used for debugging purposes in addition to the
  data requested.
  
  * service a string which represents the services provided by the AQS
  API. For a list of available services refer to
  https://aqs.epa.gov/aqsweb/documents/data_api.html#services
  for the complete listing of services available through the
  Datamart API
  
  * sitenum: a R character object which represents the 4 digit site number (with
  leading zeros) within the county and state being requested.
  
  * stateFIPS: a R character object which represents the 2 digit state FIPS code
  (with leading zero) for the state being requested.
  
  * pqao_code: a R character object which represents the 4 digit AQS Primary
  Quality Assurance Organization code (with leading zeroes).
  
  * username: a R character object which represents the email account that will
  be used to connect to the AQS API.
  
  <a name="RAQSAPI families of functions"> </a>
  
  # RAQSAPI functional families
  ## Sign up and credentials
  The functions included in this family of functions are:
  
  ```{r SIGNUPANDCREDENTIALS, echo = FALSE, comment = NA}
signupandcredentials <- paste(".sign_up", ".credentials", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = signupandcredentials) %>%
  cat(sep = "  \n")


## ----METADATAFUNCTIONS, echo = FALSE, comment = NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metadatafunctions <- paste(".available",
                           ".fields_by_service",
                           ".knownissues", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = metadatafunctions) %>%
  cat(sep = "  \n")


## ----LISTFUNCTIONS, echo = FALSE, comment = NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
listfunctions <- paste(".states",
                       ".counties_by_state",
                       ".sites_by_county",
                       ".cbsas",
                       ".classes",
                       ".pqaos",
                       ".mas",
                       sep = "|"
                         )

str_subset(string = RAQSAPI_functions, pattern = listfunctions) %>%
  cat(sep = "  \n")


## ----_by_Sitefunctions, echo = FALSE, comment = NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_sitefunctions <- paste("_by_site", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_sitefunctions) %>%
  cat(sep = "  \n")


## ----_by_countyfuncions, echo = FALSE, comment = NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_countyfunctions <- paste("._by_county", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_countyfunctions) %>%
  cat(sep = "  \n")


## ----_by_STATEfunctions, echo = FALSE, comment = NA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_STATEfunctions <- paste("._by_state", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_STATEfunctions) %>%
  cat(sep = "  \n")


## ----_by_MAfunctions, echo = FALSE, comment = NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_MAfunctions <- paste("._by_MA", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_MAfunctions) %>%
  cat(sep = "  \n")


## ----bycbsafunctions, echo = FALSE, comment = NA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_cbsafunctions <- paste("._by_cbsa", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_cbsafunctions) %>%
  cat(sep = "  \n")


## ----_by_pqaofunctions, echo = FALSE, comment = NA------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_pqaofunctions <- paste("._by_pqao", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_pqaofunctions) %>%
  cat(sep = "  \n")


## ----_by_BOXfunctions, echo = FALSE, comment = NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
by_BOXfunctions <- paste("._by_box", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = by_BOXfunctions) %>%
  cat(sep = "  \n")


## ----misc, echo = FALSE, comment = NA-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
misc_functions <- paste("aqs_removeheader", sep = "|")

str_subset(string = RAQSAPI_functions, pattern = misc_functions) %>%
  cat(sep = "  \n")

