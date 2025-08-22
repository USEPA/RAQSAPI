# RAQSAPIredactors <- function(response)
# {
#   require(magrittr, quietly=TRUE)
#   response %>%
#     gsub_response(getOption("aqs_key"), "<redacted>", fixed = TRUE) %>%
#     gsub_response(getOption("aqs_username"), "<redacted>", fixed = TRUE)
# }


set_redactor(~ gsub_response(.,getOption(x="aqs_key", default=""), "<redacted>", fixed = TRUE) %>%
               gsub_response(.,getOption(x="aqs_username", default=""), "<redacted>", fixed = TRUE) %>%
               #shorten mock path by removing the base URL
               gsub_response(., "aqs.epa.gov/data/api/", "datamart/", fixed = TRUE) %>%
               #abbreviate long function causes path in mocks
               gsub_response(., "qaAnnualPerformanceEvaluations", "QAape", fixed = TRUE) %>%
               #abbreviate long function causes path in mocks
               gsub_response(., "transactionsQaAnnualPerformanceEvaluations", "tQAape", fixed = TRUE) %>%
               gsub_response(., "quarterlyData", "QD", fixed = TRUE)
             )
