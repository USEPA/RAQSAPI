#' @title RAQSAPI_redactors
#' @description redacts sensitive information from httptest2 recorded HTTP responses
#' @param response character string representing the HTTP reponse to be redacted
#' @retruns character string with sensitive information redacted
#' @importFrom magrittr `%>%`()
#' @importFrom httptest2 gsub_response
#' @NoRd
#' @keywords internal
#' @examples This is an internal function that should only be called by package:httptest2 and is not intended
#'           to be called directly.
RAQSAPI_redactors <- function(response)
{
  require(magrittr, quietly = TRUE)
  response %>%
    httptest2::gsub_response(response = .,
                             pattern = getOption(x = "aqs_key",
                                                 default = "<redacted>"),
                             replacement = "<redacted>", fixed = TRUE) %>%
    httptest2::gsub_response(response = .,
                             pattern = getOption(x = "aqs_username",
                                                 default = "<redacted>"),
                             replacement = "<redacted>", fixed = TRUE) %>%
    #shorten mock path by removing the base URL
    httptest2::gsub_response(response = .,
                             pattern = "aqs.epa.gov/data/api/",
                             replacement = "datamart/",
                             fixed = TRUE) %>%
    #abbreviate long function causes path in mocks
    httptest2::gsub_response(response = .,
                             pattern = "qaAnnualPerformanceEvaluations",
                             replacement = "QAape",
                             fixed = TRUE) %>%
    #abbreviate long function causes path in mocks
    httptest2::gsub_response(response = .,
                             pattern = "transactionsQaAnnualPerformanceEvaluations",
                             replacement = "tQAape",
                             fixed = TRUE) %>%
    httptest2::gsub_response(response = .,
                             pattern = "quarterlyData",
                             replacement = "QD",
                             fixed = TRUE) %>%
    return()
}

httptest2::set_redactor(RAQSAPI_redactors)
