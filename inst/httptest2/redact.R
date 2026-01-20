function (resp) {
  key <- getOption("aqs_key")
  user <- getOption("aqs_username")

  if (!is.null(key) && nzchar(as.character(key))) {
    resp <- httptest2::gsub_response(resp, as.character(key), "redacted", fixed = TRUE)
  }
  if (!is.null(user) && nzchar(as.character(user))) {
    resp <- httptest2::gsub_response(resp, as.character(user), "redacted", fixed = TRUE)
  }

  # shorten mock path by removing the base URL
  #resp <- httptest2::gsub_response(resp, "aqs.epa.gov/data/api/", "datamart/", fixed = TRUE)
  # abbreviate long function causes path in mocks
  resp <- httptest2::gsub_response(resp, "qaAnnualPerformanceEvaluations", "QAape", fixed = TRUE)
  # abbreviate long function causes path in mocks
  resp <- httptest2::gsub_response(resp, "transactionsQaAnnualPerformanceEvaluations", "tQAape", fixed = TRUE)
  resp <- httptest2::gsub_response(resp, "quarterlyData", "QD", fixed = TRUE)

  return(resp)
}

