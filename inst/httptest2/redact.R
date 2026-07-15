function(resp) {
  user <- .RAQSAPI_env$aqs_username
  key <- .RAQSAPI_env$aqs_key

  if (!is.null(key) && nzchar(as.character(key))) {
    resp <- httptest2::gsub_response(resp, as.character(key),
                                     "redacted", fixed = TRUE)
  }
  if (!is.null(user) && nzchar(as.character(user))) {
    resp <- httptest2::gsub_response(resp, as.character(user),
                                     "redacted", fixed = TRUE)
  }

  # shorten all mock paths by abbreviating the base URL
  resp <- httptest2::gsub_response(resp,
                                   "https://aqs.epa.gov/data/api",
                                   "aqs",
                                   fixed = TRUE)
  # abbreviate long function, changes path in mocks
  resp <- httptest2::gsub_response(resp,
                                   "qaAnnualPerformanceEvaluations",
                                   "QAape",
                                   fixed = TRUE)
  # abbreviate long function, causes path in mocks
  resp <- httptest2::gsub_response(resp,
                                   "transactionsQaAnnualPerformanceEvaluations",
                                   "tQAape",
                                   fixed = TRUE)
  resp <- httptest2::gsub_response(resp,
                                   "quarterlyData",
                                   "QD",
                                   fixed = TRUE)

  return(resp)
}
