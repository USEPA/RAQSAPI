function(resp) {
  user <- paste0("?email=", .RAQSAPI_env$aqs_username)
  key <- paste0("&key=", .RAQSAPI_env$aqs_key)

  if (!is.null(key) && nzchar(as.character(key))) {
    resp <- httptest2::gsub_response(resp, as.character(key),
                                     "",
                                     fixed = TRUE)
  }
  if (!is.null(user) && nzchar(as.character(user))) {
    resp <- httptest2::gsub_response(resp, as.character(user),
                                     "",
                                     fixed = TRUE)
  }

  # shorten all mock paths by abbreviating the base URL
  resp <- httptest2::gsub_response(resp, "https://aqs.epa.gov/data/api",
                                   "aqs",
                                   fixed = TRUE)
  # abbreviate long long API paths, shortens path in mocks
  resp <- httptest2::gsub_response(resp,
                                   "qaAnnualPerformanceEvaluations",
                                   "QAape",
                                   fixed = TRUE)
  # abbreviate long long API paths, shortens path in mocks
  resp <- httptest2::gsub_response(resp,
                                   "transactionsQaAnnualPerformanceEvaluations",
                                   "tQAape",
                                   fixed = TRUE)
  # abbreviate long long API paths, shortens path in mocks
  resp <- httptest2::gsub_response(resp,
                                   "quarterlyData",
                                   "QD",
                                   fixed = TRUE)

  return(resp)
}
