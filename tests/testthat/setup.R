library(httptest2)
source("helper.R")

# Match requests with and without local credentials against the same fixtures.
redact_credentials <- httptest2:::get_current_redactor()
httptest2::set_redactor(function(request) {
  request <- redact_credentials(request)
  httptest2::gsub_response(
    request,
    "email=redacted&key=redacted&",
    "",
    fixed = TRUE
  )
})

mock_roots <- c("bybox", "bycbsa", "byco", "byMA", "bypqao", "bysite", "bystate", "listfunctions")
for (mock_root in mock_roots) {
  hashed_mocks <- list.files(
    mock_root,
    pattern = "-2ec5e9\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  for (mock_file in hashed_mocks) {
    alias <- sub("-2ec5e9\\.json$", ".json", mock_file)
    if (!file.exists(alias)) {
      file.copy(mock_file, alias)
    }
  }
}

httptest2::.mockPaths(c(
  ".",
  "./bybox",
  "./bycbsa",
  "./byco",
  "./byMA",
  "./bypqao",
  "./bysite",
  "./bystate",
  "./listfunctions"
))
