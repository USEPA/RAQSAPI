#' @importFrom dplyr pull
#' @importFrom magrittr `%>%`() `%T>%`()

#with_mock_api({
with_mock_dir("listfunctions", {
  test_that("list functions", {
    if (file.exists("local.R")) {
      source("helper.R")
      AQScredentials <- RAQSAPItestsetup_helper()
      datamartAPI_user <- AQScredentials$datamartAPI_user
      datamartAPI_key <- AQScredentials$datamartAPI_key
    } else {
      datamartAPI_user <- Sys.getenv("RAQSAPIUSERNAME", names = TRUE)
      datamartAPI_key <- Sys.getenv("RAQSAPIKEY", names = TRUE)
    }

    RAQSAPI::aqs_credentials(username = datamartAPI_user, key = datamartAPI_key)

    if (
      RAQSAPI:::invalid(datamartAPI_user) ||
        RAQSAPI:::invalid(datamartAPI_key) ||
        datamartAPI_key == "redacted" ||
        datamartAPI_user == "redacted"
    ) {
      stop("credentials not loaded in unit tests")
    }
    if (!exists(x = "RAQSAPItestsetup_helper", mode = "function")) {
      rlang::abort(message = "RAQSAPItestsetup_helper function not loaded during unit test")
    }

    aqs_isavailable() %>%
      dplyr::pull(status) %>%
      expect_match(regexp = "API service is up and running healthy", fixed = FALSE)

    aqs_fields_by_service(service = "list", return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_knownissues(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_revisionhistory(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_states(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_counties_by_state(stateFIPS = "40", return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_mas(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_pqaos(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_cbsas(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_classes(return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_sites_by_county(stateFIPS = "15", countycode = "001", return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_parameters_by_class(class = "CRITERIA", return_header = TRUE) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()
  })
})
