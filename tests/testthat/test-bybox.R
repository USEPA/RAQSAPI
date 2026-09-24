#' @importFrom magrittr `%>%`() `%T>%`()
if (file.exists("local.R")) {
  AQScredentials <- RAQSAPItestsetup_helper()
  datamartAPI_user <- AQScredentials$datamartAPI_user
  datamartAPI_key <- AQScredentials$datamartAPI_key
} else {
  datamartAPI_user <- Sys.getenv("RAQSAPIUSERNAME", names = TRUE)
  datamartAPI_key <- Sys.getenv("RAQSAPIKEY", names = TRUE)
}

RAQSAPI::aqs_credentials(username = datamartAPI_user, key = datamartAPI_key)

with_mock_dir("bybox", {
  test_that("bybox functions", {
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

    aqs_sampledata_by_box(
      parameter = "44201",
      bdate = as.Date("20150501", format = "%Y%m%d"),
      edate = as.Date("20150502", format = "%Y%m%d"),
      minlat = "33.3",
      maxlat = "33.6",
      minlon = "-87.0",
      maxlon = "-86.7",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_monitors_by_box(
      parameter = "44201",
      bdate = as.Date("19950101", format = "%Y%m%d"),
      edate = as.Date("19951231", format = "%Y%m%d"),
      minlat = "33.3",
      maxlat = "33.6",
      minlon = "-87.0",
      maxlon = "-86.7",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_annualsummary_by_box(
      parameter = "44201",
      bdate = as.Date("20150501", format = "%Y%m%d"),
      edate = as.Date("20150502", format = "%Y%m%d"),
      minlat = "33.3",
      maxlat = "33.6",
      minlon = "-87.0",
      maxlon = "-86.7",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_dailysummary_by_box(
      parameter = "44201",
      bdate = as.Date("20150501", format = "%Y%m%d"),
      edate = as.Date("20150502", format = "%Y%m%d"),
      minlat = "33.3",
      maxlat = "33.6",
      minlon = "-87.0",
      maxlon = "-86.7",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_quarterlysummary_by_box(
      parameter = "44201",
      bdate = as.Date("20150101", format = "%Y%m%d"),
      edate = as.Date("20171231", format = "%Y%m%d"),
      minlat = "33.3",
      maxlat = "33.6",
      minlon = "-87.0",
      maxlon = "-86.7",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()
  })
})
