test_that("bybox functions", {
  vcr::local_cassette("bybox")

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
