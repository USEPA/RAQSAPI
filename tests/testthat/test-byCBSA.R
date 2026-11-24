test_that("byCBSA functions", {
  vcr::local_cassette("cbsa")
  aqs_monitors_by_cbsa(
    parameter = "42602",
    bdate = as.Date("20170101", format = "%Y%m%d"),
    edate = as.Date("20170102", format = "%Y%m%d"),
    cbsa_code = "16740",
    return_header = TRUE
  ) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()

  aqs_annualsummary_by_cbsa(
    parameter = "42602",
    bdate = as.Date("20170101", format = "%Y%m%d"),
    edate = as.Date("20170101", format = "%Y%m%d"),
    cbsa_code = "16740",
    return_header = TRUE
  ) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()

  aqs_dailysummary_by_cbsa(
    parameter = "42602",
    bdate = as.Date("20170101", format = "%Y%m%d"),
    edate = as.Date("20170101", format = "%Y%m%d"),
    cbsa_code = "16740",
    return_header = TRUE
  ) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()

  aqs_sampledata_by_cbsa(
    parameter = "42602",
    bdate = as.Date("20170101", format = "%Y%m%d"),
    edate = as.Date("20170101", format = "%Y%m%d"),
    cbsa_code = "16740",
    return_header = TRUE
  ) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()

  aqs_quarterlysummary_by_cbsa(
    parameter = "42602",
    bdate = as.Date("20170101", format = "%Y%m%d"),
    edate = as.Date("20171231", format = "%Y%m%d"),
    cbsa_code = "16740",
    return_header = TRUE
  ) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()

  aqs_sampledurations(return_header = TRUE) %T>%
    expect_no_error() %>%
    RAQSAPI:::AQS_DATAMART_APIv2_validator()
})
