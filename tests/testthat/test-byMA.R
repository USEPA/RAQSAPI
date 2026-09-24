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

with_mock_dir("byMA", {
  test_that("byMA functions", {
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

    aqs_qa_blanks_by_MA(
      parameter = "88101",
      bdate = as.Date("20180101", format = "%Y%m%d"),
      edate = as.Date("20180131", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_collocated_assessments_by_MA(
      parameter = "88101",
      bdate = as.Date("20130101", format = "%Y%m%d"),
      edate = as.Date("20130131", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_one_point_qc_by_MA(
      parameter = "44201",
      bdate = as.Date("20180101", format = "%Y%m%d"),
      edate = as.Date("20180131", format = "%Y%m%d"),
      MA_code = "0660",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_flowrateaudit_by_MA(
      parameter = "88101",
      bdate = as.Date("20180101", format = "%Y%m%d"),
      edate = as.Date("20180131", format = "%Y%m%d"),
      MA_code = "0550",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_flowrateverification_by_MA(
      parameter = "88101",
      bdate = as.Date("20130101", format = "%Y%m%d"),
      edate = as.Date("20130131", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_pep_audit_by_MA(
      parameter = "88101",
      bdate = as.Date("20170601", format = "%Y%m%d"),
      edate = as.Date("20170630", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_transactionsample_by_MA(
      parameter = "44201",
      bdate = as.Date("20150515", format = "%Y%m%d"),
      edate = as.Date("20150515", format = "%Y%m%d"),
      MA_code = "0972",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_annualperformanceeval_by_MA(
      parameter = "44201",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20171231", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()

    aqs_qa_annualperformanceevaltransaction_by_MA(
      parameter = "44201",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20171231", format = "%Y%m%d"),
      MA_code = "0013",
      return_header = TRUE
    ) %T>%
      expect_no_error() %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator()
  })
})
