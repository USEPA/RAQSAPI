#' @importFrom magrittr `%>%`()
#' @import testthat
#' @import httptest2
if (file.exists("local.R"))
{
  source("helper.R")
  AQScredentials <- RAQSAPItestsetup_helper()
  datamartAPI_user <- AQScredentials$datamartAPI_user
  datamartAPI_key <- AQScredentials$datamartAPI_key
} else
  {
    datamartAPI_user <- Sys.getenv("RAQSAPIUSERNAME", names = TRUE)
    datamartAPI_key <- Sys.getenv("RAQSAPIKEY", names = TRUE)
  }
RAQSAPI::aqs_credentials(username = datamartAPI_user, key = datamartAPI_key)

with_mock_dir("bycbsa",{
test_that(
  "byCBSA functions", {

    aqs_monitors_by_cbsa(
      parameter = "42602",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20170102", format = "%Y%m%d"),
      cbsa_code = "16740",
      return_header = TRUE
    ) %>%
      expect_no_error()

    aqs_annualsummary_by_cbsa(
      parameter = "42602",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20170101", format = "%Y%m%d"),
      cbsa_code = "16740", return_header = FALSE
    ) %>%
      expect_no_error()

    aqs_dailysummary_by_cbsa(
      parameter = "42602",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20170101", format = "%Y%m%d"),
      cbsa_code = "16740",
      return_header = FALSE
    ) %>%
      expect_no_error()

    aqs_sampledata_by_cbsa(
      parameter = "42602",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20170101", format = "%Y%m%d"),
      cbsa_code = "16740",
      return_header = FALSE
    ) %>%
      expect_no_error()

    aqs_quarterlysummary_by_cbsa(
      parameter = "42602",
      bdate = as.Date("20170101", format = "%Y%m%d"),
      edate = as.Date("20171231", format = "%Y%m%d"),
      cbsa_code = "16740",
      return_header = FALSE
    ) %>%
      expect_no_error()

    aqs_sampledurations(return_header = FALSE) %>%
      expect_no_error()
  }
  )
  }
)
