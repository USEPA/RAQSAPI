#' @importFrom magrittr `%>%`()
#' @importFrom stringr str_detect

#Tests in this file do not require AQS Datamart credentials
test_that("helperfunctions (checkaqsparams()) functions", {
  RAQSAPI:::checkaqsparams("hi") %>% #checkaqsparams function does not accept unnamed/positional arguments
    expect_error()
  RAQSAPI:::checkaqsparams(service = 99999) %>%
    expect_error()
  RAQSAPI:::checkaqsparams(service = "notanactualservice") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(parameter = "12") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(parameter = "abcdefg") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(bdate = "notadate") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(edate = "notadate") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(cbdate = "notadate") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(cedate = "notadate") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(stateFIPS = "90210") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(countycode = "341478") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(sitenum = "1") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(cbsa_code = "something") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(pqao_code = "99999") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(minlat = "3245253") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(maxlat = "647352") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(minlon = 45425252) %>%
    expect_error()
  RAQSAPI:::checkaqsparams(maxlon = 463753415) %>%
    expect_error()
  RAQSAPI:::checkaqsparams(MA_code = "MA") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(return_header = 1) %>%
    expect_error()
  RAQSAPI:::checkaqsparams(POC = "POC") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(email = "not a valid email") %>%
    expect_error()
  RAQSAPI:::checkaqsparams(duration = "not a valid duration") %>%
    expect_error()
  RAQSAPI:::format_variables_for_api(x = list()) %>%
    expect_identical("")
  RAQSAPI:::format_multiple_params_for_api(x = list()) %>%
    expect_identical("")
  RAQSAPI:::aqsmultiyearparams(
    parameter = "99999",
    bdate = as.Date("2000-01-01", format = "%Y-%m-%d"),
    edate = as.Date("1999-01-01", format = "%Y-%m-%d"),
    service = "not a service"
  ) %>%
    expect_error()
  RAQSAPI:::format_variables_for_api(x = list()) %>%
    expect_identical("")
})
