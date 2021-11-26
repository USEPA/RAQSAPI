#' @importFrom magrittr `%>%`()
test_that("helperfunctions (checkaqsparams()) functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"

# RAQSAPI::checkaqsparams(parameter = "abcdefg", date = 1995,
#                          stateFIPS = "90210", countycode = "341478",
#                          sitenum = "1", cbsa_code = "something",
#                          pqao_code = "9999", minlat = "3245253",
#                          maxlat = "647352", minlon = 45425252,
#                          maxlat = 463753415, ma_code = "MA",
#                          return_header = 1) %>%
#   expect_error()

RAQSAPI::checkaqsparams(parameter = "abcdefg") %>%
  expect_error()
RAQSAPI::checkaqsparams(bdate = 1995) %>%
  expect_error()
RAQSAPI::checkaqsparams(edate = 1995) %>%
  expect_error()
RAQSAPI::checkaqsparams(stateFIPS = "90210") %>%
  expect_error()
RAQSAPI::checkaqsparams(countycode = "341478") %>%
  expect_error()
RAQSAPI::checkaqsparams(sitenum = "1") %>%
  expect_error()
RAQSAPI::checkaqsparams(cbsa_code = "something") %>%
  expect_error()
RAQSAPI::checkaqsparams(pqao_code = "99999") %>%
  expect_error()
RAQSAPI::checkaqsparams(minlat = "3245253") %>%
  expect_error()
RAQSAPI::checkaqsparams(maxlat = "647352") %>%
  expect_error()
RAQSAPI::checkaqsparams(minlon = 45425252) %>%
  expect_error()
RAQSAPI::checkaqsparams(maxlon = 463753415) %>%
  expect_error()
RAQSAPI::checkaqsparams(MA_code = "MA") %>%
  expect_error()
RAQSAPI::checkaqsparams(return_header = 1) %>%
  expect_error()

})
