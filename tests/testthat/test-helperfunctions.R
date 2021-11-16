#' @importFrom magrittr `%>%`()
test_that("helperfunctions functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"

RAQSAPI::checkaqsparams(parameter = "abcdefg", date = 1995,
                         stateFIPS = "90210", countycode = "341478",
                         sitenum = "1", cbsa_code = "something",
                         pqao_code = "9999", minlat = "3245253",
                         maxlat = "647352", minlon = 45425252,
                         maxlat = 463753415, ma_code = "MA",
                         return_header = 1) %>%
  expect_error()
})
