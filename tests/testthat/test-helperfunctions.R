#' @importFrom magrittr `%>%`()
#' @importFrom stringr str_detect
#' @import testthat
test_that("helperfunctions (checkaqsparams()) functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"

if(file.exists("local.R"))
{
  source("helper.R")
  AQScredentials <- RAQSAPItestsetup_helper()
  datamartAPI_user <- AQScredentials$datamartAPI_user
  datamartAPI_key <- AQScredentials$datamartAPI_key
} else {
  datamartAPI_user <- Sys.getenv("RAQSAPIUSERNAME", names = TRUE)
  datamartAPI_key <- Sys.getenv("RAQSAPIKEY", names = TRUE)
}
RAQSAPI::aqs_credentials(username = datamartAPI_user,
                         key = datamartAPI_key
)

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
  expect_equal("")
RAQSAPI:::format_multiple_params_for_api(x = list()) %>%
  expect_equal("")
RAQSAPI:::aqsmultiyearparams(parameter = "99999",
                             bdate = as.Date("2000-01-01", format = "%Y-%m-%d"),
                             edate = as.Date("1999-01-01", format = "%Y-%m-%d"),
                             service = "not a service"
                             ) %>%
  expect_error()
RAQSAPI:::format_variables_for_api(x = list()) %>% expect_equal("")
})
