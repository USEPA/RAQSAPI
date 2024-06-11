#' @importFrom magrittr `%>%`()
#' @import testthat
test_that("byCBSA functions", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

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

  aqs_monitors_by_cbsa(parameter = "42602",
                       bdate = as.Date("20170101", format = "%Y%m%d"),
                       edate = as.Date("20170102", format = "%Y%m%d"),
                       cbsa_code = "16740",
                       return_header = TRUE
                       )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_annualsummary_by_cbsa(parameter = "42602",
                            bdate = as.Date("20170101", format = "%Y%m%d"),
                            edate = as.Date("20170101", format = "%Y%m%d"),
                            cbsa_code = "16740",
                            return_header = TRUE
                            )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_dailysummary_by_cbsa(parameter = "42602",
                           bdate = as.Date("20170101", format = "%Y%m%d"),
                           edate = as.Date("20170101", format = "%Y%m%d"),
                           cbsa_code = "16740",
                           return_header = TRUE
                           )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sampledata_by_cbsa(parameter = "42602",
                         bdate = as.Date("20170101", format = "%Y%m%d"),
                         edate = as.Date("20170101", format = "%Y%m%d"),
                         cbsa_code = "16740",
                         return_header = TRUE
                         )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_quarterlysummary_by_cbsa(parameter = "42602",
                               bdate = as.Date("20170101", format = "%Y%m%d"),
                               edate = as.Date("20171231", format = "%Y%m%d"),
                               cbsa_code = "16740",
                               return_header = TRUE
                               )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_sampledurations(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")
})
