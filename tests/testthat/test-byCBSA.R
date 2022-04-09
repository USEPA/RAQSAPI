#' @importFrom magrittr `%>%`()
test_that("by_cbsa functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
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
