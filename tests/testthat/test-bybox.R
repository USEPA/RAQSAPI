#' @importFrom magrittr `%>%`()
test_that("bybox functions", {
testthat::skip_on_cran()
testthat::skip_if_offline()
server <- "AQSDatamartAPI"
datamartapi_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartapi_user,
                           key = "test"
                           )

  RAQSAPI::aqs_isavailable(return_header = TRUE)$Header$status %>%
  expect_match(regexp = "API service is up and running healthy",
               fixed = FALSE
               )

  aqs_sampledata_by_box(parameter = "44201",
                        bdate = as.Date("20150501", format = "%Y%m%d"),
                        edate = as.Date("20150502", format = "%Y%m%d"),
                        minlat = "33.3",
                        maxlat = "33.6",
                        minlon = "-87.0",
                        maxlon = "-86.7",
                        return_header = TRUE
                       )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_monitors_by_box(parameter = "44201",
                      bdate = as.Date("19950101", format = "%Y%m%d"),
                      edate = as.Date("19951231", format = "%Y%m%d"),
                      minlat = "33.3",
                      maxlat = "33.6",
                      minlon = "-87.0",
                      maxlon = "-86.7",
                      return_header = TRUE
                      )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_annualsummary_by_box(parameter = "44201",
                           bdate = as.Date("20150501", format = "%Y%m%d"),
                           edate = as.Date("20150502", format = "%Y%m%d"),
                           minlat = "33.3",
                           maxlat = "33.6",
                           minlon = "-87.0",
                           maxlon = "-86.7",
                           return_header = TRUE
                          )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_dailysummary_by_box(parameter = "44201",
                          bdate = as.Date("20150501", format = "%Y%m%d"),
                          edate = as.Date("20150502", format = "%Y%m%d"),
                          minlat = "33.3",
                          maxlat = "33.6",
                          minlon = "-87.0",
                          maxlon = "-86.7",
                          return_header = TRUE
                          )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_quarterlysummary_by_box(parameter = "44201",
                              bdate = as.Date("20150501", format = "%Y%m%d"),
                              edate = as.Date("20170502", format = "%Y%m%d"),
                              minlat = "33.3",
                              maxlat = "33.6",
                              minlon = "-87.0",
                              maxlon = "-86.7",
                              return_header = TRUE
                             )[[1]]$Header$status %>%
    expect_match(regexp = "Success")
})
