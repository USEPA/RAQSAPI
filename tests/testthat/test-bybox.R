#' @importFrom magrittr `%>%`()
#' @import testthat
test_that("bybox functions", {
testthat::skip_on_cran()
testthat::skip_if_offline()

print(paste0("inside of directory: ", getwd()))
if(file.exists("local.R"))
  {
    source("local.R")
    AQScredentials <- RAQSAPItestsetup_local()
    datamartAPI_user <- AQScredentials$AQSusername
    datamartAPI_key <- AQScredentials$AQSkey
    print("inside of if statement")
    print(paste(datamartAPI_user, datamartAPI_key), sep=", ")
  } else {
    print("inside of else statement")
   #datamartAPI_user <- Sys.getenv(x = "RAQSAPIUSERNAME")
   #datamartAPI_key <- Sys.getenv(x = "RAQSAPIKEY")
  }

print(paste(datamartAPI_user, datamartAPI_key), sep=", ")
  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = datamartAPI_key
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
                              bdate = as.Date("20150101", format = "%Y%m%d"),
                              edate = as.Date("20171231", format = "%Y%m%d"),
                              minlat = "33.3",
                              maxlat = "33.6",
                              minlon = "-87.0",
                              maxlon = "-86.7",
                              return_header = TRUE
                             )[[1]]$Header$status %>%
    expect_match(regexp = "Success")
})
