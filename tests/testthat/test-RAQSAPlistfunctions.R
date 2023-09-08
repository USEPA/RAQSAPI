#' @importFrom magrittr `%>%`()
#' @import testthat
#' @importFrom magrittr `%>%`()
#' @import testthat

test_that("test list functions", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  if(file.exists("local.R"))
  {
    source("helper.R")
    AQScredentials <- RAQSAPItestsetup_helper()
    datamartAPI_user <- AQScredentials$datamartAPI_user
    datamartAPI_key <- AQScredentials$datamartAPI_key
  } else {
    datamartAPI_user <- Sys.getenv("RAQSAPIKEY", names = TRUE)
    datamartAPI_key <- Sys.getenv("RAQSAPIUSERNAME", names = TRUE)
  }
  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = datamartAPI_key
  )

  aqs_isavailable(return_header = TRUE)$Header$status %>%
  expect_match(regexp = "API service is up and running healthy",
               fixed = FALSE
               )

  aqs_fields_by_service(service = "list",
                        return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_knownissues(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_revisionhistory(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_states(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_counties_by_state(stateFIPS = "40",
                        return_header = TRUE
                        )$Header$status %>%
    expect_match(regexp = "Success")

  aqs_pqaos(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_cbsas(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_classes(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sites_by_county(stateFIPS = "15",
                    countycode = "001",
                    return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_parameters_by_class(class = "CRITERIA",
                          return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  #reuse test for aqs_mas() for testing of aqs_removeheader() without
  # calling the API again
  mas <- aqs_mas(return_header = TRUE)

  mas$Header$status %>%
    expect_match(regexp = "Success")

  mas %>%
    aqs_removeheader() %>%
    testthat::expect_s3_class(class=c("tbl_df", "tbl", "data.frame"))

})
