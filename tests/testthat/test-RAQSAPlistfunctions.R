#' @importFrom magrittr `%>%`()
#' @import testthat
test_that("bybox functions", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # if(file.exists("./tests/testthat/local.R")) { source("./tests/testthat/local.R") }
  #
  # datamartAPI_user <- Sys.getenv(x = "RAQSAPIUSERNAME")
  # datamartAPI_key <- Sys.getenv(x = "RAQSAPIKEY")
  #
  # RAQSAPI::aqs_credentials(username = datamartAPI_user,
  #                          key = datamartAPI_key
  # )

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

})
