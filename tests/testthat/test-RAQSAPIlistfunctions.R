#' @importFrom magrittr `%>%`()
#' @importfrom stringr str_detect
test_that("RAQSAPIlistfunctions ", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
                           )

  aqs_isavailable(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "API service is up and running healthy.")

  aqs_knownissues(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_counties_by_state(stateFIPS = "37",
                        return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sites_by_county(stateFIPS = "15",
                      countycode = "001",
                      return_header = TRUE)$Header$status %>%
       expect_match(regexp = "Success")

  aqs_classes(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_parameters_by_class(class = "CRITERIA",
                          return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_mas(return_header = TRUE)$Header$status %>%
     expect_match(regexp = "Success")

  aqs_pqaos(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_cbsas(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_states(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_revisionhistory(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_fields_by_service(service = "sampleData",
                        return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sampledurations(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sampledurations(return_header = TRUE) %>%
    aqs_removeheader() %>%
    expect_s3_class(class = "data.frame")
})
