testthat::skip_on_cran()
testthat::skip_if_offline()
#library("magrittr")

#context("RAQSAPI by_county functions")
#' @importFrom magrittr `%>%`()
test_that("by_county functions", {
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
                           )

  aqs_annualsummary_by_county(parameter = "88101",
                              bdate = as.Date("20160101", format = "%Y%m%d"),
                              edate = as.Date("20160228", format = "%Y%m%d"),
                              stateFIPS = "37",
                              countycode = "183",
                              return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_dailysummary_by_county(parameter = "88101",
                             bdate = as.Date("20160101", format = "%Y%m%d"),
                             edate = as.Date("20160228", format = "%Y%m%d"),
                             stateFIPS = "37",
                             countycode = "183",
                             return_header = TRUE
                            )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_sampledata_by_county(parameter = "88101",
                                     bdate = as.Date("20160101",
                                                     format = "%Y%m%d"),
                                     edate = as.Date("20160101",
                                                     format = "%Y%m%d"),
                                     stateFIPS = "37",
                                     countycode = "183",
                                     return_header = TRUE
                                     )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_monitors_by_county(parameter = "42401",
                                 bdate = as.Date("20150501", format = "%Y%m%d"),
                                 edate = as.Date("20150502", format = "%Y%m%d"),
                                 stateFIPS = "15",
                                 countycode = "001",
                                 return_header = TRUE
                                 )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_collocated_assessments_by_county(parameter = "88101",
                                        bdate = as.Date("20130101",
                                                      format = "%Y%m%d"),
                                        edate = as.Date("20130131",
                                                      format = "%Y%m%d"),
                                        stateFIPS = "01",
                                        countycode = "089",
                                        return_header = TRUE
                                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_blanks_by_county(parameter = "88101",
                        bdate = as.Date("20180101", format = "%Y%m%d"),
                        edate = as.Date("20180131", format = "%Y%m%d"),
                        stateFIPS = "01",
                        countycode = "033",
                        return_header = TRUE
                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_flowrateaudit_by_county(parameter = "88101",
                               bdate = as.Date("20180101", format = "%Y%m%d"),
                               edate = as.Date("20180131", format = "%Y%m%d"),
                               stateFIPS = "01", countycode = "073",
                               return_header = TRUE
                               )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_flowrateverification_by_county(parameter = "88101",
                                                   bdate = as.Date("20180101",
                                                           format = "%Y%m%d"),
                                                   edate = as.Date("20180131",
                                                           format = "%Y%m%d"),
                                                   stateFIPS = "01",
                                                   countycode = "033",
                                                   return_header = TRUE
                                                   )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_one_point_qc_by_county(parameter = "44201",
                              bdate = as.Date("20180101", format = "%Y%m%d"),
                              edate = as.Date("20180131", format = "%Y%m%d"),
                              stateFIPS = "25",
                              countycode = "001",
                              return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_pep_audit_by_county(parameter = "88101",
                           bdate = as.Date("20170101", format = "%Y%m%d"),
                           edate = as.Date("20171231", format = "%Y%m%d"),
                           stateFIPS = "01",
                           countycode = "089",
                           return_header = TRUE
                           )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_transactionsample_by_county(parameter = "88101",
                                bdate = as.Date("20160228", format = "%Y%m%d"),
                                edate = as.Date("20160228", format = "%Y%m%d"),
                                stateFIPS = "37",
                                countycode = "183",
                                return_header = TRUE
                                )[[1]]$Header$status %>%
    expect_match(regexp = "Success")
})

#devtools::unload(package = "magrittr")
