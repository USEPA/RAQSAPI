#' @importFrom magrittr `%>%`()
test_that("by_state functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
                           )

aqs_monitors_by_state(parameter = "88101",
                      bdate = as.Date("20170101", format = "%Y%m%d"),
                      edate = as.Date("20171231", format = "%Y%m%d"),
                      stateFIPS = "01",
                      return_header = TRUE
                      )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_sampledata_by_state(parameter = "45201",
                        bdate = as.Date("19950515", format = "%Y%m%d"),
                        edate = as.Date("19950515", format = "%Y%m%d"),
                        stateFIPS = "37",
                        return_header = TRUE
                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_annualsummary_by_state(parameter = "45201",
                                  bdate = as.Date("19950515",
                                                 format = "%Y%m%d"),
                                  edate = as.Date("19990515",
                                                 format = "%Y%m%d"),
                                  stateFIPS = "37",
                                  return_header = TRUE
                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_dailysummary_by_state(parameter = "45201",
                          bdate = as.Date("19950515", format = "%Y%m%d"),
                          edate = as.Date("19950515", format = "%Y%m%d"),
                          stateFIPS = "37",
                          return_header = TRUE
                          )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_blanks_by_state(parameter = "88101",
                                  bdate = as.Date("20180101",
                                                  format = "%Y%m%d"),
                                  edate = as.Date("20180131",
                                                  format = "%Y%m%d"),
                                  stateFIPS = "01",
                                  return_header = TRUE
                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_collocated_assessments_by_state(parameter = "88101",
                                       bdate = as.Date("20130101",
                                                     format = "%Y%m%d"),
                                       edate = as.Date("20130131",
                                                     format = "%Y%m%d"),
                                       stateFIPS = "01",
                                       return_header = TRUE
                                      )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_flowrateaudit_by_state(parameter = "88101",
                              bdate = as.Date("20180101", format = "%Y%m%d"),
                              edate = as.Date("20180131", format = "%Y%m%d"),
                              stateFIPS = "01",
                              return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_flowrateverification_by_state(parameter = "88101",
                                  bdate = as.Date("20170101",
                                                  format = "%Y%m%d"),
                                  edate = as.Date("20190131",
                                                  format = "%Y%m%d"),
                                  stateFIPS = "01",
                                  return_header = TRUE
                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_one_point_qc_by_state(parameter = "44201",
                             bdate = as.Date("20180101", format = "%Y%m%d"),
                             edate = as.Date("20180131", format = "%Y%m%d"),
                             stateFIPS = "25",
                             return_header = TRUE
                             )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_one_point_qc_by_state(parameter = "44201",
                             bdate = as.Date("20180101", format = "%Y%m%d"),
                             edate = as.Date("20180131", format = "%Y%m%d"),
                             stateFIPS = "25",
                             return_header = TRUE
                            )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_pep_audit_by_state(parameter = "88101",
                          bdate = as.Date("20170101", format = "%Y%m%d"),
                          edate = as.Date("20171231", format = "%Y%m%d"),
                          stateFIPS = "01",
                          return_header = TRUE
                          )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_transactionsample_by_state(parameter = "45201",
                               bdate = as.Date("19950515",
                                               format = "%Y%m%d"),
                               edate = as.Date("19950515",
                                               format = "%Y%m%d"),
                               stateFIPS = "37",
                               return_header = TRUE
                               )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

 aqs_qa_annualperformanceeval_by_state(parameter = "44201",
                                       bdate = as.Date("20170101",
                                                       format = "%Y%m%d"),
                                       edate = as.Date("20171231",
                                                       format = "%Y%m%d"),
                                       stateFIPS = "01",
                                       return_header = TRUE
                                       )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

 aqs_qa_annualperformanceevaltransaction_by_state(parameter = "44201",
                                                  bdate = as.Date("20170101",
                                                           format = "%Y%m%d"),
                                                  edate = as.Date("20171231",
                                                            format = "%Y%m%d"),
                                                  stateFIPS = "01",
                                                  return_header = TRUE
                                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

 aqs_quarterlysummary_by_state(parameter = "88101",
                                        bdate = as.Date("20160101",
                                                        format = "%Y%m%d"),
                                        edate = as.Date("20171231",
                                                        format = "%Y%m%d"),
                                        stateFIPS = "37",
                                        return_header = TRUE
                                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")


})
