#' @importFrom magrittr `%>%`()
test_that("by_PQAO functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
                           )

  aqs_pqaos(return_header = TRUE)$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_blanks_by_pqao(parameter = "88101",
                        bdate = as.Date("20180101", format = "%Y%m%d"),
                        edate = as.Date("20180131", format = "%Y%m%d"),
                        pqao_code = "0013",
                        return_header =  TRUE
                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_collocated_assessments_by_pqao(parameter = "88101",
                                        bdate = as.Date("20130101",
                                                      format = "%Y%m%d"),
                                        edate = as.Date("20130131",
                                                      format = "%Y%m%d"),
                                        pqao_code = "0013",
                                        return_header = TRUE
                                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_flowrateaudit_by_pqao(parameter = "88101",
                               bdate = as.Date("20180101", format = "%Y%m%d"),
                               edate = as.Date("20180131", format = "%Y%m%d"),
                               pqao_code = "0550",
                               return_header = TRUE
                               )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_flowrateverification_by_pqao(parameter = "88101",
                                      bdate = as.Date("20180101",
                                                    format = "%Y%m%d"),
                                      edate = as.Date("20180131",
                                                    format = "%Y%m%d"),
                                      pqao_code = "0013",
                                      return_header = TRUE
                                      )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_one_point_qc_by_pqao(parameter = "44201",
                              bdate = as.Date("20180101", format = "%Y%m%d"),
                              edate = as.Date("20180131", format = "%Y%m%d"),
                              pqao_code = "0660",
                              return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_pep_audit_by_pqao(parameter = "88101",
                           bdate = as.Date("20170601", format = "%Y%m%d"),
                           edate = as.Date("20170630", format = "%Y%m%d"),
                           pqao_code = "0013",
                           return_header = TRUE
                           )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_collocated_assessments_by_pqao(parameter = "88101",
                                        bdate = as.Date("20130101",
                                                      format = "%Y%m%d"),
                                        edate = as.Date("20130131",
                                                      format = "%Y%m%d"),
                                        pqao_code = "0013",
                                        return_header = TRUE
                                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_annualpeferomanceeval_by_pqao(parameter = "44201",
                                       bdate = as.Date("20170101",
                                                       format = "%Y%m%d"),
                                       edate = as.Date("20171231",
                                                       format = "%Y%m%d"),
                                       pqao_code = "0013",
                                       return_header = TRUE
                                       )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_annualperformanceevaltransaction_by_pqao(parameter = "44201",
                                                  bdate = as.Date("20170101",
                                                             format = "%Y%m%d"),
                                                  edate = as.Date("20171231",
                                                             format = "%Y%m%d"),
                                                  pqao_code = "0013",
                                                  return_header = TRUE
                                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

})
