#' @importFrom magrittr `%>%`()
#' @import testthat
test_that("bypqao functions", {
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

  aqs_qa_annualperformanceeval_by_pqao(parameter = "44201",
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
