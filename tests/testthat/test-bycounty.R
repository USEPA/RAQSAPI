#' @importFrom magrittr `%>%`()
#' @import testthat
test_that("bycounty functions", {
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
                                        bdate = as.Date("20150101",
                                                      format = "%Y%m%d"),
                                        edate = as.Date("20150131",
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

aqs_qa_annualperformanceeval_by_county(parameter = "44201",
                                       bdate = as.Date("20170101",
                                                       format = "%Y%m%d"),
                                       edate = as.Date("20171231",
                                                       format = "%Y%m%d"),
                                       stateFIPS = "01",
                                       countycode = "003",
                                       return_header = TRUE
                                       )[[1]]$Header$status %>%
      expect_match(regexp = "Success")

aqs_qa_annualperformanceevaltransaction_by_county(parameter = "44201",
                                                  bdate = as.Date("20170101",
                                                             format = "%Y%m%d"),
                                                  edate = as.Date("20171231",
                                                             format = "%Y%m%d"),
                                                  stateFIPS = "01",
                                                  countycode = "003",
                                                  return_header = TRUE
                                                 )[[1]]$Header$status %>%
      expect_match(regexp = "Success")

aqs_quarterlysummary_by_county(parameter = "88101",
                               bdate = as.Date("20160101", format = "%Y%m%d"),
                               edate = as.Date("20170228", format = "%Y%m%d"),
                               stateFIPS = "37",
                               countycode = "183",
                               return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")
})
