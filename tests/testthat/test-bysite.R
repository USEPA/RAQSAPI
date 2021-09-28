#' @importFrom magrittr `%>%`()
test_that("by_site functions", {
testthat::skip_if_offline()
testthat::skip_on_cran()
server <- "AQSDatamartAPI"
datamartAPI_user <- "test@aqs.api"

  RAQSAPI::aqs_credentials(username = datamartAPI_user,
                           key = "test"
                           )

  RAQSAPI::aqs_isavailable(return_header = TRUE)$Header$status %>%
  expect_match(regexp = "API service is up and running healthy",
               fixed = FALSE
               )

  RAQSAPI::aqs_sampledata_by_site(parameter = "44201",
                                  bdate = as.Date("20170618",
                                                  format = "%Y%m%d"),
                                  edate = as.Date("20170618",
                                                  format = "%Y%m%d"),
                                  stateFIPS = "37",
                                  countycode = "183",
                                  sitenum = "0014",
                                  return_header = TRUE
                                  )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_annualsummary_by_site(parameter = "44201",
                            bdate = as.Date("20170618", format = "%Y%m%d"),
                            edate = as.Date("20170618", format = "%Y%m%d"),
                            stateFIPS = "37",
                            countycode = "183",
                            sitenum = "0014",
                            return_header = TRUE
                            )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_dailysummary_by_site(parameter = "44201",
                           bdate = as.Date("20170618", format = "%Y%m%d"),
                           edate = as.Date("20170618", format = "%Y%m%d"),
                           stateFIPS = "37",
                           countycode = "183",
                           sitenum = "0014",
                           return_header = TRUE
                          )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_blanks_by_site(parameter = "88101",
                        bdate = as.Date("20180101", format = "%Y%m%d"),
                        edate = as.Date("20180131", format = "%Y%m%d"),
                        stateFIPS = "01",
                        countycode = "033",
                        sitenum = "1002",
                        return_header = TRUE
                        )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_monitors_by_site(parameter = "42401",
                       bdate = as.Date("20150501", format = "%Y%m%d"),
                       edate = as.Date("20150502", format = "%Y%m%d"),
                       stateFIPS = "15",
                       countycode = "001",
                       sitenum = "0007",
                       return_header = TRUE
                      )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_collocated_assessments_by_site(parameter = "88101",
                                        bdate = as.Date("20130101",
                                                        format = "%Y%m%d"),
                                        edate = as.Date("20130131",
                                                        format = "%Y%m%d"),
                                        stateFIPS = "01",
                                        countycode = "089",
                                        sitenum = "0014",
                                        return_header = TRUE
                                       )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_flowrateaudit_by_site(parameter = "88101",
                                          bdate = as.Date("20180101",
                                                          format = "%Y%m%d"),
                                           edate = as.Date("20180131",
                                                           format = "%Y%m%d"),
                                           stateFIPS = "01",
                                           countycode = "073",
                                           sitenum = "2003",
                                           return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

  aqs_qa_one_point_qc_by_site(parameter = "44201",
                                    bdate = as.Date("20180101",
                                                    format = "%Y%m%d"),
                                    edate = as.Date("20180131",
                                                    format = "%Y%m%d"),
                                    stateFIPS = "25",
                                    countycode = "001",
                                    sitenum = "0002",
                                    return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_flowrateverification_by_site(parameter =  "88101",
                                    bdate = as.Date("20180101",
                                                    format = "%Y%m%d"),
                                    edate = as.Date("20180131",
                                                    format = "%Y%m%d"),
                                    stateFIPS = "01",
                                    countycode = "033",
                                    sitenum = "1002",
                                    return_header = TRUE
                                    )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_transactionsample_by_site(parameter = "44201",
                              bdate = as.Date("20170618", format = "%Y%m%d"),
                              edate = as.Date("20170618",format = "%Y%m%d"),
                              stateFIPS = "37",
                              countycode = "183",
                              sitenum = "0014",
                              return_header = TRUE
                              )[[1]]$Header$status %>%
    expect_match(regexp = "Success")

aqs_qa_annualpeferomanceeval_by_site(parameter = "44201",
                                     bdate = as.Date("20170101",
                                                     format = "%Y%m%d"),
                                     edate = as.Date("20171231",
                                                     format = "%Y%m%d"),
                                     stateFIPS = "01",
                                     countycode = "003",
                                     sitenum = "0010",
                                     return_header = TRUE
                                     )[[1]]$Header$status %>%
  expect_match(regexp = "Success")

aqs_qa_annualperformanceevaltransaction_by_site(parameter = "44201",
                                                bdate = as.Date("20170101",
                                                             format = "%Y%m%d"),
                                                edate = as.Date("20171231",
                                                             format = "%Y%m%d"),
                                                stateFIPS = "01",
                                                countycode = "003",
                                                sitenum = "0010",
                                                return_header = TRUE
                                                )[[1]]$Header$status %>%
  expect_match(regexp = "Success")


})
