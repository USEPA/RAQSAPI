#' @section by_pqao aggregate functions

#' @title aqs_qa_blanks_by_pqao
#' @description \lifecycle{stable}
#'        Returns a table of blank quality assurance data.
#'        Blanks are unexposed sample collection devices (e.g.,
#'        filters) that are transported with the exposed sample devices
#'        to assess if contamination is occurring during the transport
#'        or handling of the samples. Data is aggregated by
#'        Primary Quality Assurance Organization (PQAO).
#' @note The AQS API only allows for a single year of flow rate audit data to
#'         be retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance blank data for monitors within a pqao. An
#'           AQS_Data Mart_APIv2 is a 2 item named list in which the first item
#'           ($Header) is a tibble of header information from the AQS API and
#'           the second item ($Data) is a tibble of the data returned.
#' @examples # Returns tibble of PM2.5 blank data in
#'           #  January 2018 where the PQAO is the Alabama Department of
#'           #  Environmental Management (agency 0013)
#'           \dontrun{aqs_qa_blanks_by_pqao(parameter = "88101",
#'                                          bdate = as.Date("20180101",
#'                                                          format = "%Y%m%d"),
#'                                          edate = as.Date("20180131",
#'                                                          format = "%Y%m%d"),
#'                                          pqao_code = "0013"
#'                                          )
#'                    }
#' @export
aqs_qa_blanks_by_pqao <- function(parameter, bdate, edate, pqao_code,
                                  cbdate = NA_Date_, cedate = NA_Date_,
                                  return_header = FALSE
                                  )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaBlanks",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  blanks <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) blanks %<>% aqs_removeheader
  return(blanks)
}


#' @title aqs_qa_collocated_assessments_by_pqao
#' @description \lifecycle{stable}
#'                Returns a table of collocated assessment data aggregated by
#'                 matching input parameter, and Primary Quality Assurance
#'                 Organisation (PQAO) code provided for bdate - edate
#'                 time frame.
#' @note The AQS API only allows for a single year of collocated assessments
#'         data to be retrieved at a time. This function conveniently extracts
#'         date information from the bdate and edate parameters then makes
#'         repeated calls to the AQSAPI retrieving a maximum of one calendar
#'         year of data at a time. Each calendar year of data requires a
#'         separate API call so multiple years of data will require multiple API
#'         calls. As the number of years of data being requested increases so
#'         does the length of time that it will take to retrieve results. There
#'         is also a 5 second wait time inserted between successive API calls to
#'         prevent overloading the API server. This operation has a linear run
#'         time of /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a pqao.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # Returns a tibble of collocated assessment
#'           #  data for FRM PM2.5 in January 2013 where the PQAO is the Alabama
#'           #  Department of Environmental Management (agency 0013)
#'           \dontrun{aqs_qa_collocated_assessments_by_pqao(parameter = "88101",
#'                                                   bdate = as.Date("20130101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20150131",
#'                                                           format = "%Y%m%d"),
#'                                                         pqao_code = "0013"
#'                                                         )
#'                    }
#' @export
aqs_qa_collocated_assessments_by_pqao <- function(parameter, bdate, edate,
                                                  pqao_code, cbdate = NA_Date_,
                                                  cedate = NA_Date_,
                                                  return_header = FALSE
                                                  )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaCollocatedAssessments",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  colocatedsummary <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) colocatedsummary %<>% aqs_removeheader
  return(colocatedsummary)
}


#' @title aqs_qa_flowrateverification_by_pqao
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate Verification data for a
#'                parameter code aggregated by matching input parameter, and
#'                Primary Quality Assurance Organization (PQAO) code provided
#'                for bdate - edate time.
#' @note The AQS API only allows for a single year of flow rate verifications to
#'         be retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance flow rate verification data for monitors within a pqao.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of flow rate verification
#'   #  data for January 2018 where the PQAO is the Alabama Department
#'   #  of Environmental Management (agency 0013)
#'   \dontrun{aqs_qa_flowrateverification_by_pqao(parameter = "88101",
#'                                                bdate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                               ),
#'                                                edate = as.Date("20190131",
#'                                                             format = "%Y%m%d"
#'                                                               ),
#'                                                pqao_code = "0013"
#'                                                )
#'          }
#' @export
aqs_qa_flowrateverification_by_pqao <- function(parameter, bdate, edate,
                                                pqao_code,
                                                cbdate = NA_Date_,
                                                cedate = NA_Date_,
                                                return_header = FALSE
                                                )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaFlowRateVerifications",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  frv <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) frv %<>% aqs_removeheader
  return(frv)
}


#' @title aqs_qa_flowrateaudit_by_pqao
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate audit
#'                data aggregated by parameter code and Primary Quality
#'                Assurance Organization (PQAO) code for bdate - edate
#'                time frame.
#' @note The AQS API only allows for a single year of flow rate audit data to
#'         be retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing flow rate
#'           audit data  for the requested pqao_code. An
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item ($Header) is a tibble of header information from the
#'           AQS API and the second item ($Data) is a tibble of the data
#'           returned.
#' @examples # Returns a tibble of flow rate audit data for January
#'   #  2018 where the PQAO is the Jefferson County, AL Department of
#'   #  Health (agency 0550).
#'   \dontrun{aqs_qa_flowrateaudit_by_pqao(parameter = "88101",
#'                                         bdate = as.Date("20170101",
#'                                                         format = "%Y%m%d"),
#'                                         edate = as.Date("20180131",
#'                                                         format = "%Y%m%d"),
#'                                         pqao_code = "0550"
#'                                        )
#'           }
#' @export
aqs_qa_flowrateaudit_by_pqao <- function(parameter, bdate, edate, pqao_code,
                                         cbdate = NA_Date_, cedate = NA_Date_,
                                         return_header = FALSE
                                         )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaFlowRateAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  fra <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) fra %<>% aqs_removeheader
  return(fra)
}


#' @title aqs_qa_one_point_qc_by_pqao
#' @description \lifecycle{stable}
#'   Returns a tibble or an AQS_Data Mart_APIv2 S3 object containing Quality
#'     assurance data - collocated assessment raw data aggregated by Primary
#'     Quality Assurance Organization (PQAO) code.
#' @note The AQS API only allows for a single year of one point qc data to
#'         be retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing one point
#'            qc data within a pqao. A AQS_Data_Mart_APIv2 object is a
#'            2 item named list in which the first item ($Header) is a tibble
#'            of header information from the AQS API and the second item
#' @examples
#'  # returns a tibble of ozone One Point QC
#'  #  data for Jan 2017 - January 2018 where the PQAO is the Massachusetts
#'  #  Department of Environmental Protection (agency 0660)
#'  \dontrun{aqs_qa_one_point_qc_by_pqao(parameter = "88101",
#'                                       bdate = as.Date("20170101",
#'                                                       format = "%Y%m%d"),
#'                                       edate = as.Date("20180131",
#'                                                     format = "%Y%m%d"),
#'                                       pqao_code = "0660"
#'                                      )
#'          }
#' @export
aqs_qa_one_point_qc_by_pqao <- function(parameter, bdate, edate, pqao_code,
                                        cbdate = NA_Date_, cedate = NA_Date_,
                                        return_header = FALSE
                                        )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaOnePointQcRawData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  opqcc <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) opqcc %<>% aqs_removeheader
  return(opqcc)
}


#' @title aqs_qa_pep_audit_by_pqao
#' @description \lifecycle{stable}
#'                Returns a table of Performance Evaluation Program (PEP) audit
#'                data aggregated by Primary Quality Assurance Organization
#'                (PQAO) code for the time frame between bdate and edate.
#' @note The AQS API only allows for a single year of pep audit data
#'         to be retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance PEP audit data for a Primary Quality Assurance
#'           Organization. A AQS_Data_Mart_APIv2 object is a 2 item named list
#'           in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # returns a tibble of PEP audit data for
#'           #  June 2017 where the pqao is the Alabama Department of
#'           #  Environmental Management (agency 0013)
#'   \dontrun{aqs_qa_pep_audit_by_pqao(parameter = "88101",
#'                                     bdate = as.Date("20170601",
#'                                                     format = "%Y%m%d"
#'                                                    ),
#'                                     edate = as.Date("20190630",
#'                                                     format = "%Y%m%d"),
#'                                     pqao_code = "0013"
#'                                     )
#'                    }
#' @export
aqs_qa_pep_audit_by_pqao <- function(parameter, bdate, edate, pqao_code,
                                     cbdate = NA_Date_,
                                     cedate = NA_Date_,
                                     return_header = FALSE
                                     )
{
  checkaqsparams(parameter, bdate, edate, pqao_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaPepAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  pepaudit <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) pepaudit %<>% aqs_removeheader
  return(pepaudit)
}


#' @title aqs_qa_annualperformanceeval_by_pqao
#' @description \lifecycle{stable}
#'        Returns quality assurance performance evaluation data - aggregated by
#'          Primary Quality Assurance Organization (PQAO) for a parameter
#'          code aggregated by matching input parameter and pqao_code for the
#'          time frame between bdate and edate.
#' @note The AQS API only allows for a single year of quality assurance
#'         Annual Performance Evaluation data to be retrieved at a time. This
#'         function conveniently extracts date information from the bdate
#'         and edate parameters then makes repeated calls to the AQSAPI
#'         retrieving a maximum of one calendar year of data at a time. Each
#'         calendar year of data requires a separate API call so multiple years
#'         of data will require multiple API calls. As the number of years of
#'         data being requested increases so does the length of time that it
#'         will take to retrieve results. There is also a 5 second wait time
#'         inserted between successive API calls to prevent overloading the API
#'         server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation data
#'           # for ozone where the PQAO is the Alabamaba Department of
#'           # Environmental Management (pqao_code 0013).
#'  \dontrun{ aqs_qa_annualperformanceeval_by_pqao(parameter = "44201",
#'                                                 bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                 edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                 pqao_code = "0013"
#'                                                 )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data. for single monitoring site for the
#'           pqao_code requested for the time frame
#'           between bdate and edate. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @export
aqs_qa_annualperformanceeval_by_pqao <- function(parameter, bdate, edate,
                                                 pqao_code,
                                                 return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, pqao_code, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "qaAnnualPerformanceEvaluations"
                               )

  qaape <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) qaape %<>% aqs_removeheader
  return(qaape)
}


#' @title aqs_qa_annualperformanceevaltransaction_by_pqao
#' @description \lifecycle{stable}
#'          Returns AQS submissions transaction format (RD) of the annual
#'             performance evaluation data (raw). Includes data pairs for
#'             QA - aggregated by Primary Quality Assurance Organization (PQAO)
#'             for a parameter code aggregated by matching input parameter and
#'             pqao_code provided for bdate - edate time frame.
#' @note The AQS API only allows for a single year of quality assurance
#'         Annual Performance Evaluations transaction data to be retrieved at a
#'         time. This function conveniently extracts date information from the
#'         bdate and edate parameters then makes repeated calls to the AQSAPI
#'         retrieving a maximum of one calendar year of data at a time. Each
#'         calendar year of data requires a separate API call so multiple years
#'         of data will require multiple API calls. As the number of years of
#'         data being requested increases so does the length of time that it
#'         will take to retrieve results. There is also a 5 second wait time
#'         inserted between successive API calls to prevent overloading the API
#'         server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_pqao functions
#' @inheritParams aqs_services_by_pqao
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples #Returns a tibble containing annual performance evaluation data for
#'           # ozone in where the PQAO is the Alabama Department of
#'           # Environmental Management (pqao_code 0013) for 2017 in RD format.
#'  \dontrun{aqs_qa_annualperformanceevaltransaction_by_pqao(parameter =
#'                                                                      "44201",
#'                                                 bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                 edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                 pqao_code = "0013"
#'                                                )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data. for single monitoring site for the
#'           sitenum, countycode and stateFIPS requested for the time frame
#'           between bdate and edate. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @export
aqs_qa_annualperformanceevaltransaction_by_pqao <- function(parameter,
                                                            bdate, edate,
                                                            pqao_code,
                                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, pqao_code, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               pqao_code = pqao_code,
                               service = "transactionsQaAnnualPerformanceEvaluations"
                               )

  tqaape <- purrr::pmap(.l = params, .f = aqs_services_by_pqao)
  if (!return_header) tqaape %<>% aqs_removeheader
  return(tqaape)
}
