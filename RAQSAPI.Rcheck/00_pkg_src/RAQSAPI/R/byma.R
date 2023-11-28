#' @section by_ma aggregate functions


#' @title aqs_qa_blanks_by_MA
#' @description \lifecycle{stable}
#'        Returns a table of blank quality assurance data.
#'        Blanks are unexposed sample collection devices (e.g.,
#'        filters) that are transported with the exposed sample devices
#'        to assess if contamination is occurring during the transport
#'        or handling of the samples. Data is aggregated by monitoring agency
#'        code (MA_code).
#' @note The AQS API only allows for a single year of qa_blank data to be
#'         retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of data
#'         at a time. Each calendar year of data requires a separate API call so
#'         multiple years of data will require multiple API calls. As the number
#'         of years of data being requested increases so does the length of time
#'         that it will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
#'           assurance blank sample data for all monitors within the input
#'           MA_code. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # Returns a tibble containing PM2.5 blank data in
#'           #  January 2018 where the Monitoring Agency is the Alabama
#'           #  Department of Environmental Management (agency 0013)
#'           \dontrun{aqs_qa_blanks_by_MA(parameter = "88101",
#'                                        bdate = as.Date("20170101",
#'                                                        format = "%Y%m%d"),
#'                                        edate = as.Date("20190131",
#'                                                        format = "%Y%m%d"),
#'                                        MA_code = "0013"
#'                                        )
#'                    }
#' @export
aqs_qa_blanks_by_MA <- function(parameter, bdate, edate, MA_code,
                                cbdate = NA_Date_, cedate = NA_Date_,
                                return_header = FALSE
                                )
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaBlanks",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  blanks <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) blanks %<>% aqs_removeheader
  return(blanks)
}


#' @title aqs_qa_collocated_assessments_by_MA
#' @description \lifecycle{stable}
#'                Returns a table of collocated assessment data aggregated by
#'                 matching input parameter, and monitoring agency (MA) code
#'                 provided for bdate - edate time frame.
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a
#'           monitoring agency. An AQS_Data Mart_APIv2 is a 2 item named list
#'           in which the first item ($Header) is a tibble of header information
#'           from the AQS API and the second item ($Data) is a tibble of the
#'           data returned.
#' @examples # Returns a tibble containing collocated assessment
#'           #   data for FRM PM2.5 January 2013 where the Monitoring Agency is
#'           #   the Alabama Department of Environmental Management
#'           # (agency 0013)
#'           \dontrun{aqs_qa_collocated_assessments_by_MA(parameter="88101",
#'                                                   bdate = as.Date("20130101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20150131",
#'                                                             format="%Y%m%d"),
#'                                                   MA_code = "0013"
#'                                                        )
#'                    }
#' @export
aqs_qa_collocated_assessments_by_MA <- function(parameter, bdate,
                                                edate, MA_code,
                                                cbdate = NA_Date_,
                                                cedate = NA_Date_,
                                                return_header = FALSE
                                                )
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaCollocatedAssessments",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  colocatedsummary <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) colocatedsummary %<>% aqs_removeheader
  return(colocatedsummary)
}


#' @title aqs_qa_flowrateverification_by_MA
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate Verification data for a
#'                parameter code aggregated by matching input parameter, and
#'                monitoring agency (MA) code provided for bdate - edate time
#'                frame.
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance flow rate verification data for monitors within a
#'           Monitoring agency. An AQS_Data Mart_APIv2 is a 2 item named list in
#'           which the first item ($Header) is a tibble of header information
#'           from the AQS API and the second item ($Data) is a tibble of the
#'           data returned.
#' @examples
#'   # Returns a tibble containing collocated assessment
#'   #   data for FRM PM2.5 January 2013 where the Monitoring Agency is
#'   #   the Alabama Department of Environmental Management (agency 0013)
#'   \dontrun{aqs_qa_flowrateverification_by_MA(parameter = "88101",
#'                                              bdate = as.Date("20130101",
#'                                                             format = "%Y%m%d"
#'                                                             ),
#'                                              edate = as.Date("20150131",
#'                                                            format = "%Y%m%d"
#'                                                             ),
#'                                              MA_code = "0013"
#'                                              )
#'           }
#' @export
aqs_qa_flowrateverification_by_MA <- function(parameter, bdate, edate,
                                              MA_code, cbdate = NA_Date_,
                                              cedate = NA_Date_,
                                              return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaFlowRateVerifications",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  frv <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) frv %<>% aqs_removeheader
  return(frv)
}


#' @title aqs_qa_flowrateaudit_by_MA
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate audit
#'                data aggregated by parameter code and monitoring agency code
#'                (_by_MA) for bdate - edate time frame.
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing flow rate
#'           audit data for the requested MA_code. An
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item ($Header) is a tibble of header information from the
#'           AQS API and the second item ($Data) is a tibble of the data
#'           returned.
#' @examples
#'  # returns a tibble of flow rate audit data
#'  #   for FRM PM2.5 January 2016 - Januray 2018 where the Monitoring Agency is
#'  #   the Jefferson County, AL  Department of Health (agency 0550)
#'  \dontrun{aqs_qa_flowrateaudit_by_MA(parameter = "88101",
#'                                      bdate = as.Date("20160101",
#'                                                   format = "%Y%m%d"),
#'                                      edate = as.Date("20180131",
#'                                                      format = "%Y%m%d"),
#'                                      MA_code = "0550"
#'                                      )
#'          }
#' @export
aqs_qa_flowrateaudit_by_MA <- function(parameter, bdate, edate, MA_code,
                                       cbdate = NA_Date_,
                                       cedate = NA_Date_,
                                       return_header = FALSE
                                       )
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaFlowRateAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  fra <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) fra %<>% aqs_removeheader
  return(fra)
}


#' @title aqs_qa_one_point_qc_by_MA
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3 object
#'                containing one point QC check data aggregated by monitoring
#'                agency code (_by_MA).
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing one point
#'            qc data for a single monitoring agency. A AQS_Data_Mart_APIv2
#'            object is a 2 item named list in which the first item ($Header)
#'            is a tibble of header information from the AQS API and the second
#'            item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of ozone One Point QC data
#'           #  in January 2018 where the Monitoring Agency is the
#'           #  Massachusetts Department of Environmental Protection
#'           #  (agency 0660)
#'           \dontrun{aqs_qa_one_point_qc_by_MA(parameter = "44201",
#'                                              bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                              edate = as.Date("20180131",
#'                                                           format = "%Y%m%d"),
#'                                              MA_code = "0660"
#'                                             )
#'                    }
#' @export
aqs_qa_one_point_qc_by_MA <- function(parameter, bdate, edate, MA_code,
                                      cbdate = NA_Date_, cedate = NA_Date_,
                                      return_header = FALSE
                                      )
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaOnePointQcRawData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  opqcc <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) opqcc %<>% aqs_removeheader
  return(opqcc)
}


#' @title aqs_qa_pep_audit_by_MA
#' @description \lifecycle{stable}
#'                Returns a table of Performance Evaluation Program (PEP) audit
#'                data aggregated by monitoring agency code (_by_MA) for the
#'                time frame between bdate and edate.
#' @note The AQS API only allows for a single year of one point pep audit data
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance PEP audit data for a monitoring agency. A
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item ($Header) is a tibble of header information from the
#'           AQS API and the second item ($Data) is a tibble of the data
#'           returned.
#' @examples # Returns a tibble of PEP audit data for
#'           #  June 2017 where the Monitoring Agency is the Alabama Department
#'           #  of Environmental Management (agency 0013)
#'           \dontrun{aqs_qa_pep_audit_by_MA(parameter = "88101",
#'                                           bdate = as.Date("20170601",
#'                                                           format = "%Y%m%d"),
#'                                           edate = as.Date("20170630",
#'                                                           format = "%Y%m%d"),
#'                                           MA_code = "0013"
#'                                          )
#'                    }
#' @export
aqs_qa_pep_audit_by_MA <- function(parameter, bdate, edate, MA_code,
                                   cbdate = NA_Date_, cedate = NA_Date_,
                                   return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaPepAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  pepaudit <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) pepaudit %<>% aqs_removeheader
  return(pepaudit)
}


#' @title aqs_transactionsample_MA
#' @description \lifecycle{stable}
#'        Returns transactionsample data - aggregated by Monitoring agency (MA)
#'          in the AQS Submission Transaction Format (RD) sample (raw) data for
#'          a parameter code aggregated by matching input parameter, and
#'          monitoring agency (MA) code provided for bdate - edate time
#'          frame. Includes data both in submitted and standard units
#'
#' @note The AQS API only allows for a single year of transaction data to be
#'         retrieved at a time. This function conveniently extracts date
#'         information from the bdate and edate parameters then makes repeated
#'         calls to the AQSAPI retrieving a maximum of one calendar year of
#'         data at a time. Each calendar year of data requires a separate API
#'         call so multiple years of data will require multiple API calls.
#'         As the number of years of data being requested increases so does the
#'         length of time that it will take to retrieve results. There is also a
#'         5 second wait time inserted between successive API calls to prevent
#'         overloading the API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate _by_state functions
#' @inheritParams aqs_services_by_MA
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples #Returns a tibble of ozone transaction sample data for all monitors
#'           #operated by South Coast Air Quality Management District collected
#'           #on May 15, 2015
#'           \dontrun{aqs_transactionsample_by_MA(parameter = "44201",
#'                                                bdate = as.Date("20150515",
#'                                                           format = "%Y%m%d"),
#'                                                edate = as.Date("20150515",
#'                                                           format = "%Y%m%d"),
#'                                                MA_code = "0972"
#'                                                )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of transaction sample
#'           (raw) data in the AQS submission transaction format (RD)
#'           corresponding to the inputs provided.
#' @export
aqs_transactionsample_by_MA <- function(parameter, bdate, edate, MA_code,
                                cbdate = NA_Date_, cedate = NA_Date_,
                                return_header = FALSE
                                )
{
  checkaqsparams(parameter, bdate, edate, MA_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "transactionsSample"
                               )

  transactionsample <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) transactionsample %<>% aqs_removeheader
  return(transactionsample)
}


#' @title aqs_qa_annualperformanceeval_by_MA
#' @description \lifecycle{stable}
#'        Returns quality assurance performance evaluation data - aggregated by
#'          by Monitoring agency (MA) for a parameter
#'          code aggregated by matching input parameter and MA_code for the
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples
#'           # Returns a tibble containing annual performance evaluation data
#'           #  for ozone where the monitoring agency is the Alabama Department
#'           #  of Environmental Management (MA_code 0013).
#'           \dontrun{aqs_qa_annualperformanceeval_by_MA(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                           MA_code = "0013"
#'                                                      )
#'                   }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data. for all monitoring sites for with
#'           the MA_code requested for the time frame between bdate and edate.
#'           An AQS_Data_Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @export
aqs_qa_annualperformanceeval_by_MA <- function(parameter, bdate, edate,
                                               MA_code,
                                               return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, MA_code, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service = "qaAnnualPerformanceEvaluations"
                               )

  qaape <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) qaape %<>% aqs_removeheader
  return(qaape)
}


#' @title aqs_qa_annualperformanceevaltransaction_by_MA
#' @description \lifecycle{stable}
#'          Returns AQS submissions transaction format (RD) of the annual
#'             performance evaluation data (raw). Includes data pairs for
#'             QA - aggregated by Monitoring agency (MA)
#'             for a parameter code aggregated by matching input parameter and
#'             MA_code provided for bdate - edate time frame.
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
#' @family Aggregate _by_MA functions
#' @inheritParams aqs_services_by_MA
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation data
#'           #  for ozone in where the MA is the Alabama Department of
#'           #  Environmental Management (MA_code 0013) for 2017 in RD format.
#'  \dontrun{aqs_qa_annualperformanceevaltransaction_by_MA(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                   MA_code = "0013"
#'                                                         )
#'          }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data in the AQS submissions transaction
#'           format (RD)for all sites matching the MA_code requested for the
#'           time frame between bdate and edate. An AQS_Data_Mart_APIv2 is a 2
#'           item named list in which the first item ($Header) is a tibble of
#'           header information from the AQS API and the second item ($Data) is
#'           a tibble of the data returned.
#' @export
aqs_qa_annualperformanceevaltransaction_by_MA <- function(parameter, bdate,
                                                          edate, MA_code,
                                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, MA_code, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               MA_code = MA_code,
                               service =
                                 "transactionsQaAnnualPerformanceEvaluations"
                               )

  tqaape <- purrr::pmap(.l = params, .f = aqs_services_by_MA)
  if (!return_header) tqaape %<>% aqs_removeheader
  return(tqaape)
}
