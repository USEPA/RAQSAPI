#' @section by_state aggregate functions


#' @title aqs_monitors_by_state
#' @description \lifecycle{stable}
#'  Returns a table of monitors and related metadata at sites with the
#'    provided parameter, and stateFIPS for bdate - edate time frame.
#' @note All monitors that operated between the bdate and edate will be returned
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from the
#'           selected state
#' @examples # returns a tibble of SO2 monitors in Hawaii
#'           #  that were operating on May 01, 2017
#'  \dontrun{aqs_monitors_by_state(parameter="88101",
#'                                   bdate=as.Date("20170101",
#'                                                  format="%Y%m%d"),
#'                                   edate=as.Date("20171231",
#'                                                  format="%Y%m%d"),
#'                                   stateFIPS="01"
#'                                   )
#'           }
#'
#' @export
aqs_monitors_by_state <- function(parameter, bdate, edate, stateFIPS,
                                    cbdate = NA_Date_, cedate = NA_Date_,
                                    return_header = FALSE)
{
    checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                   return_header)

  # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::pmap
  #  is used so that the output is consistent with other RAQSAPI functions.
  params <- tibble(parameter = parameter,
                   bdate = bdate,
                   edate = edate,
                   stateFIPS = stateFIPS,
                   service = "monitors",
                   cbdate = cbdate,
                   cedate = cedate) %>%
     dplyr::select_if(function(x) {!all(is.na(x))})

  monitors <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) monitors %<>% aqs_removeheader
  return(monitors)
}


#' @title aqs_sampledata_by_state
#' @description \lifecycle{stable}
#'                 Returns sample data where the data is aggregated at the state
#'                 level. If return_header is FALSE (default) this function
#'                 returns a single dataframe with the requested data. If
#'                 return_header is TRUE returns a list of AQSAPI_v2 objects
#'                 where each index of the list is an individual RAQSAPI_v2
#'                 object returned from each successive call to the AQS API.
#'                 RAQSAPI_v2 objects are two item list where the $Data portion
#'                 contains data that contains sample air monitoring data at a
#'                 site with the input parameter and stateFIPS provided for
#'                 bdate - edate time frame. The $Header is a tibble of header
#'                 information from the API call /(useful for debugging/). This
#'                 function returns NULL is bdate > edate.
#' @note The AQS API only allows for a single year of sampledata to be retrieved
#'         at a time. This function conveniently extracts date information from
#'         the bdate and edate parameters then makes repeated calls to the
#'         AQSAPI retrieving a maximum of one calendar year of data at a time.
#'         Each calendar year of data requires a separate API call so multiple
#'         years of data will require multiple API calls. As the number of years
#'         of data being requested increases so does the length of time that it
#'         will take to retrieve results. There is also a 5 second wait
#'         time inserted between successive API calls to prevent overloading the
#'         API server. This operation has a linear run time of
#'         /(Big O notation: O/(n + 5 seconds/)/).
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @importFrom purrr pmap
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data
#'           for all monitors matching stateFIPS for the given parameter.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item /(/$Header/) is a tibble of header information from the
#'           AQS API and the second item /(/$Data/) is a tibble of the data
#'           returned.
#' @examples # Returns a tibble with all benzene samples from
#'           #  North Carolina collected from May 15th, 1995 - May 15, 1999
#'           \dontrun{aqs_sampledata_by_state(parameter = "45201",
#'                                            bdate = as.Date("19950515",
#'                                                            format="%Y%m%d"
#'                                                            ),
#'                                            edate = as.Date("19990515",
#'                                                           format = "%Y%m%d"),
#'                                            stateFIPS = "37"
#'                                           )
#'                    }
#' @export
aqs_sampledata_by_state <- function(parameter, bdate, edate, stateFIPS,
                                    duration = NA_character_,
                                    cbdate = NA_Date_, cedate = NA_Date_,
                                    return_header = FALSE
                                    )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, duration, cbdate,
                 cedate, return_header)

params <- aqsmultiyearparams(parameter = parameter,
                                   bdate = bdate,
                                   edate = edate,
                                   stateFIPS = stateFIPS,
                                   duration = duration,
                                   service = "sampleData",
                                   cbdate = cbdate,
                                   cedate = cedate
                              )

  sampledata <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) sampledata %<>% aqs_removeheader
  return(sampledata)
}


#' @title aqs_annualsummary_by_state
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where annual data is
#'                 aggregated at the state level. Returned is an annual summary
#'                 matching the input parameter and stateFIPS
#'                 provided for bdate - edate time frame. The data
#'                 returned is summarized at the annual level. Variables
#'                 returned include mean value, maxima, percentiles, and etc. If
#'                 return_header is FALSE (default) the object returned is a
#'                 tibble, if TRUE an AQS_API_v2 object.
#' @note The AQS API only allows for a single year of annualsummary to be
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the stateFIPS requested. A AQS_Data Mart_APIv2 is
#'           a 2 item named list in which the first item ($Header) is a tibble
#'           of header information from the AQS API and the second item ($Data
#'           is a tibble of the data returned.
#' @examples # returns a tibble of all benzene annualy
#'           #  summaries from North Carolina collected for 1995
#'           \dontrun{aqs_annualsummary_by_state(parameter = "45201",
#'                                               bdate = as.Date("19950515",
#'                                                               format="%Y%m%d"
#'                                                               ),
#'                                               edate = as.Date("19950515",
#'                                                             format = "%Y%m%d"
#'                                                              ),
#'                                               stateFIPS = "37"
#'                                               )
#'                    }
#' @export
aqs_annualsummary_by_state <- function(parameter, bdate, edate, stateFIPS,
                                       cbdate = NA_Date_, cedate = NA_Date_,
                                       return_header = FALSE
                                       )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "annualData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  annualsummary <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) annualsummary %<>% aqs_removeheader
  return(annualsummary)
}


#' @title aqs_qa_blanks_by_state
#' @description \lifecycle{stable}
#'        Returns a table of blank quality assurance data .
#'        Blanks are unexposed sample collection devices (e.g.,
#'        filters) that are transported with the exposed sample devices
#'        to assess if contamination is occurring during the transport
#'        or handling of the samples. Data is aggregated at the state level.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
#'           assurance blank sample data for all monitors within the input
#'           stateFIPS. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # returns a tibble which contains PM2.5 blank data
#'           #  for Alabama for January 2018
#'           \dontrun{aqs_qa_blanks_by_state(parameter = "88101",
#'                                           bdate = as.Date("20180101",
#'                                                           format = "%Y%m%d"
#'                                                          ),
#'                                           edate = as.Date("20180131",
#'                                           format = "%Y%m%d"),
#'                                           stateFIPS = "01"
#'                                           )
#'                    }
#' @export
aqs_qa_blanks_by_state <- function(parameter, bdate, edate, stateFIPS,
                                     cbdate = NA_Date_, cedate = NA_Date_,
                                     return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaBlanks",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  blanks <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) blanks %<>% aqs_removeheader
  return(blanks)
}


#' @title aqs_dailysummary_by_state
#' @description \lifecycle{stable}
#'        Returns multiple years of data where daily data is
#'        aggregated at the state level. Returned is a daily summary
#'        matching the input parameter and stateFIPS provided for bdate - edate
#'        time frame. Data is aggregated at the state level. Variables returned
#'        include mean value, maxima, percentiles, and etc.
#' @note The AQS API only allows for a single year of dailysummary to be
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains daily
#'           summary statistics for the given parameter for a single stateFIPS.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of all benzene daily
#'           #  summaries from North Carolina collected on May 15th, 1995
#'           \dontrun{aqs_dailysummary_by_state(parameter = "45201",
#'                                              bdate = as.Date("19950515",
#'                                                              format="%Y%m%d"
#'                                                              ),
#'                                              edate = as.Date("19970515",
#'                                                             format = "%Y%m%d"
#'                                                              ),
#'                                              stateFIPS = "37"
#'                                              )
#'                    }
#' @export
aqs_dailysummary_by_state <- function(parameter, bdate, edate, stateFIPS,
                                      cbdate = NA_Date_, cedate = NA_Date_,
                                      return_header = FALSE
                                      )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "dailyData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_qa_collocated_assessments_by_state
#' @description \lifecycle{stable}
#'                Returns a table of collocated assessment data aggregated by
#'                matching input parameter and stateFIPS provided for bdate -
#'                edate time frame.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a state.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble  of collocated
#'           #  assessment data for FRM2.5 for January 2013
#'           \dontrun{aqs_qa_collocated_assessments_by_state(parameter="88101",
#'                                                   bdate = as.Date("20130101",
#'                                                             format = "%Y%m%d"
#'                                                                  ),
#'                                                   edate = as.Date("20150131",
#'                                                             format = "%Y%m%d"
#'                                                                  ),
#'                                                              stateFIPS = "01"
#'                                                           )
#'                    }
#' @export
aqs_qa_collocated_assessments_by_state <- function(parameter, bdate, edate,
                                                   stateFIPS, cbdate = NA_Date_,
                                                   cedate = NA_Date_,
                                                   return_header = FALSE
                                                   )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaCollocatedAssessments",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  colocatedsummary <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) colocatedsummary %<>% aqs_removeheader
  return(colocatedsummary)
}


#' @title aqs_qa_flowrateverification_by_state
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate Verification data for a
#'                parameter code aggregated matching input parameter, and
#'                stateFIPS, provided for bdate - edate time frame.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance flow rate verification data for monitors within a state.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # Returns a tibble of flow rate verification data for the state of
#'           # Alabama for 2017-2019
#'           \dontrun{aqs_qa_flowrateverification_by_state(parameter = "88101",
#'                                                   bdate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                                   ),
#'                                                   edate = as.Date("20190131",
#'                                                               format="%Y%m%d"
#'                                                                   ),
#'                                                   stateFIPS = "01"
#'                                                         )
#'                    }
#' @export
aqs_qa_flowrateverification_by_state <- function(parameter, bdate, edate,
                                                 stateFIPS, cbdate = NA_Date_,
                                                 cedate = NA_Date_,
                                                 return_header = FALSE
                                                 )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaFlowRateVerifications",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  frv <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) frv %<>% aqs_removeheader
  return(frv)
}


#' @title aqs_qa_flowrateaudit_by_state
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate audit
#'                data aggregated by parameter code and stateFIPS for
#'                bdate - edate time frame.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing flow rate
#'           audit data for the requested stateFIPS. An
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item ($Header) is a tibble of header information from the
#'           AQS API and the second item ($Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of flow rate audit
#'           #  data for Alabama in January 2018
#'           \dontrun{aqs_qa_flowrateaudit_by_state(parameter = "88101",
#'                                                  bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                  edate = as.Date("20180131",
#'                                                           format = "%Y%m%d"),
#'                                                  stateFIPS = "01"
#'                                                  )
#'                    }
#' @export
aqs_qa_flowrateaudit_by_state <- function(parameter, bdate, edate, stateFIPS,
                                          cbdate = NA_Date_, cedate = NA_Date_,
                                          return_header = FALSE
                                          )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaFlowRateAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  fra <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) fra %<>% aqs_removeheader
  return(fra)
}


#' @title aqs_qa_one_point_qc_by_state
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing Quality assurance data - flow rate audit
#'                raw data aggregated by state FIPS.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing one point
#'            qc data within a state. A AQS_Data_Mart_APIv2 object is a
#'            2 item named list in which the first item ($Header) is a tibble
#'            of header information from the AQS API and the second item
#'            ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of one point QC check
#'           #  data for ozone in Massachusettes in January 2018
#'           \dontrun{aqs_qa_one_point_qc_by_state(parameter = "44201",
#'                                                 bdate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                                 ),
#'                                                 edate = as.Date("20190131",
#'                                                            format = "%Y%m%d"
#'                                                                ),
#'                                                 stateFIPS = "25"
#'                                                )
#'                    }
#' @export
aqs_qa_one_point_qc_by_state <- function(parameter, bdate, edate, stateFIPS,
                                         cbdate = NA_Date_, cedate = NA_Date_,
                                         return_header = FALSE
                                         )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaOnePointQcRawData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  opqcc <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) opqcc %<>% aqs_removeheader
  return(opqcc)
}


#' @title aqs_qa_pep_audit_by_state
#' @description \lifecycle{stable}
#'                Returns a table of Performance Evaluation Program (PEP) audit
#'                data aggregated by parameter code, and stateFIPS for the time
#'                frame between bdate and edate.
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
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance PEP audit data within a state. A AQS_Data_Mart_APIv2
#'           object is a 2 item named list in which the first item ($Header) is
#'           a tibble of header information from the AQS API and the second item
#'           ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of PEP audit data for PM2.5 in Alabama 2017
#'           \dontrun{aqs_qa_pep_audit_by_state_multiyear(parameter="88101",
#'                                              bdate=as.Date("20160101",
#'                                                           format="%Y%m%d"),
#'                                              edate=as.Date("20171231",
#'                                                            format="%Y%m%d"),
#'                                              stateFIPS="01"
#'                                              )
#'                    }
#' @export
aqs_qa_pep_audit_by_state <- function(parameter, bdate, edate, stateFIPS,
                                      cbdate = NA_Date_, cedate = NA_Date_,
                                      return_header = FALSE
                                      )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaPepAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  pepaudit <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) pepaudit %<>% aqs_removeheader
  return(pepaudit)
}


#' @title aqs_transactionsample_by_state
#' @description \lifecycle{stable}
#'        Returns transactionsample data - aggregated by state
#'          in the AQS Submission Transaction Format (RD) sample (raw) data for
#'          a parameter code aggregated by matching input parameter, and
#'          stateFIPS provided for bdate - edate time frame. Includes data both
#'          in submitted and standard units
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
#' @inheritParams aqs_services_by_state
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples #Returns a AQS_Data Mart_APIv2 S3 object of
#'          \dontrun{ # Returns a tibble containing benzene transaction sample
#'                    # data for North Carolina on May 15, 1995
#'                    aqs_transactionsample_by_state(parameter = "45201",
#'                                                  bdate = as.Date("19950515",
#'                                                          format = "%Y%m%d"),
#'                                                  edate = as.Date("19950515",
#'                                                           format = "%Y%m%d"),
#'                                                  stateFIPS = "37"
#'                                                  )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of transaction sample
#'           (raw) data in the AQS submission transaction format (RD)
#'           corresponding to the inputs provided.
#' @export
aqs_transactionsample_by_state <- function(parameter, bdate, edate, stateFIPS,
                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "transactionsSample"
                               )

  transactionsample <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) transactionsample %<>% aqs_removeheader
  return(transactionsample)
}

#' @title aqs_qa_annualperformanceeval_by_state
#' @description \lifecycle{stable}
#'        Returns quality assurance performance evaluation data - aggregated by
#'          state for a parameter code aggregated by matching input
#'          parameter, countycode and stateFIPS provided for
#'          bdate - edate time frame.
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
#' @family Aggregate _by_state functions
#' @inheritParams aqs_services_by_state
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation
#'           # data for ozone in Alabamba for 2017.
#'  \dontrun{ aqs_qa_annualperformanceeval_by_state(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                   stateFIPS = "01"
#'                                                   )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data. for single monitoring site for the
#'           sitenum, countycode and stateFIPS requested for the time frame
#'           between bdate and edate. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @export
aqs_qa_annualperformanceeval_by_state <- function(parameter, bdate, edate,
                                                   stateFIPS,
                                                   return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaAnnualPerformanceEvaluations"
                               )

  qaape <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) qaape %<>% aqs_removeheader
  return(qaape)
}


#' @title aqs_qa_annualperformanceevaltransaction_by_state
#' @description \lifecycle{stable}
#'          Returns AQS submissions transaction format (RD) of the annual
#'             performance evaluation data (raw). Includes data pairs for
#'             QA - aggregated by state for a parameter code aggregated by
#'             matching input parameter and stateFIPS provided for bdate - edate
#'             time frame.
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
#' @family Aggregate _by_state functions
#' @inheritParams aqs_services_by_state
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation data
#'           # for ozone in Alabmba for 2017 in RD format.
#' \dontrun{
#'         aqs_qa_annualperformanceevaltransaction_by_state(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                            format = "%Y%m%d")
#'                                                          stateFIPS = "01"
#'                                                          )
#'          }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data. for single monitoring site for the
#'           sitenum, countycode and stateFIPS requested for the time frame
#'           between bdate and edate. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @export
aqs_qa_annualperformanceevaltransaction_by_state <- function(parameter, bdate,
                                                             edate, stateFIPS,
                                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "qaAnnualPerformanceEvaluations"
                               )

  tqaape <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) tqaape %<>% aqs_removeheader
  return(tqaape)
}


#' @title aqs_quarterlysummary_by_state
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object of quarterly summary data aggregated by stateFIPS.
#' @note The AQS API only allows for a single year of quarterly summary to be
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
#'
#'         Also Note that for quarterly data, only the year portion of the bdate
#'         and edate are used and all 4 quarters in the year are returned.
#' @family Aggregate _by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains quarterly
#'           summary statistics for the given parameter for a stateFIPS.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # Returns an aqs S3 object containing quarterly summaries for
#'           #  FRM/FEM PM2.5 data for North Carolina for each quater of  2016
#'  \dontrun{aqs_quarterlysummary_by_state(parameter = "88101",
#'                                         bdate = as.Date("20160101",
#'                                                         format = "%Y%m%d"),
#'                                         edate = as.Date("20171231",
#'                                                         format = "%Y%m%d"),
#'                                         stateFIPS = "37"
#'                                        )
#'          }
#' @export
aqs_quarterlysummary_by_state <- function(parameter, bdate, edate, stateFIPS,
                                          cbdate = NA_Date_, cedate = NA_Date_,
                                          return_header = FALSE)
{
  AQS_domain <- "aqs.epa.gov"
  checkaqsparams(parameter, bdate, edate, stateFIPS,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               service = "quarterlyData",
                               cbdate = cbdate,
                               cedate = cedate,
                               AQS_domain = AQS_domain
                               )

  quarterlysummary <- purrr::pmap(.l = params, .f = aqs_services_by_state)
  if (!return_header) quarterlysummary %<>% aqs_removeheader
  return(quarterlysummary)
}
