#' @section by_county aggregate functions

#' @title aqs_monitors_by_county
#' @description \lifecycle{stable}
#'  Returns a table of monitors and related metadata at sites with the
#'    provided parameter, stateFIPS and county_code for
#'    bdate - edate time frame.
#' @note All monitors that operated between the bdate and edate will be returned
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from a
#'           selected county
#' @examples # returns an aqs_v2 S3 object containing all SO2 monitors in
#'           #  Hawaii County, HI that were operating between May 01-02, 2015.
#'  \dontrun{aqs_monitors_by_county(parameter="42401",
#'                                  bdate=as.Date("20150501", format="%Y%m%d"),
#'                                  edate=as.Date("20150502", format="%Y%m%d"),
#'                                  stateFIPS="15",
#'                                  countycode="001"
#'                                  )
#'          }
#' @export
aqs_monitors_by_county <- function(parameter, bdate, edate, stateFIPS,
                                     countycode,
                                     cbdate = NA_Date_, cedate = NA_Date_,
                                     return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::pmap
  #  is used so that the output is consistent with other RAQSAPI functions.
  params <- tibble(parameter = parameter,
                   bdate = bdate,
                   edate = edate,
                   stateFIPS = stateFIPS,
                   countycode = countycode,
                   service = "monitors",
                   cbdate = cbdate,
                   cedate = cedate) %>%
     dplyr::select_if(function(x) {!all(is.na(x))})

  monitors <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) monitors %<>% aqs_removeheader
  return(monitors)
}


#' @title aqs_sampledata_by_county
#' @description \lifecycle{stable}
#'                 Returns a single tibble with the requested data. If
#'                 return_header is TRUE returns a list
#'                 of AQSAPI_v2 objects where each index of the list is an
#'                 individual RAQSAPI_v2 object returned from each successive
#'                 call to the AQS API. RAQSAPI_v2 objects are two item list
#'                 where the $Data portion contains data that contains
#'                 sample air monitoring data at a site with the input
#'                 parameter, stateFIPS and county_code provided for
#'                 bdate - edate time frame. The $Header is a tibble of
#'                 header information from the API call /(useful for
#'                 debugging/). This function returns NULL is bdate > edate.
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
#' @family Aggregate _by_county functions
#' @param return_header If FALSE (default) only returns data requested.
#'                      If TRUE returns a AQSAPI_v2 object which is a two item
#'                      list that contains header information returned from
#'                      the API server mostly used for debugging purposes in
#'                      addition to the data requested.
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @importFrom purrr pmap
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data
#'           for all monitors matching stateFIPS and county_code for the given
#'           parameter. An AQS_Data Mart_APIv2 is a 2 item named list in which
#'           the first item /(/$Header/) is a tibble of header information from
#'           the AQS API and the second item /(/$Data/) is a tibble of the data
#'           returned.
#' @examples # returns all FRM/FEM PM2.5 data for Wake County, NC between
#'           #  January 1, 2015 - February 28, 2016
#'  \dontrun{aqs_sampledata_by_county(parameter = "88101",
#'                                    bdate = as.Date("20150101",
#'                                                    format = "%Y%m%d"),
#'                                    edate=as.Date("20160228",
#'                                                    format = "%Y%m%d"),
#'                                    stateFIPS = "37",
#'                                    countycode = "183"
#'                                    )
#'          }
#' @export
aqs_sampledata_by_county <- function(parameter, bdate, edate, stateFIPS,
                                     countycode,
                                     duration = NA_character_,
                                     cbdate = NA_Date_, cedate = NA_Date_,
                                     return_header = FALSE
                                     )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 duration, cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                                   bdate = bdate,
                                   edate = edate,
                                   stateFIPS = stateFIPS,
                                   countycode = countycode,
                                   duration = duration,
                                   service = "sampleData",
                                   cbdate = cbdate,
                                   cedate = cedate
                              )

  sampledata <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) sampledata %<>% aqs_removeheader
  return(sampledata)
}


#' @title aqs_annualsummary_by_county
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where annual data is
#'                 aggregated at the county level. Returned is an annual summary
#'                 matching the input parameter, stateFIPS, and county_code
#'                 provided for bdate - edate time frame. Variables
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If
#'                        TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the countycode and stateFIPS requested.
#'           A AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns an aqs S3 object with annual summary FRM/FEM
#'           #  PM2.5 data for Wake County, NC between January
#'           #  and February 2016
#'  \dontrun{aqs_annualsummary_by_county(parameter = "88101",
#'                                       bdate = as.Date("20160101",
#'                                                       format = "%Y%m%d"),
#'                                        edate = as.Date("20180228",
#'                                                       format = "%Y%m%d"),
#'                                        stateFIPS = "37",
#'                                        countycode = "183"
#'                                        )
#'          }
#' @export
aqs_annualsummary_by_county <- function(parameter, bdate, edate, stateFIPS,
                                        countycode, cbdate = NA_Date_,
                                        cedate = NA_Date_, return_header = FALSE
                                        )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "annualData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_qa_blanks_by_county
#' @description \lifecycle{stable}
#'        Returns a table of blank quality assurance data.
#'        Blanks are unexposed sample collection devices (e.g.,
#'        filters) that are transported with the exposed sample devices
#'        to assess if contamination is occurring during the transport
#'        or handling of the samples. Data is aggregated at the county level.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If
#'                        TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
#'           assurance blank sample data for all monitors within the input
#'           stateFIPS and countycode. An AQS_Data_Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # returns a tibble with PM2.5 blank data for
#'           #  Colbert County, AL for January 2018
#'    \dontrun{aqs_qa_blanks_by_county(parameter = "88101",
#'                                     bdate = as.Date("20170101",
#'                                                    format="%Y%m%d"),
#'                                     edate = as.Date("20190131",
#'                                                     format="%Y%m%d"),
#'                                     stateFIPS = "01",
#'                                     countycode = "033"
#'                                    )
#'            }
#' @export
aqs_qa_blanks_by_county <- function(parameter, bdate, edate, stateFIPS,
                                    countycode, cbdate = NA_Date_,
                                    cedate = NA_Date_, return_header = FALSE
                                              )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaBlanks",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  blanks <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) blanks %<>% aqs_removeheader
  return(blanks)
}


#' @title aqs_dailysummary_by_county
#' @description \lifecycle{stable}
#'        Returns multiple years of data where daily data is
#'        aggregated at the site level. Returned is a daily summary
#'        matching the input parameter, stateFIPS and county_code
#'        provided for bdate - edate time frame. Variables returned include
#'        mean value, maxima, percentiles, and etc.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains daily
#'           summary statistics for the given parameter for a single countycode
#'           and stateFIPS combination. An AQS_Data Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # returns an aqs S3 object of daily summary FRM/FEM PM2.5 data
#'           #  for Wake County, NC between January and February 2016
#'  \dontrun{aqs_dailysummary_by_county(parameter = "88101",
#'                                      bdate = as.Date("20160101",
#'                                                    format = "%Y%m%d"),
#'                                      edate = as.Date("20170228",
#'                                                      format = "%Y%m%d"),
#'                                      stateFIPS = "37",
#'                                      countycode = "183"
#'                                     )
#'          }
#' @export
aqs_dailysummary_by_county <- function(parameter, bdate, edate, stateFIPS,
                                       countycode, cbdate = NA_Date_,
                                       cedate = NA_Date_, return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "dailyData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_qa_collocated_assessments_by_county
#' @description \lifecycle{stable}
#'                Returns a table of collocated assessment data aggregated by
#'                matching input parameter, stateFIPS and county_code provided
#'                for bdate - edate time frame.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a county.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # Returns a tibble with collocated assessment data
#'           #  for FRM PM2.5 in Madison County, AL for January 2015
#'  \dontrun{aqs_qa_collocated_assessments_by_county(parameter = "88101",
#'                                                   bdate = as.Date("20150101",
#'                                                             format = "%Y%m%d"
#'                                                                  ),
#'                                                   edate = as.Date("20150131",
#'                                                             format = "%Y%m%d"
#'                                                                  ),
#'                                                   stateFIPS = "01",
#'                                                   countycode = "089"
#'                                                   )
#'          }
#' @export
aqs_qa_collocated_assessments_by_county <- function(parameter, bdate, edate,
                                                    stateFIPS, countycode,
                                                    cbdate = NA_Date_,
                                                    cedate = NA_Date_,
                                                    return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaCollocatedAssessments",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  colocatedsummary <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) colocatedsummary %<>% aqs_removeheader
  return(colocatedsummary)
}


#' @title aqs_qa_flowrateverification_by_county
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate Verification data for a
#'                parameter code aggregated matching input parameter, stateFIPS,
#'                and county_code, provided for bdate - edate time frame.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance flow rate verification data for monitors within a county.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of flow rate verification data for
#'           #  Colbert County, AL for January 2018
#'   \dontrun{aqs_qa_flowrateverification_by_county(parameter = "88101",
#'                                                  bdate = as.Date("20180101",
#'                                                             format = "%Y%m%d"
#'                                                                  ),
#'                                                  edate = as.Date("20190131",
#'                                                             format = "%Y%m%d"
#'                                                                 ),
#'                                                  stateFIPS = "01",
#'                                                  countycode = "033"
#'                                                    )
#'            }
#' @export
aqs_qa_flowrateverification_by_county <- function(parameter, bdate, edate,
                                                  stateFIPS, countycode,
                                                  cbdate = NA_Date_,
                                                  cedate = NA_Date_,
                                                  return_header = FALSE
                                                  )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

     params <- aqsmultiyearparams(parameter = parameter,
                                  bdate = bdate,
                                  edate = edate,
                                  stateFIPS = stateFIPS,
                                  countycode = countycode,
                                  service = "qaFlowRateVerifications",
                                  cbdate = cbdate,
                                  cedate = cedate
                                  )

  frv <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) frv %<>% aqs_removeheader
  return(frv)
}


#' @title aqs_qa_flowrateaudit_by_county
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate audit
#'                data aggregated by parameter code, stateFIPS and countycode
#'                for bdate - edate time frame.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing flow rate
#'           audit data for the requested countycode and stateFIPS. An
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item ($Header) is a tibble of header information from the
#'           AQS API and the second item ($Data) is a tibble of the data
#'           returned.
#' @examples #Returns a tibble of flow rate audit data for
#'           #  Jefferson County, AL for January 2018
#'    \dontrun{aqs_qa_flowrateaudit_by_county(parameter = "88101",
#'                                            bdate = as.Date("20170101",
#'                                                          format="%Y%m%d"),
#'                                            edate = as.Date("20190131",
#'                                                          format = "%Y%m%d"),
#'                                            tateFIPS = "01",
#'                                            countycode = "073"
#'                                            )
#'            }
#' @export
aqs_qa_flowrateaudit_by_county <- function(parameter, bdate, edate, stateFIPS,
                                           countycode, cbdate = NA_Date_,
                                           cedate = NA_Date_, return_header
                                           )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaFlowRateAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  fra <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) fra %<>% aqs_removeheader
  return(fra)
}


#' @title aqs_qa_one_point_qc_by_county_
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3 object
#'                containing one point QC check data aggregated by county_code.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing one point
#'            qc data within a county. A AQS_Data_Mart_APIv2 object is a
#'            2 item named list in which the first item ($Header) is a tibble
#'            of header information from the AQS API and the second item
#'            ($Data) is a tibble of the data returned.
#' @examples #returns a tibble of One Point QC data for ozone
#'           #  in Barnstable County, MA for January 2018
#'   \dontrun{aqs_qa_one_point_qc_by_county(parameter= "44201",
#'                                          bdate = as.Date("20170101",
#'                                                        format = "%Y%m%d"),
#'                                          edate = a s.Date("20180131",
#'                                                          format = "%Y%m%d"),
#'                                          stateFIPS = "25",
#'                                          countycode = "001"
#'                                          )
#'            }
#' @export
aqs_qa_one_point_qc_by_county <- function(parameter, bdate, edate, stateFIPS,
                                          countycode, cbdate = NA_Date_,
                                          cedate = NA_Date_,
                                          return_header = FALSE
                                          )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaOnePointQcRawData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  opqcc <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) opqcc %<>% aqs_removeheader
  return(opqcc)
}


#' @title aqs_qa_pep_audit_by_county
#' @description \lifecycle{stable}
#'                Returns a table of Performance Evaluation Program (PEP) audit
#'                data aggregated by parameter code, stateFIPS and countycode
#'                for the time frame between bdate and edate.
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
#' @family Aggregate _by_county functions
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @inheritParams aqs_services_by_county
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance PEP audit data within a county. A AQS_Data_Mart_APIv2
#'           object is a 2 item named list in which the first item ($Header) is
#'           a tibble of header information from the AQS API and the second item
#'           ($Data) is a tibble of the data returned.
#' @examples # returns a tibble with PEP Audit data for FRM
#'           #  PM2.5 in Madison County, AL for 2017
#'  \dontrun{aqs_qa_pep_audit_by_county_multiyear(parameter = "88101",
#'                                                bdate = as.Date("20150101",
#'                                                          format = "%Y%m%d"
#'                                                               ),
#'                                                edate = as.Date("20171231",
#'                                                            format = "%Y%m%d"
#'                                                               ),
#'                                                stateFIPS = "01",
#'                                                countycode = "089"
#'                                                )
#'           }
#' @export
aqs_qa_pep_audit_by_county <- function(parameter, bdate, edate, stateFIPS,
                                       countycode, cbdate = NA_Date_,
                                        cedate = NA_Date_, return_header = FALSE
                                      )
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaPepAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  pepaudit <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) pepaudit %<>% aqs_removeheader
  return(pepaudit)
}


#' @title aqs_transactionsample_by_county
#' @description \lifecycle{stable}
#'         Returns transactionsample data - aggregated by county
#'            in the AQS Submission Transaction Format (RD) sample (raw) data
#'            for a parameter code aggregated by matching input parameter,
#'            stateFIPS and countycode provided for bdate - edate time frame.
#'            Includes data both in submitted and standard units
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples
#'          \dontrun{ #   Returns all FRM/FEM transaction data for
#'                    #   Wake County, NC between on Feb 28, 2016.
#'                    aqs_transactionsample_by_county(parameter = "88101",
#'                                                  bdate = as.Date("20160228",
#'                                                          format = "%Y%m%d"),
#'                                                  edate = as.Date("20160228",
#'                                                           format = "%Y%m%d"),
#'                                                  stateFIPS = "37",
#'                                                  countycode = "183"
#'                                                  )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of transaction sample
#'           (raw) data in the AQS submission transaction format (RD)
#'           corresponding to the inputs provided.
#' @export
aqs_transactionsample_by_county <- function(parameter, bdate, edate,
                                            stateFIPS, countycode,
                                            return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "transactionsSample"
                               )

  transactionsample <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) transactionsample %<>% aqs_removeheader
  return(transactionsample)
}


#' @title aqs_qa_annualperformanceeval_by_county
#' @description \lifecycle{stable}
#'          Returns AQS submissions transaction format (RD) of the annual
#'             performance evaluation data (raw). Includes data pairs for
#'             QA - aggregated by county for a parameter code aggregated by
#'             matching input parameter, countycode and stateFIPS
#'             provided for bdate - edate time frame.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation data
#'           # (raw) for ozone in Baldwin County, AL for 2017 in RD format.
#'  \dontrun{ aqs_qa_annualperformanceeval_by_county(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                            format = "%Y%m%d"),
#'                                                   stateFIPS = "01",
#'                                                   countycode = "003"
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
aqs_qa_annualperformanceeval_by_county <- function(parameter, bdate,
                                                              edate, stateFIPS,
                                                              countycode,
                                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "qaAnnualPerformanceEvaluations"
                               )

  qaape <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) qaape %<>% aqs_removeheader
  return(qaape)
}


#' @title aqs_qa_annualperformanceevaltransaction_by_site
#' @description \lifecycle{stable}
#'        Returns AQS submissions transaction format (RD) of the annual
#'          performance evaluation data (raw). Includes data pairs for
#'          QA - aggregated by site for a parameter code aggregated by matching
#'          input parameter, countycode and stateFIPS provided for
#'          bdate - edate time frame.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_county
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples # Returns a tibble containing annual performance evaluation data
#'           # (raw) for ozone in Baldwin County, AL for 2017 in RD format.
#'  \dontrun{aqs_qa_annualperformanceevaltransaction_by_county(parameter = "44201",
#'                                                   bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                   edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                   stateFIPS = "01",
#'                                                   countycode = "003"
#'                                                   )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of quality assurance
#'           performance evaluation data in the RD format for a single
#'           monitoring site for the countycode and stateFIPS requested
#'           for the time frame between bdate and edate in the AQS. An
#'           AQS_Data_Mart_APIv2 is a 2 item named list in which the first item
#'           ($Header) is a tibble of header information from the AQS API and
#'           the second item ($Data) is a tibble of the data returned.
#' @export
aqs_qa_annualperformanceevaltransaction_by_county <- function(parameter, bdate,
                                                              edate, stateFIPS,
                                                              countycode,
                                                          return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "transactionsQaAnnualPerformanceEvaluations"
                               )

  tqaape <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) tqaape %<>% aqs_removeheader
  return(tqaape)
}


#' @title aqs_quarterlysummary_by_county
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object of quarterly summary data aggregated by cbsa
#'                (Core Based Statistical Area) code.
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
#' @family Aggregate _by_county functions
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains quarterly
#'           summary statistics for the given parameter for a single countycode
#'           and stateFIPS combination. An AQS_Data Mart_APIv2 is a 2 item named
#'           list in which the first item ($Header) is a tibble of header
#'           information from the AQS API and the second item ($Data) is a
#'           tibble of the data returned.
#' @examples # returns a tibble containing quarterly summaries for
#'           #  FRM/FEM PM2.5 data for Wake County, NC for each quarter of 2016
#'  \dontrun{aqs_quarterlysummary_by_county(parameter = "88101",
#'                                          bdate = as.Date("20160101",
#'                                                           format = "%Y%m%d"),
#'                                          edate = as.Date("20170228",
#'                                                           format = "%Y%m%d"),
#'                                          stateFIPS = "37",
#'                                          countycode = "183"
#'                                         )
#'          }
#' @export
aqs_quarterlysummary_by_county <- function(parameter, bdate, edate, stateFIPS,
                                       countycode, cbdate = NA_Date_,
                                       cedate = NA_Date_, return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, stateFIPS, countycode,
                 cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               service = "quarterlyData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  quarterlysummary <- purrr::pmap(.l = params, .f = aqs_services_by_county)
  if (!return_header) quarterlysummary %<>% aqs_removeheader
  return(quarterlysummary)
}
