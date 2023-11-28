#' @section by_cbsa (By Core Based Statistical Area, as defined by the US Census Bureau) aggregate functions


#' @title aqs_monitors_by_cbsa
#' @description \lifecycle{stable}
#'  Returns a table of monitors at all sites with the provided
#'    parameter, aggregated by Core Based Statistical Area (CBSA) for
#'    bdate - edate time frame.
#' @note All monitors that operated between the bdate and edate will be returned
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item list
#'                        that contains header information returned from the
#'                        API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @examples # returns a tibble of $NO_{2}$ monitors
#'           #  for Charlotte-Concord-Gastonia, NC cbsa that were operating
#'           #  on Janurary 01, 2017
#'           \dontrun{aqs_monitors_by_cbsa(parameter="42602",
#'                                                bdate=as.Date("20170101",
#'                                                            format="%Y%m%d"),
#'                                                edate=as.Date("20170101",
#'                                                             format="%Y%m%d"),
#'                                                cbsa_code="16740"
#'                                                    )
#'                    }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that is the return value
#'            from the AQS API. A AQS_Data Mart_APIv2 object is a 2 item named
#'            list in which the first item ($Header) is a tibble of header
#'            information from the AQS API and the second item ($Data) is a
#'            tibble of the data returned.
#' @export
aqs_monitors_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                    cbdate = NA_Date_, cedate = NA_Date_,
                                    return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, cbsa_code, cbdate, cedate,
                 return_header)
 # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::pmap
  #  is used so that the output is consistent with other RAQSAPI functions.
  params <- tibble(parameter = parameter,
                   bdate = bdate,
                   edate = edate,
                   cbsa_code = cbsa_code,
                   service = "monitors",
                   cbdate = cbdate,
                   cedate = cedate) %>%
     dplyr::select_if(function(x) {!all(is.na(x))})

  monitors <- purrr::pmap(.l = params, .f = aqs_services_by_cbsa)
  if (!return_header) monitors %<>% aqs_removeheader
  return(monitors)
}


#' @title aqs_sampledata_by_cbsa
#' @description \lifecycle{stable}
#'                 Returns sample data where the data is aggregated at the Core
#'                 Based Statistical Area (cbsa) level. If return_header is
#'                 FALSE (default) this function returns a single dataframe with
#'                 the requested data. If return_header is TRUE returns a list
#'                 of AQSAPI_v2 objects where each index of the list is an
#'                 individual RAQSAPI_v2 object returned from each successive
#'                 call to the AQS API. RAQSAPI_v2 objects are two item list
#'                 where the $Data portion contains data that contains
#'                 sample air monitoring data at a site with the input
#'                 parameter and cbsa_code provided for
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
#'         API server. Fortunately this operation has a linear run time
#'         /(Big O notation: O/(n + 5 seconds/)/)
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @importFrom purrr pmap
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data
#'           for all monitors matching cbsa_code for the given parameter. An
#'           AQS_Data Mart_APIv2 is a 2 item named list in which the first item
#'           /(/$Header/) is a tibble of header information from the AQS API and
#'           the second item /(/$Data/) is a tibble of the data returned.
#' @examples # returns tibble which contains $NO_{2}$ data
#'           #  for Charlotte-Concord-Gastonia, NC cbsa for
#'           #  Janurary 1, 2015 - Janurary 01, 2017
#'           \dontrun{aqs_sampledata_by_cbsa(parameter = "42602",
#'                                           bdate = as.Date("20150101",
#'                                                           format = "%Y%m%d"),
#'                                           edate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                           cbsa_code = "16740"
#'                                          )
#'                    }
#' @export
aqs_sampledata_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                   duration = NA_character_,
                                   cbdate = NA_Date_, cedate = NA_Date_,
                                   return_header = FALSE
                                   )
{
  checkaqsparams(parameter, bdate, edate, cbsa_code, duration, cbdate,
                 cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               cbsa_code = cbsa_code,
                               duration = duration,
                               service = "sampleData",
                               cbdate = cbdate,
                               cedate = cedate
                              )

  sampledata <- purrr::pmap(.l = params, .f = aqs_services_by_cbsa)
  if (!return_header) sampledata %<>% aqs_removeheader
  return(sampledata)
}


#' @title aqs_annualsummary_by_cbsa
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where annual data is
#'                 aggregated at the Core Based Statistical Area (CBSA) level.
#'                 Returned is an annual summary
#'                 matching the input parameter, and cbsa_code
#'                 provided for bdate - edate time frame. Variables returned
#'                 include mean value, maxima, percentiles, and etc. If
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
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the cbsa_code requested. A AQS_Data Mart_APIv2 is
#'           a 2 item named list in which the first item ($Header) is a tibble
#'           of header information from the AQS API and the second item ($Data)
#'           is a tibble of the data returned.
#' @examples # Returns a tibble of annual summary $NO_{2}$
#'           #  data the for Charlotte-Concord-Gastonia, NC cbsa on
#'           #  Janurary 01, 2017
#'           \dontrun{aqs_annualsummary_by_cbsa(parameter = "42602",
#'                                              bdate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                             ),
#'                                              edate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                             ),
#'                                              cbsa_code = "16740"
#'                                              )
#'                    }
#' @export
aqs_annualsummary_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                      cbdate = NA_Date_, cedate = NA_Date_,
                                      return_header = FALSE
                                      )
{
  checkaqsparams(parameter, bdate, edate, cbsa_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               cbsa_code = cbsa_code,
                               service = "annualData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  annualsummary <- purrr::pmap(.l = params, .f = aqs_services_by_cbsa)
  if (!return_header) annualsummary %<>% aqs_removeheader
  return(annualsummary)

}


#' @title aqs_dailysummary_by_cbsa
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing daily summary data aggregated by cbsa
#'                (Core Based Statistical Area) code.
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
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains daily
#'           summary statistics for the given parameter for a single cbsa_code.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # Returns a tibble of $NO_{2}$ daily summary
#'           #  data the for Charlotte-Concord-Gastonia, NC cbsa on
#'           #  Janurary 01, 2017
#'           \dontrun{aqs_dailysummary_by_cbsa(parameter = "42602",
#'                                                bdate = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                               ),
#'                                                edate = as.Date("20190101",
#'                                                             format = "%Y%m%d"
#'                                                               ),
#'                                                cbsa_code = "16740"
#'                                            )
#'                    }
#' @export
aqs_dailysummary_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                     cbdate = NA_Date_, cedate = NA_Date_,
                                     return_header = FALSE
                                     )
{
  checkaqsparams(parameter, bdate, edate, cbsa_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               cbsa_code = cbsa_code,
                               service = "dailyData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_cbsa)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_quarterlysummary_by_cbsa
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
#' @inheritParams aqs_services_by_cbsa
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
#' @examples # Returns a tibble of $NO_{2}$ quartyerly summary
#'           #  data the for Charlotte-Concord-Gastonia, NC cbsa for
#'           #  each quarter in 2017.
#'           \dontrun{aqs_quarterlysummary_by_cbsa(parameter = "42602",
#'                                                 bdate = as.Date("20170101",
#'                                                           format = "%Y%m%d"),
#'                                                 edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                 cbsa_code = "16740"
#'                                                 )
#'                    }
#' @export
aqs_quarterlysummary_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                         cbdate = NA_Date_, cedate = NA_Date_,
                                         return_header = FALSE)
{
  AQS_domain <- "aqs.epa.gov"
  checkaqsparams(parameter, bdate, edate, cbsa_code, cbdate, cedate,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               cbsa_code = cbsa_code,
                               service = "quarterlyData",
                               cbdate = cbdate,
                               cedate = cedate,
                               AQS_domain = AQS_domain
                               )

  quarterlysummary <- purrr::pmap(.l = params, .f = aqs_services_by_cbsa)
  if (!return_header) quarterlysummary %<>% aqs_removeheader
  return(quarterlysummary)
}
