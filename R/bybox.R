#' @section by_box aggregate functions


#' @title aqs_monitors_by_box
#' @description \lifecycle{stable}
#'  Returns a table of monitors and related metadata sites with the provided
#'    parameter, aggregated by latitude/longitude bounding box (_by_box) for
#'    bdate - edate time frame.
#' @note All monitors that operated between the bdate and edate will be returned
#' @family Aggregate _by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from a
#'           latitude/longitude bounding box (_by_box).
#' @examples #  Returns a tibble of all ozone
#'           #  monitors in the vicinity of central Alabama that operated in
#'           #  1995
#'           \dontrun{aqs_monitors_by_box(parameter="44201",
#'                                        bdate=as.Date("19950101",
#'                                                      format="%Y%m%d"),
#'                                        edate=as.Date("19951231",
#'                                                      format="%Y%m%d"),
#'                                        minlat="33.3",
#'                                        maxlat="33.6",
#'                                        minlon="-87.0",
#'                                        maxlon="-86.7"
#'                                        )
#'                    }
#' @export
aqs_monitors_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                    minlon, maxlon, return_header = FALSE)
{
  checkaqsparams(parameter, bdate, edate, minlat, maxlat, minlon, maxlon,
                 return_header)

  # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::map
  #  is used so that the output is consistent with other RAQSAPI functions.
  params <- tibble(parameter = parameter,
                   bdate = bdate,
                   edate = edate,
                   minlat = minlat,
                   maxlat = maxlat,
                   minlon = minlon,
                   maxlon = maxlon,
                   service = "monitors"
                   ) %>%
     dplyr::select_if(function(x) {!all(is.na(x))})

  monitors <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) monitors %<>% aqs_removeheader
  return(monitors)
}


#' @title aqs_sampledata_by_box
#' @description \lifecycle{stable}
#'                 Returns sample data where the data is aggregated by
#'                 latitude/longitude bounding box (_by_box).
#'                 If return_header is FALSE (default) this function returns a
#'                 single dataframe with the requested data. If return_header
#'                 is TRUE returns a list of AQSAPI_v2 objects where each index
#'                 of the list is an individual RAQSAPI_v2 object returned from
#'                 each successive call to the AQS API. RAQSAPI_v2 objects are
#'                 two item list where the $Data portion contains data that
#'                 contains sample air monitoring data at a site with the input
#'                 parameter and cbsa_code provided for bdate - edate time
#'                 frame. The $Header is a tibble of header information from the
#'                 API call /(useful for debugging/). This function returns NULL
#'                 is bdate > edate.
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
#' @family Aggregate _by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @importFrom purrr pmap
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data
#'           for all monitors within the input latitude/longitude bounding box
#'           for a single parameter. An AQS_Data Mart_APIv2 is a 2 item named
#'           list in which the first item /(/$Header/) is a tibble of header
#'           information from the AQS API and the second item /(/$Data/) is a
#'           tibble of the data returned.
#' @examples # Returns a tibble containing all ozone samples
#'              #  in the vicinity of central Alabama between
#'              #  May 1, 2015 - May 2, 2017
#'           \dontrun{aqs_sampledata_by_box(parameter = "44201",
#'                                          bdate = as.Date("20150501",
#'                                                          format = "%Y%m%d"),
#'                                          edate = as.Date("20170502",
#'                                                          format = "%Y%m%d"),
#'                                          minlat = "33.3",
#'                                          maxlat = "33.6",
#'                                          minlon = "-87.0",
#'                                          maxlon = "-86.7"
#'                                          )
#'                    }
#' @export
aqs_sampledata_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                  minlon, maxlon,
                                  duration = NA_character_,
                                  cbdate = NA_Date_, cedate = NA_Date_,
                                  return_header = FALSE
                                  )
{
  checkaqsparams(parameter, bdate, edate, minlat, maxlat, minlon, maxlon,
                 duration, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               minlat = minlat,
                               maxlat = maxlat,
                               minlon = minlon,
                               maxlon = maxlon,
                               duration = duration,
                               service = "sampleData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  sampledata <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) sampledata %<>% aqs_removeheader
  return(sampledata)
}


#' @title aqs_annualsummary_by_box
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where annual data is
#'                 aggregated at the bounding box level. Returned is an annual
#'                 summary within the input parameter, latitude/longitude
#'                 bounding box provided for bdate - edate time frame. Variables
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
#' @family Aggregate _by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the box (area) requested. A AQS_Data Mart_APIv2
#'           is a 2 item named list in which the first item ($Header) is a
#'           tibble of header information from the AQS API and the second item
#'           ($Data) is a tibble of the data returned.
#' @examples # Returns a tibble containing ozone annual summaries
#'           #  in the vicinity of central Alabama for the first two days
#'           # of May, 2015
#'           \dontrun{aqs_annualsummary_by_box(parameter = "44201",
#'                                             bdate = as.Date("20150501",
#'                                                           format = "%Y%m%d"),
#'                                             edate = as.Date("20170502",
#'                                                           format = "%Y%m%d"),
#'                                             minlat = "33.3",
#'                                             maxlat = "33.6",
#'                                             minlon = "-87.0",
#'                                             maxlon = "-86.7"
#'                                             )
#'                    }
#' @export
aqs_annualsummary_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                     minlon, maxlon,
                                     cbdate = NA_Date_,
                                     cedate = NA_Date_,
                                     return_header = FALSE
                                     )
{
  checkaqsparams(parameter, bdate, edate, minlat, maxlat, minlon, maxlon,
                 return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               minlat = minlat,
                               maxlat = maxlat,
                               minlon = minlon,
                               maxlon = maxlon,
                               service = "annualData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  annualsummary <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) annualsummary %<>% aqs_removeheader
  return(annualsummary)

}


#' @title aqs_dailysummary_by_box
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing daily summary data bounded within a
#'                latitude/longitude bounding box
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
#' @family Aggregate_by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object that contains daily
#'           summary statistics for the given parameter for an area bounded
#'           within a latitude/longitude bounding box. An AQS_Data Mart_APIv2 is
#'           a 2 item named list in which the first item ($Header) is a tibble
#'           of header information from the AQS API and the second item ($Data)
#'           is a tibble of the data returned.
#' @examples #Returns a tibble of ozone daily summaries in the vicinity of
#'           #  central Alabama for the first two days of May 2015
#'
#'           \dontrun{aqs_dailysummary_by_box(parameter = "44201",
#'                                            bdate = as.Date("20140501",
#'                                                            format = "%Y%m%d"
#'                                                           ),
#'                                             edate = as.Date("20160502",
#'                                                             format = "%Y%m%d"
#'                                                             ),
#'                                             mqinlat ="33.3",
#'                                             maxlat = "33.6",
#'                                             minlon = "-87.0",
#'                                             maxlon = "-86.7"
#'                                             )
#'                    }
#' @export
aqs_dailysummary_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                    minlon, maxlon, cbdate = NA_Date_,
                                    cedate = NA_Date_, return_header = FALSE
                                    )
{
  checkaqsparams(parameter, bdate, edate, minlat, maxlat, minlon, maxlon,
                 return_header)

    params <- aqsmultiyearparams(parameter = parameter,
                                 bdate = bdate,
                                 edate = edate,
                                 minlat = minlat,
                                 maxlat = maxlat,
                                 minlon = minlon,
                                 maxlon = maxlon,
                                 service = "dailyData",
                                 cbdate = cbdate,
                                 cedate = cedate
                                 )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_quarterlysummary_by_box
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object of quarterly summary data aggregated by and area within
#'                a latitude/longitude bounding box.
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
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains quarterly
#'           summary statistics for an area within a latitude/longitude
#'           bounding box. An AQS_Data Mart_APIv2 is a 2 item named list in
#'           which the first item ($Header) is a tibble of header information
#'           from the AQS API and the second item ($Data) is a tibble of the
#'           data returned.
#' @examples # Returns a tibble containing ozone quarterly summaries
#'           #  in the vicinity of central Alabama for each quarter in
#'           #  between 2015 - 2017
#'           \dontrun{aqs_quarterlysummary_by_box(parameter = "44201",
#'                                                bdate = as.Date("20150101",
#'                                                           format = "%Y%m%d"),
#'                                                edate = as.Date("20171231",
#'                                                           format = "%Y%m%d"),
#'                                                minlat = "33.3",
#'                                                maxlat = "33.6",
#'                                                minlon = "-87.0",
#'                                                maxlon = "-86.7"
#'                                               )
#'                    }
#' @export
aqs_quarterlysummary_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                        minlon, maxlon, cbdate = NA_Date_,
                                        cedate = NA_Date_, return_header = FALSE
                                       )
{
  AQS_domain <- "aqs.epa.gov"
  checkaqsparams(parameter, bdate, edate, minlat, maxlat,
                 minlon, maxlon, cbdate, cedate, return_header)

  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               minlat = minlat,
                               maxlat = maxlat,
                               minlon = minlon,
                               maxlon = maxlon,
                               service = "quarterlyData",
                               cbdate = cbdate,
                               cedate = cedate,
                               AQS_domain = AQS_domain
                               )

  quarterlysummary <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) quarterlysummary %<>% aqs_removeheader
  return(quarterlysummary)
}
