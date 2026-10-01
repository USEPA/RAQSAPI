## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecated
## usethis namespace: end

#' @importFrom lubridate today year mdy '%within%' NA_Date_
#' @importFrom lifecycle deprecate_soft badge

#' @title RAQSAPI: A R Interface to The United States Environmental Protection
#' Agency's Air Quality System Data Mart RESTful API server
#'
#' @description `r lifecycle::badge("maturing")`
#' RAQSAPI is a package for R that connects
#' the R programming environment to the United States Environmental protection
#' agency's Air Quality System (AQS) Data Mart API for retrieval of air
#' monitoring data.
#'
#' There are two things that you must do before using this package.
#' 1) If you have not done so yet register your username with Data Mart
#' 2) Every time this library is reloaded AQS_API_credentials() function
#'       must be called before continuing.
#'
#' please use \code{vignette(RAQSAPI)} for more details about this package.
#'
#' EPA Disclaimer:
#' This software/application was developed by the U.S. Environmental Protection
#' Agency (USEPA). No warranty expressed or implied is made regarding the
#' accuracy or utility of the system, nor shall the act of distribution
#' constitute any such warranty. The USEPA has relinquished control of the
#' information and no longer has responsibility to protect the integrity,
#' confidentiality or availability of the information. Any reference to specific
#' commercial products, processes, or services by service mark, trademark,
#' manufacturer, or otherwise, does not constitute or imply their endorsement,
#' recommendation or favoring by the USEPA. The USEPA seal and logo shall not
#' be used in any manner to imply endorsement of any commercial product or
#' activity by the USEPA or the United States Government.
#'
#' @docType package
#' @name RAQSAPI
#' @keywords internal
"_PACKAGE"


#' @title RAQSAPI_parameters
#' @description null
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @param .AQSobject A 2 item named list in which the first item ($Header) is a
#'              tibble of header information from the AQS API and the second
#'              item ($Data) is a tibble of the data returned.
#' @param bdate a R date object which represents the begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param cbdate a R date object which represents a 'beginning
#'                   date of last change' that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cbsa_code a R character object which represents the 5 digit AQS Core
#'                   Based Statistical Area code (the same as the census code,
#'                   with leading zeros). Use [RAQSAPI::aqs_cbsas()]
#'                   for a list of all CBSA codes and names available,
#' @param cedate a R date object which represents an 'end
#'                   date of last change' that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param countycode a R character object which represents the 3 digit state
#'                       FIPS code for the county being requested (with leading
#'                       zero(s)). Use [RAQSAPI::aqs_counties_by_state()]
#'                       for the list of available county codes in each state.
#' @param delimiter a string that should be used to separate variables
#'                   in the return value.
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages. Use
#'                           [RAQSAPI::aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param edate a R date object which represents the end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param email a string which represents the parameter code of the air
#'                   pollutant related to the data being requested.
#' @param filter a character string representing the filter being applied
#' @param MA_code a R character object which represents the 4 digit AQS
#'                    Monitoring Agency code (with leading zeroes). Use
#'                    [RAQSAPI::aqs_mas()] for a list of all
#'                  Monitoring Agency (MA) codes and names available,
#' @param maxlat a R character object which represents the maximum latitude of
#'                   a geographic box. Decimal latitude with north being
#'                   positive. Only data south of this latitude will be
#'                   returned.
#' @param maxlon a R character object which represents the maximum longitude
#'                   of a geographic box. Decimal longitude with east begin
#'                   positive. Only data west of this longitude will be
#'                   returned. Note that -80 is less than -70.
#' @param minlat a R character object that represents the minimum latitude of
#'                   a geographic box.  Decimal latitude with north being
#'                   positive. Only data north of this latitude will be
#'                   returned.
#' @param minlon a R character object which represents the minimum longitude
#'                   of a geographic box. Decimal longitude with east begin
#'                   positive. Only data east of this longitude will be
#'                   returned.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param pqao_code a R character object which represents the 4 digit AQS
#'                   Primary Quality Assurance Organization code
#'                   (with leading zeroes). Use [RAQSAPI::aqs_pqaos()] for a
#'                   list of all Primary Quality Assurance Organization (pqao)
#'                   codes and names available,
#' @param service a string which represents the services provided by the AQS
#'                API. For a list of available services Refer to
#'                \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param sitenum a R character object which represents the 4 digit site number
#'                 (with leading zeros) within the county and state being
#'                 requested. Use [RAQSAPI::aqs_sites_by_county()]
#'                for the list of available site numbers in  given county and
#'                state.
#' @param stateFIPS a R character object which represents the 2 digit state
#'                   FIPS code (with leading zero) for the state being
#'                   requested. Use [RAQSAPI::aqs_states()] for the list of
#'                   available FIPS codes.
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @name RAQSAPI_parameters
#' @keywords internal
NULL
