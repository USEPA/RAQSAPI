user_agent <- "RAQSAPI library for R"
server <- "AQSDatamartAPI"


#' @title checkaqsparams
#' @description \lifecycle{experimental}
#'              a helper function used to check the validity of parameters being
#'                sent to the API
#' @param  ... variables to be checked. Must be one of the parameters
#'                         being exported RAQSAPI functions. All other
#'                         variables will be ignored.
#' @note  This function will only check the variables exported by RAQSAPI meant
#'           to be used in RAQSAPI functions. This function is not meant to be
#'           called directly by the end user or to be called outside of RAQSAPI.
#'           variables will remain unchanged.
#'        Escaping of variables is provided by the AQS DataMart API, this
#'          function is provided to assist users from receiving confusing
#'          results from incorrect parameters but should not be used to verify
#'          that parameters are properly escaped.
#' @importFrom lubridate is.Date
#' @importFrom rlang abort format_error_bullets call_name
#' @importFrom dplyr between
#' @importFrom glue glue
#' @importFrom magrittr `%<>%`
#' @importFrom stringr str_detect
#' @return NULL
#' @noRd
checkaqsparams <- function(...)
{
  #Note: the variable errmessage does not follow this project's style
  #  This is to ensure that the output string format looks nice.
  errmessage <- vector()
  error <- FALSE
  ellipsis_args <- list(...)
  names(ellipsis_args) <- names(match.call(expand.dots = FALSE)$...)

  if ("parameter" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$parameter) != 5 |
        !is.character(ellipsis_args$parameter) |
        !str_detect(ellipsis_args$parameter, "^[:digit:]+$")
        )
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "parameter must be a 5 digit number (represented as a character string)"
                        )
    }
  }

  if ("stateFIPS" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$stateFIPS) != 2 |
        !is.character(ellipsis_args$stateFIPS)
        )
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "stateFIPS must be a two digit number (represented as a character string),
       please pad stateFIPS less than 2 digits with leading zeros"
                        )
    }
  }

  if ("countycode" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$countycode) != 3 |
        !is.character(ellipsis_args$countycode))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "countycode must be a three digit number (represented as a character
      string), please pad countycode less than three digits with leading zeros"
                        )
    }
  }

  if ("sitenum" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$sitenum) != 4 |
        !is.character(ellipsis_args$sitenum))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "sitenum must be a four digit number (represented as a character string),
       please pad sitenum less than four digits with leading zeros"
                       )
          }
  }

  if ("MA_code" %in% names(ellipsis_args))
  {
    if ((nchar(ellipsis_args$MA_code) != 4 |
         nchar(ellipsis_args$MA_code) != 3) |
        !is.character(ellipsis_args$MA_code))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "MA_code must be a three or four digit number (represented as a character
       string), please pad MA_code less than three or four digits with
       leading zeros"
                        )
    }
  }

  if ("pqao_code" %in% names(ellipsis_args))
  {
    if ((nchar(ellipsis_args$pqao_code) != 4 |
         nchar(ellipsis_args$pqao_code) != 3) |
        !is.character(ellipsis_args$pqao_code))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "pqao_code must be a three or four digit number (represented as a
      character string), please pad pqao_code less than three or four digits
      with leading zeros"
                        )
    }
  }
  if ("cbsa_code" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$cbsa_code) != 5 |
        !is.character(ellipsis_args$cbsa_code))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "cbsa_code must be a five digit number (represented as a character
      string), please pad cbsa_code less than five digits with leading zeros"
                        )
    }
  }
  if ("POC" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$POC) != 1 |
        !is.character(ellipsis_args$POC))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "POC must be a single digit number (represented as a character string)"
                       )
    }
  }
  if ("bdate" %in% names(ellipsis_args))
  {
    if (!is.Date(ellipsis_args$bdate))
    {
      error <- TRUE
      errmessage %<>% c("x" = "bdate must be an R date object")
        }
  }
  if ("edate" %in% names(ellipsis_args))
  {
    if (!is.Date(ellipsis_args$edate))
    {
      error <- TRUE
      errmessage %<>% c("x" = "edate must be an R date object")
    }
  }
  if ("cbdate" %in% names(ellipsis_args))
  {
    if (!is.Date(ellipsis_args$cbdate) & !is.null(ellipsis_args$cbdate))
    {
      error <- TRUE
      errmessage %<>% c("x" = "cbdate must be an R date object")
    }
  }
  if ("cedate" %in% names(ellipsis_args))
  {
    if (!is.Date(ellipsis_args$cedate) & !is.null(ellipsis_args$cedate))
    {
      error <- TRUE
      errmessage %<>% c("x" = "cedate must be an R date object")
    }
  }
  if ("email" %in% names(ellipsis_args))
  {
    if (!isValidEmail(ellipsis_args$email))
    {
      error <- TRUE
      errmessage %<>% c("x" = "invalid email address entered")
    }
  }
  if ("minlat" %in% names(ellipsis_args))
  {
    if ((!between(as.double(ellipsis_args$minlat), -90, 90)) |
        !is.character(ellipsis_args$minlat))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "minlat must be a numeric (expressed as a string) between -90 and 90"
                       )
    }
  }
  if ("maxlat" %in% names(ellipsis_args))
  {
    if ((!between(as.double(ellipsis_args$maxlat), -90, 90)) |
        !is.character(ellipsis_args$minlat))
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "maxlat must be a numeric (expressed as a string) between -90 and 90"
                        )
    }
  }
  if ("minlon" %in% names(ellipsis_args))
  {
    if ((!between(as.double(ellipsis_args$minlon), -180, 180)) |
        !is.character(ellipsis_args$minlon)
       )
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "minlon must be a numeric (expressed as a string) between -180 and 180"
                        )
    }
  }
    if ("maxlon" %in% names(ellipsis_args))
  {
    if ((!between(as.double(ellipsis_args$maxlon), -180, 180)) |
        !is.character(ellipsis_args$maxlon)
       )
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "maxlon must be a numeric (expressed as a string) between -180 and 180"
                        )
    }
  }
  if ("duration" %in% names(ellipsis_args))
  {
    if (nchar(ellipsis_args$duration) != 1 |
        !is.character(ellipsis_args$duration) &
        ellipsis_args$duration %in% 1:9 |
        ellipsis_args$duration %in% LETTERS[1:26]
        )
    {
      error <- TRUE
      errmessage %<>% c("x" =
      "duration must be a character from '1' to '9' or 'A' to 'Z'
       (represented as a character string)"
                        )
    }
  }
  if ("return_header" %in% names(ellipsis_args))
    {
      if (!is.logical(ellipsis_args$return_header))
        {
          error <- TRUE
          errmessage %<>% c("x" = "return_header must be of type logical")
        }
  }
  if (error)
    {
      callingfunction <- rlang::call_name(sys.call(sys.parent(2)))
      if (is.null(callingfunction)) callingfunction <- "Unknown Environment"
      callingfunction <- glue(" in: {callingfunction}")
      c("i" = callingfunction, errmessage) %>% abort
    }
}


#' @title format.terms.for.api
#' @description a helper function that accepts a named list of
#'                 parameters and returns a string vector of
#'                 separator separated variables for use in
#'                 sending parameters to AQS RESTFUL API calls,
#'                 All NA and NULL values will be removed. This
#'                 function is not intended for use by end users.
#' @param x a named list of variables, all values will be coerced to
#'          strings.
#' @param separator a string that should be used to separate variables
#'                   in the return value
#' @return a string that is properly formatted for use in AQS RESTFUL API
#'            calls.
#' @importFrom magrittr `%>%`
#' @noRd
format_variables_for_api <- function(x, separator="&")
{
  if (length(x) == 0) {
    return("")
  }
  #first check for NULLs, if found remove them

  x[vapply(x, is.null, FUN.VALUE = NA)] <- NULL
  #don't forget to remove NAs
  x[vapply(x, is.na, FUN.VALUE = NA)] <- NULL
  x <- purrr::map_chr(x, as.character)
  stringr::str_c(names(x), "=", x, collapse = separator) %>%
  return()
}


#' @title format_multiple_params_for_api
#' @description a helper function that accepts a list of parameters
#'                 and returns a string vector of separator separated variables
#'                 for use in sending parameters to AQS RESTFUL API calls, All
#'                 NA and NULL values will be removed. This function is not
#'                 intended for use by end users and is specifically designed
#'                 for use with API code with multiple pollution codes for other
#'                 use cases use the generic form of this function use the
#'                 helper function @seealso format_variables_for_api.
#' @param x a named list of variables, all values will be coerced to
#'          strings.
#' @param separator a string that should be used to separate variables
#'                   in the return value.
#' @return a string that is properly formatted for use in AQS RESTFUL API
#'            calls.
#' @noRd
format_multiple_params_for_api <- function(x, separator=",")
{
  if (length(x) == 0) {
    return("")
  }
  #first check for NULLs, if found remove them

  x[vapply(x, is.null, FUN.VALUE = NA)] <- NULL
  #don't forget to remove NAs
  x[vapply(x, is.na, FUN.VALUE = NA)] <- NULL
  x <- purrr::map_chr(x, as.character)
  paste0(x, collapse = separator)
}


#' @title RAQSAPI_error_msg
#'
#' @param AQSresponse a httr2 request object
#' @importFrom glue glue
#' @importFrom httr2 last_response resp_body_json
#' @importFrom magrittr `%<>%`
#' @importFrom stringr str_replace_all
#'
#' @description A helper function that is called by httr2::req_error when an
#'              error is encountered performing a request. This function returns
#'              a helpful error message for users. This function is not intended
#'              to be called directly by end users and should only be called
#'              within the aqs function.
#'
#' @return a string error message that is formatted for httr2 to display
#'         request errors for end users.
#' @noRd
#'
#' @example None
RAQSAPI_error_msg <- function(AQSresponse)
{
  #nocov start
  AQSerr <- last_response() %>%
              resp_body_json()

  #debug
  msg <- glue("At server request time: {AQSresponse$headers$Date}
               RAQSAPI experienced an error while processing the following url:
               {AQSresponse$url}
               with status_code: {AQSresponse$status_code}
               and status message: {AQSresponse$status}
               Server error message: {AQSerr$Header[[1]]$error}")

     return(msg)
  #nocov end
}


#' @title aqs
#' @description a helper function sends a AQS RESTful request to the AQS API
#'                 and returns the result as a aqs data type. This helper
#'                 function is used to abstract the call to AQS API away from
#'                 functions that need it's result. This helper function is not
#'                 meant to be called directly from external functions.
#' @param service the service requested by the AQS API encoded as a string;
#'                 For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param filter a string which represents the filter used in conjunction with
#'                   the service requested. For a list of available services
#'                   and filters @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param user A string which represents the registered user name used to
#'                 connect to the AQS API. Note that the '@' character needs
#'                 to be escaped with the '/' character.
#' @param user_key the AQS API user key used to grant the registered user access
#'                  to the AQS API.
#' @param variables A named list a variables used to send to the AQS API.
#'          @seealso \url{https://aqs.epa.gov/aqsweb/documents/data_api.html}
#'                      for the variables that are required for each
#'                      service/filter combination.
#' @param return_header If false (default) only reurns data requested.
#'                        If true returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @importFrom magrittr `%<>%` `%>%`
#' @importFrom dplyr mutate select arrange
#' @importFrom lubridate ymd_hm
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom rlang caller_call
#' @importFrom httr2 request req_user_agent req_url_path_append resp_body_json
#'                   req_perform req_options req_retry req_throttle req_error
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
#' @noRd
aqs <- function(service, filter = NULL, user = NA,
                    user_key = NA, variables = NULL, AQS_domain = "aqs.epa.gov")
{
  if (is.null(user) | is.null(user_key))
         {stop("please enter user credentials before using RAQSAPI functions,\n
                please refer to \'?aqs_credentials()\' for useage infomation \n"
              )
          }
  # AQS DataMart API does not accept headers so user_agent not working
  # user_agent <- glue("User:{user} via RAQSAPI-{packageVersion('RAQSAPI')}
  #                     library for R")

  AQSpath <- glue("https://{AQS_domain}/data/api/{service}/{filter}?") %>%
    glue(format_variables_for_api(c(list(email = I(user), key = user_key),
                                  variables)))
  AQSrequest <- AQSpath %>%
    request() %>%

    req_throttle(rate = 10/60, realm = "RAQSAPI") %>%
    req_retry(max_tries = 5, max_seconds = 30, backoff = ~10) %>%
    req_error(body = RAQSAPI_error_msg)
    # AQS DataMart API does not accept headers so user_agent not working
    #%>% req_user_agent(string = user_agent)

    AQSresponse <- AQSrequest %>%
      req_perform(verbosity = 0)

    if(httr2::resp_is_error(AQSresponse))
    {
      message(glue("RAQSAPI experienced an error with in aqs function from
                   {rlang::caller_call(n=2)} /n
                   url: {AQSpath}"))
    }

    AQSresponse %<>%
      resp_body_json(simplifyVector = TRUE,
                     simplifyDataFrame = TRUE)
    AQSresult <- vector("list", length = 2)
    AQSresult[[1]] <- AQSresponse$Header
    AQSresult[[2]] <- AQSresponse$Data
    names(AQSresult) <- c("Header", "Data")
    AQSresult <- structure(.Data = AQSresult, class = "AQS_DATAMART_APIv2")
     #aqs_ratelimit() #depricated
     return(AQSresult)

}


#' @title isValidEmail
#' @description a helper function that checks the input string has the form
#'                \<character\>\<AT\>\<character\>.\<character\> with length
#'                of at least 2 can be used to check if the input has the form
#'                of a valid e-mail address.
#' @param email a string which represents the parameter code of the air
#'                   pollutant related to the data being requested.
#' @note since this code relies on using regex the implementation is not perfect
#'         and may not work as expected all the time but overall generally works
#'         as expected.
#' @return Boolean
#' @noRd
isValidEmail <- function(email) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(email),
        ignore.case = TRUE)
}


#' @title aqs_services_by_site
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by site then calls the aqs and returns the
#'                 result. This helper function is not meant to be called
#'                 directly from external functions.
#' @family Aggregate _by_site functions
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param stateFIPS a R character object which represents the 2 digit state
#'                   FIPS code (with leading zero) for the state being
#'                   requested. @seealso [aqs_states()] for the list of
#'                   available FIPS codes.
#' @param countycode a R character object which represents the 3 digit state
#'                       FIPS code for the county being requested (with leading
#'                       zero(s)). @seealso [aqs_counties_by_state()] for the
#'                       list of available county codes for each state.
#' @param sitenum a R character object which represents the 4 digit site number
#'                 (with leading zeros) within the county and state being
#'                 requested.
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages.
#'                           @seealso [aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param service a string which represents the services provided by the AQS
#'                    API. For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_site <- function(parameter, bdate, edate,
                                 stateFIPS, countycode, sitenum,
                                 duration = NA_character_, service,
                                 cbdate = NA_Date_, cedate = NA_Date_,
                                 AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
      filter = "bySite",
      user =  getOption("aqs_username"),
      user_key =  getOption("aqs_key"),
      variables = list(param = format_multiple_params_for_api(parameter),
                       bdate = format(bdate, format = "%Y%m%d"),
                       edate = format(edate, format = "%Y%m%d"),
                       state = stateFIPS,
                       county = countycode,
                       site = sitenum,
                       duration = duration,
                       cbdate = cbdate,
                       cedate = cedate
                      ),
      AQS_domain = AQS_domain
      )
}

#' @title aqs_services_by_county
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by county then calls the aqs and returns the
#'                 result. This helper function is not meant to be called
#'                 directly from external functions.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'                  selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'                  selection. Only data on or before this date will be
#'                  returned.
#' @param stateFIPS a R character object which represents the 2 digit state
#'                      FIPS code (with leading zero) for the state being
#'                      requested. @seealso [aqs_states()] for the list of
#'                      available FIPS codes.
#' @param countycode a R character object which represents the 3 digit state
#'                       FIPS code for the county being requested (with leading
#'                       zero(s)). @seealso [aqs_counties_by_state()] for the
#'                       list of available county codes for each state.
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages.
#'                           @seealso [aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param service a string which represents the services provided by the AQS API
#'                    For a list of available services @seealso
#'             \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_county <- function(parameter, bdate, edate,
                                   stateFIPS, countycode, service,
                                   duration = NA_character_,
                                   cbdate = NA_Date_, cedate = NA_Date_,
                                   AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
          filter = "byCounty",
          user =  getOption("aqs_username"),
          user_key =  getOption("aqs_key"),
          variables = list(param = format_multiple_params_for_api(parameter),
                           bdate = format(bdate, format = "%Y%m%d"),
                           edate = format(edate, format = "%Y%m%d"),
                           state = stateFIPS,
                           county = countycode,
                           duration = duration,
                           cbdate = cbdate,
                           cedate = cedate
          ),
      AQS_domain = AQS_domain
  )
}


#' @title aqs_services_by_state
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by State then calls the aqs and returns the
#'                 result. This helper function is not meant to be called
#'                 directly from external functions.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param stateFIPS a R character object which represents the 2 digit state
#'                      FIPS code (with leading zero) for the state being
#'                      requested. @seealso [aqs_states()] for the list of
#'                      available FIPS codes.
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages.
#'                           @seealso [aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param service a string which represents the services provided by the
#'                    AQS API. For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_state <- function(parameter, bdate, edate, stateFIPS,
                                  duration = NA_character_, service,
                                  cbdate = NA_Date_, cedate = NA_Date_,
                                  AQS_domain = "aqs.epa.gov")
{

  aqs(service = service,
      filter = "byState",
      user =  getOption("aqs_username"),
      user_key =  getOption("aqs_key"),
      variables = list(param = format_multiple_params_for_api(parameter),
                       bdate = format(bdate, format = "%Y%m%d"),
                       edate = format(edate, format = "%Y%m%d"),
                       state = stateFIPS,
                       duration = duration,
                       cbdate = cbdate,
                       cedate = cedate
          ),
      AQS_domain = AQS_domain
  )
}

#' @title aqs_services_by_box
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by a box formed by minimum/maximum
#'                 latitude/longitude coordinates then calls the aqs
#'                 and returns the result. This helper function is not meant
#'                 to be called directly from external functions.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param minlat a R character object which represents the minimum latitude of
#'                   a geographic box.  Decimal latitude with north begin
#'                   positive. Only data north of this latitude will be
#'                   returned.
#' @param maxlat a R character object which represents the maximum latitude of
#'                   a geographic box. Decimal latitude with north begin
#'                   positive. Only data south of this latitude will be
#'                   returned.
#' @param minlon a R character object which represents the minimum longitude
#'                   of a geographic box. Decimal longitude with east begin
#'                   positive. Only data east of this longitude will be
#'                   returned.
#' @param maxlon a R character object which represents the maximum longitude
#'                   of a geographic box. Decimal longitude with east begin
#'                   positive. Only data west of this longitude will be
#'                   returned. Note that -80 is less than -70.
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages.
#'                           @seealso [aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param service a string which represents the services provided by the
#'                    AQS API. For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning date of last
#'                   change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                minlon, maxlon, duration = NA_character_,
                                service, cbdate = NA_Date_, cedate = NA_Date_,
                                AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
          filter = "byBox",
          user =  getOption("aqs_username"),
          user_key =  getOption("aqs_key"),
          variables = list(param = format_multiple_params_for_api(parameter),
                           bdate = format(bdate, format = "%Y%m%d"),
                           edate = format(edate, format = "%Y%m%d"),
                           minlon = minlon,
                           maxlon = maxlon,
                           minlat = minlat,
                           maxlat = maxlat,
                           duration = duration,
                           cbdate = cbdate,
                           cedate = cedate
          ),
      AQS_domain = AQS_domain
  )
}

#' @title aqs_services_by_cbsa
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by cbsa then calls the aqs and returns the
#'                 result. This helper function is not meant to be called
#'                 directly from external functions.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param cbsa_code a R character object which represents the 5 digit AQS Core
#'                   Based Statistical Area code (the same as the census code,
#'                   with leading zeros)
#' @param duration an optional R character string that represents the
#'                           parameter duration code that limits returned data
#'                           to a specific sample duration. The default value of
#'                           NA_character_ results in no filtering based on
#'                           duration code.Valid durations include actual sample
#'                           durations and not calculated durations such as 8
#'                           hour CO or $O_3$ rolling averages, 3/6 day PM
#'                           averages or Pb 3 month rolling averages.
#'                           @seealso [aqs_sampledurations()] for a list of all
#'                           available duration codes.
#' @param service a string which represents the services provided by the AQS
#'                    API For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                 duration = NA_character_, service,
                                 cbdate = NA_Date_, cedate = NA_Date_,
                                 AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
          filter = "byCBSA",
          user =  getOption("aqs_username"),
          user_key =  getOption("aqs_key"),
          variables = list(param = format_multiple_params_for_api(parameter),
                           bdate = format(bdate, format = "%Y%m%d"),
                           edate = format(edate, format = "%Y%m%d"),
                           cbsa = cbsa_code,
                           duration = duration,
                           cbdate = cbdate,
                           cedate = cedate
                           ),
      AQS_domain = AQS_domain
      )
}

#' @title aqs_services_by_pqao
#'
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by Primary Quality Assurance Organization (pqao)
#'                 then calls the aqs and returns the result.
#'                 This helper function is not meant to be called directly from
#'                 external functions.
#'
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#'
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#'
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#'
#' @param pqao_code a R character object which represents the 4 digit AQS
#'                   Primary Quality Assurance Organization code
#'                   (with leading zeroes).
#'
#' @param service a string which represents the services provided by the
#'                    AQS API. For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#'
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#'
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#'
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#'
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_pqao <- function(parameter, bdate, edate, pqao_code,
                                 service, cbdate = NA_Date_, cedate = NA_Date_,
                                 AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
      filter = "byPQAO",
      user =  getOption("aqs_username"),
      user_key =  getOption("aqs_key"),
      variables = list(param = format_multiple_params_for_api(parameter),
                       bdate = format(bdate, format = "%Y%m%d"),
                       edate = format(edate, format = "%Y%m%d"),
                       pqao = pqao_code,
                       cbdate = cbdate,
                       cedate = cedate
          ),
      AQS_domain = AQS_domain
  )
}

#' @title aqs_services_by_MA
#' @description a helper function that abstracts the formatting of the inputs
#'                 for a call to aqs away from the calling function for
#'                 aggregations by Monitoring Agency (MA)
#'                 then calls the aqs and returns the result.
#'                 This helper function is not meant to be called directly from
#'                 external functions.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'                  selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'                  selection. Only data on or before this date will be
#'                  returned.
#' @param MA_code a R character object which represents the 4 digit AQS
#'                    Monitoring Agency code (with leading zeroes).
#' @param service a string which represents the services provided by the AQS API
#'                    For a list of available services @seealso
#'            \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services}
#' @param cbdate a R date object which represents a "beginning
#'                   date of last change" that indicates when the data was last
#'                   updated. cbdate is used to filter data based on the change
#'                   date. Only data that changed on or after this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param cedate a R date object which represents an "end
#'                   date of last change" that indicates when the data was last
#'                   updated. cedate is used to filter data based on the change
#'                   date. Only data that changed on or before this date will be
#'                   returned. This is an optional variable which defaults
#'                   to NA_Date_.
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_services_by_MA <- function(parameter, bdate, edate, MA_code, service,
                               cbdate = NA_Date_, cedate = NA_Date_,
                               AQS_domain = "aqs.epa.gov")
{
  aqs(service = service,
      filter = "byMA",
      user =  getOption("aqs_username"),
      user_key =  getOption("aqs_key"),
      variables = list(param = format_multiple_params_for_api(parameter),
                       bdate = format(bdate, format = "%Y%m%d"),
                       edate = format(edate, format = "%Y%m%d"),
                       agency = MA_code,
                       cbdate = cbdate,
                       cedate = cedate
          ),
      AQS_domain = AQS_domain
  )
}

#' @title aqs_metadata_service
#' @description A helper function for functions which use the metaData service
#'                from the AQS API. This function is not intended to be called
#'                directly by the end user
#' @param filter a character string representing the filter being applied
#' @param service a character string representing the service
#' @param AQS_domain a R string object containing the domain that should be
#'                     used in constructing the API call.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
aqs_metadata_service <- function(filter, service = NA_character_,
                                 AQS_domain = "aqs.epa.gov")
{
  aqs(service = "metaData",
      filter = filter,
      user =  getOption("aqs_username"),
      user_key =  getOption("aqs_key"),
      variables = list(service = service),
      AQS_domain = AQS_domain
  )
}


#' @title renameaqsvariables
#' @description \lifecycle{experimental}
#'                This is a helper function not intended to be called directly
#'                by the end user. Renames the two columns returned in the $Data
#'                portion of a RAQSAPI_v2 object from "value"and
#'                "value_represented" to name1 and name2 respectively.
#' @importFrom dplyr rename rename_at vars
#' @importFrom rlang `:=` `!!`
#' @param aqsobject A RAQSAPI_v2 object
#' @param name1 a character string representing the new name of the first
#'                column of the $Data portion of the RAQSAPI_v2 object.
#' @param name2 a character string representing the new name of the second
#'                column of the $Data portion of the RAQSAPI_v2 object.
#' @noRd
renameaqsvariables <- function(aqsobject, name1, name2)
{
if (is.null(aqsobject))
    {
    return(aqsobject)
    } else if(inherits(x = aqsobject, what = "AQS_DATAMART_APIv2"))
             {
                  #using tidyevaluation and substitute operator
                  aqsobject$Data %<>%  dplyr::rename(!!name1 := 1)
                  aqsobject$Data %<>%  dplyr::rename(!!name2 := 2)

              } else if (all(inherits(x = aqsobject[[1]], what = "AQS_DATAMART_APIv2")))
                       {
                          #using tidyevaluation and substitute operator
                          aqsobject %<>%  lapply("[[", "Data") %>%
                            dplyr::rename(!!name1 := 1)
                          aqsobject %<>%  lapply("[[", "Data") %>%
                            dplyr::rename(!!name2 := 2)
                       }
 return(aqsobject)
}


#' @title aqsmultiyearparams
#' @description \lifecycle{experimental}
#'                This is a helper function intended to build a tibble of
#'                parameters used to generate the inputs to the purrr::map
#'                functions used with functional calls to services_by_*
#'                functions. This function is not intended for end use by the
#'                user.
#' @param parameter a character list or a single character string
#'                    which represents the parameter code of the air
#'                    pollutant related to the data being requested.
#' @param bdate a R date object which represents that begin date of the data
#'               selection. Only data on or after this date will be returned.
#' @param edate a R date object which represents that end date of the data
#'               selection. Only data on or before this date will be returned.
#' @param ... Other parameters returned to the calling function.
#' @importFrom rlang abort
#' @importFrom utils tail
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @importFrom lubridate year ymd month day years
#' @importFrom glue glue
#' @importFrom dplyr select_if
#' @importFrom magrittr `%>%` `%<>%`
#' @noRd
aqsmultiyearparams <- function(parameter, bdate, edate, service, ...)
{
  ellipsis_args <- list(...)
  if (bdate > edate)
   {
   return(rlang::abort(message = "bdate > edate"))
   } else if (year(bdate) == year(edate))
           {
             bdatevector <- bdate
             edatevector <- edate

   } else if (year(bdate) < year(edate))
           {
              bdatevector <- c(bdate, seq.Date(from = ymd(
                                                   glue("{year(bdate) + 1}-1-1")
                                                         ),
                                               to = edate, by = "year")
                                               )
              if (month(edate) != 12 && day(edate) != 31)
               {
                 edatevector <- c(seq.Date(from = ymd(glue("{year(bdate)}-12-31"
                                                           )
                                                      ),
                                           to = edate, by = "year"), edate)
               } else
                 {
                edatevector <- seq.Date(from = ymd(glue("{year(bdate)}-12-31")),
                                        to = edate, by = "year")
                 }
             }
             if (length(bdatevector) > length(edatevector))
               {
                 edatevector %<>% c(ymd(tail(edatevector, n = 1)) + years(1))
               }
   params <- tibble(parameter = format_multiple_params_for_api(parameter),
                    bdate = bdatevector,
                    edate = edatevector,
                    stateFIPS = ellipsis_args$stateFIPS,
                    countycode = ellipsis_args$countycode,
                    sitenum = ellipsis_args$sitenum,
                    duration = ellipsis_args$duration,
                    service = service,
                    cbdate = ellipsis_args$cbdate,
                    cedate = ellipsis_args$cedate,
                    minlat = ellipsis_args$minlat,
                    maxlat = ellipsis_args$maxlat,
                    minlon = ellipsis_args$minlon,
                    maxlon = ellipsis_args$maxlon,
                    cbsa_code = ellipsis_args$cbsa_code,
                    pqao_code = ellipsis_args$pqao_code,
                    MA_code = ellipsis_args$MA_code,
                    filter = ellipsis_args$filter,
                    AQS_domain = ellipsis_args$AQS_domain
                   )
  params %>%
    #remove all columns that have all NA values
    dplyr::select_if(function(x) {!all(is.na(x))}) %>%
    return()
}
