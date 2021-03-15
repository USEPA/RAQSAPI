#' @section  RAQSAPI setup functions


#' @title aqs_credentials
#' @description \lifecycle{questioning}
#'                 Sets the user credentials for the AQS API. This function
#'                 needs to be called once and only once every time this library
#'                 is re-loaded. Users must have a valid username and key which
#'                 can be obtained through the use of the aqs_sign_up function,
#'                 @seealso [aqs_sign_up()] to sign up for AQS data mart
#'                 credentials.
#' @importFrom rlang is_character
#' @param username a R character object which represents the email account that
#'                    will be used to connect to the AQS API.
#' @param key the key used in conjunction with the username given to connect to
#'              AQS Data Mart.
#' @return None
#' @examples
#'  #to authenticate an existing user the email address
#'  # "John.Doe@myemail.com" and key = "MyKey"
#'  #  after calling this function please follow the instructions that are sent
#'  #  in the verification e-mail before proceeding.
#'  \dontrun{aqs_credentials(username = "John.Doe@myemail.com",
#'                               key = "MyKey")
#'          }
#' @export
aqs_credentials <- function(username = NA_character_, key = NA_character_)
{
  # nocov
  #The code simply stores the credentials as a R global variable since the
  #Data Mart server only issues "key" and not "passwords" we don't need to
  #worry about securing the credentials with complicated code such as involving
  #salt and hashes and etc.
   if (!is.na(username) ||
       !is.na(key) ||
       !is_character(username) ||
       !is_character(key)
       )
   {
    options(aqs_username = username)
    options(aqs_key = key)
  } else {cat("Please enter a valid username and key  \n") }
} #no cov end


#' @title aqs_sign_up
#' @description \lifecycle{questioning}
#'              Use this service to register as a new user or to reset an
#'              existing user's key. A verification email will be sent to the
#'              email account specified. To reset a password: If the request is
#'              made with an email that is already registered, a new key will
#'              be issued for that account and emailed to the listed address.
#'              Usage is the same in either case. Refer to the email
#'              message for further instructions before continuing.
#' @param email a R character object which represents the email account that
#'                 will be used to register with the AQS API or change an
#'                 existing user's key. A verification email will be sent to
#'                 the account specified. Follow the instructions
#'                 in the verification e-mail before proceeding to use any other
#'                 functionality of the AQS API. Register your credential
#'                 with the @3 [aqs_credentials()] before using the
#'                 other functions in this library.
#' @note The '@' character needs to be escaped with the '/' character.
#' @importFrom glue glue
#' @importFrom magrittr `%>%`
#' @importFrom httr GET
#' @importFrom glue glue
#' @examples # to register a new user or generate a new key with the email
#'           #  address "John.Doe/@myemail.com"
#'           \dontrun{aqs_sign_up(email = "John.Doe/@myemail.com")}
#'           #  after calling this function please follow the instructions that
#'           #  are sent in the verification e-mail before proceeding.
#' @return None
#' @export
aqs_sign_up <- function(email)
{ #nocov start

  url <- glue("https://aqs.epa.gov/data/api/signup?email={email}")
  httr::GET(url)

  #can not use aqs() if the user has not registered yet.
  #aqs(service = "signup", variables = list(email = email))
  glue("A verification email has been sent to {email}  \n") %>%
    message()
} #nocov end


#' @section list functions


#' @title aqs_isavailable
#' @description \lifecycle{stable}
#'                returns a tibble or an AQS_Data Mart_APIv2 S3 object
#'                explaining the status of the AQS API.
#' @importFrom magrittr `%>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object which details the status
#'             of the AQS API (The status information is located in the header)
#' @examples
#'   #check if the AQS API is up, running and accepting requests.
#'   \dontrun{ aqs_isAvailable() }
#' @export
aqs_isavailable <- function(return_header = FALSE)
{
  if (!return_header)
  {
  aqs(service = "metaData",
          filter = "isAvailable",
          user =  getOption("aqs_username"),
          user_key =  getOption("aqs_key")
          )$Header %>%
    return()
  } else
  {
    aqs(service = "metaData",
          filter = "isAvailable",
          user =  getOption("aqs_username"),
          user_key =  getOption("aqs_key")
          )
  }
}


#' @title aqs_knownissues
#' @description \lifecycle{stable}
#'                Returns a table of any known issues with system functionality
#'                or the data. These are usually issues that have been
#'                identified internally and will require some time to correct
#'                in Data Mart or the API. This function implements a direct
#'                API call to Data Mart and returns data directly from the API.
#'                Issues returned via this function do not include any issues
#'                from the RAQSAPI R package.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains
#'   information involving known issues with the Data Mart API.
#' @examples
#'       #retrieve the list of known issues directly from the AQS data mart API
#'        \dontrun{aqs_knownissues()}
#' @export
aqs_knownissues <- function(return_header = FALSE)
{
  issues <- aqs_metadata_service(filter = "issues", service = NULL)
   if (!return_header) issues %<>% aqs_removeheader
  return(issues)

}


#' @title aqs_counties_by_state
#' @description \lifecycle{stable}
#'                 Returns a table of all counties in within the
#'                 stateFIPS provided.
#' @importFrom magrittr `%<>%`
#' @param stateFIPS a R character object which represents the 2 digit state
#'                   FIPS code (with leading zeros) for the state being
#'                   requested. @seealso [aqs_states()] for the list of
#'                   available FIPS codes.
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of all counties in the
#'            requested state.
#' @examples
#'           #returns an tibble all the counties
#'           #   in North Carolina the county FIPS codes (county codes) for
#'           #   each.
#'           \dontrun{aqs_counties_by_state(stateFIPS = "37")}
#' @export
aqs_counties_by_state <- function(stateFIPS, return_header = FALSE)
{
  counties <- aqs(service = "list",
                      filter = "countiesByState",
                      user =  getOption("aqs_username"),
                      user_key =  getOption("aqs_key"),
                      variables = list(state = stateFIPS)
                  )
     counties %<>% renameaqsvariables("county_code", "county_name")
     if (!return_header) counties %<>% aqs_removeheader
     return(counties)

}


#' @title aqs_sites_by_county
#' @description \lifecycle{stable}
#'                 Returns data containing a table of all air monitoring sites
#'                 with the input state and county FIPS code combination.
#' @inheritParams aqs_services_by_county
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of all air monitoring
#'            sites with the requested state and county FIPS codes.
#' @examples #returns an AQS_Data Mart_APIv2 S3 object witch returns all sites
#'           #  in Hawaii County, HI
#'           \dontrun{aqs_sites_by_county(stateFIPS = "15",
#'                                            countycode = "001")
#'                  }
#' @export
aqs_sites_by_county <- function(stateFIPS, countycode, return_header = FALSE)
{
  sites <- aqs(service = "list",
                   filter = "sitesByCounty",
                   user =  getOption("aqs_username"),
                   user_key =  getOption("aqs_key"),
                   variables = list(state = stateFIPS,
                                    county = countycode
                                    )
                )
  sites %<>% renameaqsvariables(name1 = "site_number", name2 = "site_name")
  if (!return_header) sites %<>% aqs_removeheader
  return(sites)

}


#' @title aqs_classes
#' @description \lifecycle{stable}
#'                 Returns a table of Parameter classes (groups of parameters,
#'                 i.e. "criteria" or "all"). The information from this function
#'                 can be used as input to other API calls.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of Parameter classes
#'                 (groups of parameters, i.e. "criteria" or "all").
#' @examples #returns a tibble or an AQS_Data Mart_APIv2 S3 object of
#'           #  parameter classes (groups of parameters, i.e. "criteria" or
#'           #  all")
#'          \dontrun{ aqs_classes() }
#' @export
aqs_classes <- function(return_header = FALSE)
{
  classes <- aqs(service = "list",
                 filter = "classes",
                 user =  getOption("aqs_username"),
                 user_key =  getOption("aqs_key"),
                 variables = NULL
                 )

  if (!return_header) classes %<>% aqs_removeheader
  return(classes)
}


#' @title aqs_parameters_by_class
#' @description \lifecycle{stable}
#'                 Returns parameters associated with the input class.
#' @importFrom magrittr `%<>%`
#' @param class a R character object that represents the class requested,
#'                  @seealso [aqs_classes()] for retrieving
#'                  available classes.  The class R character object must be a
#'                  valid class as returned from aqs_classes(). The class must
#'                  be an exact match to what is returned from aqs_classes().
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing the
#'            parameters associated with the class requested. NULL is returned
#'            for classes not found.
#' @examples # Returns a tibble of AQS parameters in the criteria class
#'           \dontrun{ aqs_parameters_by_class(class = "CRITERIA") }
#' @export
aqs_parameters_by_class <- function(class, return_header = FALSE)
{
  parameters <- aqs(service = "list",
                    filter = "parametersByClass",
                    user =  getOption("aqs_username"),
                    user_key =  getOption("aqs_key"),
                    variables = list(pc = class)
                   )
  if (!return_header) parameters %<>% aqs_removeheader
  return(parameters)
}


#' @title aqs_mas
#' @description \lifecycle{stable}
#'                 Returns a table of monitoring agencies (MA).
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitoring agencies
#'              and their associated agency code.
#' @examples #returns a tibble or an AQS_Data Mart_APIv2 S3 object
#'           #   of monitoring agencies and their respective
#'           #   monitoring agency codes.
#'           \dontrun{aqs_mas()}
#' @export
aqs_mas <- function(return_header = FALSE)
{
  mas <- aqs(service = "list",
                 filter = "mas",
                 user =  getOption("aqs_username"),
                 user_key =  getOption("aqs_key"),
                 variables = NULL
  )
  mas %<>% renameaqsvariables(name1 = "MA_code", name2 = "MonitoringAgency")
  if (!return_header) mas %<>% aqs_removeheader
  return(mas)
}


#' @title aqs_pqaos
#' @description \lifecycle{stable}
#'                 Returns a table of primary quality assurance
#'                 organizations (pqaos).
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of pqaos and
#'            their associated pqao code.
#' @examples #To retrieve a tibble of primary quality assurance
#'           #  organizations (pqaos)
#'            \dontrun{ pqaos <- aqs_pqaos() }
#' @export
aqs_pqaos <- function(return_header = FALSE)
{
  pqaos <- aqs(service = "list",
                   filter = "pqaos",
                   user =  getOption("aqs_username"),
                   user_key =  getOption("aqs_key"),
                   variables = NULL
  )
  pqaos %<>% renameaqsvariables(name1 = "PQAO_code", name2 = "PQAO")
  if (!return_header) pqaos %<>% aqs_removeheader
  return(pqaos)
}


#' @title aqs_cbsas
#' @description \lifecycle{stable}
#'                 Returns a table of all cbsas and their cbsa codes.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2  S3 object of all cbsas and
#'            their cbsa codes for constructing other requests.
#' @examples #returns a tibble or an AQS_Data Mart_APIv2 S3 object of cbsas
#'           #  and their respective cbsa codes
#'           \dontrun{ aqs_cbsas() }
#' @export
aqs_cbsas <- function(return_header = FALSE)
{
  cbsas <- aqs(service = "list",
                   filter = "cbsas",
                   user =  getOption("aqs_username"),
                   user_key =  getOption("aqs_key"),
                   variables = NULL
  )

  cbsas %<>% renameaqsvariables(name1 = "CBSA_code", name2 = "CBSA_name")
  if (!return_header) cbsas %<>% aqs_removeheader
  return(cbsas)
}


#' @title aqs_states
#' @description \lifecycle{stable}
#'                 Returns a table of US states, US territories, and the
#'                 district or Columbia with their respective FIPS codes used
#'                 for constructing other requests
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns an AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of states and their
#'            FIPS codes used for constructing other requests.
#' @examples #returns a tibble of states and their FIPS codes
#'           \dontrun{aqs_states()}
#' @export
aqs_states <- function(return_header = FALSE)
{
  states <- aqs(service = "list",
                filter = "states",
                user =  getOption("aqs_username"),
                user_key =  getOption("aqs_key"),
                variables = NULL
                )
  states %<>% renameaqsvariables(name1 = "stateFIPS", name2 = "state")
  if (!return_header) states %<>% aqs_removeheader
  return(states)
}


#' @section by_site aggregate functions


#' @title aqs_monitors_by_site
#' @description \lifecycle{stable}
#'  Returns a table of monitors at all sites with the provided parameternum,
#'    stateFIPS, county_code, and sitenum for bdate - edate time frame.
#' @note all monitors that operated between the bdate and edate will be returned
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @importFrom tibble tibble
#' @importFrom dplyr select_if
#' @importFrom purrr pmap
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from a
#'           selected site
#' @examples
#'  #Returns a tibble of the SO2 monitors at Hawaii
#'  #  Volcanoes NP site (\#0007) in Hawaii County, HI that were operating on
#'  #  May 1 , 2015. (Note, all monitors that operated between the bdate and
#'  #  edate will be returned)
#'  \dontrun{
#'            aqs_monitors_by_site(parameter = "42401",
#'                                   bdate = as.Date("20150501",
#'                                                      format="%Y%m%d"),
#'                                   edate = as.Date("20150502",
#'                                                      format="%Y%m%d"),
#'                                   stateFIPS = "15",
#'                                   countycode = "001",
#'                                   sitenum = "0007"
#'                                  )
#'          }
#'
#' @export
aqs_monitors_by_site <- function(parameter, bdate, edate, stateFIPS,
                                      countycode, sitenum,
                                      cbdate = NA_Date_,
                                      cedate = NA_Date_,
                                      return_header = FALSE)
{
  # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::map
  #  is used so that the output is consistent with other RAQSAPI functions.
  params <- tibble(parameter = parameter,
                   bdate = bdate,
                   edate = edate,
                   stateFIPS = stateFIPS,
                   countycode = countycode,
                   service = "monitors",
                   sitenum = sitenum,
                   cbdate = cbdate,
                   cedate = cedate) %>%
     dplyr::select_if(function(x) {!all(is.na(x))})

  monitors <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) monitors %<>% aqs_removeheader
  return(monitors)
}


#' @title aqs_qa_flowrate audit_by_site
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate audit
#'                data aggregated by parameter code, stateFIPS, countycode and
#'                site number for bdate - edate time frame.
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
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
#' @family Aggregate _by_site functions
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing flow rate
#'           audit data for the requested sitenum, countycode and stateFIPS. An
#'           AQS_Data_Mart_APIv2 object is a 2 item named list in which the
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of flow rate audit data
#'           #  for the Wylam site (#2003) in Jefferson County, AL
#'           #  for January 2018:
#'  \dontrun{aqs_qa_flowrateaudit_by_site(parameter = "88101",
#'                                                  bdate = as.Date("20150101",
#'                                                             format="%Y%m%d"),
#'                                                  edate = as.Date("20180131",
#'                                                             format="%Y%m%d"),
#'                                                  stateFIPS = "01",
#'                                                  countycode = "073",
#'                                                  sitenum = "2003"
#'                                                  )
#'            }
#' @export
aqs_qa_flowrateaudit_by_site <- function(parameter, bdate, edate,
                                                   stateFIPS,countycode,
                                                   sitenum,
                                                   cbdate = NA_Date_,
                                                   cedate = NA_Date_,
                                                   return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "qaFlowRateAudits",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  fra <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) fra %<>% aqs_removeheader
  return(fra)
}


#' @title aqs_qa_one_point_qc_by_site
#' @description \lifecycle{stable}
#'                Returns a table of one point QC raw data
#'                aggregated by parameter code, stateFIPS, countycode and
#'                site number.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing one point
#'            qc data for the requested site. A AQS_Data_Mart_APIv2 object is a
#'            2 item named list in which the first item (\$Header) is a tibble
#'            of header information from the AQS API and the second item
#'            (\$Data) is a tibble of the data returned.
#' @examples # returns a tibbble of One Point QC data for
#'           #  ozone at the Truro National Seashore site (\#0002) in
#'           #  Barnstable County, MA for January 2018:
#'  \dontrun{aqs_qa_one_point_qc_by_site_multiyear(parameter = "44201",
#'                                     bdate = as.Date("20180101",
#'                                                    format = "%Y%m%d"),
#'                                     edate = as.Date("20180131",
#'                                                    format = "%Y%m%d"),
#'                                     stateFIPS = "25",
#'                                     countycode = "001",
#'                                     sitenum = "0002"
#'                                     )
#'          }
#' @export
aqs_qa_one_point_qc_by_site <- function(parameter, bdate, edate,
                                                  stateFIPS, countycode,
                                                  sitenum, cbdate = NA_Date_,
                                                  cedate = NA_Date_,
                                                  return_header = FALSE
                                                  )
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "qaOnePointQcRawData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  opqcc <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) opqcc %<>% aqs_removeheader
  return(opqcc)
}


#' @title aqs_qa_pep_audit_by_site
#' @description \lifecycle{stable}
#'                Returns a table of Performance Evaluation Program (PEP) audit
#'                data aggregated by parameter code, stateFIPS, countycode and
#'                site number for the time frame between bdate and edate.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance PEP audit data within a site. A AQS_Data_Mart_APIv2
#'           object is a 2 item named list in which the first item (\$Header) is
#'           a tibble of header information from the AQS API and the second item
#'           (\$Data) is a tibble of the data returned.
#' @examples # returns a aqs_v2 S3 object of pep Audit data for FRM PM2.5
#'           #  at the Huntsville Old Airport site (\#0014) in Madison County,
#'           #  AL for 2017
#'  \dontrun{aqs_qa_pep_audit_by_site(parameter = "88101",
#'                                    bdate = as.Date("20150101",
#'                                                    format = "%Y%m%d"),
#'                                    edate = as.Date("20171231",
#'                                                      format = "%Y%m%d"),
#'                                    stateFIPS = "01",
#'                                    countycode = "089",
#'                                    sitenum = "0014"
#'                                        )
#'            }
#' @export
aqs_qa_pep_audit_by_site <- function(parameter, bdate, edate,
                                               stateFIPS, countycode, sitenum,
                                               cbdate = NA_Date_,
                                               cedate = NA_Date_,
                                               return_header = FALSE)
{
    params <- aqsmultiyearparams(parameter = parameter,
                                 bdate = bdate,
                                 edate = edate,
                                 stateFIPS = stateFIPS,
                                 countycode = countycode,
                                 sitenum = sitenum,
                                 service = "qaPepAudits",
                                 cbdate = cbdate,
                                 cedate = cedate
                                 )

  pepaudit <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) pepaudit %<>% aqs_removeheader
  return(pepaudit)
}


#' @title aqs_sampledata_by_site
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where sample data is
#'                 aggregated at the site level. If return_header is
#'                 FALSE (default) returns a single data frame with
#'                 the requested data. If return_header is TRUE returns a list
#'                 of AQSAPI_v2 objects where each index of the list is an
#'                 individual RAQSAPI_v2 object returned from each successive
#'                 calls to the AQS API. RAQSAPI_v2 objects are two item list
#'                 where the $Data portion contains data that contains
#'                 sample air monitoring data at a site with the input
#'                 parameter, stateFIPS and county_code provided for
#'                 bdate - edate time frame. The $Header is a tibble of
#'                 header information from the API call /(useful for
#'                 debugging/).  Returns NULL is bdate > edate.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @param return_header If FALSE (default) returns a single data frame with the
#'                        data requested. If TRUE returns a AQSAPI_v2 object
#'                        which is a two item list that contains header
#'                        information returned from the API server mostly used
#'                        for debugging purposes in addition to the data
#'                        requested. This is mostly useful for debugging
#'                        purposes, in case the user wishes to see the header
#'                        information from each api call.
#' @importFrom magrittr `%>%` `%<>%`
#' @importFrom purrr pmap
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data
#'           for a single site with the input parameter. An AQS_Data Mart_APIv2
#'           is a 2 item named list in which the first item /(/$Header/) is a
#'           tibble of header information from the AQS API and the second item
#'           /(/$Data/) is a tibble of the data returned.
#' @examples #Returns a AQS_Data Mart_APIv2 S3 object of ozone monitoring
#'           #  data for the Millbrook School site (/#0014) in
#'           #  Wake County, NC for June 18, 2017.
#'  \dontrun{
#'             aqs_sampledata_by_site(parameter = "44201",
#'                                    bdate = as.Date("20170618",
#'                                                    format = "%Y%m%d"),
#'                                    edate = as.Date("20190618",
#'                                                       format = "%Y%m%d"),
#'                                    stateFIPS = "37",
#'                                    countycode = "183",
#'                                    sitenum = "0014"
#'                                    )
#'          }
#' @export
aqs_sampledata_by_site <- function(parameter, bdate, edate, stateFIPS,
                                   countycode, sitenum, cbdate = NA_Date_,
                                   cedate = NA_Date_, return_header = FALSE
                                   )
{

  params <- aqsmultiyearparams(parameter = parameter,
                                   bdate = bdate,
                                   edate = edate,
                                   stateFIPS = stateFIPS,
                                   countycode = countycode,
                                   sitenum = sitenum,
                                   service = "sampleData",
                                   cbdate = cbdate,
                                   cedate = cedate
                              )

  sampledata <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) sampledata %<>% aqs_removeheader
  return(sampledata)
}


#' @title aqs_annualsummary_by_site
#' @description \lifecycle{stable}
#'                 Returns multiple years of data where annual data is
#'                 aggregated at the site level. Returned is an annual summary
#'                 matching the input parameter, stateFIPS, county_code, and
#'                 sitenum provided for bdate - edate time frame. The data
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested as a
#'                        single tibble. If TRUE returns a list of AQSAPI_v2
#'                        objects which is a two item list that contains header
#'                        information returned from the API server mostly used
#'                        for debugging purposes in addition to the data
#'                        requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the Sitenum, countycode and stateFIPS requested.
#'           A AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item (\$Header) is a tibble of header information from the AQS API
#'           and the second item (\$Data) is a tibble of the data returned.
#' @examples # Returns a tibble of annual summary ozone
#'           #  data for the Millbrook School site (\#0014) in Wake County,
#'           #  NC for 2017 (Note, for annual data, only the
#'           #  year portion of the bdate and edate are used and only whole
#'           #  years of data are returned. For example, bdate = 2017-12-31 and
#'           #  edate = 2018-01-01 will return full data for 2017 and 2018 )
#'  \dontrun{
#'           aqs_annualsummary_by_site(parameter = "44201",
#'                                     bdate = as.Date("20170618",
#'                                                     format="%Y%m%d"),
#'                                     edate = as.Date("20190618",
#'                                                     format="%Y%m%d"),
#'                                     stateFIPS = "37",
#'                                     countycode = "183",
#'                                     sitenum = "0014"
#'                                    )
#'           }
#' @export
aqs_annualsummary_by_site <- function(parameter, bdate, edate, stateFIPS,
                                      countycode, sitenum, cbdate = NA_Date_,
                                      cedate = NA_Date_, return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "annualData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  annualdata <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) annualdata %<>% aqs_removeheader
  return(annualdata)
}


#' @title aqs_qa_blanks_by_site
#' @description \lifecycle{stable}
#'                 Aggregates multiple years of qa blank data where the blank
#'                 data is aggregated at the site level and returns table
#'                 with a tibble of Quality assurance data - blanks samples.
#'                 Blanks are unexposed sample collection devices (e.g.,
#'                 filters) that are transported with the exposed sample devices
#'                 to assess if contamination is occurring during the transport
#'                 or handling of the samples.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
#'           assurance blank sample data for single monitoring site. An
#'           AQS_Data_Mart_APIv2 is a 2 item named list in which the first item
#'           ($Header) is a tibble of header information from the AQS API and
#'           the second item ($Data) is a tibble of the data returned.
#' @examples #Returns a tibble of PM2.5 blank
#'           #  data for the Muscle Shoals site (#0014) in Colbert County, AL
#'           #  for January 2018
#'           \dontrun{
#'                     aqs_qa_blanks_by_site(parameter = "88101",
#'                                           bdate = as.Date("20170101",
#'                                                           format="%Y%m%d"),
#'                                           edate = as.Date("20190131",
#'                                                           format="%Y%m%d"),
#'                                           stateFIPS = "01",
#'                                           countycode = "033",
#'                                           sitenum = "1002"
#'                                           )
#'                  }
#' @export
aqs_qa_blanks_by_site <- function(parameter, bdate, edate, stateFIPS,
                                  countycode, sitenum, cbdate = NA_Date_,
                                  cedate = NA_Date_, return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "qaBlanks",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  blanks <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) blanks %<>% aqs_removeheader
  return(blanks)
}


#' @title aqs_dailysummary_by_site
#' @description \lifecycle{stable}
#'                 Returns a table of daily summaries with the
#'                 matching input parameter, stateFIPS, county_code, and sitenum
#'                 provided for bdate - edate time frame. The data returned is
#'                 summarized at the daily level. All daily summaries are
#'                 calculated on midnight to midnight basis in local time.
#'                 Variables returned include date, mean value, maximum value,
#'                 etc.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that contains daily
#'           summary statistics for the given parameter for a single site. An
#'           AQS_Data Mart_APIv2 is a 2 item named list in which the first item
#'           (\$Header) is a tibble of header information from the AQS API and
#'           the second item (\$Data) is a tibble of the data returned.
#' @examples #Returns a tibble of daily summary ozone
#'           #  data for the Millbrook School site (\#0014) in Wake County,
#'           #  NC for June 18, 2017.
#'  \dontrun{
#'              aqs_dailysummary_by_site(parameter = "44201",
#'                                       bdate = as.Date("20160618",
#'                                                       format = "%Y%m%d"),
#'                                       edate = as.Date("20190618",
#'                                                       format = "%Y%m%d"),
#'                                       stateFIPS = "37",
#'                                       countycode = "183",
#'                                      sitenum = "0014"
#'                                      )
#'          }
#' @export
aqs_dailysummary_by_site <- function(parameter, bdate, edate, stateFIPS,
                                     countycode, sitenum, cbdate = NA_Date_,
                                     cedate = NA_Date_, return_header = FALSE
                                     )
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "dailyData",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  dailysummary <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) dailysummary %<>% aqs_removeheader
  return(dailysummary)
}


#' @title aqs_qa_collocated_assessments_by_site
#' @description \lifecycle{stable}
#'                Returns a table of collocated assessment data aggregated by
#'                 matching input parameter, stateFIPS, county_code, and
#'                 sitenum provided for bdate - edate time frame.
#' @note The AQS API only allows for a single year of collocated assessments to
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a site.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples #returns a tibble of collocated assessment data
#'           #  for FRM PM2.5 at the Huntsville Old Airport site (\#0014)
#'           #  in Madison County, AL for January 2013:
#'  \dontrun{aqs_qa_collocated_assessments_by_site(parameter = "88101",
#'                                                 bdate = as.Date("20130101",
#'                                                             format = "%Y%m%d"
#'                                                                ),
#'                                                 edate = as.Date("20150131",
#'                                                             format = "%Y%m%d"
#'                                                                 ),
#'                                                 stateFIPS = "01",
#'                                                 countycode = "089",
#'                                                 sitenum = "0014"
#'                                                 )
#'          }
#' @export
aqs_qa_collocated_assessments_by_site <- function(parameter, bdate, edate,
                                                  stateFIPS, countycode,
                                                  sitenum,
                                                  cbdate = NA_Date_,
                                                  cedate = NA_Date_,
                                                  return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "qaCollocatedAssessments",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  colocatedsummary <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) colocatedsummary %<>% aqs_removeheader
  return(colocatedsummary)
}


#' @title aqs_qa_flowrateverification_by_site
#' @description \lifecycle{stable}
#'                Returns a table containing flow rate Verification data for a
#'                parameter code aggregated matching input parameter, stateFIPS,
#'                county_code, and sitenum provided for
#'                bdate - edate time frame.
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance flow rate verification data for monitors at a site.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of flow rate verification
#'           #  data for the Muscle Shoals site (#0014) in Colbert County, AL
#'           #  for January 2018:
#'  \dontrun{aqs_qa_flowrateverification_by_site(parameter = "88101",
#'                                               bdate = as.Date("20170101",
#'                                                          format = "%Y%m%d"),
#'                                               edate = as.Date("20180131",
#'                                                          format = "%Y%m%d"),
#'                                              stateFIPS = "01",
#'                                              countycode = "033",
#'                                              sitenum = "1002"
#'                                                )
#'           }
#' @export
aqs_qa_flowrateverification_by_site <- function(parameter, bdate, edate,
                                                stateFIPS, countycode, sitenum,
                                                cbdate = NA_Date_,
                                                cedate = NA_Date_,
                                                return_header = FALSE
                                                )
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "qaFlowRateVerifications",
                               cbdate = cbdate,
                               cedate = cedate
                               )

  frv <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) frv %<>% aqs_removeheader
  return(frv)
}


#' @title aqs_transactionsample_by_site
#' @description \lifecycle{stable}
#'        Returns transactionsample data - aggregated by site
#'          in the AQS Submission Transaction Format (RD) sample (raw) data for
#'          a parameter code aggregated by matching input parameter, sitenum,
#'          countycode and stateFIPS provided for bdate - edate time frame.
#'          Includes data both in submitted and standard units
#' @note The AQS API only allows for a single year of transactiondata to be
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
#' @family Aggregate _by_site functions
#' @inheritParams aqs_services_by_site
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @importFrom magrittr `%<>%`
#' @examples #Returns a AQS_Data Mart_APIv2 S3 object of the returns
#'          \dontrun{ #   returns all ozone transaction data for the
#'                    #   Millbrook School site (#0014) in Wake County, NC for
#'                    #   June 18, 2017
#'                    aqs_transactionsample_by_site(parameter = "44201",
#'                                                  bdate = as.Date("20170618",
#'                                                          format = "%Y%m%d"),
#'                                                  edate = as.Date("20170618",
#'                                                           format = "%Y%m%d"),
#'                                                  stateFIPS = "37",
#'                                                  countycode = "183",
#'                                                  sitenum = "0014"
#'                                                  )
#'                  }
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of transaction sample
#'           (raw) data in the AQS submission transaction format (RD)
#'           corresponding to the inputs provided.
#' @export
aqs_transactionsample_by_site <- function(parameter, bdate, edate,
                                          stateFIPS,countycode, sitenum,
                                          return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               stateFIPS = stateFIPS,
                               countycode = countycode,
                               sitenum = sitenum,
                               service = "transactionsSample"
                               )

  transactionsample <- purrr::pmap(.l = params, .f = aqs_services_by_site)
  if (!return_header) transactionsample %<>% aqs_removeheader
  return(transactionsample)
}


#' @title aqs_monitors_by_county
#' @description \lifecycle{stable}
#'    Returns a table of monitors at a site with the provided parameternum,
#'      stateFIPS and county_code for bdate - edate time frame.
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
#'           #  Hawaii County, HI that were operating on May 01, 2015.
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
   # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::map
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
#'         API server. Fortunately this operation has a linear run time
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
                                     countycode, cbdate = NA_Date_,
                                     cedate = NA_Date_, return_header = FALSE)
{

  params <- aqsmultiyearparams(parameter = parameter,
                                   bdate = bdate,
                                   edate = edate,
                                   stateFIPS = stateFIPS,
                                   countycode = countycode,
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
#'           item (\$Header) is a tibble of header information from the AQS API
#'           and the second item (\$Data) is a tibble of the data returned.
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
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing blank sample data aggregated by
#'                county number.
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
#' @examples # returns an tibble with PM2.5 blank data for
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
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object of daily summary data aggregated by county number.
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
#'           list in which the first item (\$Header) is a tibble of header
#'           information from the AQS API and the second item (\$Data) is a
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
#' @note The AQS API only allows for a single year of collocated assessments to
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
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object containing quality
#'           assurance collocated assessment data for monitors within a county.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble with collocated assessment data
#'           #  for FRM PM2.5 in Madison County, AL for January 2013
#'  \dontrun{aqs_qa_collocated_assessments_by_county(parameter = "88101",
#'                                                   bdate = as.Date("20130101",
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
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
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
#'            2 item named list in which the first item (\$Header) is a tibble
#'            of header information from the AQS API and the second item
#'            (\$Data) is a tibble of the data returned.
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
#'           object is a 2 item named list in which the first item (\$Header) is
#'           a tibble of header information from the AQS API and the second item
#'           (\$Data) is a tibble of the data returned.
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
#' @note The AQS API only allows for a single year of transactiondata to be
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
#' @examples #Returns a AQS_Data Mart_APIv2 S3 object of the returns
#'          \dontrun{ #   returns all FRM/FEM transaction data for
#'                    #   Wake County, NC between January and February 2016
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
                                            stateFIPS,countycode,
                                            return_header = FALSE)
{
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


#' @section by_state aggregate functions


#' @title aqs_monitors_by_state
#' @description \lifecycle{stable}
#'  Returns a table of monitors at all sites with the provided parameternum,
#'    stateFIPS and county_code for bdate - edate time frame.
#' @family Aggregate_by_state functions
#' @inheritParams aqs_services_by_state
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two item
#'                        list that contains header information returned from
#'                        the API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from a
#'           selected state
#' @examples # returns a tibble of SO2 monitors in Hawaii
#'           #  that were operating on May 01, 2015
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
   # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::map
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
#'         API server. Fortunately this operation has a linear run time
#'         /(Big O notation: O/(n + 5 seconds/)/)
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
#' @examples # returns an aqs_v2 S3 object with all benzene samples from
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
                                    cbdate = NA_Date_, cedate = NA_Date_,
                                    return_header = FALSE
                                    )
{
params <- aqsmultiyearparams(parameter = parameter,
                                   bdate = bdate,
                                   edate = edate,
                                   stateFIPS = stateFIPS,
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
#'           a 2 item named list in which the first item (\$Header) is a tibble
#'           of header information from the AQS API and the second item (\$Data
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
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing Quality assurance blank sample data
#'                aggregated by state FIPS.
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
#' @examples # returns a aqs_v2 object which contains PM2.5 blank data
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
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing sample data data aggregated by state FIPS.
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
#'           summary statistics for the given parameter for a single stateFIPS
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item (\$Header) is a tibble of header information from the AQS API
#'           and the second item (\$Data) is a tibble of the data returned.
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
#' @note The AQS API only allows for a single year of collocated assessments to
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
#'           assurance collocated assessment data for monitors within a state.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble  of collocated
#'           #  assessment data fofr FRM2.5 for January 2013
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
#' @examples # returns a tibble of flow rate verification
#'           #  data for Alabama 2018
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
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of flow rate audit
#'           #  data for Alabama in January 2018
#'           \dontrun{aqs_qa_flowrateaudit_by_state(parameter = "88101",
#'                                                  b date = as.Date("20170101",
#'                                                             format = "%Y%m%d"
#'                                                                 ),
#'                                                   edate = as.Date("20180131",
#'                                                             format = "%Y%m%d"
#'                                                                 ),
#'                                                  stateFIPS = "01"
#'                                                  )
#'                    }
#' @export
aqs_qa_flowrateaudit_by_state <- function(parameter, bdate, edate, stateFIPS,
                                          cbdate = NA_Date_, cedate = NA_Date_,
                                          return_header = FALSE
                                          )
{
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
#'            2 item named list in which the first item (\$Header) is a tibble
#'            of header information from the AQS API and the second item
#'            (\$Data) is a tibble of the data returned.
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
#'           object is a 2 item named list in which the first item (\$Header) is
#'           a tibble of header information from the AQS API and the second item
#'           (\$Data) is a tibble of the data returned.
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
#' @note The AQS API only allows for a single year of transactiondata to be
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
#'          \dontrun{ #   all benzene transaction sample data for
#'                    #   North Carolina on May 15, 1995
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


#' @section by_cbsa aggregate functions


#' @title aqs_monitors_by_cbsa
#' @description \lifecycle{stable}
#'  Returns a table of monitors at all sites with the provided
#'    parameternum, aggregated by Core Based Statistical Area (CBSA) for
#'    bdate - edate time frame.
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'                        returns a AQSAPI_v2 object which is a two item list
#'                        that contains header information returned from the
#'                        API server mostly used for debugging purposes in
#'                        addition to the data requested.
#' @examples # returns a tibble of NO2 monitors
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
#'            list in which the first item (\$Header) is a tibble of header
#'            information from the AQS API and the second item (\$Data) is a
#'            tibble of the data returned.
#' @export
aqs_monitors_by_cbsa <- function(parameter, bdate, edate, cbsa_code,
                                    cbdate = NA_Date_, cedate = NA_Date_,
                                    return_header = FALSE)
{
 # aqs_monitors_by_* functions don't call aqsmultiyearparams() since the
  #  monitors API call accepts multiple years of data on the server, purrr::map
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
#' @examples # returns an aqs_v2 s3 object which contains NO2 data
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
                                   cbdate = NA_Date_, cedate = NA_Date_,
                                   return_header = FALSE
                                   )
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               cbsa_code = cbsa_code,
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
#' @family Aggregate _by_cbsa functions
#' @inheritParams aqs_services_by_cbsa
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the cbsa_code requested. A AQS_Data Mart_APIv2 is
#'           a 2 item named list in which the first item (\$Header) is a tibble
#'           of header information from the AQS API and the second item (\$Data)
#'           is a tibble of the data returned.
#' @examples # returns a tibble of annual sunnary NO2
#'           #  data the for Charlotte-Concord-Gastonia, NC cbsa for
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
#'           item (\$Header) is a tibble of header information from the AQS API
#'           and the second item (\$Data) is a tibble of the data returned.
#' @examples # returns a tibble of daily sunnary NO2
#'           #  data the for Charlotte-Concord-Gastonia, NC cbsa for
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


#' @section by_ma aggregate functions


#' @title aqs_qa_blanks_by_MA
#' @description \lifecycle{stable}
#'   Returns a tibble or an AQS_Data Mart_APIv2 S3
#'   object containing Quality assurance - blanks sample data aggregated by
#'   monitoring agency code (_by_MA).
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
#' @examples # returns a tibble containing PM2.5 blank data for
#'            #  January 2018 where the Monitoring Agency is the Alabama
#'            #  Department of Environmental Management (agency 0013)
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
#' @note The AQS API only allows for a single year of collocated assessments to
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
#'           assurance collocated assessment data for monitors within a
#'           monitoring agency. An AQS_Data Mart_APIv2 is a 2 item named list
#'           in which the first item ($Header) is a tibble of header information
#'           from the AQS API and the second item ($Data) is a tibble of the
#'           data returned.
#' @examples # returns a tibble containing collocated assessment
#'           #  data for FRM PM2.5 January 2013 where the Monitoring Agency is
#'           #  the Alabama Department of Environmental Management (agency 0013)
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
#' @examples # returns a tibble containing collocated assessment
#'   #  data for FRM PM2.5 January 2013 where the Monitoring Agency is
#'   #  the Alabama Department of Environmental Management (agency 0013)
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
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of flow rate audit data
#'  #  for FRM PM2.5 January 2016 - Januray 2018 where the Monitoring Agency is
#'  #  the Jefferson County, AL  Department of Health (agency 0550)
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
#'   Returns a tibble or an AQS_Data Mart_APIv2 S3 object containing Quality
#'   assurance data - collocated assessment raw data aggregated by monitoring
#'   agency code (_by_MA).
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
#'            object is a 2 item named list in which the first item (\$Header)
#'            is a tibble of header information from the AQS API and the second
#'            item (\$Data) is a tibble of the data returned.
#' @examples # returns a tibble of ozone One Point QC data
#'           #  for January 2018 where the Monitoring Agency is the
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
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of PEP audit data for
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
#' @note The AQS API only allows for a single year of transactiondata to be
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
#' @examples #Returns a AQS_Data Mart_APIv2 S3 object of
#'          \dontrun{ #   all ozone transaction sample data for all monitors
#'                    #   operated by South Coast Air Quality Management
#'                    #   District collected on May 15, 2015
#'                    #   North Carolina on May 15, 1995
#'                    aqs_transactionsample_by_MA(parameter = "44201",
#'                                                  bdate = as.Date("20150515",
#'                                                          format = "%Y%m%d"),
#'                                                  edate = as.Date("20150515",
#'                                                           format = "%Y%m%d"),
#'                                                  MA_code = "0972"
#'                                                  )
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


#' @section by_pqao aggregate functions

#' @title aqs_qa_blanks_by_pqao
#' @description \lifecycle{stable}
#'   Returns a tibble or an AQS_Data Mart_APIv2 S3
#'   object containing Quality assurance data - blanks sample data aggregated
#'   by Primary Quality Assurance Organization (PQAO) code.
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
#' @examples # returns tibble of PM2.5 blank data for
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
#' @note The AQS API only allows for a single year of collocated assessments to
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
#'           assurance collocated assessment data for monitors within a pqao.
#'           An AQS_Data Mart_APIv2 is a 2 item named list in which the first
#'           item ($Header) is a tibble of header information from the AQS API
#'           and the second item ($Data) is a tibble of the data returned.
#' @examples # returns a tibble of collocated assessment
#'           #  data for FRM PM2.5 January 2013 where the PQAO is the Alabama
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
#'           first item (\$Header) is a tibble of header information from the
#'           AQS API and the second item (\$Data) is a tibble of the data
#'           returned.
#' @examples # returns a tibble of flow rate audit data for January
#'   #  2018 where the PQAO is the Jefferson County, AL Department Of
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
#'            2 item named list in which the first item (\$Header) is a tibble
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
#'           in which the first item (\$Header) is a tibble of header
#'           information from the AQS API and the second item (\$Data) is a
#'           tibble of the data returned.
#' @examples # returns a tibble of PEP audit data for
#'   #  June 2017 where the pqao is the Alabama Department of
#'   #  Environmental Management (agency 0013)
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


#' @section by_box aggregate functions


#' @title aqs_monitors_by_box
#' @description \lifecycle{stable}
#'  Returns a table of monitors at all sites with the provided
#'    parameternum, aggregated by latitude/longitude bounding box (_by_box) for
#'    bdate - edate time frame.
#' @family Aggregate _by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of monitors from a
#'           latitude/longitude bounding box (_by_box).
#' @examples #  returns a tibble of all ozone
#'           #  monitors in the vicinity of central Alabama that operated in
#'           #  1995
#'           \dontrun{aqs_monitors_by_box(parameter="44201",
#'                                                bdate=as.Date("19950101",
#'                                                            format="%Y%m%d"),
#'                                                edate=as.Date("19951231",
#'                                                             format="%Y%m%d"),
#'                                                minlat="33.3",
#'                                                maxlat="33.6",
#'                                                minlon="-87.0",
#'                                                maxlon="-86.7"
#'                                                    )
#'                    }
#' @export
aqs_monitors_by_box <- function(parameter, bdate, edate, minlat, maxlat,
                                    minlon, maxlon, return_header = FALSE)
{
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
#'         API server. Fortunately this operation has a linear run time
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
#' @examples # returns a aqs_v2 S3 object containing all ozone samples
#'              #  in the vicinity of central Alabama for
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
                                  minlon, maxlon, cbdate = NA_Date_,
                                  cedate = NA_Date_, return_header = FALSE)
{
  params <- aqsmultiyearparams(parameter = parameter,
                               bdate = bdate,
                               edate = edate,
                               minlat = minlat,
                               maxlat = maxlat,
                               minlon = minlon,
                               maxlon = maxlon,
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
#'                 bounding box provided for bdate - edate time frame. The data
#'                 returned is summarized at the annual level Variables returned
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
#' @family Aggregate _by_box functions
#' @inheritParams aqs_services_by_box
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that containing annual
#'           summary data for the box (area) requested. A AQS_Data Mart_APIv2
#'           is a 2 item named list in which the first item (\$Header) is a
#'           tibble of header information from the AQS API and the second item
#'           (\$Data) is a tibble of the data returned.
#' @examples # returns a tibble containing ozone annual summaries
#'           #  in the vicinity of central Alabama for the first two days
#'           #  in May, 2015
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
browser()
  annualsummary <- purrr::pmap(.l = params, .f = aqs_services_by_box)
  if (!return_header) annualsummary %<>% aqs_removeheader
  return(annualsummary)

}


#' @title aqs_dailysummary_by_box
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object containing sample data bounded within a
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
#'           a 2 item named list in which the first item (\$Header) is a tibble
#'           of header information from the AQS API and the second item (\$Data)
#'           is a tibble of the data returned.
#' @examples #returns a tibble of ozone daily summaries in the vicinity of
#'          #  central Alabama for the first two days in May 2015
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

#' @title aqs_removeheader
#' @description \lifecycle{stable}
#'                Coerces a single AQS_Data_Mart_APIv2 S3 object or
#'                a list of AQS_Data_Mart_APIv2 S3 objects into a single tibble
#'                object. This function decouples the $Data from the AQSAPI_v2
#'                object and returns only the $Data portion as a tibble. If the
#'                input is a list of AQSAPI_v2 objects combines the $Data
#'                portion of each AQS_Data_Mart_APIv2 S3 object into a single
#'                tibble with $Header information discarded. Else returns the
#'                input with no changes.
#' @note Since this function returns only the $Data portion of RAQSAPI_v2
#'   objects this means that the $Header information will not be present in the
#'   object being returned.
#' @param AQSobject An object of AQSAPI_v2 or a list of AQSAPI_v2 objects.
#' @importFrom dplyr bind_rows
#' @return a tibble of the combined $data portions of the input
#'           AQS_Data_Mart_APIv2 S3 object with the $Header portion discarded.
#' @examples
#'           \dontrun{ AQSobject <- aqs_removeheader(AQSobject)}
#' @export
aqs_removeheader <- function(AQSobject)
{
  #browser()
  if (is.null(AQSobject))
    {
    return(AQSobject)
    } else if (class(AQSobject) == "AQS_DATAMART_APIv2")
             {
                AQSobject <- AQSobject$Data
              } else if (class(AQSobject[[1]]) == "AQS_DATAMART_APIv2"
                         && is.list(AQSobject))
                       {
                          AQSobject %<>% lapply("[[", "Data") %>%
                            dplyr::bind_rows()
                        }
 return(AQSobject)
}


#' @title aqs_revisionhistory
#'
#' @description \lifecycle{stable}
#'                 Returns the change history  to the AQS Data Mart API.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested. If TRUE
#'   returns a AQSAPI_v2 object which is a two item list that contains header
#'   information returned from the API server mostly used for debugging
#'   purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object that is the return value
#'   from the AQS API. A AQS_Data Mart_APIv2 is a 2 item named list in which the
#'   first item ($Header) is a tibble of header information from the AQS API
#'   and the second item ($Data) is a tibble of the data
#'            returned.
#' @examples
#'  # read the Data Mart API revision history
#'  #  \dontrun{aqs_revisionHistory()}
#' @export
aqs_revisionhistory <- function(return_header = FALSE)
{
  history <- aqs(service = "metaData",
                 filter = "revisionHistory",
                 user =  getOption("aqs_username"),
                 user_key =  getOption("aqs_key"),
                 variables = NULL
                )
   if (!return_header) history %<>% aqs_removeheader
  return(history)
}


#' @title aqs_fieldsbyservice
#' @description \lifecycle{stable}
#'                Returns a tibble or an AQS_Data Mart_APIv2 S3
#'                object with the list and definitions of fields in the
#'                service requested.
#' @importFrom magrittr `%<>%`
#' @param service a string which represents the services provided by the AQS
#'                    API. For a list of available services
#'    @seealso \url{https://aqs.epa.gov/aqsweb/documents/data_api.html#services
#'                   }
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @examples # Returns a tibble containing a list and definitions
#'          #  of fields in the Sample Data service
#'          \dontrun{fields_by_service(service = "list")}
#'
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object with containing the list
#'   and definitions of fields requested service
#' @export
aqs_fields_by_service <- function(service, return_header = FALSE)
{
  fields <- aqs_metadata_service(filter = "fieldsByService", service = service)
   if (!return_header) fields %<>% aqs_removeheader
  return(fields)
}
