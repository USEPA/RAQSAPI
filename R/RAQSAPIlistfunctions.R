#' @section list functions


#' @title aqs_isavailable
#' @description \lifecycle{stable}
#'                returns a tibble that details the status of the
#'                AQS Data Mart API.
#' @importFrom magrittr `%>%`
#' @return a tibble that details the status of the AQS Data Mart API.
#' @examples
#'   # Check if the AQS API is up, running and accepting requests.
#'   \dontrun{ aqs_isAvailable() }
#' @export
aqs_isavailable <- function()
{
    aqs(service = "metaData",
        filter = "isAvailable",
        user =  getOption("aqs_username"),
        user_key =  getOption("aqs_key")
        )$Header
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
#'       # Retrieve a tibble of known issues directly from the AQS data mart API
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
#'           # Returns a tibble all the counties
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
     counties %<>% renameaqsvariables(name1 = "county_code",
                                      name2 = "county_name")
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
#' @examples # Returns an AQS_Data Mart_APIv2 S3 object witch returns all sites
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
#' @examples # Returns a tibble of parameter classes (groups of parameters, i.e.
#'           # "criteria" or all")
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
#'                  be an exact match to what is returned from aqs_classes()
#'                  (case sensitive).
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
#' @examples # Returns a tibble or an AQS_Data Mart_APIv2 S3 object
#'           # of monitoring agencies and their respective
#'           # monitoring agency codes.
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
#' @examples # Returns a tibble of primary quality assurance
#'           # organizations (pqaos)
#'            \dontrun{ aqs_pqaos() }
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
#'                 Returns a table of all Core Based Statistical Areas (cbsa)
#'                 and their associated cbsa_codes. for constructing other
#'                 requests.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2  S3 object of all Core Based
#'         Statistical Areas (cbsa) and their cbsa_codes for constructing
#'         other requests.
#' @examples # Returns a tibble of Core Based Statistical Areas (cbsas)
#'           # and their respective cbsa codes
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
#'                 district or Columbia with their respective FIPS codes.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns an AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of states and their
#'            associated FIPS codes.
#' @examples # Returns a tibble of states and their FIPS codes
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
  if (is.null(AQSobject))
    {
    return(AQSobject)
    } else if (isa(x = AQSobject, what = "AQS_DATAMART_APIv2"))
      #if (class(AQSobject) %in% "AQS_DATAMART_APIv2")
    {
      AQSobject <- AQSobject$Data
    } else if (isa(x = AQSobject[[1]], what = "AQS_DATAMART_APIv2") &&
                                    is.list(AQSobject))
    {
      AQSobject %<>% lapply("[[", "Data") %>%
        dplyr::bind_rows()
    }

 return(AQSobject)
}


#' @title aqs_revisionhistory
#'
#' @description \lifecycle{stable}
#'                 Returns the change history to the AQS Data Mart API.
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
#'  # Returns a DataFrame of the EPA AQS Data Mart API revision history
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
#'           #  of fields in the Sample Data service
#'          \dontrun{aqs_fieldsbyservice(service = "sampleData")}
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


#' @title aqs_sampledurations
#' @description \lifecycle{stable}
#'                 Returns a table of sample durations and their
#'                 associated duration codes. Returned values are not calculated
#'                 durations such as 8 hour CO or O$_3$ rolling averages, 3/6
#'                 day PM averages or Pb 3 month rolling averages.
#' @importFrom magrittr `%<>%`
#' @param return_header If FALSE (default) only returns data requested.
#'                        If TRUE returns a AQSAPI_v2 object which is a two
#'                        item list that contains header information returned
#'                        from the API server mostly used for debugging
#'                        purposes in addition to the data requested.
#' @note Not all sample durations that are available through AQS are available
#'       through the AQS Data Mart API, including certain calculated sample
#'       durations. Only sample durations that are available through the
#'       AQS Data Mart API are returned.
#' @return a tibble or an AQS_Data Mart_APIv2 S3 object of sample durations and
#'         their associated duration codes
#'                 (groups of parameters, i.e. "criteria" or "all").
#' @examples # Returns a tibble or an AQS_Data Mart_APIv2 S3 object of
#'          \dontrun{ aqs_sampledurations() }
#' @export
aqs_sampledurations <- function(return_header = FALSE)
{
  AQS_domain <- "aqs.epa.gov"

    durations <- aqs(service = "list",
                 filter = "duration",
                 user =  getOption("aqs_username"),
                 user_key =  getOption("aqs_key"),
                 variables = NULL,
                 AQS_domain = AQS_domain
                 )

  if (!return_header) durations %<>% aqs_removeheader
  return(durations)
}
