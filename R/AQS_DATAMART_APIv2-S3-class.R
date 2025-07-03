#' @title `AQS_DATAMART_APIv2` S3 class
#' @description \lifecycle{stable} AQS_DATAMART_APIv2
#'    AQS_DATAMART_APIv2 is a relatively simple R S3 object composed of two
#'    items, both of which are named lists. The first item, ($Header) in an
#'    AQS_DATAMART_APIv2 object is a tibble which contains the header
#'    information. The Header contains status information regarding the request
#'    (success/fail), any applicable error messages returned from the API, if
#'    any exist, the URL used in the request, a date and time stamp noting when
#'    request was received and other useful information. The second item of an
#'    AQS_DATAMART_APIv2 S3 object ($Data) is a tibble which contains the actual
#'    data being requested.
#'  @section `AQS_DATAMART_APIv2` S3 class properties
#'    * AQS_DATAMART_APIv2 S2 objects are feature to lists.
#'        * The first item in the list ($HEADER), is a tibble which contains
#'          the header information. The Header contains status information
#'          regarding the request (success/fail), any applicable error messages
#'          returned from the API, if any exist, the URL used in the request, a
#'          date and time stamp noting when request was received and other
#'          useful information.
#'        * The seconds item in the list ($Data), is a tibble which contains the
#'          actual data being requested. This is stored as a tibble.
#' @importFrom methods setOldClass
#' @name AQS_DATAMART_APIv2-S3-class
#' @seealso tibble::tibble#'
setOldClass("AQS_DATAMART_APIv2")

