#' @title AQS_DATAMART_APIv2_validator
#' @description `r lifecycle::badge("stable")`
#'              Helper function to Validate data structure of data before
#'              converting it into an AQS_DATAMART_APIv2 S3 object.
#' @param .AQSobject A 2 item named list in which the first item ($Header) is a
#'              tibble of header information from the AQS API and the second
#'              item ($Data) is a tibble of the data returned.
#' @importFrom tibble is_tibble
#' @importFrom rlang is_list
#' @note The aqsobject must be a two item list each containing a tibble with the
#'       first item named 'Header' and the second 'Data'.
#' @seealso AQS_DATAMART_APIv2-S3-class
#' @return NULL
#' @noRd
#' @keywords internal
AQS_DATAMART_APIv2_validator <- function(.AQSobject) {
  stopifnot(
    length(.AQSobject) == 2 |
      all(names(.AQSobject) == list("Header", "Data")) |
    rlang::is_list(.AQSobject) |
    tibble::is_tibble(.AQSobject$Header) |
    tibble::is_tibble(.AQSobject$Data)
  )
  return(invisible())
}


#' @title AQS_DATAMART_APIv2-S3-class
#' @name new_AQS_DATAMART_APIv2
#' @description `r lifecycle::badge("stable")` AQS_DATAMART_APIv2
#'    AQS_DATAMART_APIv2 is a relatively simple R S3 object composed of two
#'    named lists. The first list, ($Header) in a tibble which contains the header
#'    information. The Header contains status information regarding the request
#'    (success/fail), any applicable error messages returned from the API, if
#'    any exist, the URL used in the request, a date and time stamp noting when
#'    request was received and other useful information. The second item of an
#'    AQS_DATAMART_APIv2 S3 object ($Data) is a tibble which contains the actual
#'    data being requested.
#'  @param x A 2 item named list in which the first item ($Header) is a
#'               tibble of header information from the AQS API and the second
#'               item ($Data) is a tibble of the data returned.
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
#' @importFrom tibble tibble tibble is_tibble
#' @importFrom magrittr %>%
#' @importFrom rlang abort is_list
#' @note The .x must be a two item list each containing a tibble with the
#'       first item named 'Header' and the second 'Data'.
#' @return a AQS_DATAMART_APIv2 S3 object that is the return value from the
#'            AQS API. A AQS_DATAMART_APIv2 is a 2 item named list in which the
#'            first item ($Header) is a tibble of header information from the
#'            AQS API and the second item ($Data) is a tibble of the data
#'            returned.
#' @seealso tibble::tibble#'
#' @keywords internal
new_AQS_DATAMART_APIv2 <- function(x) {
  if(!(rlang::is_list(x) & length(x) == 2))
  {rlang::abort(message="x should be a two item list")}
  class(x) <- "AQS_DATAMART_APIv2"
  names(x) <- list("Header", "Data")
  AQS_DATAMART_APIv2_validator(x)
  return(x)
}
