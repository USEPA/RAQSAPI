#' #' @title print
#' #' @description \lifecycle{experimental}
#' #'                S3 Dispatch of the print function for AQS_Data Mart_APIv2 S3
#' #'                objects.
#' #' @param aqsObject a R S3 object that is returned from RAQSAPI aggregate
#' #'                  functions where return_header is TRUE. An
#' #'                  AQS_Data_Mart_APIv2 is a 2 item named list in which the
#' #'                  first item ($Header) is a tibble of header information from
#' #'                  the AQS API and the second item ($Data) is a tibble of the
#' #'                  data returned.
#' #' @param ... Other parameters to be passed on to tibble:::print.tbl_df().
#' #' @importFrom tibble print.tbl_df
#' #' @export
#' print <- function(aqsObject, ...)
#'   {
#'     print(aqsObject$Header)
#'     print(aqsObject$Data)
#'   }
#'
#' #' @title mutate
#' #' @description \lifecycle{experimental}
#' #'                S3 Dispatch of the mutate function for AQS_Data Mart_APIv2 S3
#' #'                objects.
#' #' @param aqsObject a R S3 object that is returned from RAQSAPI aggregate
#' #'                  functions where return_header is TRUE. An
#' #'                  AQS_Data_Mart_APIv2 is a 2 item named list in which the
#' #'                  first item ($Header) is a tibble of header information from
#' #'                  the AQS API and the second item ($Data) is a tibble of the
#' #'                  data returned.
#' #' @param ... Other parameters to be passed on to tibble:::print.tbl_df().
#' #' @importFrom tibble print.tbl_df
#' #' @export
#' mutate <- function(aqsObject, ...)
#'   {
#'     UseMethod("dplyr:::mutate_.tbl_df(.data = aqsObject, ...)")
#'   }
