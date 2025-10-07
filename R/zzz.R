# nocov start
#' @noRd
#' @keywords internal # do not include deprecated function in
#'                    # package reference manual

#' @title .onload
#' @description setup and configuration of RAQSAPI package when loaded into an
#' R environment. This function is not to be manually called by the end-user.
#' @inheritParams base ns-hooks
#' @return return NULL invisibly
#' @keywords internal # do not include deprecated function in
#'                    # package reference manual
#' @noRd
.onLoad <- function(libname, pkgname)
  {
  setOldClass("AQS_DATAMART_APIv2")
  utils::globalVariables(
    names = c("env.RAQSAPI", "AQSObject", "datetime"),
    package = "RAQSAPI"
  )
  env.RAQSAPI <- Sys.getenv()
  Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = "TRUE")
  invisible()
}


#' @title .onUnLoad
#' @description restore configuration of R environment DURING UNLOADING OF RAQSAPI
#'           to a state in which is was set before loading RASQSAPI.
#' @inheritDotParams base ns-hooks
#' @return return NULL invisibly
#' @keywords internal # do not include deprecated function in
#'                    # package reference manual
#' @noRd
.onUnLoad <- function(libname, pkgname)
  {
  Sys.setenv(env.RAQSAPI)
  invisible()
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user when
#'              the RAQSAPI library is attached to a R environment.
#' @inheritParams base .onAttech
#' @return NULL
#' @keywords internal # do not include deprecated function in
#'                    # package reference manual
#' @noRd
.onAttach <- function(libname, pkgname)
  {
  RAQSAPIstartupmessage <- paste(
    "Use the function", "RAQSAPI::aqs_credentials(username, key)", "before using other RAQSAPI functions", "See ?RAQSAPI::aqs_credentials for more information",
    sep = "\n"
  )
  packageStartupMessage(RAQSAPIstartupmessage)
  invisible()
}
# nocov end
