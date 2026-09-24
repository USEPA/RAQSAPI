# nocov start

#' @noRd
#' @keywords internal # do not include deprecated function in package reference manual

#' @title .onload
#' @description setup and configuration of RAQSAPI package when loaded into an
#' R environment. This function is not to be manually called by the end-user.
#' @return return NULL invisibly
#' @keywords internal # do not include deprecated function in package reference manual
#' @noRd
.onLoad <- function(libname, pkgname) {
  .RAQSAPI_env <<- new.env(parent = emptyenv())
  setOldClass("AQS_DATAMART_APIv2")
  utils::globalVariables(
    names = c("env.RAQSAPI", "AQSObject", "datetime", "."),
    package = "RAQSAPI"
  )
  .RAQSAPI_env$old_R_CHECK_LENGTH_1_CONDITION_ <- Sys.getenv("_R_CHECK_LENGTH_1_CONDITION_", unset = NA_character_)
  return(invisible())
}


#' @title .onUnLoad
#' @description restore configuration of R environment DURING UNLOADING OF RAQSAPI
#'           to a state in which is was set before loading RASQSAPI.
#' @param libpath A character string giving the path to the package installation directory.
#' @return return NULL invisibly
#' @keywords internal # do not include deprecated function in package reference manual
#' @noRd
.onUnLoad <- function(libpath) {
  if (is.na(.RAQSAPI_env$old_R_CHECK_LENGTH_1_CONDITION_)) {
    Sys.unsetenv("_R_CHECK_LENGTH_1_CONDITION_")
  } else {
    Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = .RAQSAPI_env$old_R_CHECK_LENGTH_1_CONDITION_)
  }
  return(invisible())
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user when
#'              the RAQSAPI library is attached to a R environment.
#' @param libname A character string giving the library directory where the
#'   package defining the namespace was found.
#' @param pkgname A character string giving the name of the package.
#' @return NULL
#' @keywords internal # do not include deprecated function in package reference manual
#' @noRd
.onAttach <- function(libname, pkgname) {
  RAQSAPIstartupmessage <- paste(
    "Use the function",
    "RAQSAPI::aqs_credentials(username, key)",
    "before using other RAQSAPI functions",
    "See ?RAQSAPI::aqs_credentials for more information",
    sep = "\n"
  )
  packageStartupMessage(RAQSAPIstartupmessage)
  return(invisible())
}
# nocov end
