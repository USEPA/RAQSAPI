#nocov start
.onLoad <- function(libname, pkgname)
  {
  utils::globalVariables(names = c("env.RAQSAPI", "AQSObject", "datetime"),
                         package = "RAQSAPI")
  env.RAQSAPI <- Sys.getenv()
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")
  invisible()
}

.onUnLoad <- function(libname, pkgname)
{
  Sys.setenv(env.RAQSAPI)
  invisible()
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user when
#'              the RAQSAPI library is loaded.
#' @inheritParams base .onAttech
#' @return NULL
#' @noRd
.onAttach <- function(libname, pkgname)
  {
     RAQSAPIstartupmessage <- paste("Use the function",
           "RAQSAPI::aqs_credentials(username, key)",
           "before using other RAQSAPI functions",
           "See ?RAQSAPI::aqs_credentials for more information",
           sep = "\n"
          )
     packageStartupMessage(RAQSAPIstartupmessage)
  }
#nocov end
