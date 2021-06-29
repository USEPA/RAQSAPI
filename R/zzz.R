#nocov start
.onLoad <- function(libname, pkgname) {
  utils::globalVariables(names = "env.RAQSAPI", package = "RAQSAPI")
  env.RAQSAPI <- Sys.getenv()
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")
  invisible()
}

.onUnLoad <- function(libname, pkgname) {
  Sys.setenv(env.RAQSAPI)
  invisible()
}

.onAttach <- function(libname, pkgname) {
packageStartupMessage("Use the function
                      RAQSAPI::aqs_credentials(username, key)
                      before using other RAQSAPI functions")
}
#nocov end
