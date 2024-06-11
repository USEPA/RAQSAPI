#' @importFrom lubridate today year mdy '%within%' NA_Date_
#' @importFrom lifecycle deprecate_soft badge


#' @title RAQSAPI: A R Interface to The United States Environmental Protection
#' Agency's Air Quality System Data Mart RESTful API server
#'
#' @description RAQSAPI is a package for R that connects the R programming
#' environment to the United State's Environmental protection agency's
#' Air Quality System (AQS) Data Mart API for retrieval of air
#' monitoring data.
#'
#' There are two things that you must do before using this package.
#' 1) If you have not done so yet register your username with Data Mart
#' 2) Every time this library is reloaded AQS_API_credentials() function
#'       must be called before continuing.
#'
#' please use vignette(RAQSAPI) for more details about this package.
#'
#' EPA Disclaimer:
#' This software/application was developed by the U.S. Environmental Protection
#' Agency (USEPA). No warranty expressed or implied is made regarding the
#' accuracy or utility of the system, nor shall the act of distribution
#' constitute any such warranty. The USEPA has relinquished control of the
#' information and no longer has responsibility to protect the integrity,
#' confidentiality or availability of the information. Any reference to specific
#' commercial products, processes, or services by service mark, trademark,
#' manufacturer, or otherwise, does not constitute or imply their endorsement,
#' recommendation or favoring by the USEPA. The USEPA seal and logo shall not
#' be used in any manner to imply endorsement of any commercial product or
#' activity by the USEPA or the United States Government.
#' @docType package
#' @name RAQSAPI
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecated
##' importFrom lifecycle deprecate_soft badge
## usethis namespace: end
