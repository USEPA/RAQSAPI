#' install_RAQSAPI.R
#'  This file includes functions to make it easier to deal with building
#'  RAQSAPI and is targeted at RAQSAPI development.


library(devtools)
library(usethis)
library(spelling)
library(goodpractice)
library(knitr)

#' @name RAQSAPICLEAN
#' @description removes Package:RASAPI then removes files created during the
#'   build phase of RAQSAPI including the NAMESPACE file (if NAMESPACE = TRUE
#'   [default]), compiled vignettes and intermediate files, Rd files and README
#'   files.
#' @param NAMESPACE logical, if TRUE (default) deletes the RAQSAPI
#'   NAMESPACEFILE. Useful if it is expected that building the package will
#'   alter the NAMESPACE so that the developer can ensure that a new NAMESPACE
#'   was created.
#' @note This will remove older versions of RAQSAPI be sure that you intend on
#'   removing the older version of RAQSAPI before continuing. It has been
#'   witnessed that sometimes the NAMESPACE is not updated after rebuilding a
#'   package. By deleting the NAMESPACE file before rebuilding the developer
#'   can ensure that they are using the most recent version of the NAMESPACE.
#' @examples
#'   # to remove RAQSAPI and clean the project directory of files
#'   #  created during the package build process.
#'   \dontrun{
#'            RAQSAPICLEAN()
#'           }
#' @noRd
RAQSAPICLEAN <- function(NAMESPACE = TRUE)
{
  if ("RAQSAPI" %in% loaded_packages()$package) { unload("RAQSAPI") }
  if ("RAQSAPI" %in% installed.packages()) { remove.packages("RAQSAPI") }
  if (NAMESPACE == TRUE) {unlink("NAMESPACE")}
  unlink("README.md")
  unlink("doc", recursive = TRUE)
  unlink("man", recursive = TRUE)
  unlink("Meta", recursive = TRUE)
  unlink("README.html")
  unlink("README.aux")
  unlink("README.tex")
  unlink("README.log")
  unlink("README.pdf")
  unlink("MD5")
  unlink("./vignettes/AQSAPI-concordance.tex")
}


#' @name buildRAQSAPIbase
#' @description a helper function, not to be called directly by the user to
#'   build the base of the RAQSAPI package
#'
#' @examples
#'   # builds the base RAQSAPI package
#'   \dontrun{buildRAQSAPI()}
#' @nord
buildRAQSAPIbase <- function()
{
  if ("RAQSAPI" %in% loaded_packages()$package) { unload("RAQSAPI") }
  invisible(usethis::use_lifecycle())
  devtools::document(quiet = TRUE,
                     roclets = c("collate", "namespace", "rd", "vignette"))
  devtools::build_readme()
  knitr::knit(input = "./dev/contributing.Rmd", output = "./dev/contributing.md")
  knitr::knit(input = "./cran-comments.Rmd", output = "./cran-comments.md")
  tools:::.installMD5sums(pkgDir = ".")
}


#' @name RAQSAPIBUILD
#' @description builds the development version RAQSAPI
#' @note do not use if the intent is to build RAQSAPI for end users, instead
#'   use @seealso [RAQSAPIINSTALL()] as this build will export objects
#'   not intended for end use.
#' @examples
#'  builds a development build of RAQSAPI
#'  \dontrun{ RAQSAPIBUILD() }
#'  @noRd
RAQSAPIBUILD <- function()
{
  buildRAQSAPIbase()
  devtools::build(binary = TRUE, quiet = TRUE)
}


#' @name RAQSAPIINSTALL
#' @description builds and installs package:RAQSAPI
#' @note this will build and install a generic version RAQSAPI as if it were
#'   being installed from CRAN. Package developers might want to consider using
#'   @seealso [RAQSAPIBUILD()] instead.
#' @examples
#'  builds a development build of RAQSAPI
#'  \dontrun{ RAQSAPIINSTALL() }
#'  @noRd
RAQSAPIINSTALL <- function()
{
  buildRAQSAPIbase()
  devtools::install(reload = TRUE, quiet = TRUE, dependencies = TRUE,
                    upgrade = "always", build_vignettes = TRUE, quick = FALSE)
}


#' @name RAQSAPICHECK
#' @description A set of basic checks for those developing RAQSAPI before
#'   pushing code upstream, these check include those provided by spelling,
#'   testthat and goodpractice libraries.
#' @note This function may take a few minutes to finish. Some of functions will
#'   hit the Data Mart API server and that process may take a while to return
#'   results. CRAN has strict policies about accepting code that produces
#'   warnings, significant notes or build errors. In addition they implement
#'   rigid guidelines for code quality, legal requirements and build quality. In
#'   order to comply with CRAN's code submission policies please use this
#'   function and remove any errors, warning or notes that your code change has
#'   introduced. Any new code that produces new error messages, notes or
#'   warnings will be rejected unless the project maintainers have given prior
#'   approval. Unfortunately some of the check in groodpracticee duplicate the
#'   checks in devtools. At some future point in time we will streamline the
#'   check process so that it is more efficient. Run RAQSAPIBUILD() first.
#' @examples
#'   #to check the RAQSAPI package for errors before pushing changes upstream
#'   \dontrun(RAQSAPICHECK()
#'   }
#' @noRd
RAQSAPICHECK <- function()
{
  if ("RAQSAPI" %in% .packages()) {detach("package:RAQSAPI", unload = TRUE)}
  if (!file.exists("NAMESPACE")) {stop("NAMESPACE fille missing!")}
  if (!file.exists("README.md")) {stop("README.md fille missing!")}
  devtools::spell_check(vignettes = TRUE, use_wordlist = TRUE)
  # spelling::spell_check_files(path = "./dev/contributing.Rmd",
  #                   ignore = read.csv(file = "./inst/WORDLIST",
  #                                     header = FALSE)$V1
  #                                     )
  devtools::check_built(path = paste0("../RAQSAPI_",
                                      desc::desc_get_field(key = "Version"),
                                      ".tar.gz"),
                        cran = TRUE,
                        remote = TRUE,
                        manual = TRUE,
                        quiet = FALSE,
                        run_dont_test = TRUE,
                        error_on = "error",
                        force_suggests = TRUE
                        )
  if ("RAQSAPI" %in% .packages()) {detach("package:RAQSAPI", unload = TRUE)}
  goodpractice::gp(quiet = TRUE)
  devtools::revdep(pkg = "RAQSAPI", recursive = TRUE)
  if (!tools::checkMD5sums(dir = ".")) { warning("checkMD5sums, failed") }
  }
