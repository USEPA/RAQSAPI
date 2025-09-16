# install_RAQSAPI.R This file includes functions to make it easier to deal with building RAQSAPI and is targeted at RAQSAPI
# development.

library(devtools)
library(usethis)
library(dplyr)
library(testthat)
library(spelling)
library(knitr)
library(magrittr)
library(glue)
library(goodpractice)
library(pkgstats)
library(pkgcheck)
library(vroom)
library(tibble)
library(desc)

removehttptest2mocks <- function()
{
  unlink("./tests/testthat/bybox", recursive = TRUE)
  unlink("./tests/testthat/bycbsa", recursive = TRUE)
  unlink("./tests/testthat/byco", recursive = TRUE)
  unlink("./tests/testthat/byMA", recursive = TRUE)
  unlink("./tests/testthat/bypqao", recursive = TRUE)
  unlink("./tests/testthat/bysite", recursive = TRUE)
  unlink("./tests/testthat/bystate", recursive = TRUE)
  unlink("./tests/testthat/listfunctions", recursive = TRUE)
  return(NULL)
}

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
#'   # To remove RAQSAPI and clean the project directory of files
#'   #  created during the package build process.
#'   \dontrun{
#'            RAQSAPICLEAN()
#'           }
#' @internal
#' @noRd
RAQSAPICLEAN <- function(NAMESPACE = TRUE)
  {
  if ("RAQSAPI" %in% loaded_packages()$package)
  {
    unload("RAQSAPI")
  }
  if ("RAQSAPI" %in% installed.packages())
  {
    remove.packages("RAQSAPI")
  }
  if (NAMESPACE == TRUE)
  {
    unlink("NAMESPACE")
  }
  unlink("README.md")
  unlink("doc", recursive = TRUE)
  unlink("man/html", recursive = TRUE)
  unlink("man", recursive = TRUE)
  unlink("Meta", recursive = TRUE)
  unlink("README.html")
  unlink("README.aux")
  unlink("README.tex")
  unlink("README.log")
  unlink("README.pdf")
  unlink("index.md")
  unlink("MD5")
  unlink("./vignettes/AQSAPI-concordance.tex")
  unlink("./man/html/", recursive = TRUE)
  #unlink("./inst/contributorcodeofconduct.md")
  #unlink(".vignettes/contributing.md")
  return(NULL)
}

RAQSAPIinstallMD5sum <- function(path=".")
{
  #  tools:::.installMD5sums(pkgDir = ".")  #requires rtools #does not allow excluding files

  filenames <- dir(path, recursive = TRUE)
  filenames <- filenames[!grepl("*MD5|dev/|local.R", filenames)]
  md5files <- bind_cols(md5checksum = tools::md5sum(filenames),
                        filename = stringr::str_c("*", filenames), .name_repair = "universal")
  #colnames(md5files) <- c("md5checksum", "filename")
  vroom::vroom_write(x = md5files, file="MD5", delim=" ", eol="\n", col_names=FALSE, append=FALSE, bom=TRUE)
}


#' @name buildRAQSAPIbase
#' @description a helper function, not to be called directly by the user to
#'   build the base of the RAQSAPI package
#' @retun This function is called for its side effects,therefore returns
#'        NULL (invisibly).
#' @examples
#'   # Builds the base RAQSAPI package
#'   \dontrun{buildRAQSAPI()}
#' @internal
#' @nord
buildRAQSAPIbase <- function()
{
  if ("RAQSAPI" %in% loaded_packages()$package)
  {
    unload("RAQSAPI")
  }
  invisible(usethis::use_lifecycle())
  source("./dev/alphabetize_WORDLIST.R")
  #need to run document twice to work out the circuular dependencies with RAQSAPI_helperfunctions.R
  devtools::document(quiet = TRUE, roclets = c("collate", "namespace", "rd", "vignette"))
  devtools::document(quiet = TRUE, roclets = c("collate", "namespace", "rd", "vignette"))
  devtools::build_readme()
  # knitr::knit(input = './dev/contributing.Rmd', output = './dev/contributing.md') #removed this file
  knitr::knit(input = "./cran-comments.Rmd", output = "./cran-comments.md")
  RAQSAPIinstallMD5sum()
  #knitr::knit(input = "./vignettes/contributing.Rmd", output = "./vignettes/contributing.md")
  #knitr::knit(input = "./inst/contributorcodeofconduct.Rmd", output = "./inst/contributorcodeofconduct.md")
  return(NULL)
}


#' @name RAQSAPIBUILD
#' @description builds the development version RAQSAPI
#' @note do not use if the intent is to build RAQSAPI for end users, instead
#'   use @seealso [RAQSAPIINSTALL()] as this build will export objects
#'   not intended for end use.
#' @examples
#'  # builds a development build of RAQSAPI
#'  \dontrun{ RAQSAPIBUILD() }
#'  @internal
#'  @noRd
RAQSAPIBUILD <- function()
{
  buildRAQSAPIbase()
  devtools::build(binary = FALSE,
                  quiet = TRUE,
                  manual = TRUE,
                  vignettes = TRUE,
                  clean_doc = TRUE,
                  args="--no-multiarch --with-keep.source --preclean --byte-compile --use-vanilla --compact-vignettes --md5")
  return(NULL)
}


#' @name RAQSAPIINSTALL
#' @description builds and installs package:RAQSAPI
#' @note this will build and install a generic version RAQSAPI as if it were
#'   being installed from CRAN. Package developers might want to consider using
#'   @seealso [RAQSAPIBUILD()] instead.
#' @examples # builds a development build of RAQSAPI
#'  \dontrun{ RAQSAPIINSTALL() }
#'  @internal
#'  @noRd
RAQSAPIINSTALL <- function()
{
  buildRAQSAPIbase()
  devtools::install(pkg=".", reload = TRUE, quiet = TRUE, dependencies = TRUE, upgrade = "ask", build_vignettes = TRUE,
                    quick = FALSE, keep_source = TRUE)
  return(NULL)
}

RAQSAPICHECK <- function()
{
  RAQSAPIversion <- desc::desc_get("Version")
  RAQSAPIpkg <- normalizePath(glue("../RAQSAPI_{RAQSAPIversion}.tar.gz"))
  if(file.exists(RAQSAPIpkg))
  {
    check_built(path = RAQSAPIpkg, cran = TRUE, manual = TRUE, remote = TRUE, incoming = TRUE, force_suggests = TRUE)
    goodpractice::goodpractice(checks=all_checks())
    builtpackage <- list.files(path="../", pattern="^RAQSAPI_......tar.gz$")
    if(length(builtpackage) != 1) {error("no or multiple built packages exists!!! Delete one of them, or build one")}
    testthat::test_local(path=".", load_package="source")
  } else
    {
      RAQSAPIBUILD()
      RAQSAPICHECK()
    }
    return(NULL)
}


#' @name RAQSAPITESTCOVERAGE
#' @description generates a package unit test coverage report
#' @note This function is intended for development purposes.
#' Due to the fact that the unit test for RAQSAPI calls RAQSAPI calls
#' testthat::skip_on_cran covr typically reports incorrect results. This
#' function implements the suggestions given by the package maintainers
#' (https://issueexplorer.com/issue/r-lib/covr/466) to correctly report package
#' coverage
#' @importFrom devtools test_coverage()
#' @importFrom withr with_envvar()
#' @example
#'   # To check the RAQSAPI package for errors before pushing changes upstream
#'   \dontrun(RAQSAPITESTCOVERAGE()}
#' @internal
#' @noRd
RAQSAPITESTCOVERAGE <- function()
{
  withr::with_envvar(
    new = c(NOT_CRAN = TRUE),
    code = Sys.getenv("NOT_CRAN")
  )
  test_coverage()
  # restore the NOT_CRAN environment variable to it's former state Sys.setenv(NOT_CRAN = NOT_CRAN_TEMP)
  return(NULL)
}

#' @name RAQSAPIQUICKINSTALL
#' @description a shortcut to quickly uninstall RAQSAPI then quickly install the
#' latest development RAQSAPI package locally from source.
#' @Note This function is intended for development purposes.
#' @Seealso [RAQSAPICLEAN()], [RAQSAPICLEAN()], [RAQSAPIBUILD()],
#' [RAQSAPIINSTALL()]
#' @noRd
RAQSAPIQUICKINSTALL <- function()
{
  RAQSAPICLEAN(NAMESPACE = TRUE)
  RAQSAPIBUILD()
  RAQSAPIINSTALL()
  return(NULL)
}


#' @name RAQSAPIFULLINSTALL
#' @description shortcut to re-install a new development version of RAQSAPI from
#' source then runs testing and checking of the newly installed RAQSAPI package.
#' This function is intended for development purposes only.
#' @Note This function may take a while to run, especially running tests/checks.
#' @Seealso [RAQSAPICLEAN()], [RAQSAPICLEAN()], [RAQSAPIBUILD()],
#' [RAQSAPIINSTALL()], [RAQSAPICHECK()]
#' @internal
#' @noRd
RAQSAPIFULLINSTALL <- function()
{
  RAQSAPIQUICKINSTALL()
  RAQSAPICHECK()
  return(NULL)
}
