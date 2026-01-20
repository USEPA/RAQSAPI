library(httptest2)
source("helper.R")
httptest2::.mockPaths(c("./tests/testthat/",
  "./tests/testthat/bybox",
  "./tests/testthat/bycbsa",
  "./tests/testthat/byco",
  "./tests/testthat/byMA",
  "./tests/testthat/bypqao",
  "./tests/testthat/bysite",
  "./tests/testthat/bystate",
  "./tests/testthat/listfunctions"))

# for contexts where the package needs to be fooled
# (CRAN, forks)
# this is ok because the package will used recorded responses
# so no need for a real secret
if (!nzchar(Sys.getenv("RAQSAPIKEY"))) {
  Sys.setenv(RAQSAPIKEY = "<redacted>")
}

if (!nzchar(Sys.getenv("RAQSAPIUSERNAME"))) {
  Sys.setenv(RAQSAPIUSERNAME = "<redacted>")
}
