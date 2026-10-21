# RAQSAPI repository instructions

## Project structure

RAQSAPI is an R package wrapping the US EPA Air Quality System (AQS) Data Mart REST API. Public retrieval functions are organized by geography in `R/bysite.R`, `R/bycounty.R`, `R/bystate.R`, `R/byma.R`, `R/bypqao.R`, `R/bycbsa.R`, and `R/bybox.R`; metadata and lookup endpoints are in `R/RAQSAPIlistfunctions.R`.

Those public functions validate inputs with `checkaqsparams()` and delegate request construction to the geography-specific `aqs_services_by_*()` helpers in `R/AQSAPI_helperfunctions.R`. The shared `aqs()` helper handles the `httr2` request, throttling, retries, error handling, JSON parsing, and construction of an `AQS_DATAMART_APIv2` object (`Header` and `Data` tibbles). Many public functions remove the header by default and return the data tibble; preserve the `return_header` behavior. Services limited to one calendar year use `aqsmultiyearparams()` to split date ranges into yearly requests.

## Build, test, and lint

Run from the repository root:

```r
devtools::install_dev_deps()
devtools::build()
devtools::test()
devtools::test(filter = "helperfunctions")
devtools::check()
lintr::lint_package() # requires lintr
```

`devtools::test(filter = "helperfunctions")` runs the matching test file; replace `helperfunctions` with another test filename stem to focus on that file. The test suite uses `testthat` and `httptest2` response fixtures under `tests/testthat/`. Tests that exercise authenticated endpoints still check that credentials are available: set `RAQSAPIUSERNAME` and `RAQSAPIKEY` in the local environment, or configure the test-only `local.R` keyring helper. Do not commit credentials.

## Project conventions

- Keep API functions' parameter validation centralized in `checkaqsparams()` and build requests through the existing shared helpers rather than duplicating endpoint handling.
- AQS identifiers such as parameter, state FIPS, county, and site codes are character strings; their leading zeroes are significant. Date-range arguments are `Date` objects.
- Preserve the two-part `AQS_DATAMART_APIv2` response (`Header`, `Data`) when `return_header = TRUE`; use the established header-removal path for the default tibble result.
- Public documentation is authored in roxygen comments alongside the R code. Update those source comments rather than generated `.Rd` files.
- Follow the tidyverse style configured by `air.toml` and `.lintr.R` (two-space indentation, 125-character line width, `<-` assignment, and the existing magrittr pipe style). Avoid reformatting unrelated code.
- User-facing changes should get a bullet near the top of `NEWS.md`, as described in `contributing.md`.
