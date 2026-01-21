#' @importFrom magrittr `%>%`()
#' @importFrom tibble tibble
#' @importFrom lubridate now leap_year
test_that(
  "AQS_DATAMART_API S3 class", {
    .Data <- tibble::tibble(a = 1:10, b = 11:20, c = 21:30)
    .Header <- tibble::tibble(
      Error = "NA", datetime = lubridate::now(), status_code = 400, user = "RAQSAPI testing", url = "RAQSAPI package/tests",
      format = "application/json",
      content = "fake data"
    )
    AQSobject <- .Data
    new_AQS_DATAMART_APIv2(AQSobject) %>%
      expect_error()
    AQSobject <- list(Header = .Header, Data = .Data) %>%
      expect_no_error()
  }
)


#' @importFrom magrittr `%>%`()
#' @importFrom tibble tibble tribble
#' @importFrom lubridate mdy_hms now year
#' @import from glue glue
#' @import testthat
#' @importFrom lubridate now year mdy_hms
test_that(
  "test AQS_DATAMART_APIv2_validator", {

    year <- lubridate::now() %>%
      year()
    fakeData <- tibble(
      datetime = seq.POSIXt(
        from = lubridate::mdy_hms(glue("01-01-{lubridate::now() %>% year()} 00:00:00")),
        to = lubridate::mdy_hms(glue("12-31-{lubridate::now() %>% year()} 23:59:59")),
        by = "hour"
      ),
      sample = rnorm(
        n = ifelse(
          lubridate::leap_year(year),
          87840, 8760
        ),
        mean = 100, sd = 50
      ),
      site_name = "Fake air monitoring data", pollutant_code = 253046, lat = 22, long = -175
    )

    fakeheader <- tibble(
      timestamp = paste0("07-01-", year, "12:00:00Z"),
      url = "https://aqs.epa.gov/data/api/"
    )
    list(Header = fakeheader, Data = fakeData) %>%
      RAQSAPI:::AQS_DATAMART_APIv2_validator() %>%
      expect_no_error()
  }
)
