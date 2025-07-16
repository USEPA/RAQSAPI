library(lubridate)
library(testthat)


#' @importFrom magrittr `%>%`()
#' @import testthat
#' @importFrom tibble tibble
#' @importFrom lubridate now
#' @import testthat`
test_that("AQS_DATAMART_API S3 class",
          {
            .Data <- tibble('a' = 1:10, 'b' = 11:20, 'c' = 21:30 )
            .Header <- tibble(Error = "NA",
                              datetime = now(),
                              status_code = 400,
                              user = "RAQSAPI testing",
                              url = "RAQSAPI package/tests",
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
