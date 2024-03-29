% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RAQSAPIlistfunctions.R
\name{aqs_mas}
\alias{aqs_mas}
\title{aqs_mas}
\usage{
aqs_mas(return_header = FALSE)
}
\arguments{
\item{return_header}{If FALSE (default) only returns data requested.
If TRUE returns a AQSAPI_v2 object which is a two
item list that contains header information returned
from the API server mostly used for debugging
purposes in addition to the data requested.}
}
\value{
a tibble or an AQS_Data Mart_APIv2 S3 object of monitoring agencies
and their associated agency code.
}
\description{
\lifecycle{stable}
Returns a table of monitoring agencies (MA).
}
\examples{
# Returns a tibble or an AQS_Data Mart_APIv2 S3 object
          # of monitoring agencies and their respective
          # monitoring agency codes.
          \dontrun{aqs_mas()}
}
