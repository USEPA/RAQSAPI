# aqs_revisionhistory

**\[stable\]** Returns the change history to the AQS Data Mart API.

## Usage

``` r
aqs_revisionhistory(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object that is the return value
from the AQS API. A AQS_DataMart_APIv2 is a 2 item named list in which
the first item (\$Header) is a tibble of header information from the AQS
API and the second item (\$Data) is a tibble of the data returned.

## Examples

``` r
 # Returns a DataFrame of the EPA AQS Data Mart API revision history
 #  \dontrun{ aqs_revisionHistory() }
```
