# aqs_knownissues

**\[stable\]** Returns a table of any known issues with system
functionality or the data. These are usually issues that have been
identified internally and will require some time to correct in Data Mart
or the API. This function implements a direct API call to Data Mart and
returns data directly from the API. Issues returned via this function do
not include any issues from the RAQSAPI R package.

## Usage

``` r
aqs_knownissues(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object that contains information
involving known issues with the Data Mart API.

## Examples

``` r
      # Retrieve a tibble of known issues directly from the AQS data mart API
       if (FALSE) aqs_knownissues() # \dontrun{}
```
