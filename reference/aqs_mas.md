# aqs_mas

**\[stable\]** Returns a table of monitoring agencies (MA).

## Usage

``` r
aqs_mas(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of monitoring agencies and
their associated agency code.

## Examples

``` r
# Returns a tibble or an AQS_DataMart_APIv2 S3 object
          # of monitoring agencies and their respective
          # monitoring agency codes.
          if (FALSE) aqs_mas() # \dontrun{}
```
