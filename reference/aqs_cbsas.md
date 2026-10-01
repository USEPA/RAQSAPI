# aqs_cbsas

**\[stable\]** Returns a table of all Core Based Statistical Areas
(cbsa) and their associated cbsa_codes. for constructing other requests.

## Usage

``` r
aqs_cbsas(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of all Core Based
Statistical Areas (cbsa) and their cbsa_codes for constructing other
requests.

## Examples

``` r
# Returns a tibble of Core Based Statistical Areas (cbsas)
          # and their respective cbsa codes
          if (FALSE)  aqs_cbsas()  # \dontrun{}
```
