# aqs_states

**\[stable\]** Returns a table of US states, US territories, and the
district or Columbia with their respective FIPS codes.

## Usage

``` r
aqs_states(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns an
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of states and their
associated FIPS codes.

## Examples

``` r
# Returns a tibble of states and their FIPS codes
          if (FALSE)  aqs_states()  # \dontrun{}
```
