# aqs_counties_by_state

**\[stable\]** Returns a table of all counties in within the stateFIPS
provided.

## Usage

``` r
aqs_counties_by_state(stateFIPS, return_header = FALSE)
```

## Arguments

- stateFIPS:

  a R character object which represents the 2 digit state FIPS code
  (with leading zeros) for the state being requested. Use
  [`aqs_states()`](https://usepa.github.io/RAQSAPI/reference/aqs_states.md)
  for the list of available FIPS codes.

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of all counties in the
requested state.

## Examples

``` r
          # Returns a tibble all the counties
          #   in North Carolina the county FIPS codes (county codes) for
          #   each.
          if (FALSE) aqs_counties_by_state(stateFIPS = '37') # \dontrun{}
```
