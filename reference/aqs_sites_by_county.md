# aqs_sites_by_county

**\[stable\]** Returns data containing a table of all air monitoring
sites with the input state and county FIPS code combination.

## Usage

``` r
aqs_sites_by_county(stateFIPS, countycode, return_header = FALSE)
```

## Arguments

- stateFIPS:

  a R character object which represents the 2 digit state FIPS code
  (with leading zero) for the state being requested. Use
  [`aqs_states()`](https://usepa.github.io/RAQSAPI/reference/aqs_states.md)
  for the list of available FIPS codes.

- countycode:

  a R character object which represents the 3 digit state FIPS code for
  the county being requested (with leading zero(s)). Use
  [`aqs_counties_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_counties_by_state.md)
  for the list of available county codes in each state.

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of all air monitoring sites
with the requested state and county FIPS codes.

## Examples

``` r
# Returns an AQS_DataMart_APIv2 S3 object witch returns all sites
          #  in Hawaii County, HI
          if (FALSE) aqs_sites_by_county(stateFIPS = '15',
                                       countycode = '001')
                  # \dontrun{}
```
