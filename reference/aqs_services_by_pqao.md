# aqs_services_by_pqao

A helper function that abstracts the formatting of the inputs for a call
to aqs away from the calling function for aggregations by Primary
Quality Assurance Organization (pqao) then calls the aqs and returns the
result. This helper function is not meant to be called directly from
external functions.

## Usage

``` r
aqs_services_by_pqao(
  parameter,
  bdate,
  edate,
  pqao_code,
  service,
  cbdate = lubridate::NA_Date_,
  cedate = lubridate::NA_Date_,
  AQS_domain = "aqs.epa.gov"
)
```

## Arguments

- parameter:

  a character list or a single character string which represents the
  parameter code of the air pollutant related to the data being
  requested.

- bdate:

  a R date object which represents the begin date of the data selection.
  Only data on or after this date will be returned.

- edate:

  a R date object which represents the end date of the data selection.
  Only data on or before this date will be returned.

- pqao_code:

  a R character object which represents the 4 digit AQS Primary Quality
  Assurance Organization code (with leading zeroes). Use
  [`aqs_pqaos()`](https://usepa.github.io/RAQSAPI/reference/aqs_pqaos.md)
  for a list of all Primary Quality Assurance Organization (pqao) codes
  and names available,

- service:

  a string which represents the services provided by the AQS API. For a
  list of available services @seealso
  <https://aqs.epa.gov/aqsweb/documents/data_api.html#services>

- cbdate:

  a R date object which represents a 'beginning date of last change'
  that indicates when the data was last updated. cbdate is used to
  filter data based on the change date. Only data that changed on or
  after this date will be returned. This is an optional variable which
  defaults to NA_Date\_.

- cedate:

  a R date object which represents an 'end date of last change' that
  indicates when the data was last updated. cedate is used to filter
  data based on the change date. Only data that changed on or before
  this date will be returned. This is an optional variable which
  defaults to NA_Date\_.

- AQS_domain:

  a R string object containing the domain that should be used in
  constructing the API call.

## Value

a AQS_DATAMART_APIv2 S3 object that is the return value from the AQS
API. A AQS_DATAMART_APIv2 is a 2 item named list in which the first item
(\$Header) is a tibble of header information from the AQS API and the
second item (\$Data) is a tibble of the data returned.

## Examples
