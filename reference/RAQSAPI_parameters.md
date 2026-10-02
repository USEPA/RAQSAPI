# RAQSAPI_parameters

null

## Arguments

- AQS_domain:

  a R string object containing the domain that should be used in
  constructing the API call.

- .AQSobject:

  A 2 item named list in which the first item (\$Header) is a tibble of
  header information from the AQS API and the second item (\$Data) is a
  tibble of the data returned.

- bdate:

  a R date object which represents the begin date of the data selection.
  Only data on or after this date will be returned.

- cbdate:

  a R date object which represents a 'beginning date of last change'
  that indicates when the data was last updated. cbdate is used to
  filter data based on the change date. Only data that changed on or
  after this date will be returned. This is an optional variable which
  defaults to NA_Date\_.

- cbsa_code:

  a R character object which represents the 5 digit AQS Core Based
  Statistical Area code (the same as the census code, with leading
  zeros). Use
  [`aqs_cbsas()`](https://usepa.github.io/RAQSAPI/reference/aqs_cbsas.md)
  for a list of all CBSA codes and names available,

- cedate:

  a R date object which represents an 'end date of last change' that
  indicates when the data was last updated. cedate is used to filter
  data based on the change date. Only data that changed on or before
  this date will be returned. This is an optional variable which
  defaults to NA_Date\_.

- countycode:

  a R character object which represents the 3 digit state FIPS code for
  the county being requested (with leading zero(s)). Use
  [`aqs_counties_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_counties_by_state.md)
  for the list of available county codes in each state.

- delimiter:

  a string that should be used to separate variables in the return
  value.

- duration:

  an optional R character string that represents the parameter duration
  code that limits returned data to a specific sample duration. The
  default value of NA_character\_ results in no filtering based on
  duration code.Valid durations include actual sample durations and not
  calculated durations such as 8 hour CO or \$O_3\$ rolling averages,
  3/6 day PM averages or Pb 3 month rolling averages. Use
  [`aqs_sampledurations()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledurations.md)
  for a list of all available duration codes.

- edate:

  a R date object which represents the end date of the data selection.
  Only data on or before this date will be returned.

- email:

  a string which represents the parameter code of the air pollutant
  related to the data being requested.

- filter:

  a character string representing the filter being applied

- MA_code:

  a R character object which represents the 4 digit AQS Monitoring
  Agency code (with leading zeroes). Use
  [`aqs_mas()`](https://usepa.github.io/RAQSAPI/reference/aqs_mas.md)
  for a list of all Monitoring Agency (MA) codes and names available,

- maxlat:

  a R character object which represents the maximum latitude of a
  geographic box. Decimal latitude with north being positive. Only data
  south of this latitude will be returned.

- maxlon:

  a R character object which represents the maximum longitude of a
  geographic box. Decimal longitude with east begin positive. Only data
  west of this longitude will be returned. Note that -80 is less than
  -70.

- minlat:

  a R character object that represents the minimum latitude of a
  geographic box. Decimal latitude with north being positive. Only data
  north of this latitude will be returned.

- minlon:

  a R character object which represents the minimum longitude of a
  geographic box. Decimal longitude with east begin positive. Only data
  east of this longitude will be returned.

- parameter:

  a character list or a single character string which represents the
  parameter code of the air pollutant related to the data being
  requested.

- pqao_code:

  a R character object which represents the 4 digit AQS Primary Quality
  Assurance Organization code (with leading zeroes). Use
  [`aqs_pqaos()`](https://usepa.github.io/RAQSAPI/reference/aqs_pqaos.md)
  for a list of all Primary Quality Assurance Organization (pqao) codes
  and names available,

- service:

  a string which represents the services provided by the AQS API. For a
  list of available services Refer to
  <https://aqs.epa.gov/aqsweb/documents/data_api.html#services>

- sitenum:

  a R character object which represents the 4 digit site number (with
  leading zeros) within the county and state being requested. Use
  [`aqs_sites_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_sites_by_county.md)
  for the list of available site numbers in given county and state.

- stateFIPS:

  a R character object which represents the 2 digit state FIPS code
  (with leading zero) for the state being requested. Use
  [`aqs_states()`](https://usepa.github.io/RAQSAPI/reference/aqs_states.md)
  for the list of available FIPS codes.

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.
