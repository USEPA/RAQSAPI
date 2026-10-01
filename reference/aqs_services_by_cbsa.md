# aqs_services_by_cbsa

A helper function that abstracts the formatting of the inputs for a call
to aqs away from the calling function for aggregations by cbsa then
calls the aqs and returns the result. This helper function is not meant
to be called directly from external functions.

## Usage

``` r
aqs_services_by_cbsa(
  parameter,
  bdate,
  edate,
  cbsa_code,
  duration = NA_character_,
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

- cbsa_code:

  a R character object which represents the 5 digit AQS Core Based
  Statistical Area code (the same as the census code, with leading
  zeros). Use
  [`aqs_cbsas()`](https://usepa.github.io/RAQSAPI/reference/aqs_cbsas.md)
  for a list of all CBSA codes and names available,

- duration:

  an optional R character string that represents the parameter duration
  code that limits returned data to a specific sample duration. The
  default value of NA_character\_ results in no filtering based on
  duration code.Valid durations include actual sample durations and not
  calculated durations such as 8 hour CO or \$O_3\$ rolling averages,
  3/6 day PM averages or Pb 3 month rolling averages. Use
  [`aqs_sampledurations()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledurations.md)
  for a list of all available duration codes.

- service:

  a string which represents the services provided by the AQS API. For a
  list of available services Refer to
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

``` r
# if an user were to call aqs_annualsummary_by_cbsa() for  annual
          # summary $NO_{2}$ data the for Charlotte-Concord-Gastonia, NC
          # cbsa on Janurary 01, 2017
          #[aqs_annualsummary_by_cbsa(parameter = '42602',
          #                           bdate = as.Date('20170101',
          #                                            format = '%Y%m%d'
          #                                            ),
          #                           edate = as.Date('20170101',
          #                                            format = '%Y%m%d'
          #                                           ),
          #                           cbsa_code = '16740'
          #                            )]
          # then aqs_annualsummary_by_cbsa() would call this helper
          # function with the following inputs.
          if (FALSE) aqs_services_by_cbsa(parameter = '42602',
                                        bdate = as.Date('20170101',
                                                     format = '%Y%m%d'),
                                        edate = as.Date('20170101',
                                                     format = '%Y%m%d'),
                                        cbsa_code = '16740',
                                        service = 'annualData')
                   # \dontrun{}
```
