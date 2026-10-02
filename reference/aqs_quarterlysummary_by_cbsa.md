# aqs_quarterlysummary_by_cbsa

**\[stable\]** Returns a tibble or an AQS_DataMart_APIv2 S3 object of
quarterly summary data aggregated by stateFIPS.

## Usage

``` r
aqs_quarterlysummary_by_cbsa(
  parameter,
  bdate,
  edate,
  cbsa_code,
  cbdate = lubridate::NA_Date_,
  cedate = lubridate::NA_Date_,
  return_header = FALSE
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

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object that contains quarterly
summary statistics for the given parameter for a stateFIPS. An
AQS_DataMart_APIv2 is a 2 item named list in which the first item
(\$Header) is a tibble of header information from the AQS API and the
second item (\$Data) is a tibble of the data returned.

## Note

The AQS API only allows for a single year of quarterly summary to be
retrieved at a time. This function conveniently extracts date
information from the bdate and edate parameters then makes repeated
calls to the AQSAPI retrieving a maximum of one calendar year of data at
a time. Each calendar year of data requires a separate API call so
multiple years of data will require multiple API calls. As the number of
years of data being requested increases so does the length of time that
it will take to retrieve results. There is also a 5 second wait time
inserted between successive API calls to prevent overloading the API
server. This operation has a linear run time of /(Big O notation: O/(n +
5 seconds/)/).

        Also note that for quarterly data, only the year portion of the bdate
        and edate are used and all 4 quarters in the year are returned.

## See also

Other Aggregate \_by_state functions:
[`aqs_qa_annualperformanceeval_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_state.md),
[`aqs_qa_annualperformanceevaltransaction_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_state.md),
[`aqs_quarterlysummary_by_box()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_box.md),
[`aqs_quarterlysummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_state.md),
[`aqs_transactionsample_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_MA.md),
[`aqs_transactionsample_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_state.md)

## Examples

``` r
# Returns a tibble of $NO_{2}$ quartyerly summary
          #  data the for Charlotte-Concord-Gastonia, NC cbsa for
          #  each quarter in 2017.
          if (FALSE) aqs_quarterlysummary_by_cbsa(parameter = '42602',
                                                bdate = as.Date('20170101',
                                                          format = '%Y%m%d'),
                                                edate = as.Date('20171231',
                                                          format = '%Y%m%d'),
                                                cbsa_code = '16740'
                                                )
                    # \dontrun{}
```
