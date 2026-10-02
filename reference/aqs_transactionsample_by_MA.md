# aqs_transactionsample_MA

**\[stable\]** Returns transactionsample data - aggregated by Monitoring
agency (MA) in the AQS Submission Transaction Format (RD) sample (raw)
data for a parameter code aggregated by matching input parameter, and
monitoring agency (MA) code provided for bdate - edate time frame.
Includes data both in submitted and standard units

## Usage

``` r
aqs_transactionsample_by_MA(
  parameter,
  bdate,
  edate,
  MA_code,
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

- MA_code:

  a R character object which represents the 4 digit AQS Monitoring
  Agency code (with leading zeroes). Use
  [`aqs_mas()`](https://usepa.github.io/RAQSAPI/reference/aqs_mas.md)
  for a list of all Monitoring Agency (MA) codes and names available,

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

a tibble or an AQS_DataMart_APIv2 S3 object of transaction sample (raw)
data in the AQS submission transaction format (RD) corresponding to the
inputs provided.

## Note

The AQS API only allows for a single year of transaction data to be
retrieved at a time. This function conveniently extracts date
information from the bdate and edate parameters then makes repeated
calls to the AQSAPI retrieving a maximum of one calendar year of data at
a time. Each calendar year of data requires a separate API call so
multiple years of data will require multiple API calls. As the number of
years of data being requested increases so does the length of time that
it will take to retrieve results. There is also a 5 second wait time
inserted between successive API calls to prevent overloading the API
server. This operation has a linear run time of \$mathcal(n + 5
seconds)\$.

## See also

Other Aggregate \_by_state functions:
[`aqs_qa_annualperformanceeval_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_state.md),
[`aqs_qa_annualperformanceevaltransaction_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_state.md),
[`aqs_quarterlysummary_by_box()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_box.md),
[`aqs_quarterlysummary_by_cbsa()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_cbsa.md),
[`aqs_quarterlysummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_state.md),
[`aqs_transactionsample_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_state.md)

## Examples

``` r
#Returns a tibble of ozone transaction sample data for all monitors
          #operated by South Coast Air Quality Management District collected
          #on May 15, 2015
          if (FALSE) aqs_transactionsample_by_MA(parameter = '44201',
                                               bdate = as.Date('20150515',
                                                          format = '%Y%m%d'),
                                               edate = as.Date('20150515',
                                                          format = '%Y%m%d'),
                                               MA_code = '0972'
                                               )
                  # \dontrun{}
```
