# aqs_qa_annualperformanceevaltransaction_by_state

**\[stable\]** Returns AQS submissions transaction format (RD) of the
annual performance evaluation data (raw). Includes data pairs for QA -
aggregated by state for a parameter code aggregated by matching input
parameter and stateFIPS provided for bdate - edate time frame.

## Usage

``` r
aqs_qa_annualperformanceevaltransaction_by_state(
  parameter,
  bdate,
  edate,
  stateFIPS,
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

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of quality assurance
performance evaluation data. for single monitoring site for the sitenum,
countycode and stateFIPS requested for the time frame between bdate and
edate. An AQS_Data_Mart_APIv2 is a 2 item named list in which the first
item (\$Header) is a tibble of header information from the AQS API and
the second item (\$Data) is a tibble of the data returned.

## Note

The AQS API only allows for a single year of quality assurance Annual
Performance Evaluations transaction data to be retrieved at a time. This
function conveniently extracts date information from the bdate and edate
parameters then makes repeated calls to the AQSAPI retrieving a maximum
of one calendar year of data at a time. Each calendar year of data
requires a separate API call so multiple years of data will require
multiple API calls. As the number of years of data being requested
increases so does the length of time that it will take to retrieve
results. There is also a 5 second wait time inserted between successive
API calls to prevent overloading the API server. This operation has a
linear run time of \$mathcal(n + 5 seconds)\$\$.

## See also

Other Aggregate \_by_state functions:
[`aqs_qa_annualperformanceeval_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_state.md),
[`aqs_quarterlysummary_by_box()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_box.md),
[`aqs_quarterlysummary_by_cbsa()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_cbsa.md),
[`aqs_quarterlysummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_state.md),
[`aqs_transactionsample_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_MA.md),
[`aqs_transactionsample_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_state.md)

## Examples

``` r
# Returns a tibble containing annual performance evaluation data
          # for ozone in Alabmba for 2017 in RD format.
if (FALSE) { # \dontrun{
        aqs_qa_annualperformanceevaltransaction_by_state(parameter = '44201',
                                                  bdate = as.Date('20170101',
                                                          format = '%Y%m%d'),
                                                  edate = as.Date('20171231',
                                                          format = '%Y%m%d'),
                                                         stateFIPS = '01'
                                                         )
         } # }
```
