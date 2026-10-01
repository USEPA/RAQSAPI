# aqs_monitors_by_state

**\[stable\]** Returns a table of monitors and related metadata at sites
with the provided parameter, and stateFIPS for bdate - edate time frame.

## Usage

``` r
aqs_monitors_by_state(
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
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of monitors from the
selected state

## Note

All monitors that operated between the bdate and edate will be returned

## See also

Other Aggregate_by_state functions:
[`aqs_annualsummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_state.md),
[`aqs_dailysummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_state.md),
[`aqs_qa_blanks_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_state.md),
[`aqs_qa_collocated_assessments_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_state.md),
[`aqs_qa_flowrateaudit_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_state.md),
[`aqs_qa_flowrateverification_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_state.md),
[`aqs_qa_one_point_qc_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_state.md),
[`aqs_qa_pep_audit_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_state.md),
[`aqs_sampledata_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_state.md)

## Examples

``` r
# returns a tibble of SO2 monitors in Hawaii
          #  that were operating on May 01, 2017
 if (FALSE) aqs_monitors_by_state(parameter='88101',
                                  bdate=as.Date('20170101',
                                                 format='%Y%m%d'),
                                  edate=as.Date('20171231',
                                                 format='%Y%m%d'),
                                  stateFIPS='01'
                                  )
           # \dontrun{}
```
