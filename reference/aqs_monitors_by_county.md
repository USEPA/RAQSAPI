# aqs_monitors_by_county

**\[stable\]** Returns a table of monitors and related metadata at sites
with the provided parameter, stateFIPS and county_code for bdate - edate
time frame.

## Usage

``` r
aqs_monitors_by_county(
  parameter,
  bdate,
  edate,
  stateFIPS,
  countycode,
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

a tibble or an AQS_DataMart_APIv2 S3 object of monitors from a selected
county

## Note

All monitors that operated between the bdate and edate will be returned

## See also

Other Aggregate \_by_county functions:
[`aqs_annualsummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_county.md),
[`aqs_dailysummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_county.md),
[`aqs_qa_annualperformanceeval_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_county.md),
[`aqs_qa_blanks_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_county.md),
[`aqs_qa_collocated_assessments_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_county.md),
[`aqs_qa_flowrateaudit_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_county.md),
[`aqs_qa_flowrateverification_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_county.md),
[`aqs_qa_one_point_qc_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_county.md),
[`aqs_qa_pep_audit_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_county.md),
[`aqs_quarterlysummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_county.md),
[`aqs_quarterlysummary_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_site.md),
[`aqs_sampledata_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_county.md),
[`aqs_transactionsample_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_county.md)

## Examples

``` r
# returns an aqs_v2 S3 object containing all SO2 monitors in
          #  Hawaii County, HI that were operating between May 01-02, 2015.
 if (FALSE) aqs_monitors_by_county(parameter='42401',
                                 bdate=as.Date('20150501', format='%Y%m%d'),
                                 edate=as.Date('20150502', format='%Y%m%d'),
                                 stateFIPS='15',
                                 countycode='001'
                                 )
          # \dontrun{}
```
