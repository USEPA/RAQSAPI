# aqs_monitors_by_site

**\[stable\]** Returns a table of monitors and related metadata at sites
with the provided parameter, stateFIPS, county_code, and sitenum for
bdate - edate time frame.

## Usage

``` r
aqs_monitors_by_site(
  parameter,
  bdate,
  edate,
  stateFIPS,
  countycode,
  sitenum,
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

- sitenum:

  a R character object which represents the 4 digit site number (with
  leading zeros) within the county and state being requested. Use
  [`aqs_sites_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_sites_by_county.md)
  for the list of available site numbers in given county and state.

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of monitors from a selected
stateFIPS, county, and sitenum combination.

## Note

All monitors that operated between the bdate and edate will be returned

## See also

Other Aggregate \_by_site functions:
[`aqs_annualsummary_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_site.md),
[`aqs_dailysummary_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_site.md),
[`aqs_qa_annualperformanceeval_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_site.md),
[`aqs_qa_annualperformanceevaltransaction_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_county.md),
[`aqs_qa_annualperformanceevaltransaction_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_site.md),
[`aqs_qa_blanks_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_site.md),
[`aqs_qa_collocated_assessments_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_site.md),
[`aqs_qa_flowrateaudit_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_site.md),
[`aqs_qa_flowrateverification_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_site.md),
[`aqs_qa_one_point_qc_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_site.md),
[`aqs_qa_pep_audit_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_site.md),
[`aqs_sampledata_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_site.md),
[`aqs_transactionsample_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_site.md)

## Examples

``` r
 #Returns a tibble of the SO2 monitors at Hawaii
 #  Volcanoes NP site (\#0007) in Hawaii County, HI that were operating
 # between May 1 , 2015-2019. (Note, all monitors that operated between the
 # bdate and edate will be returned).
 if (FALSE) { # \dontrun{
           aqs_monitors_by_site(parameter = '42401',
                                  bdate = as.Date('20150501',
                                                     format='%Y%m%d'),
                                  edate = as.Date('20190501',
                                                     format='%Y%m%d'),
                                  stateFIPS = '15',
                                  countycode = '001',
                                  sitenum = '0007'
                                 )
         } # }
```
