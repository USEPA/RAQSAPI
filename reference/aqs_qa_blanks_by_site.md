# aqs_qa_blanks_by_site

**\[stable\]** Returns a table of blank quality assurance data. Blanks
are unexposed sample collection devices (e.g., filters) that are
transported with the exposed sample devices to assess if contamination
is occurring during the transport or handling of the samples. Data is
aggregated at the site level.

## Usage

``` r
aqs_qa_blanks_by_site(
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
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
assurance blank sample data for single monitoring site for the sitenum,
countycode and stateFIPS requested for the time frame between bdate and
edate. An AQS_Data_Mart_APIv2 is a 2 item named list in which the first
item (\$Header) is a tibble of header information from the AQS API and
the second item (\$Data) is a tibble of the data returned.

## Note

The AQS API only allows for a single year of qa_blank data to be
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

Other Aggregate \_by_site functions:
[`aqs_annualsummary_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_site.md),
[`aqs_dailysummary_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_site.md),
[`aqs_monitors_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_monitors_by_site.md),
[`aqs_qa_annualperformanceeval_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_site.md),
[`aqs_qa_annualperformanceevaltransaction_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_county.md),
[`aqs_qa_annualperformanceevaltransaction_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_site.md),
[`aqs_qa_collocated_assessments_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_site.md),
[`aqs_qa_flowrateaudit_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_site.md),
[`aqs_qa_flowrateverification_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_site.md),
[`aqs_qa_one_point_qc_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_site.md),
[`aqs_qa_pep_audit_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_site.md),
[`aqs_sampledata_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_site.md),
[`aqs_transactionsample_by_site()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_site.md)

## Examples

``` r
#Returns a tibble of PM2.5 blank
          #  data for the Muscle Shoals site (#0014) in Colbert County, AL
          #  for January 2018
          if (FALSE) { # \dontrun{
                    aqs_qa_blanks_by_site(parameter = '88101',
                                          bdate = as.Date('20170101',
                                                          format='%Y%m%d'),
                                          edate = as.Date('20190131',
                                                          format='%Y%m%d'),
                                          stateFIPS = '01',
                                          countycode = '033',
                                          sitenum = '1002'
                                          )
                 } # }
```
