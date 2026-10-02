# aqs_quarterlysummary_by_site

**\[stable\]** Returns a tibble or an AQS_DataMart_APIv2 S3 object of
quarterly summary data aggregated by site with the provided
parameternum, stateFIPS, county_code, and sitenum for bdate - edate time
frame.

## Usage

``` r
aqs_quarterlysummary_by_site(
  parameter,
  bdate,
  edate,
  stateFIPS,
  countycode,
  sitenum,
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
summary statistics for the given parameter for a single countycode and
stateFIPS combination. An AQS_DataMart_APIv2 is a 2 item named list in
which the first item (\$Header) is a tibble of header information from
the AQS API and the second item (\$Data) is a tibble of the data
returned.

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
server. This operation has a linear run time of \$mathcal(n + 5
seconds)\$.

        Also note that for quarterly data, only the year portion of the bdate
        and edate are used and all 4 quarters in the year are returned.

## See also

Other Aggregate \_by_county functions:
[`aqs_annualsummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_county.md),
[`aqs_dailysummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_county.md),
[`aqs_monitors_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_monitors_by_county.md),
[`aqs_qa_annualperformanceeval_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_county.md),
[`aqs_qa_blanks_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_county.md),
[`aqs_qa_collocated_assessments_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_county.md),
[`aqs_qa_flowrateaudit_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_county.md),
[`aqs_qa_flowrateverification_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_county.md),
[`aqs_qa_one_point_qc_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_county.md),
[`aqs_qa_pep_audit_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_county.md),
[`aqs_quarterlysummary_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_quarterlysummary_by_county.md),
[`aqs_sampledata_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_county.md),
[`aqs_transactionsample_by_county()`](https://usepa.github.io/RAQSAPI/reference/aqs_transactionsample_by_county.md)

## Examples

``` r
# returns a tibble containing quarterly summaries for
          #  FRM/FEM PM2.5 data for Millbrook School in Wake County, NC
          #  for each quarter of 2016
 if (FALSE) aqs_quarterlysummary_by_site(parameter = '88101',
                                       bdate = as.Date('20160101',
                                                       format = '%Y%m%d'),
                                       edate = as.Date('20160331',
                                                       format = '%Y%m%d'),
                                       stateFIPS = '37',
                                       countycode = '183',
                                       sitenum = '0014'
                                       )
          # \dontrun{}
```
