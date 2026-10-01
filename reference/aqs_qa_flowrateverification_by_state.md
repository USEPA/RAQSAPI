# aqs_qa_flowrateverification_by_state

**\[stable\]** Returns a table containing flow rate Verification data
for a parameter code aggregated matching input parameter, and stateFIPS,
provided for bdate - edate time frame.

## Usage

``` r
aqs_qa_flowrateverification_by_state(
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

a tibble or an AQS_DataMart_APIv2 S3 object containing quality assurance
flow rate verification data for monitors within a state. An
AQS_DataMart_APIv2 is a 2 item named list in which the first item
(\$Header) is a tibble of header information from the AQS API and the
second item (\$Data) is a tibble of the data returned.

## Note

The AQS API only allows for a single year of flow rate verifications to
be retrieved at a time. This function conveniently extracts date
information from the bdate and edate parameters then makes repeated
calls to the AQSAPI retrieving a maximum of one calendar year of data at
a time. Each calendar year of data requires a separate API call so
multiple years of data will require multiple API calls. As the number of
years of data being requested increases so does the length of time that
it will take to retrieve results. There is also a 5 second wait time
inserted between successive API calls to prevent overloading the API
server. This operation has a linear run time of \$mathcal(n + 5
seconds)\$\$.

## See also

Other Aggregate_by_state functions:
[`aqs_annualsummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_state.md),
[`aqs_dailysummary_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_state.md),
[`aqs_monitors_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_monitors_by_state.md),
[`aqs_qa_blanks_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_state.md),
[`aqs_qa_collocated_assessments_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_state.md),
[`aqs_qa_flowrateaudit_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_state.md),
[`aqs_qa_one_point_qc_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_state.md),
[`aqs_qa_pep_audit_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_state.md),
[`aqs_sampledata_by_state()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_state.md)

## Examples

``` r
# Returns a tibble of flow rate verification data for the state of
          # Alabama for 2017-2019
          if (FALSE) aqs_qa_flowrateverification_by_state(parameter = '88101',
                                                  bdate = as.Date('20170101',
                                                            format = '%Y%m%d'
                                                                  ),
                                                  edate = as.Date('20190131',
                                                              format='%Y%m%d'
                                                                  ),
                                                  stateFIPS = '01'
                                                        )
                    # \dontrun{}
```
