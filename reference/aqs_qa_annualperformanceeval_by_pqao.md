# aqs_qa_annualperformanceeval_by_pqao

**\[stable\]** Returns quality assurance performance evaluation data -
aggregated by Primary Quality Assurance Organization (PQAO) for a
parameter code aggregated by matching input parameter and pqao_code for
the time frame between bdate and edate.

## Usage

``` r
aqs_qa_annualperformanceeval_by_pqao(
  parameter,
  bdate,
  edate,
  pqao_code,
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

- pqao_code:

  a R character object which represents the 4 digit AQS Primary Quality
  Assurance Organization code (with leading zeroes). Use
  [`aqs_pqaos()`](https://usepa.github.io/RAQSAPI/reference/aqs_pqaos.md)
  for a list of all Primary Quality Assurance Organization (pqao) codes
  and names available,

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of quality assurance
performance evaluation data. for single monitoring site for the
pqao_code requested for the time frame between bdate and edate. An
AQS_Data_Mart_APIv2 is a 2 item named list in which the first item
(\$Header) is a tibble of header information from the AQS API and the
second item (\$Data) is a tibble of the data returned.

## Note

The AQS API only allows for a single year of quality assurance Annual
Performance Evaluation data to be retrieved at a time. This function
conveniently extracts date information from the bdate and edate
parameters then makes repeated calls to the AQSAPI retrieving a maximum
of one calendar year of data at a time. Each calendar year of data
requires a separate API call so multiple years of data will require
multiple API calls. As the number of years of data being requested
increases so does the length of time that it will take to retrieve
results. There is also a 5 second wait time inserted between successive
API calls to prevent overloading the API server. This operation has a
linear run time of /(Big O notation: O/(n + 5 seconds/)/).

## See also

Other Aggregate \_by_pqao functions:
[`aqs_qa_annualperformanceevaltransaction_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_pqao.md),
[`aqs_qa_blanks_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_blanks_by_pqao.md),
[`aqs_qa_collocated_assessments_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_pqao.md),
[`aqs_qa_flowrateaudit_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_pqao.md),
[`aqs_qa_flowrateverification_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_pqao.md),
[`aqs_qa_one_point_qc_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_pqao.md),
[`aqs_qa_pep_audit_by_pqao()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_pqao.md)

## Examples

``` r
# Returns a tibble containing annual performance evaluation data
          # for ozone where the PQAO is the Alabamaba Department of
          # Environmental Management (pqao_code 0013).
 if (FALSE)  aqs_qa_annualperformanceeval_by_pqao(parameter = '44201',
                                                bdate = as.Date('20170101',
                                                          format = '%Y%m%d'),
                                                edate = as.Date('20171231',
                                                          format = '%Y%m%d'),
                                                pqao_code = '0013'
                                                )
                  # \dontrun{}
```
