# aqs_qa_blanks_by_MA

**\[stable\]** Returns a table of blank quality assurance data. Blanks
are unexposed sample collection devices (e.g., filters) that are
transported with the exposed sample devices to assess if contamination
is occurring during the transport or handling of the samples. Data is
aggregated by monitoring agency code (MA_code).

## Usage

``` r
aqs_qa_blanks_by_MA(parameter, bdate, edate, MA_code, return_header = FALSE)
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

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_Data_Mart_APIv2 S3 object that contains quality
assurance blank sample data for all monitors within the input MA_code.
An AQS_Data_Mart_APIv2 is a 2 item named list in which the first item
(\$Header) is a tibble of header information from the AQS API and the
second item (\$Data) is a tibble of the data returned.

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

Other Aggregate \_by_MA functions:
[`aqs_qa_annualperformanceeval_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceeval_by_MA.md),
[`aqs_qa_annualperformanceevaltransaction_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_annualperformanceevaltransaction_by_MA.md),
[`aqs_qa_collocated_assessments_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_collocated_assessments_by_MA.md),
[`aqs_qa_flowrateaudit_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateaudit_by_MA.md),
[`aqs_qa_flowrateverification_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_flowrateverification_by_MA.md),
[`aqs_qa_one_point_qc_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_one_point_qc_by_MA.md),
[`aqs_qa_pep_audit_by_MA()`](https://usepa.github.io/RAQSAPI/reference/aqs_qa_pep_audit_by_MA.md)

## Examples

``` r
# Returns a tibble containing PM2.5 blank data in
          #  January 2018 where the Monitoring Agency is the Alabama
          #  Department of Environmental Management (agency 0013)
          if (FALSE) aqs_qa_blanks_by_MA(parameter = '88101',
                                       bdate = as.Date('20170101',
                                                       format = '%Y%m%d'),
                                       edate = as.Date('20190131',
                                                       format = '%Y%m%d'),
                                       MA_code = '0013'
                                       )
                    # \dontrun{}
```
