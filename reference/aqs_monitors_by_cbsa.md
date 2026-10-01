# aqs_monitors_by_cbsa

**\[stable\]** Returns a table of monitors at all sites with the
provided parameter, aggregated by Core Based Statistical Area (CBSA) for
bdate - edate time frame.

## Usage

``` r
aqs_monitors_by_cbsa(parameter, bdate, edate, cbsa_code, return_header = FALSE)
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

- cbsa_code:

  a R character object which represents the 5 digit AQS Core Based
  Statistical Area code (the same as the census code, with leading
  zeros). Use
  [`aqs_cbsas()`](https://usepa.github.io/RAQSAPI/reference/aqs_cbsas.md)
  for a list of all CBSA codes and names available,

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object that is the return value
from the AQS API. A AQS_DataMart_APIv2 object is a 2 item named list in
which the first item (\$Header) is a tibble of header information from
the AQS API and the second item (\$Data) is a tibble of the data
returned.

## Note

All monitors that operated between the bdate and edate will be returned

## See also

Other Aggregate \_by_cbsa functions:
[`aqs_annualsummary_by_cbsa()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_cbsa.md),
[`aqs_dailysummary_by_cbsa()`](https://usepa.github.io/RAQSAPI/reference/aqs_dailysummary_by_cbsa.md),
[`aqs_sampledata_by_cbsa()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledata_by_cbsa.md)

## Examples

``` r
# returns a tibble of $NO_{2}$ monitors
          #  for Charlotte-Concord-Gastonia, NC cbsa that were operating
          #  on Janurary 01, 2017
          if (FALSE) aqs_monitors_by_cbsa(parameter='42602',
                                               bdate=as.Date('20170101',
                                                           format='%Y%m%d'),
                                               edate=as.Date('20170101',
                                                            format='%Y%m%d'),
                                               cbsa_code='16740'
                                                   )
                    # \dontrun{}
```
