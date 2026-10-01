# aqs_sampledata_by_box

**\[stable\]** Returns sample data where the data is aggregated by
latitude/longitude bounding box (\_by_box). If return_header is FALSE
(default) this function returns a single dataframe with the requested
data. If return_header is TRUE returns a list of AQSAPI_v2 objects where
each index of the list is an individual RAQSAPI_v2 object returned from
each successive call to the AQS API. RAQSAPI_v2 objects are two item
list where the \$Data portion contains data that contains sample air
monitoring data at a site with the input parameter and cbsa_code
provided for bdate - edate time frame. The \$Header is a tibble of
header information from the API call /(useful for debugging/). This
function returns NULL is bdate \> edate.

## Usage

``` r
aqs_sampledata_by_box(
  parameter,
  bdate,
  edate,
  minlat,
  maxlat,
  minlon,
  maxlon,
  duration = NA_character_,
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

- minlat:

  a R character object that represents the minimum latitude of a
  geographic box. Decimal latitude with north being positive. Only data
  north of this latitude will be returned.

- maxlat:

  a R character object which represents the maximum latitude of a
  geographic box. Decimal latitude with north being positive. Only data
  south of this latitude will be returned.

- minlon:

  a R character object which represents the minimum longitude of a
  geographic box. Decimal longitude with east begin positive. Only data
  east of this longitude will be returned.

- maxlon:

  a R character object which represents the maximum longitude of a
  geographic box. Decimal longitude with east begin positive. Only data
  west of this longitude will be returned. Note that -80 is less than
  -70.

- duration:

  an optional R character string that represents the parameter duration
  code that limits returned data to a specific sample duration. The
  default value of NA_character\_ results in no filtering based on
  duration code.Valid durations include actual sample durations and not
  calculated durations such as 8 hour CO or \$O_3\$ rolling averages,
  3/6 day PM averages or Pb 3 month rolling averages. Use
  [`aqs_sampledurations()`](https://usepa.github.io/RAQSAPI/reference/aqs_sampledurations.md)
  for a list of all available duration codes.

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
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_Data_Mart_APIv2 S3 object containing sample data for
all monitors within the input latitude/longitude bounding box for a
single parameter. An AQS_DataMart_APIv2 is a 2 item named list in which
the first item /(/\$Header/) is a tibble of header information from the
AQS API and the second item /(/\$Data/) is a tibble of the data
returned.

## Note

The AQS API only allows for a single year of sampledata to be retrieved
at a time. This function conveniently extracts date information from the
bdate and edate parameters then makes repeated calls to the AQSAPI
retrieving a maximum of one calendar year of data at a time. Each
calendar year of data requires a separate API call so multiple years of
data will require multiple API calls. As the number of years of data
being requested increases so does the length of time that it will take
to retrieve results. There is also a 5 second wait time inserted between
successive API calls to prevent overloading the API server. This
operation has a linear run time of \$mathcal(n + 5 seconds).

## See also

Other Aggregate \_by_box functions:
[`aqs_annualsummary_by_box()`](https://usepa.github.io/RAQSAPI/reference/aqs_annualsummary_by_box.md),
[`aqs_monitors_by_box()`](https://usepa.github.io/RAQSAPI/reference/aqs_monitors_by_box.md)

## Examples

``` r
# Returns a tibble containing all ozone samples
             #  in the vicinity of central Alabama between
             #  May 1, 2015 - May 2, 2017
          if (FALSE) aqs_sampledata_by_box(parameter = '44201',
                                         bdate = as.Date('20150501',
                                                         format = '%Y%m%d'),
                                         edate = as.Date('20170502',
                                                         format = '%Y%m%d'),
                                         minlat = '33.3',
                                         maxlat = '33.6',
                                         minlon = '-87.0',
                                         maxlon = '-86.7'
                                         )
                    # \dontrun{}
```
