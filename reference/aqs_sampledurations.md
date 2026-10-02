# aqs_sampledurations

**\[stable\]** Returns a table of sample durations and their associated
duration codes. Returned values are not calculated durations such as 8
hour CO or \$O_3\$ rolling averages, 3/6 day PM averages or Pb 3 month
rolling averages.

## Usage

``` r
aqs_sampledurations(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server, mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of sample durations and
their associated duration codes (groups of parameters, i.e. 'criteria'
or 'all').

## Note

Not all sample durations that are available through AQS are available
through the AQS Data Mart API, including certain calculated sample
durations. Only sample durations that are available through the AQS Data
Mart API are returned.

## Examples

``` r
# Returns a tibble or an AQS_DataMart_APIv2 S3 object of
         if (FALSE)  aqs_sampledurations()  # \dontrun{}
```
