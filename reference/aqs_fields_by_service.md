# aqs_fieldsbyservice

**\[stable\]** Returns a tibble or an AQS_DataMart_APIv2 S3 object with
the list and definitions of fields in the service requested.

## Usage

``` r
aqs_fields_by_service(service, return_header = FALSE)
```

## Arguments

- service:

  a string which represents the services provided by the AQS API. For a
  list of available services, refer to
  <https://aqs.epa.gov/aqsweb/documents/data_api.html#services>

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object with containing the list and
definitions of fields requested service

## Examples

``` r
# Returns a tibble containing a list and definitions
          #  of fields in the Sample Data service
         if (FALSE)  aqs_fieldsbyservice(service = 'sampleData')  # \dontrun{}
```
