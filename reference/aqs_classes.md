# aqs_classes

**\[stable\]** Returns a table of Parameter classes (groups of
parameters, i.e. 'criteria' or 'all'). The information from this
function can be used as input to other API calls.

## Usage

``` r
aqs_classes(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of Parameter classes (groups
of parameters, i.e. 'criteria' or 'all').

## Examples

``` r
# Returns a tibble of parameter classes (groups of parameters, i.e.
          # 'criteria' or all')
         if (FALSE)  aqs_classes()  # \dontrun{}
```
