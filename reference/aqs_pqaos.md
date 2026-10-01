# aqs_pqaos

**\[stable\]** Returns a table of primary quality assurance
organizations (pqaos).

## Usage

``` r
aqs_pqaos(return_header = FALSE)
```

## Arguments

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object of pqaos and their
associated pqao code.

## Examples

``` r
# Returns a tibble of primary quality assurance
          # organizations (pqaos)
           if (FALSE)  aqs_pqaos()  # \dontrun{}
```
