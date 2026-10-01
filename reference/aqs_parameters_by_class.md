# aqs_parameters_by_class

**\[stable\]** Returns parameters associated with the input class.

## Usage

``` r
aqs_parameters_by_class(class, return_header = FALSE)
```

## Arguments

- class:

  a R character object that represents the class requested, Use
  [`aqs_classes()`](https://usepa.github.io/RAQSAPI/reference/aqs_classes.md)
  for retrieving available classes. The class R character object must be
  a valid class as returned from aqs_classes(). The class must be an
  exact match to what is returned from aqs_classes() (case sensitive).

- return_header:

  If FALSE (default) only returns data requested. If TRUE returns a
  AQSAPI_v2 object which is a two item list that contains header
  information returned from the API server mostly used for debugging
  purposes in addition to the data requested.

## Value

a tibble or an AQS_DataMart_APIv2 S3 object containing the parameters
associated with the class requested. NULL is returned for classes not
found.

## Examples

``` r
# Returns a tibble of AQS parameters in the criteria class
          if (FALSE)  aqs_parameters_by_class(class = 'CRITERIA')  # \dontrun{}
```
