# aqs_removeheader

**\[stable\]** Coerces a single AQS_Data_Mart_APIv2 S3 object or a list
of AQS_Data_Mart_APIv2 S3 objects into a single tibble object. This
function decouples the \$Data from the AQSAPI_v2 object and returns only
the \$Data portion as a tibble. If the input is a list of AQSAPI_v2
objects combines the \$Data portion of each AQS_Data_Mart_APIv2 S3
object into a single tibble with \$Header information discarded. Else
returns the input with no changes.

## Usage

``` r
aqs_removeheader(AQSobject)
```

## Arguments

- AQSobject:

  An object of AQSAPI_v2 or a list of AQSAPI_v2 objects.

## Value

a tibble of the combined \$data portions of the input
AQS_Data_Mart_APIv2 S3 object with the \$Header portion discarded.

## Note

Since this function returns only the \$Data portion of RAQSAPI_v2
objects this means that the \$Header information will not be present in
the object being returned.

## Examples

``` r
#coerce a AQS_Data_MART_APIv2 object to a single tibble.
           if (FALSE)  aqs_removeheader(AQSobject)  # \dontrun{}
```
