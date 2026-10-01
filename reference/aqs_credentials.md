# aqs_credentials

**\[stable\]** Sets the user credentials for the AQS API. This function
needs to be called once and only once every time this library is
re-loaded. Users must have a valid username and key which can be
obtained through the use of the aqs_sign_up function, Use
[`aqs_sign_up()`](https://usepa.github.io/RAQSAPI/reference/aqs_sign_up.md)
to sign up for AQS datamart credentials.

## Usage

``` r
aqs_credentials(username = NA_character_, key = NA_character_)
```

## Arguments

- username:

  a R character object which represents the email account that will be
  used to connect to the AQS API.

- key:

  the key used in conjunction with the username given to connect to AQS
  Data Mart.

## Value

NULL (invisible) This functions is called for its side effect and does
not return meaningful data.

## Examples

``` r
 #to authenticate an existing user the email address
 # 'John.Doe@myemail.com' and key = 'MyKey'
 #  after calling this function please follow the instructions that are sent
 #  in the verification e-mail before proceeding.
 if (FALSE) aqs_credentials(username = 'John.Doe@myemail.com',
                              key = 'MyKey')
          # \dontrun{}
```
