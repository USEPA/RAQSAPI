# aqs_sign_up

**\[stable\]** Use this function to register as a new user or to reset
an existing user's key. A verification email will be sent to the email
account specified. To reset a password: If the request is made with an
email that is already registered, a new key will be issued for that
account and emailed to the listed address. Usage is the same in either
case. Refer to the email message for further instructions before
continuing.

## Usage

``` r
aqs_sign_up(email)
```

## Arguments

- email:

  a R character object which represents the email account that will be
  used to register with the AQS API or change an existing user's key. A
  verification email will be sent to the account specified. Follow the
  instructions in the verification e-mail before proceeding to use any
  other functionality of the AQS API. Register your credential with
  [`aqs_credentials()`](https://usepa.github.io/RAQSAPI/reference/aqs_credentials.md)
  before using the other functions in this library.

## Value

NULL, This functions is called for its side effect and does not return
meaningful data.

## Examples

``` r
# to register a new user or generate a new key with the email
          #  address 'John.Doe/@myemail.com'
          if (FALSE) aqs_sign_up(email = 'John.Doe/@myemail.com') # \dontrun{}
          #  after calling this function please follow the instructions that
          #  are sent in the verification e-mail before proceeding.
```
