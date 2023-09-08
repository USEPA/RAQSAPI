#' @section  RAQSAPI setup functions


#' @title aqs_credentials
#' @description \lifecycle{stable}
#'                 Sets the user credentials for the AQS API. This function
#'                 needs to be called once and only once every time this library
#'                 is re-loaded. Users must have a valid username and key which
#'                 can be obtained through the use of the aqs_sign_up function,
#'                 @seealso [aqs_sign_up()] to sign up for AQS data mart
#'                 credentials.
#' @importFrom rlang is_character
#' @param username a R character object which represents the email account that
#'                    will be used to connect to the AQS API.
#' @param key the key used in conjunction with the username given to connect to
#'              AQS Data Mart.
#' @return None
#' @examples
#'  #to authenticate an existing user the email address
#'  # "John.Doe@myemail.com" and key = "MyKey"
#'  #  after calling this function please follow the instructions that are sent
#'  #  in the verification e-mail before proceeding.
#'  \dontrun{aqs_credentials(username = "John.Doe@myemail.com",
#'                               key = "MyKey")
#'          }
#' @export
aqs_credentials <- function(username = NA_character_, key = NA_character_)
{
  # nocov
  #The code simply stores the credentials as a R option. Since the
  #Data Mart server only issues a "key" and not a "password" we don't need to
  #worry about securing the credentials with complicated code such as involving
  #salt and hashes and etc.
   if (!is.na(username) ||
       !is.na(key) ||
       !is_character(username) ||
       !is_character(key)
       )
   {
    options(aqs_username = username)
    options(aqs_key = key)
    #credentials <- options(list(aqs_username = username, aqs_key = key))
    #on.exit(options(credentials), add = TRUE)
  } else {warning("Please enter a valid username and key  \n") }
} #no cov end


#' @title aqs_sign_up
#' @description \lifecycle{stable}
#'              Use this service to register as a new user or to reset an
#'              existing user's key. A verification email will be sent to the
#'              email account specified. To reset a password: If the request is
#'              made with an email that is already registered, a new key will
#'              be issued for that account and emailed to the listed address.
#'              Usage is the same in either case. Refer to the email
#'              message for further instructions before continuing.
#' @param email a R character object which represents the email account that
#'                 will be used to register with the AQS API or change an
#'                 existing user's key. A verification email will be sent to
#'                 the account specified. Follow the instructions
#'                 in the verification e-mail before proceeding to use any other
#'                 functionality of the AQS API. Register your credential
#'                 with the @3 [aqs_credentials()] before using the
#'                 other functions in this library.
#' @note The '@' character needs to be escaped with the '/' character.
#' @importFrom glue glue
#' @importFrom magrittr `%>%`
#' @importFrom httr2 request
#' @importFrom glue glue
#' @examples # to register a new user or generate a new key with the email
#'           #  address "John.Doe/@myemail.com"
#'           \dontrun{aqs_sign_up(email = "John.Doe/@myemail.com")}
#'           #  after calling this function please follow the instructions that
#'           #  are sent in the verification e-mail before proceeding.
#' @return None
#' @export
aqs_sign_up <- function(email)
{ #nocov start
  #We do not want aqs_sign_up registering new users as part of
  #the unit testing procedures.

  # user_agent <- glue("User:{email} via RAQSAPI-{packageVersion('RAQSAPI')}
  #                     library for R")

  url <- glue("https://aqs.epa.gov/data/api/signup?email={email}") %>%
    request() %>%
    req_perform()
  #for some reason user_agent isn't working
  #%>%
  #req_user_agent(string = user_agent)
  glue("A verification email will be sent to {email}  \n") %>%
    message()
} #nocov end
