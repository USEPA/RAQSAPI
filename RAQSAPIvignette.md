---
title: "Introduction to the RAQSAPI package"
author:
- affiliation: "United States Environmental Protection Agency (US EPA),  \n    Region
    III,  \n    Air and Radiation Division,  \n    Air Quality and Analysis Branch"
  name: Clinton Mccrowey
abstract: RAQSAPI is a package for R that connects the R programming language environment
  to the United States Environmental Protection Agency's (US EPA) Air Quality System
  (AQS) Data Mart database API for retrieval of ambient air pollution data.
output:
  html_document:
    toc: true
    df_print: paged
  md_document:
    variant: gfm
  rmarkdown::html_vignette: default
  pdf_document:
    toc: true
lang: "en-US"
vignette: "%\\VignetteIndexEntry{Introduction to the RAQSAPI package} %\\VignetteEncoding{UTF-8}
  %\\VignetteKeyword{R} %\\VignetteKeyword{AQS} %\\VignetteKeyword{EPA} %\\VignetteKeyword{Data
  Mart} %\\VignetteKeyword{API} %\\VignetteDepends{magrittr} %\\VignetteDepends{stringr}
  %\\VignetteDepends{tibble} %\\VignetteDepends{purrr} %\\VignetteDepends{knitr} %\\VignetteDepends{rmarkdown}
  %\\VignetteDepends{RAQSAPI} %\\VignetteEngine{knitr::rmarkdown}\n"
bibliography: AQSAPI.bib
csl: "acs-nano.csl"
---


<img src="./figures/RAQSAPIhexsticker.png" alt="RAQSAPI hexsticker" width="150" height="150">


























# EPA Disclaimer

> [!NOTE]
>
> This software/application was developed by the U.S. Environmental Protection Agency (USEPA). No warranty expressed or
> implied is made regarding the accuracy or utility of the system, nor shall the act of distribution constitute any such
> warranty. The USEPA has relinquished control of the information and no longer has responsibility to protect the integrity,
> confidentiality or availability of the information. Any reference to specific commercial products, processes, or services
> by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
> favoring by the USEPA. The USEPA seal and logo shall not be used in any manner to imply endorsement of any commercial
> product or activity by the USEPA or the United States Government.



















>[!WARNING]
>
> US EPA's AQS Data Mart API V2 is currently
> in beta phase of development, the API interface has not been finalized.
> This means that certain functionality of the API may change or be removed
> without notice. As a result, this package is also currently marked as beta and
> may also change to reflect any changes made to the Data Mart API or in respect
> to improvements in the design, functionality, quality and documentation of
> this package. The authors assume no liability for any problems that may occur
> as a result of using this package, the Data Mart service, any software,
> service, hardware, or user accounts that may utilize this package.


# Introduction

The `RAQSAPI` package for the R programming environment allows a R programming
  environment to connect to and retrieve data from the United States
  Environmental Protection Agency's (US EPA) Air Quality System (AQS) Data Mart
  API v2 Air Quality System[@AQSDataMartWelcome]
  interface directly. This package enables the data user to omit legacy
  challenges including coercing data from a JSON object to a usable R object,
  retrieving multiple years of data, formatting API requests, retrieving
  results, handling credentials, requesting multiple pollutant data and rate
  limiting data requests. All the basic functionality of the API have been
  implemented that are available from the AQS API Data Mart server. The library
  connects to AQS Data Mart API via Secure Hypertext Transfer Protocol (HTTPS)
  so there is no need to install external ODBC drivers, configure ODBC
  connections or deal with the security vulnerabilities associated with them.
  Most API functions have a parameter, return_header, which by default is set to
  FALSE. If the user decides to set return_header to TRUE, then that function
  will return a R AQS_DATAMART_APIv2 S3 object which is a two item named list.
  The first item, (\$Header) in the AQS_DATAMART_APIv2 object, is a
  tibble[@package:tibble]
  which contains the header information. The Header contains status
  information regarding the request (success/fail), any applicable error
  messages returned from the API, if any exist, the URL used in the request, a
  date and time stamp noting when request was received and other useful
  information. The second item of the AQS_DATAMART_APIv2 object (\$Data) is a
  tibble which contains the actual data being requested. For functions with the
  return_header option set to FALSE (default) a simple tibble is returned with
  just the $Data portion of the request. After each call to the API a five
  second stall is invoked to help prevent overloading the Data Mart API server
  and to serve as a simple rate limit.  ^[RAQSAPI's rate limit does not
  guarantee that the user will not go over the rate limit and does not guarantee
  that API calls do not overload the AQS Data Mart system, each user should
  monitor their requests independently.]




















# About the timeliness of AQS Data

EPA's AQS DataMart API, the service that RAQSAPI retrieves data from, does not
host real time (collected now/today) data. If real time data is needed, please
use the AirNow API and direct all questions toward real time data there. RAQSAPI
does not work with AirNow and cannot retrieve real time data. For more details
see section 7.1 of the About AQS Data page[@AboutAQSdata].























# Installing RAQSAPI

Either install the stable version from CRAN or install the latest development version from GitHub.

## Option 1: Installing the stable version from CRAN (preferred method)
First install the `pak` packages if not already installed
``` r
install.packages("pak", dependencies = TRUE)
```
Then use the package `pak` to install the latest stable version of RAQSAPI from CRAN
``` r
pak::pkg_install(pkgs="RAQSAPI", dependencies = TRUE)
```

## Option 2: Installing the development version of RAQSAPI (unstable)

> [!CAUTION]
>
> The development version of RAQSAPI contains code that has not yet been thoroughly been tested and is more
> likely to contain bugs. It is recommended that the stable CRAN version be installed for production use.

To install the development version of `RAQSAPI`, first if not already installed, install the `pak` package and its
dependencies. Then run the following in a R environment.

``` r
pak::pkg_install("USEPA/RAQSAPI")
```


















# Using The RAQSAPI library

## Load RAQSAPI
  after successfully installing the `RAQSAPI` package, load the `RAQSAPI`
  library:

```R
library(RAQSAPI)
```

## Sign up and setting up user credentials with the RAQSAPI library

If you have not already done so you will need to sign up with AQS Data Mart
  using the aqs_sign_up function, ^[Use "?aqs_sign_up" after the RAQSAPI library has
  been loaded to see the full usage description of the aqs_sign_up
  function]. This function takes one input, "email", which is a R
  character object, that represents the email address that you want to use
  as a user credential to the AQS Data Mart service. After a successful call to
  aqs_sign_up an email message will be sent to the email address provided
  with a new Data Mart key which will be used as a credential key to access the
  Data Mart API. The aqs_sign_up function can also be used to regenerate a
  new key for an existing user. To generate a new key simply call the
  aqs_sign_up function with the parameter "email" set to an existing
  account. A new key will be e-mailed to the account given.

The credentials used to access the Data Mart API service are stored in a R
  environment variable that needs to be set every time the `RAQSAPI` library is
  attached or the key is changed. Without valid credentials, the Data Mart
  server will reject any request sent to it. The key used with Data Mart is a
  key and is not a password, so the RAQSAPI library does not treat the key as a
  password; this means that the key is stored in plain text and there are no
  attempts to encrypt Data Mart credentials as would be done for a username and
  password combination. The key that is supplied to use with Data Mart is not
  intended for authentication but only account monitoring. Each time RAQSAPI is
  loaded and before using any of its functions use the aqs_credentials ^[Use
  "?aqs_credentials" after the RAQSAPI library has been loaded to see the full
  usage description of the aqs_credentials function.] function
  to enter in the user credentials so that RAQSAPI can access the AQS Data Mart
  server.

> [!NOTE]
>
> The credentials used to access AQS Data Mart
> API is not the same as the credentials used to access AQS. AQS users who do
> not have access to the AQS Data Mart will need to create new credentials.


## (suggested) Use the `keyring` package to manage credentials
It is highly suggested that users use a keyring manager to store and retrieve
their credentials while using RAQSAPI. One such credential manager is provided
by the `keyring`
package ^[[R `Keyring` package](https://cran.r-project.org/package=keyring)].
The `Keyring` package uses the credential manager available for most popular
operating systems to store and manage user credentials. This will help avoid
hard coding credential information into R scripts.

To use the `keyring` package with `RAQSAPI` first install `keyring`:



Ensure that your system is supported by the `keyring` package before proceeding.



then set the keyring used to access AQS Data Mart (make sure to replace the text
in the angled brackets with your specific user information):



a popup window will appear for the user to input their keyring information.
Enter the AQS Data mart credential key associated with the AQS user name
provided, then hit enter. Now the AQS Data Mart user credential is set using
`keyring`.

To retrieve the keyring to use with `RAQSAPI` load the `keyring` package and use
the function key_get to return the user credential to RAQSAPI:



then pass these variables to the aqs_credentials function when using RAQSAPI:


To change the keyring stored with the `keyring` package repeat the steps above
to call the keyring::key_set function again with the new credential information.

To retrieve a list of all keyrings managed with the `keyring` package use
the function:
> keyring::key_list()

Refer to the[`keyring` package documentation](
https://cran.r-project.org/package=keyring/readme/README.html)
for an in depth explanation on using the `keyring` package.

















## Usage tips and precautions
This section contains suggestions for completing certain data related tasks.

* Determine if or how much data exists for a time-parameter-geography
    combination:
    + Retrieve data using the annualdata service.
    + If no records are returned, we do not have the data.
    + If records are returned, use the observation count to determine the
          temporal and geographic distribution of the data.
* Monthly averages:
    + AQS does not routinely calculate monthly aggregate statistics.
    + If you need these, you must calculate them yourself.
    + These can be calculated from the sample data or the daily data without
        loss of fidelity.
* Determine a single value for a site with collocated monitors:
    + Many sites will have collocated monitors - monitors collecting the same
        parameter at the same time.
    + The API currently provides only monitor level values. (site-level values
        will be added in the future.)
    + For some criteria pollutants (PM2.5, ozone, lead, and NO2), the
        regulations define procedures for defining a single site-level value.
    + For other pollutants, determining a single site-level value is left to
        the investigator.     
        
* __Please adhere to the following when using the AQS Data Mart API__:
  + __Limit the size of queries__. The AQS Data Mart contains billions of values
    and you may request more than you intend. If you are unsure of the
    amount of data, start small and work your way up. Please limit
    queries to 1,000,000 rows of data each. You can use the
    "observation count" field on the annualdata service to determine how
    much data exists for a time-parameter-geography combination.  
  + __Limit the frequency of queries__. The AQS Data Mart can process a limited
    load. Please wait for one request to complete before submitting another
    and do not make more than 10 requests per minute.
  + Be advised that RAQSAPI is capable of retrieving results for multiple
    pollutants; This can result in the amount of data being returned being
    multiplied by the number of pollutants being requested.
  + Be advised that the AQS Data Mart API limits certain data requests to one
    year of data at a time with the exception of the Monitor service. In order
    to retrieve multiple years of data for these functions the RAQSAPI library
    conveniently sends multiple API requests to the Data Mart API server, one
    request for each year; this can result in the amount of data being returned
    being multiplied by the number of years of data being requested.

__The AQS Data Mart administrators may disable accounts without notice for
failure to adhere to these terms (Though they will contact the offending
user via the email address provided)__
















# RAQSAPI functions
The RAQSAPI library exports the following functions (in alphabetical order):
  

```
aqs_annualsummary_by_box  
aqs_annualsummary_by_cbsa  
aqs_annualsummary_by_county  
aqs_annualsummary_by_site  
aqs_annualsummary_by_state  
aqs_cbsas  
aqs_classes  
aqs_counties_by_state  
aqs_credentials  
aqs_dailysummary_by_box  
aqs_dailysummary_by_cbsa  
aqs_dailysummary_by_county  
aqs_dailysummary_by_site  
aqs_dailysummary_by_state  
aqs_fields_by_service  
aqs_isavailable  
aqs_knownissues  
aqs_mas  
aqs_monitors_by_box  
aqs_monitors_by_cbsa  
aqs_monitors_by_county  
aqs_monitors_by_site  
aqs_monitors_by_state  
aqs_parameters_by_class  
aqs_pqaos  
aqs_qa_annualperformanceeval_by_county  
aqs_qa_annualperformanceeval_by_MA  
aqs_qa_annualperformanceeval_by_pqao  
aqs_qa_annualperformanceeval_by_site  
aqs_qa_annualperformanceeval_by_state  
aqs_qa_annualperformanceevaltransaction_by_county  
aqs_qa_annualperformanceevaltransaction_by_MA  
aqs_qa_annualperformanceevaltransaction_by_pqao  
aqs_qa_annualperformanceevaltransaction_by_site  
aqs_qa_annualperformanceevaltransaction_by_state  
aqs_qa_blanks_by_county  
aqs_qa_blanks_by_MA  
aqs_qa_blanks_by_pqao  
aqs_qa_blanks_by_site  
aqs_qa_blanks_by_state  
aqs_qa_collocated_assessments_by_county  
aqs_qa_collocated_assessments_by_MA  
aqs_qa_collocated_assessments_by_pqao  
aqs_qa_collocated_assessments_by_site  
aqs_qa_collocated_assessments_by_state  
aqs_qa_flowrateaudit_by_county  
aqs_qa_flowrateaudit_by_MA  
aqs_qa_flowrateaudit_by_pqao  
aqs_qa_flowrateaudit_by_site  
aqs_qa_flowrateaudit_by_state  
aqs_qa_flowrateverification_by_county  
aqs_qa_flowrateverification_by_MA  
aqs_qa_flowrateverification_by_pqao  
aqs_qa_flowrateverification_by_site  
aqs_qa_flowrateverification_by_state  
aqs_qa_one_point_qc_by_county  
aqs_qa_one_point_qc_by_MA  
aqs_qa_one_point_qc_by_pqao  
aqs_qa_one_point_qc_by_site  
aqs_qa_one_point_qc_by_state  
aqs_qa_pep_audit_by_county  
aqs_qa_pep_audit_by_MA  
aqs_qa_pep_audit_by_pqao  
aqs_qa_pep_audit_by_site  
aqs_qa_pep_audit_by_state  
aqs_quarterlysummary_by_box  
aqs_quarterlysummary_by_county  
aqs_quarterlysummary_by_pqao  
aqs_quarterlysummary_by_site  
aqs_quarterlysummary_by_state  
aqs_removeheader  
aqs_revisionhistory  
aqs_sampledata_by_box  
aqs_sampledata_by_cbsa  
aqs_sampledata_by_county  
aqs_sampledata_by_site  
aqs_sampledata_by_state  
aqs_sampledurations  
aqs_sign_up  
aqs_sites_by_county  
aqs_states  
aqs_transactionsample_by_county  
aqs_transactionsample_by_site  
aqs_transactionsample_by_state  
aqs_transactionsample_by_MA
```
  
  RAQSAPI functions are named according to the service and filter variables that
  are available by the AQS Data Mart API.^[See
  (https://aqs.epa.gov/aqsweb/documents/data_api.html) for full details of the
  Data Mart API]
  
  # Variable descriptions and usage.
  These are all the available variables that can be used with various functions
  exported from the RAQSAPI library listed alphabetically. Not all of these
  variables are used with every function, and not all of these parameters are
  required. See the
  [RAQSAPI functional families](#RAQSAPI functional families) section to
  see which parameters are used with each function.
  
  * AQSobject: a R S3 object that is returned from RAQSAPI aggregate functions
  where return_header is TRUE. An AQS_Data_Mart_APIv2 is a 2 item
  named list in which the first item (\$Header) is a tibble of
  header information from the AQS API and the second item (\$Data)
  is a tibble of the data returned.
  
  * bdate: a R date object which represents the begin date of the data selection.
  Only data on or after this date will be returned.
  
  * cbdate (optional): a R date object which represents the "beginning date of
  last change" that indicates when the data was last
  updated. cbdate is used to filter data based on the
  change date. Only data that changed on or after this date
  will be returned. This is an optional variable which
  defaults to NA_Date. 
  
  * cedate (optional): a R date object which represents the "end date of last
  change" that indicates when the data was last updated.
  cedate is used to filter data based on the change date.
  Only data that changed on or before this date will be
  returned. This is an optional variable which defaults to
  NA_Date.
  
  * countycode: a R character object which represents the 3 digit state FIPS code
  for the county being requested (with leading zero(s)). Refer to
  [aqs_counties_by_state()] for a table of available county
  codes for each state.
  
  * duration (optional): a R character string that represents the parameter
  duration code that limits returned data to a specific
  sample duration. The default value of NA_character_
  will result in no filtering based on duration code.
  Valid durations include actual sample
  durations and not calculated durations such as 8 hour
  CO or O${_3}$ rolling averages, 3/6 day PM averages or
  Pb 3 month rolling averages. Refer to
  [aqs_sampledurations()] for a table of all available
  duration codes.
  
  * edate: a R date object which represents the end date of the data selection.
  Only data on or before this date will be returned.
  
  * email: a R character object which represents the email account that will be
  used to register with the AQS API or change an existing users key. A
  verification email will be sent to the account specified.
  
  * key: the key used in conjunction with the username given to connect to AQS
  Data Mart.
  
  * MA_code: a R character object which represents the 4 digit AQS Monitoring
  Agency code (with leading zeroes).
  
  * maxlat: a R character object that represents the maximum latitude of a
  geographic box. Decimal latitude with north being positive. Only
  data south of this latitude will be returned.
  
  * maxlon: a R character object which represents the maximum longitude of a
  geographic box. Decimal longitude with east being positive. Only
  data west of this longitude will be returned. Note that -80 is less
  than -70.
  
  * minlat: a R character object which represents the minimum latitude of a
  geographic box. Decimal latitude with north being positive.
  Only data north of this latitude will be returned.
  
  * minlon: a R character object which represents the minimum longitude of a
  geographic box. Decimal longitude with east being positive. Only
  data east of this longitude will be returned.
  
  * parameter: a R character list or single character object which represents
  the parameter code of the air pollutant related to the data
  being requested.
  
  * return_header: If FALSE (default) only returns data requested. If TRUE
  returns an AQSAPI_v2 object which is a two item list that
  contains header information returned from the API server
  mostly used for debugging purposes in addition to the
  data requested.
  
  * service a string which represents the services provided by the AQS
  API. For a list of available services refer to
  https://aqs.epa.gov/aqsweb/documents/data_api.html#services
  for the complete listing of services available through the
  DataMart API
  
  * sitenum: a R character object which represents the 4 digit site number (with
  leading zeros) within the county and state being requested.
  
  * stateFIPS: a R character object which represents the 2 digit state FIPS code
  (with leading zero) for the state being requested.
  
  * pqao_code: a R character object which represents the 4 digit AQS Primary
  Quality Assurance Organization code (with leading zeroes).
  
  * username: a R character object which represents the email account that will
  be used to connect to the AQS API.
  
  <a name="RAQSAPI families of functions"> </a>
  # RAQSAPI functional families
  ## Sign up and credentials
  The functions included in this family of functions are:
  

```
aqs_credentials  
aqs_sign_up
```
These functions are used to sign up with Data Mart and to store credential
information to use with RAQSAPI. The RAQSAPI::aqs_signup function takes
one parameter:

* email:

The RAQSAPI::aqs_credentials function takes two parameters:

* username:
* key:

## Data Mart API metadata functions

```
aqs_fields_by_service  
aqs_isavailable  
aqs_knownissues
```
These functions return the status of Data Mart API or metadata associated with
it.

The RAQSAPI::aqs_isavailable function takes no parameters and returns a
table which details the status of the AQS API.

The RAQSAPI::aqs_fields_by_service function takes one parameter, service,
which is a R character object which represents the services provided by
the AQS API. For a list of available services see
[Air Quality System (AQS) API - Services Overview](
https://aqs.epa.gov/aqsweb/documents/data_api.html#services)

The RAQSAPI::aqs_knownissues function takes no parameters and Returns a
table of any known issues with system functionality or the data. These are
usually issues that have been identified internally and will require some
time to correct in Data Mart or the API. This function implements a direct
API call to Data Mart and returns data directly from the API. Issues
returned via this function do not include any issues from the RAQSAPI R
package.

The RAQSAPI::aqs_revisionhistory function is used to query Data Mart for the
change history to the API.

## Data Mart API list functions

```
aqs_cbsas  
aqs_classes  
aqs_counties_by_state  
aqs_mas  
aqs_pqaos  
aqs_sites_by_county  
aqs_states
```
List functions return the API options or groupings that can be used in
conjunction with other API calls. By default each function in this category
returns results as a tibble. If return_header parameter is set to TRUE a
AQSAPI_v2 object is returned instead.

RAQSAPI::aqs_cbsas returns a table of all available Core Based Statistical
Areas (cbsas) and their respective cbsa codes.

RAQSAPI::aqs_states takes no arguments and returns a table of the available
states and their respective state FIPS codes.

RAQSAPI::aqs_sampledurations takes no arguments and returns a table of the
available sample duration code used to construct other requests.

RAQSAPI::aqs_classes takes no arguments and returns a table of parameter
classes (groups of parameters, i.e. "criteria" or "all").

RAQSAPI::aqs_counties_by_state takes one parameter, stateFIPS, which is a two
digit state FIPS code for the state being requested represented as a
R character object and returns a table of counties and their
respective FIPS code for the state requested. Use RAQSAPI::aqs_states to
receive a table of valid state FIPS codes.

RAQSAPI::aqs_sites_by_county takes two parameters, stateFIPS, which is a
two digit state FIPS code for the state being requested and county_code
which is a three digit county FIPS code for the county being requested.
Both stateFIPS and county_code should be encoded as a R character object
This function returns a table of all air monitoring sites with the
requested state and county FIPS code combination.

RAQSAPI::aqs_pqaos takes no parameters and returns an AQS_DATAMART_APIv2
S3 object containing a table of primary quality assurance
organizations (pqaos).

RAQSAPI::aqs_mas takes no parameters and returns an AQS_DATAMART_APIv2 S3
object containing a table of monitoring agencies (MA).

## Data Mart aggregate functions

> [!IMPORTANT]
> AQS Data Mart API restricts the maximum amount of monitoring data to one full year
> of data per API call. These functions are able to return multiple years of data by
> making repeated calls to the API.  Each call to the Data Mart API will take
> time to complete. The more years of data being requested the longer RAQSAPI
> will take to return the results.


These functions retrieve aggregated data from the Data Mart API and are
grouped by how each function aggregates the data. There are 5 different
families of related aggregate functions. These families are arranged by how
the Data Mart API groups the returned data, _by_site, _by_county, _by_state,
_by_<latitude/longitude bounding box> (_by_box) and
_by_<core based statistical area> (_by_cbsa). Within each family
of aggregated data functions there are functions that call on the 10
different services that the Data Mart API provides. All Aggregate
functions return a tibble by default. If the return_Header parameter is
set to TRUE an AQS_DATAMART_APIv2 S3 object is returned instead.

* **These fourteen services are**:
1. **Monitors**: Returns operational information about the samplers (monitors)
used to collect the data. Includes identifying information,
operational dates, operating organizations, etc. Functions
using this service contain *\*monitors_by_*\* in the function 
name.
2. **Sample Data**: Returns sample data - the most fine grain data reported to
EPA. Usually hourly, sometimes 5-minute, 12-hour, etc.
This service is available in several geographic selections
based on geography: site, county, state, cbsa (core based
statistical area, a grouping of counties), or
by latitude/longitude bounding box. Functions using this
service contain *\*sampledata_by_*\* in the function name.
All Sample Data functions accept two additional, optional
parameters; cbdate and cedate:
+ cbdate: a R date object which represents a "beginning date of last 
change" that indicates when the data was last updated.
cbdate is used to filter data based on the change date.
Only data that changed on or after this date will be
returned. This is an optional variable which defaults to
NA_Date_.
+ cedate: a R date object which represents an "end date of last change"
that indicates when the data was last updated. cedate is
used to filter data based on the change date. Only data
that changed on or before this date will be returned. This
is an optional variable which defaults to NA_Date_.
+ duration: an optional R character string that represents the
parameter duration code that limits returned data to
a specific sample duration. The default value of
NA_character_ results in no filtering based on
duration code. Valid durations include actual sample
durations and not calculated durations such as 8 hour
CO or $O_3$ rolling averages, 3/6 day PM averages or
Pb 3 month rolling averages. Refer to 
[aqs_sampledurations()] for a list of all available
duration codes.
3. **Daily Summary Data**: Returns data summarized at the daily level. All daily
summaries are calculated on midnight to midnight
basis in local time. Variables returned include
date, mean value, maximum value, etc. Functions
using this service contain *\*dailysummary_by_*\* in
the function name. All Daily Summary Data functions
accept two additional parameters; cbdate and cedate:
+ cbdate: a R date object which represents a "beginning date of last
change" that indicates when the data was last updated.
cbdate is used to filter data based on the change date. Only
data that changed on or after this date will be returned.
This is an optional variable which defaults to NA_Date_.
+ cedate: a R date object which represents an "end date of last change"
that indicates when the data was last updated. cedate is
used to filter data based on the change date. Only data that
changed on or before this date will be returned. This is an
optional variable which defaults to NA_Date_.
4. **Annual Summary Data**: Returns data summarized at the yearly level.
Variables include mean value, maxima, percentiles,
etc. Functions using this service contain
*\*annualdata_by_*\* in the function name. All
Annual Summary Data functions accept two
additional parameters; cbdate and cedate:
+ cbdate: a R date object which represents a "beginning date of last
change" that indicates when the data was last updated. cbdate
is used to filter data based on the change date. Only data
that changed on or after this date will be returned. This is
an optional variable which defaults to  NA_Date_.
+ cedate: a R date object which represents an "end date of last change"
that indicates when the data was last updated. cedate is used
to filter data based on the change date. Only data that
changed on or before this date will be returned. This is an
optional variable which defaults to NA_Date_.
5. **Quarterly Summary Data**: Returns data summarized at the quarterly level.
Variables include mean value, maxima, percentiles,
etc. Functions using this service contain
*\*quarterlydata_by_*\* in the function name. All
Annual Summary Data functions accept two
additional parameters; cbdate and cedate:
+ cbdate: a R date object which represents a "beginning date of last
change" that indicates when the data was last updated. cbdate
is used to filter data based on the change date. Only data
that changed on or after this date will be returned. This is
an optional variable which defaults to  NA_Date_.
+ cedate: a R date object which represents an "end date of last change"
that indicates when the data was last updated. cedate is used
to filter data based on the change date. Only data that
changed on or before this date will be returned. This is an
optional variable which defaults to NA_Date_.
6. **Quality Assurance - Blanks Data**:
Quality assurance data - blanks samples.
Blanks are unexposed sample collection devices
(e.g., filters) that are transported with the
exposed sample devices to assess if contamination
is occurring during the transport or handling of
the samples. Functions using this service contain
*\*qa\_blanks_by_*\* in the function name.                     
7. **Quality Assurance - Collocated Assessments**:
Quality assurance data - collocated assessments.
Collocated assessments are pairs of samples
collected by different samplers at the same time and
place. (These are "operational" samplers;
assessments with independently calibrated samplers
are called "audits".). Functions using this service
contain *\*qa_collocated_assessments_by_*\* in the
function name.
8. **Quality Assurance - Flow Rate Verifications**:
Quality assurance data - flow rate verifications.
Several times per year, each PM monitor must have its
(fixed) flow rate verified by an operator taking a
measurement of the flow rate. Functions using this
service contain *\*qa_flowrateverification_by_*\* in
the function name. 
9. **Quality Assurance - Flow Rate Audits**:
Quality assurance data - flow rate audits. At least twice
per year, each PM monitor must have its flow rate
measurement audited by an expert using a different
method than is used for flow rate verifications.
Functions using this service contain
*\*qa_flowrateaudit_by_*\* in the function name. 
10. **Quality Assurance - One Point Quality Control Raw Data**:
Quality assurance data - one point quality control check
raw data. At least every two weeks, certain gaseous
monitors must be challenged with a known concentration to
determine monitor performance. Functions using this
service contain *\*qa_one_point_qc_by_*\* in the function
name.  
11. **Quality Assurance - pep Audits**:
Quality assurance data - performance evaluation program
(pep) audits. pep audits are independent assessments used
to estimate total measurement system bias with a primary
quality assurance organization.  Functions using this
service contain *\*qa_pep_audit_by_*\* in the function
name.

12. **Transaction Sample - AQS Submission data in transaction format (RD)**:
Transaction sample data - The raw transaction sample data
uploaded to AQS by the agency responsible for data
submissions in RD format. Functions using this
service contain *\*transactionsample_by_*\* in the
function name. Transaction sample data is only available
aggregated by site, county, state or monitoring agency.

13. **Quality Assurance - Annual Performance Evaluations**:
Quality assurance data  - Annual performance evaluations.
A performance evaluation must be conducted on each primary
monitor once per year. The percent differences between
known and measured concentrations  at several levels are
used to assess the quality of the monitoring data.
Functions using this service contain
*\*aqs_qa_annualperformanceeval_by_*\* in the function
name. Annual performance in transaction format are
only available aggregated by site, county, state,
monitoring agency, and primary quality assurance
organization. Annual performance evaluations are only
available aggregated by site, county, state,
monitoring agency, and primary quality assurance
organization.

14. **Quality Assurance - Annual performance Evaluations in transaction** \
**format (RD)**:
Quality assurance data -   The raw transaction annual
performance evaluations data in RD format. Functions using
this service contain
*\*aqs_qa_annualperformanceevaltransaction_by_*\* in the
function name. Annual performance evaluations in transaction
format are only available aggregated by site, county, state,
monitoring agency, and primary quality assurance
organization.


### Data Mart aggregate functions _by_site

```
aqs_annualsummary_by_site  
aqs_dailysummary_by_site  
aqs_monitors_by_site  
aqs_qa_annualperformanceeval_by_site  
aqs_qa_annualperformanceevaltransaction_by_site  
aqs_qa_blanks_by_site  
aqs_qa_collocated_assessments_by_site  
aqs_qa_flowrateaudit_by_site  
aqs_qa_flowrateverification_by_site  
aqs_qa_one_point_qc_by_site  
aqs_qa_pep_audit_by_site  
aqs_quarterlysummary_by_site  
aqs_sampledata_by_site  
aqs_transactionsample_by_site
```
functions in this family of functions aggregate data at the site level. All
\*_by_site functions accept the following variables:

* parameter: 
* bdate: 
* edate: 
* stateFIPS: 
* countycode:
* sitenum:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* functions and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* functions and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).

### Data Mart aggregate functions _by_county

```
aqs_annualsummary_by_county  
aqs_dailysummary_by_county  
aqs_monitors_by_county  
aqs_qa_annualperformanceeval_by_county  
aqs_qa_annualperformanceevaltransaction_by_county  
aqs_qa_blanks_by_county  
aqs_qa_collocated_assessments_by_county  
aqs_qa_flowrateaudit_by_county  
aqs_qa_flowrateverification_by_county  
aqs_qa_one_point_qc_by_county  
aqs_qa_pep_audit_by_county  
aqs_quarterlysummary_by_county  
aqs_sampledata_by_county  
aqs_sites_by_county  
aqs_transactionsample_by_county
```
functions in this family of functions aggregate data at the county level.
All functions accept the following variables:

* parameter: 
* bdate: 
* edate:
* stateFIPS: 
* countycode:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).

### Data Mart aggregate functions _by_state

```
aqs_annualsummary_by_state  
aqs_counties_by_state  
aqs_dailysummary_by_state  
aqs_monitors_by_state  
aqs_qa_annualperformanceeval_by_state  
aqs_qa_annualperformanceevaltransaction_by_state  
aqs_qa_blanks_by_state  
aqs_qa_collocated_assessments_by_state  
aqs_qa_flowrateaudit_by_state  
aqs_qa_flowrateverification_by_state  
aqs_qa_one_point_qc_by_state  
aqs_qa_pep_audit_by_state  
aqs_quarterlysummary_by_state  
aqs_sampledata_by_state  
aqs_transactionsample_by_state
```
functions in this family of functions aggregate data at the state level.
All functions accept the following variables:

* parameter:
* bdate:
* edate:
* stateFIPS:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* functions and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).

### Data Mart aggregate functions by Monitoring agency (MA)

```
aqs_qa_annualperformanceeval_by_MA  
aqs_qa_annualperformanceevaltransaction_by_MA  
aqs_qa_blanks_by_MA  
aqs_qa_collocated_assessments_by_MA  
aqs_qa_flowrateaudit_by_MA  
aqs_qa_flowrateverification_by_MA  
aqs_qa_one_point_qc_by_MA  
aqs_qa_pep_audit_by_MA  
aqs_transactionsample_by_MA
```
functions in this family of functions aggregate data at the Monitoring Agency
(MA) level. All functions accept the following variables:

* parameter:
* bdate:
* edate:
* MA_code:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledataby*\*, *\*dailysummaryby*\*,
*\*annualdataby*\* and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledataby*\*, *\*dailysummaryby*\*,
*\*annualdataby*\* and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).

### Data Mart aggregate functions by Core Based Statistical Area (cbsa)

```
aqs_annualsummary_by_cbsa  
aqs_dailysummary_by_cbsa  
aqs_monitors_by_cbsa  
aqs_sampledata_by_cbsa
```
functions in this family of functions aggregate data at the Core Based
Statistical Area (cbsa, as defined by the US Census Bureau) level.
All functions accept the following variables:

* parameter:
* bdate:
* edate:
* cbsa_code:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).


### Data Mart aggregate functions by Primary Quality Assurance Organization (pqao)

```
aqs_qa_annualperformanceeval_by_pqao  
aqs_qa_annualperformanceevaltransaction_by_pqao  
aqs_qa_blanks_by_pqao  
aqs_qa_collocated_assessments_by_pqao  
aqs_qa_flowrateaudit_by_pqao  
aqs_qa_flowrateverification_by_pqao  
aqs_qa_one_point_qc_by_pqao  
aqs_qa_pep_audit_by_pqao  
aqs_quarterlysummary_by_pqao
```
functions in this family of functions aggregate data at the Primary Quality
Assurance Organization (pqao) level. All functions accept the following
variables:

* parameter:
* bdate:
* edate:
* pqao_code:
* return_header (optional): set to FALSE by default.

### Data Mart aggregate functions by latitude/longitude bounding box (_by_box)

```
aqs_annualsummary_by_box  
aqs_dailysummary_by_box  
aqs_monitors_by_box  
aqs_quarterlysummary_by_box  
aqs_sampledata_by_box
```
Functions in this family of functions aggregate data by a
latitude/longitude bounding box (_by_box) level. All functions accept the
following variables:

* parameter:
* bdate:
* edate:
* minlat:
* minlon:
* maxlon:
* maxlat:
* cbdate (optional): (This parameter is only used in conjunction with 
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* cedate (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\*, *\*dailysummary_by_*\*,
*\*annualdata_by_*\* and
*\*quarterlysummary_by_*\* functions).
* return_header (optional): set to FALSE by default.
* duration (optional): (This parameter is only used in conjunction with
*\*sampledata_by_*\* functions).

### RAQSAPI Miscellaneous functions

```
aqs_removeheader
```
These are miscellaneous functions exported by RAQSAPI.

RAQSAPI::aqs_removeheader is the function that the RAQSAPI library
uses internally to coerce an AQS_DATAMART_APIv2 S3 object into a tibble.
This is useful if the user saves the output from another RAQSAPI function
with return_header = TRUE set but later decides that they want just a
simple tibble object. This function takes only one variable:

* AQSobject:

















# Troubleshooting

Parameters must be supplied exactly as they are specified. For example the
stateFIPS for Alabama is "01", and entering a value of "1" for the stateFIPS
may lead to unexpected results. Do not omit leading zeros in parameters that
expect them.

In functions that have the return_header=TRUE option set, the returned object is
an AQSAPI_v2 object, this is a 2 item list where the first object is a tibble
with the label \$Header, and the second is also a tibble with the label \$Data.
sampledata functions are limited by the API to one calendar year of data per API
call so if the user requests multiple years of data the sampledata call will return
multiple AQSAPI_v2 objects, one for each call to the API. The returned result is
a list of AQSAPI_v2 objects. In R to access the data in each item in the list
the user will need to use the "double bracket operator ("[[", "]]") not the
single bracket operator ("[", "]").





























## pyaqsapi - a port of RAQSAPI to the python 3 programming environment

For users that feel more comfortable working within a python 3 environment,
[pyaqsapi](https://github.com/USEPA/pyaqsapi) [@py3package:pyaqsapi],
a port of RAQSAPI to the python 3 language has been released. Both projects aim
to maintain feature parity with the other and there are no inherent advantages
to using either project over the other, except for the ability of working within
the programming language environment of choice. The API of both packages are
structured similarly, both packages export the same data and use the same
credentials and data source to retrieve data.


























# Acknowledgements
RAQSAPI was included in the Rblogger's  March 2021: “Top 40” [New CRAN Packages](https://www.r-bloggers.com/2021/04/march-2021-top-40-new-cran-packages/).

The RAQSAPI package borrows upon functions and code provided by sources not
mentioned in the DESCRIPTION file. Here we attempt to acknowledge those sources
without them RAQSAPI would not be possible.

* README badges are provided by R package `badgecreator`[@package:badgecreatr].
* The R package `usethis`[@package:usethis] was used to generate GitHub actions
for Continuous integration (CI).
* Code cleanup was assisted by the R package `lintr`[@package:lintr]
* the function *install.packages* are provided by the R package `utils`[@RBase]
* the function *pkg_install* are provided by the R package `pak`[
@package:pak]
* .gitignore file borrowed examples from
https://github.com/github/gitignore/blob/master/R.gitignore
* . CITATION.cff file was generated by the R package `cffr` [@package:cffr]
* R package `urlchecker` [@package:urlchecker] was used to check urls in RAQSAPI
documentation
* R package `goodpractice` [@package:goodpractice] was used for static code checking
* R package `formatR` [@package:formatR] was used to form code in this repository
* R package `testthat` [@package:testthat] is used for unit testing
* unit tests are mocked using R package `httptest2` [@package:httptest2]

Additionally the project maintainers would like to thank the following people:
Cynthia Sthal, Maria Morresi both at the US Environmental Protection Agency and
Hayley Brittingham at Neptune and Company for their assistance with improving the
package documentation. This is a huge undertaking, documentation is often overlooked
and not given proper attention to. We thank you for your efforts.

# References
