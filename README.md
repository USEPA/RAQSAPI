Introduction to the RAQSAPI package
================
Clinton Mccrowey, physical scientist - US EPA

-   [EPA Disclaimer](#epa-disclaimer)
-   [Introduction](#introduction)
-   [Installing RAQSAPI](#installing-raqsapi)
    -   [Option 1: Installing the stable version from
        CRAN](#option-1-installing-the-stable-version-from-cran)
    -   [Option 2: Installing the development version of
        RAQSAPI](#option-2-installing-the-development-version-of-raqsapi)
-   [Using The RAQSAPI library](#using-the-raqsapi-library)
    -   [Load RAQSAPI](#load-raqsapi)
    -   [Sign up and setting up user credentials with the RAQSAPI
        library](#sign-up-and-setting-up-user-credentials-with-the-raqsapi-library)
    -   [(suggested) Use the `keyring` package to manage
        credentials](#suggested-use-the-keyring-package-to-manage-credentials)
    -   [Data Mart aggregate functions](#data-mart-aggregate-functions)
        -   [See the RAQSAPI vignette for more
            details](#see-the-raqsapi-vignette-for-more-details)
-   [Acknowledgements](#acknowledgements)
-   [References](#references)

<!-- ![](./vignettes/figures/RAQSAPIhexsticker.png){width=20%} -->

<img src="./vignettes/figures/RAQSAPIhexsticker.png" alt="RAQSAPI hexsticker" width="150" height="150">

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state  
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/USEPA/RAQSAPI/workflows/R-CMD-check/badge.svg)](https://github.com/USEPA/RAQSAPI/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/RAQSAPI)](https://CRAN.R-project.org/package=RAQSAPI)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/RAQSAPI)](https://cran.r-project.org/package=RAQSAPI)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![license](https://img.shields.io/badge/license-CC0-lightgrey.svg)](https://choosealicense.com/)
[![Last-changedate](https://img.shields.io/badge/last%20change-%202021--11--27-blue.svg)](/commits/master)
<!-- badges: end -->

# EPA Disclaimer

This software/application was developed by the U.S. Environmental
Protection Agency (USEPA). No warranty expressed or implied is made
regarding the accuracy or utility of the system, nor shall the act of
distribution constitute any such warranty. The USEPA has relinquished
control of the information and no longer has responsibility to protect
the integrity, confidentiality or availability of the information. Any
reference to specific commercial products, processes, or services by
service mark, trademark, manufacturer, or otherwise, does not constitute
or imply their endorsement, recommendation or favoring by the USEPA. The
USEPA seal and logo shall not be used in any manner to imply endorsement
of any commercial product or activity by the USEPA or the United States
Government.

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th><span style="color:red"> Warning: US EPA’s AQS Data Mart API V2 is currently<br />
in beta phase of development, the API interface has not been finalized.<br />
This means that certain functionality of the API may change or be removed<br />
without notice. As a result, this package is also currently marked as beta and<br />
may also change to reflect any changes made to the Data Mart API or in respect<br />
to improvements in the design, functionality, quality and documentation of<br />
this package. The authors assume no liability for any problems that may occur<br />
as a result of using this package, the Data Mart service, any software,<br />
service, hardware, or user accounts that may utilize this package. </span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

# Introduction

The `RAQSAPI` package for the R programming environment allows a R
programming environment to connect to and retrieve data from the United
States Environmental Protection Agency’s (US EPA) Air Quality System
(AQS) Data Mart API v2 (Air Quality System)<sup>1</sup> interface
directly. This package enables the data user to omit legacy challenges
including coercing data from a JSON object to a usable R object,
retrieving multiple years of data, formatting API requests, retrieving
results, handling credentials, requesting multiple pollutant data and
rate limiting data requests. All the basic functionality of the API have
been implemented that are available from the AQS API Data Mart server.
The library connects to AQS Data Mart API via Hypertext Transfer
Protocol (HTTP) so there is no need to install external ODBC drivers,
configure ODBC connections or deal with the security vulnerabilities
associated with them. Most functions have a parameter, return_header
which by default is set to FALSE. If the user decides to set
return_header to TRUE, then that function will return a R
AQS_DATAMART_APIv2 S3 object which is a two item named list.  
The first item, ($Header) in the AQS_DATAMART_APIv2 object is a
(tibble)<sup>2</sup> which contains the header information. The Header
contains status information regarding the request (success/fail), any
applicable error messages returned from the API, if any exist, the URL
used in the request, a date and time stamp noting when request was
received and other useful information. The second item of the
AQS_DATAMART_APIv2 object ($Data) is a tibble which contains the actual
data being requested. For functions with the return_header option set to
FALSE (default) a simple tibble is returned with just the $Data portion
of the request. After each call to the API a five second stall is
invoked to help prevent overloading the Data Mart API server and to
serve as a simple rate limit.[1]

# Installing RAQSAPI

Either install the stable version from CRAN or install the latest
development version from GitHub.

## Option 1: Installing the stable version from CRAN

``` r
install.packages(pkgs="RAQSAPI", dependencies = TRUE )
```

## Option 2: Installing the development version of RAQSAPI

To install the development version of `RAQSAPI`, first if not already
installed, install the `remotes` package and its dependencies. Then run
the following in a R environment.

``` r
remotes::install_github(repo = "USEPA/raqsapi",
                        dependencies = TRUE,
                        upgrade = "always",
                        build = TRUE,
                        #optional, set TRUE if the manual is desired,
                        #requires pandoc
                        build_manual = FALSE,
                        build_vignettes = TRUE 
                        )
```

# Using The RAQSAPI library

## Load RAQSAPI

after successfully installing the `RAQSAPI` package, load the `RAQSAPI`
library:

``` r
library(RAQSAPI)
```

## Sign up and setting up user credentials with the RAQSAPI library

If you have not already done so you will need to sign up with AQS Data
Mart using aqs_sign_up function,[2] this function takes one input,
“email,” which is a R character object, that represents the email
address that you want to use as a user credential to the AQS Data Mart
service. After a successful call to aqs_sign_up an email message will be
sent to the email address provided with a new Data Mart key which will
be used as a credential key to access the Data Mart API. The aqs_sign_up
function can also be used to regenerate a new key for an existing user,
to generate a new key simply call the aqs_sign_up function with the
parameter “email” set to an existing account. A new key will be e-mailed
to the account given.

The credentials used to access the Data Mart API service are stored in a
R environment variable that needs to be set every time the `RAQSAPI`
library is attached or the key is changed. Without valid credentials,
the Data Mart server will reject any request sent to it. The key used
with Data Mart is a key and is not a password, so the RAQSAPI library
does not treat the key as a password; this means that the key is stored
in plain text and there are no attempts to encrypt Data Mart credentials
as would be done for a username and password combination. The key that
is supplied to use with Data Mart is not intended for authentication but
only account monitoring. Each time RAQSAPI is loaded and before using
any of it’s functions use the aqs_credentials[3] function to enter in
the user credentials so that RAQSAPI can access the AQS Data Mart
server.

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th><span style="color:red"> Note: The credentials used to access AQS Data Mart<br />
API is not the same as the credentials used to access AQS. AQS users who do<br />
not have access to the AQS Data Mart will need to create new credentials.<br />
</span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

## (suggested) Use the `keyring` package to manage credentials

It is highly suggested that users use a keyring manager to store and
retrieve their credentials while using RAQSAPI. One such credential
manager is provided by the `keyring` package.[4] The `Keyring` package
uses the credential manager available for most popular operating systems
to store and manage user credentials. This will help avoid hard coding
credential information into R scripts.

To use the `keyring` package with `RAQSAPI` first install `keyring`:

> install.package(“keyring”)

Ensure that your system is supported by the `keyring` package before
proceeding.

> keyring::has_keyring_support()

then set the keyring used to access AQS Data Mart (make sure to replace
the text in the angled brackets with your specific user information):

> library(“keyring”) keyring::key_set(service = “AQSDatamart,” username
> = “\<user email account>”)

a popup window will appear for the user to input their keyring
information. Enter the AQS Data mart credential key associated with the
AQS user name provided, then hit enter. Now the AQS Data Mart user
credential is set using `keyring`.

To retrieve the keyring to use with `RAQSAPI` load the `keyring` package
and use the function key_get to return the user credential to RAQSAPI:

> library(RAQSAPI) library(keyring) datamartAPI_user \<- \<user email
> account> server \<- “AQSDatamart”

then pass these variables to the aqs_credentials function when using
RAQSAPI:

> aqs_credentials(username = datamartAPI_user, key = key_get(service =
> server, username = datamartAPI_user ) )

To change the keyring stored with the `keyring` package repeat the steps
above to call the keyring::key_set function again with the new
credential information.

To retrieve a list of all keyrings managed with the `keyring` package
use the function: \> keyring::key_list()

Refer the the[`keyring` package
documentation](https://cran.r-project.org/package=keyring/readme/README.html)
for an in depth explanation on using the `keyring` package.

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th><span style="color:blue"> Information: AQS Data Mart API restricts the<br />
maximum amount of monitoring data to one full year of data per API<br />
call.</span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

RAQSAPI functions are named according to the service and filter
variables that are available by the Data Mart API.[5]

## Data Mart aggregate functions

These functions retrieve aggregated data from the Data Mart API and are
grouped by how each function aggregates the data. There are 7 different
families of related aggregate functions in which the AQS Data Mart API
groups data.

**These seven families are**:

-   **\_by_site**
-   **\_by_county**
-   **\_by_state**
-   **\_by\_\<latitude/longitude bounding box>** (\_by_box)
-   **\_by\_\<monitoring agency>** (\_by_MA)
-   **\_by\_\<Primary Quality Assurance Organization>** (\_by_pqao)
-   **\_by\_\<core based statistical area (as defined by the**  
    **US census Bureau)>** (\_by_cbsa).

Within these families of aggregated data functions there are functions
that call on the 13 different aggregate services that the Data Mart API
provides. **Note that not all aggregations are available for each
service.**

**These fourteen services are**:

-   **Monitors** (monitors_by\_\*)
-   **Sample Data** (sampledata_by\_\*)
-   **Daily Summary Data** (dailydata_by\_\*)
-   **Annual Summary Data** (annualdata_by\_\*)
-   **Quarterly Summary Data** (q
-   **Quality Assurance - Blanks Data** (qa_blanks_by\_\*)
-   **Quality Assurance - Collocated Assessments**
    (qa_collocated_assessments_by\_\*)
-   **Quality Assurance - Flow Rate Verifications**
    (qa_flowrateverification_by\_\*)
-   **Quality Assurance - Flow Rate Audits**
    (aqs_qa_flowrateaudit_by\_\*)
-   **Quality Assurance - One Point Quality Control Raw Data**
    (qa_one_point_qc_by\_\*)
-   **Quality Assurance - PEP Audits** (qa_pep_audit_by\_\*)
-   **Transaction Sample - AQS Submission data in transaction Format
    (RD)** (aqs_transactionsample_by\_\*)
-   **Quality Assurance - Annual Performance Evaluations**  
    (aqs_qa_annualpeferomanceeval_by\_\*)
-   **Quality Assurance - Annual Performance Evaluations in the AQS**  
    **Submission transaction format (RD)**
    (aqs_qa_annualperformanceevaltransaction_by\_\*)

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th><span style="color:blue"> Information: AQS Data Mart API restricts the<br />
maximum amount of monitoring data to one full year of data per<br />
API call. These functions are able to return multiple years of data by<br />
making repeated calls to the API. Each call to the Data Mart API will take<br />
time to complete. The more years of data being requested the longer RAQSAPI<br />
will take to return the results.</span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

Aggregate functions are named AQS_API\<service>\_\<aggregation>() where
\<service> is one of the 13 services listed above and \<aggregation> is
either “\_by_site”, “\_by_county”, “\_by_state”, “\_by_box”,
“\_by_cbsa”.

### See the RAQSAPI vignette for more details

(RAQSAPI must be installed and built with BUILD_MANUAL = TRUE enabled)

> RShowDoc(what=“RAQSAPIvignette,” type=“html,” package=“RAQSAPI”)

# Acknowledgements

The RAQSAPI package borrows upon functions and code provided by sources
not mentioned in the DESCRIPTION file. Here we attempt to acknowledge
those sources with them RAQSAPI would not be possible.

-   README badges are provided by R package badgecreator<sup>3</sup>.
-   The R package usethis<sup>4</sup> was used to generate GitHub
    actions for Continuous integration (CI).
-   Code cleanup was assisted by the R package lintr<sup>5</sup>
-   the function *install.packages* are provided by the R package
    utils<sup>6</sup>
-   the function *install_github* are provided by the R package
    remotes<sup>7</sup>
-   .gitignore file borrowed examples from
    <https://github.com/github/gitignore/blob/master/R.gitignore>
-   The RAQSAPI hexsticker was created using the `hexSticker` package
    for R

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-AQSDataMartWelcome" class="csl-entry">

<span class="csl-left-margin">(1) </span><span
class="csl-right-inline">AQS data mart welcome
<https://aqs.epa.gov/aqsweb/documents/data_mart_welcome.html>.</span>

</div>

<div id="ref-website:tibble" class="csl-entry">

<span class="csl-left-margin">(2) </span><span
class="csl-right-inline">Müller, H., Kirill; Wickham. Tibble Part of the
Tidyverse, 2019.</span>

</div>

<div id="ref-package:badgecreatr" class="csl-entry">

<span class="csl-left-margin">(3) </span><span
class="csl-right-inline">Hogervorst, R. M. *Badgecreatr: Create Badges
for ’Travis’, ’Repostatus’ ’Codecov.io’ Etc in Github Readme*;
2019.</span>

</div>

<div id="ref-package:usethis" class="csl-entry">

<span class="csl-left-margin">(4) </span><span
class="csl-right-inline">Wickham, H.; Bryan, J. *Usethis: Automate
Package and Project Setup*; 2021.</span>

</div>

<div id="ref-package:lintr" class="csl-entry">

<span class="csl-left-margin">(5) </span><span
class="csl-right-inline">Hester, J.; Angly, F.; Hyde, R. *Lintr: A
’Linter’ for r Code*; 2020.</span>

</div>

<div id="ref-RBase" class="csl-entry">

<span class="csl-left-margin">(6) </span><span
class="csl-right-inline">Team, R. C. *R: A Language and Environment for
Statistical Computing*; R Foundation for Statistical Computing: Vienna,
Austria, 2019.</span>

</div>

<div id="ref-package:remotes" class="csl-entry">

<span class="csl-left-margin">(7) </span><span
class="csl-right-inline">Hester, J.; Csárdi, G.; Wickham, H.; Chang, W.;
Morgan, M.; Tenenbaum, D. *Remotes: R Package Installation from Remote
Repositories, Including ’GitHub’*; 2021.</span>

</div>

</div>

[1] RAQSAPI’s rate limit does not guarantee that the user will not go
over the rate limit and does not guarantee that API calls do not
overload the AQS Data Mart system, each user should monitor their
requests independently.

[2] Use “?aqs_sign_up” after the RAQSAPI library has been loaded to see
the full usage description of the aqs_sign_up function.

[3] Use “?aqs_credentials” after the RAQSAPI library has been loaded to
see the full usage description of the aqs_credentials function.

[4] \[R `Keyring` package\]<https://cran.r-project.org/package=keyring>)

[5] See (<https://aqs.epa.gov/aqsweb/documents/data_api.html>) for the
full details of the Data Mart API
