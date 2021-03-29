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
    -   [Data Mart aggregate functions](#data-mart-aggregate-functions)
        -   [See the RAQSAPI vignette for more
            details](#see-the-raqsapi-vignette-for-more-details)

<!-- badges: start -->

[![R-CMD-check](https://github.com/USEPA/RAQSAPI/workflows/R-CMD-check/badge.svg)](https://github.com/USEPA/RAQSAPI/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/RAQSAPI)](https://CRAN.R-project.org/package=RAQSAPI)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![license](https://img.shields.io/badge/license-CC0-lightgrey.svg)](https://choosealicense.com/)
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

The RAQSAPI package for the R programming environment allows a R
programming environment to connect to and retrieve data from the United
States Environmental Protection Agency’s (US EPA) Air Quality System
(AQS) Data Mart API v2 [1] interface directly. This package enables the
data user to omit legacy challenges including coercing data from a JSON
object to a usable R object, retrieving multiple years of data,
formatting API requests, retrieving results, handling credentials,
requesting multiple pollutant data and rate limiting data requests. All
the basic functionality of the API have been implemented that are
available from the AQS API Data Mart server. The library connects to AQS
Data Mart API via Hypertext Transfer Protocol (HTTP) so there is no need
to install external ODBC drivers, configure ODBC connections or deal
with the security vulnerabilities associated with them. Most functions
have a parameter, return\_header which by default is set to FALSE. If
the user decides to set return\_header to TRUE, then that function will
return a R AQS\_DATAMART\_APIv2 S3 object which is a two item named
list.  
The first item, ($Header) in the AQS\_DATAMART\_APIv2 object is a tibble
[2] which contains the header information. The Header contains status
information regarding the request (success/fail), any applicable error
messages returned from the API, if any exist, the URL used in the
request, a date and time stamp noting when request was received and
other useful information. The second item of the AQS\_DATAMART\_APIv2
object ($Data) is a tibble which contains the actual data being
requested. For functions with the return\_header option set to FALSE
(default) a simple tibble is returned with just the $Data portion of the
request. After each call to the API a five second stall is invoked to
help prevent overloading the Data Mart API server and to serve as a
simple rate limit. [3]

# Installing RAQSAPI

Either install the stable version from CRAN or install the latest
development version from GitHub.

## Option 1: Installing the stable version from CRAN

``` r
install.packages(pkgs="RAQSAPI", dependencies = TRUE )
```

## Option 2: Installing the development version of RAQSAPI

to install the development version of RAQSAPI, first If not already
installed, install the remotes package and its dependencies. Then run
the following in an R environment.

``` r
remotes::install_github(repo = "USEPA/raqsapi",
                        dependencies = TRUE,
                        upgrade = "always",
                        build = TRUE,
                        #optional, if you want the manual, requires pandoc
                        build_manual = FALSE,
                        build_vignettes = TRUE 
                        )
```

# Using The RAQSAPI library

## Load RAQSAPI

after successfully installing the RAQSAPI package, load the RAQSAPI
library:

``` r
library(RAQSAPI)
```

## Sign up and setting up user credentials with the RAQSAPI library

If you have not already done so you will need to sign up with AQS Data
Mart using aqs\_sign\_up function, [4] this function takes one input,
“email”, which is a R character object, that represents the email
address that you want to use as a user credential to the AQS Data Mart
service. After a successful call to aqs\_sign\_up an email message will
be sent to the email address provided with a new Data Mart key which
will be used as a credential key to access the Data Mart API. The
aqs\_sign\_up function can also be used to regenerate a new key for an
existing user, to generate a new key simply call the aqs\_sign\_up
function with the parameter “email” set to an existing account. A new
key will be e-mailed to the account given.

The credentials used to access the Data Mart API service are stored in
an R environment variable that needs to be set every time the RAQSAPI
library is attached or the key is changed. Without valid credentials,
the Data Mart server will reject any request sent to it. The key used
with Data Mart is a key and is not a password, so the RAQSAPI library
does not treat the key as a password; this means that the key is stored
in plain text and there are no attempts to encrypt Data Mart credentials
as would be done for a username and password combination. The key that
is supplied to use with Data Mart is not intended for authentication but
only account monitoring. Each time RAQSAPI is loaded and before using
any of it’s functions use the aqs\_credentials [5] function to enter in
the user credentials so that RAQSAPI can access the AQS Data Mart
server.

<table>
<colgroup>
<col style="width: 100%" />
</colgroup>
<thead>
<tr class="header">
<th><span style="color:red"> Note: The credentials used to access AQS Data Mart<br />
API are not the same as the credentials used to access AQS. AQS users who do<br />
not have access to the AQS Data Mart will need to create new credentials<br />
</span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

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
variables that are available by the Data Mart API.[6]

## Data Mart aggregate functions

These functions retrieve aggregated data from the Data Mart API and are
grouped by how each function aggregates the data. There are 7 different
families of related aggregate functions in which the AQS Data Mart API
groups data.

**These seven families are**:

1.  **\_by\_site**
2.  **\_by\_county**
3.  **\_by\_state**
4.  **\_by\_&lt;latitude/longitude bounding box&gt;** (\_by\_box)
5.  **\_by\_&lt;monitoring agency&gt;** (\_by\_MA)
6.  **\_by\_&lt;Primary Quality Assurance Organization&gt;**
    (\_by\_pqao)
7.  **\_by\_&lt;core based statistical area&gt;** (\_by\_cbsa).

Within these families of aggregated data functions there are functions
that call on the 11 different aggregate services that the Data Mart API
provides. **Note that not all aggregations are available for each
service.**

**These eleven services are**:

1.  **Monitors** (*monitors\_by\_*)
2.  **Sample Data** (*sampledata\_by\_*)
3.  **Daily Summary Data** (*dailydata\_by\_*)
4.  **Annual Summary Data** (*annualdata\_by\_*)
5.  **Quality Assurance - Blanks Data** (*qa\_blanks\_by\_*)
6.  **Quality Assurance - Collocated Assessments**
    (*qa\_collocated\_assessments\_by\_*)
7.  **Quality Assurance - Flow Rate verifications**
    (*qa\_flowrateverification\_by\_*)
8.  **Quality Assurance - Flow Rate Audits**
    (*aqs\_qa\_flowrateaudit\_by\_*)
9.  **Quality Assurance - One Point Quality Control Raw Data**
    (*qa\_one\_point\_qc\_by\_*)
10. **Quality Assurance - PEP Audits** (*qa\_pep\_audit\_by\_*)
11. **Transaction Sample - AQS Submission data in transaction Format
    (RD)** (*aqs\_transactionsample\_by\_*)

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

Aggregate functions are named
AQS\_API&lt;service&gt;\_&lt;aggregation&gt;() where &lt;service&gt; is
one of the 11 services listed above and &lt;aggregation&gt; is either
"\_by\_site“,”\_by\_county“,”\_by\_state“,”\_by\_box“,”\_by\_cbsa".

### See the RAQSAPI vignette for more details

(RAQSAPI must be installed first)

> RShowDoc(what=“RAQSAPIvignette”, type=“html”, package=“RAQSAPI”)

[1] [Air Quality System (AQS)
API](https://aqs.epa.gov/aqsweb/documents/data_api.html)

[2] see (<https://tibble.tidyverse.org>) for more information about
tibbles.

[3] RAQSAPI’s rate limit does not guarantee that the user will not go
over the rate limit and does not guarantee that API calls do not
overload the AQS Data Mart system, each user should monitor their
requests independently.

[4] Use “?aqs\_sign\_up” after the RAQSAPI library has been loaded to
see the full usage description of the aqs\_sign\_up function.

[5] Use “?aqs\_credentials” after the RAQSAPI library has been loaded to
see the full usage description of the aqs\_credentials function.

[6] See (<https://aqs.epa.gov/aqsweb/documents/data_api.html>) for the
full details of the Data Mart API
