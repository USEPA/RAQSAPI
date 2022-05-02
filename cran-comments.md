# cran-comments for RAQSAPI

## RAQSAPI 2.0.4

*Important API change - aqs\_qa\_annualperformanceeval* and
aqs\_qa\_annualperformanceevaltransaction\* functions have been renamed
with more appropriate spelling. This may effect external code that
depends on these functions. - many update to the reference documentation
to improve consistency and fix issues. - Minor updates to the README. -
Minor improvements to code.

## RAQSAPI 2.0.3

-   this is a minor release
-   Remove dependency on package:gtools.
-   removes calls to depreciated functions imported from package:rlang
-   updates documentation to include a note about the timeliness of data

## RAQSAPI 2.0.2

-   This is a major release

### Reason/Significant changes

-   new hexsticker!
-   Add MD5
-   Correctly throws an error if a RAQSAPI function is used without
    providing credentials (issue 5)
-   Updated documentation (Thanks Hayley Brittingham)
-   a lot of code cleanup
-   minor issues in documentation fixed
-   RAQSAPI functions now checks parameters before sending calls to API
    (issue 6)
-   renamed non-exported functions containing dots in names with
    underscores.
    -   This should not affect end users.
-   added CITATION entry
-   New service APIs aqs\_qa\_annualperformanceeval\_by\_*,
    aqs\_qa\_annualperformanceevaltransaction\_by\_*,
    quarterlysummary\_by\_\*, resulting in 15 new exported functions
    -   aqs\_qa\_annualperformanceeval\_by\_county,
    -   aqs\_qa\_annualperformanceeval\_by\_MA,
    -   aqs\_qa\_annualperformanceeval\_by\_pqao
    -   aqs\_qa\_annualperformanceeval\_by\_site,
    -   aqs\_qa\_annualperformanceeval\_by\_state,
    -   aqs\_qa\_annualperformanceevaltransaction\_by\_county,
    -   aqs\_qa\_annualperformanceevaltransaction\_by\_MA
    -   aqs\_qa\_annualperformanceevaltransaction\_by\_pqao,
    -   aqs\_qa\_annualperformanceevaltransaction\_by\_site,
    -   aqs\_qa\_annualperformanceevaltransaction\_by\_state
    -   aqs\_quarterlysummary\_by\_county
    -   aqs\_quarterlysummary\_by\_state
    -   aqs\_quarterlysummary\_by\_site
    -   aqs\_quarterlysummary\_by\_cbsa
    -   aqs\_quarterlysummary\_by\_box
-   aqs\_monitors\_by\_\* functions now include a new optional duration
    parameter to filter results by duration code
-   New list function aqs\_sampledurations to retrieve a table of valid
    sample durations.

### Test environments

-   local R installation, Windows 10 (Windows 10 Version 20H2 OS Build
    19042.1348), R 4.1.1
-   local R installation, Gentoo GNU/Linux R 4.1.0
-   Windows-latest (Microsoft Windows Server 2019 10.0.17763) (release)
    (via Github\_actions), R 4.1.2
-   macOS-latest (Mac OS X 10.15.7) (release) (via Github\_actions), R
    4.1.2
-   ubuntu-latest (Ubuntu 20.04.3 LTS) (release) (via Github\_actions),
    R 4.1.2
-   ubuntu-20.04 (Ubuntu 20.04.3 LTS) (oldrel1) (via Github\_actions), R
    4.0.5

## RAQSAPI 2.0.1

-   This is a new release.

### Reason/Significant changes

-   This version fixes issues with aqs\_annual\_summary\_by\_box.
-   updates and corrects issues in README.
-   add CI/CD (github actions) to git repository.
-   separate AQSAPI.R into smaller more manageable files.
-   devtools::package\_coverage() now works
-   lifecycle(maturing)
-   4 new functions
    -   aqs\_transactionsample\_by\_county
    -   aqs\_transactionsample\_by\_MA
    -   aqs\_transactionsample\_by\_site
    -   aqs\_transactionsample\_by\_state

### Test environments

-   local R installation, Windows 10 R 4.0.3
-   local R installation, Gentoo GNU/Linux R 4.0.4
-   Windows-latest (release) (on Github\_actions), R 4.0.4
-   macOS-latest (release) (on Github\_actions), R 4.0.4
-   ubuntu-20.04 (release) (on Github\_actions), R 4.0.4
-   ubuntu-20.04 (devel) (on Github\_actions), R 4.0.4

## remote R CMD Check results (github actions)

-   Check runs successfully on all environments.

### Local R CMD check results

0 errors | 50+ warnings | 0 notes

Found the following (possibly) invalid URLs: URL:
<https://aqs.epa.gov/aqsweb/documents/data_api.html>&gt; From:
DESCRIPTION Status: 404 Message: Not Found URL:
<https://github.com/USEPA/RAQSAPI>&gt; From: DESCRIPTION Status: 404
Message: Not Found

The above URIs work as expected, Not sure why this warning appears.
Maybe because it is a https address?

### Reverse dependency check

Currently no reverse dependencies.

### steps to reproduce locally

source dev/install\_RAQSAPI.R then run RAQSAPICHECK() (github)

## RAQSAPI 2.0.0

-   Initial RAQSAPI release on CRAN and github

0 errors | 50 warnings | 1 notes

Found the following (possibly) invalid URLs: URL:
<https://aqs.epa.gov/aqsweb/documents/data_api.html>&gt; From:
DESCRIPTION Status: 404 Message: Not Found URL:
<https://github.com/USEPA/RAQSAPI>&gt; From: DESCRIPTION Status: 404
Message: Not Found

-   The above URIs work as expected, Not sure why this note appears.
    Maybe because it is a https address.

Warning messages: 1-50+ In parse\_Rd(“/RAQSAPI/man/.Rd”, … :
C://RAQSAPI/man/.Rd:78: unknown macro ‘’ \* This is an unsolved issue in
either the lifecylce or spelling packages and not caused by this
package. See <https://github.com/r-lib/lifecycle/issues/19> for details.
