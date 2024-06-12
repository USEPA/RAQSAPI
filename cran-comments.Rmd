---
title: "cran-comments"
author: "Clinton Mccrowey"
date: "01/04/2023"
output: md_document
---

# cran-comments for RAQSAPI
## RAQSAPI 2.0.5
  - remove depricated package:goodpractice from SUGGESTS
  - Added the ability to display server side messages for errors in API calls.
  - Added a new CITATION.cff for thanks to R package cffr.
  - New hexstciker.
  - aqs_isavailable no longer accepts the return_header parameter, this function
    just returns a tibble and not a AQS_Data Mart_APIv2 object.
  - fixed an issue where the duration parameter was being ignored in
    aqs_sampledata_by_* functions.
  - add note to aqs_sampledurations function about not returning calculated
    durations
  *RAQSAPI 2.0.4 was not published on CRAN.
  - The rate limit has been changed from a constant 5 second wait time to a
    maximum of 10 requests per minute.
  - RAQSAPI will retry certain requests if they fail for a maximum of 5 times
    after a 10 second wait time.
  - Moved to the httr2 backend, removed desc from suggests and
    httr, and jsonlite as from imports.
  - There is an issue with curl connecting to the AQS Datamart API, on
    windows platforms, as a temporary fix, RAQSAPI will default to using
    the Schannel curl backend.
  - modify QA Collocated Assessments by County and by Site unit tests and
    example code to reflect modified data.
  - update CITATION file to the new style citation.
  - Package documentation referenced \*dailydata\* functions incorrectly,
    functions should be called \*dailysummary\* 
  - Switched license from CC0 to MIT, RAQSAPI will use MIT going forward.
  - a note Rd tag was added to the checkaqsparams function
  - minor correction to the documentation of the aqs_monitors_by_state
    function
  - Update github actions to the new workflow available through
    package:usethis-2.1.6
    + usethis::use_github_action("test-coverage") to add coverage
    information
    in the git repository.
    + usethis::use_github_pages() to create a github page.
  
### Test environments
* local R installation, Windows 11 R 4.4.0
* Windows-latest (release) (via Github_actions), R 4.4.0
* MacOS-latest (release) (via Github_actions), R 4.4.0
* Ubuntu-22.04.4 (release) (via Github_actions), R 4.4.0.1
* Ubuntu-22.04.4 (devel) (via Github_actions), R (unstable) (2024-06-10
    r86715)
* Ubuntu-22.04.4 (old-release) (via Github_actions), R 4.3.3

## RAQSAPI 2.0.4
*Important API change
  - aqs_qa_annualperformanceeval* and aqs_qa_annualperformanceevaltransaction*
    functions have been renamed with more appropriate spelling. This may effect
    external code that depends on these functions.
  - many update to the reference documentation to improve consistency and
    fix issues.
  - Minor updates to the README.
  - Minor improvements to code.
  - This version was not published on CRAN but builds are availble on github.

## RAQSAPI 2.0.3
* this is a minor release
* Remove dependency on package:gtools.
* removes calls to depreciated functions imported from package:rlang
* updates documentation to include a note about the timeliness of data

## RAQSAPI 2.0.2
* This is a major release

### Reason/Significant changes
* new hexsticker!
* Add MD5
* Correctly throws an error if a RAQSAPI function is used without
    providing credentials (issue 5)
* Updated documentation (Thanks Hayley Brittingham)
* a lot of code cleanup
* minor issues in documentation fixed
* RAQSAPI functions now checks parameters before sending calls to API (issue 6)
* renamed non-exported functions containing dots in names with underscores.
    - This should not affect end users.
* added CITATION entry
* New service APIs aqs_qa_annualperformanceeval_by_*,
    aqs_qa_annualperformanceevaltransaction_by_*, quarterlysummary_by_*,
    resulting in 15 new exported functions
    - aqs_qa_annualperformanceeval_by_county,
    - aqs_qa_annualperformanceeval_by_MA,
    - aqs_qa_annualperformanceeval_by_pqao
    - aqs_qa_annualperformanceeval_by_site,
    - aqs_qa_annualperformanceeval_by_state,
    - aqs_qa_annualperformanceevaltransaction_by_county,
    - aqs_qa_annualperformanceevaltransaction_by_MA
    - aqs_qa_annualperformanceevaltransaction_by_pqao,
    - aqs_qa_annualperformanceevaltransaction_by_site,
    - aqs_qa_annualperformanceevaltransaction_by_state
    - aqs_quarterlysummary_by_county
    - aqs_quarterlysummary_by_state
    - aqs_quarterlysummary_by_site
    - aqs_quarterlysummary_by_cbsa
    - aqs_quarterlysummary_by_box
* aqs_monitors_by_\* functions now include a new optional duration parameter
    to filter results by duration code
* New list function aqs_sampledurations to retrieve a table of valid sample
    durations.
    
### Test environments
* local R installation, Windows 10 (Windows 10 Version 20H2 OS Build 19042.1348),
 R 4.1.1
* local R installation, Gentoo GNU/Linux R 4.1.0
* Windows-latest (Microsoft Windows Server 2019
  10.0.17763) (release) (via Github_actions), R 4.1.2
* macOS-latest (Mac OS X 10.15.7) (release) (via Github_actions), R 4.1.2
* ubuntu-latest (Ubuntu 20.04.3 LTS) (release) (via Github_actions), R 4.1.2
* ubuntu-20.04 (Ubuntu 20.04.3 LTS) (oldrel1) (via Github_actions), R 4.0.5

## RAQSAPI 2.0.1
* This is a new release.

### Reason/Significant changes
* This version fixes issues with aqs_annual_summary_by_box.
* updates and corrects issues in README.
* add CI/CD (github actions) to git repository.
* separate AQSAPI.R into smaller more manageable files.
* devtools::package_coverage() now works
* lifecycle(maturing)
* 4 new functions
    - aqs_transactionsample_by_county
    - aqs_transactionsample_by_MA
    - aqs_transactionsample_by_site
    - aqs_transactionsample_by_state

### Test environments
* local R installation, Windows 10 R 4.0.3
* local R installation, Gentoo GNU/Linux R 4.0.4
* Windows-latest (release) (on Github_actions), R 4.0.4
* macOS-latest (release) (on Github_actions), R 4.0.4
* ubuntu-20.04 (release) (on Github_actions), R 4.0.4
* ubuntu-20.04 (devel) (on Github_actions), R 4.0.4

## remote R CMD Check results (github actions)
* Check runs successfully on all environments.

### Local R CMD check results

0 errors | 50+ warnings | 0 notes

Found the following (possibly) invalid URLs:
  URL: https://aqs.epa.gov/aqsweb/documents/data_api.html>
    From: DESCRIPTION
    Status: 404
    Message: Not Found
  URL: https://github.com/USEPA/RAQSAPI>
    From: DESCRIPTION
    Status: 404
    Message: Not Found
    
The above URIs work as expected, Not sure why this warning appears. Maybe
  because it is a https address?
  
### Reverse dependency check
  Currently no reverse dependencies.
  
### steps to reproduce locally
  source dev/install_RAQSAPI.R then run RAQSAPICHECK() (github)


## RAQSAPI 2.0.0
  * Initial RAQSAPI release on CRAN and github
  
  0 errors | 50 warnings | 1 notes

Found the following (possibly) invalid URLs:
  URL: https://aqs.epa.gov/aqsweb/documents/data_api.html>
    From: DESCRIPTION
    Status: 404
    Message: Not Found
  URL: https://github.com/USEPA/RAQSAPI>
    From: DESCRIPTION
    Status: 404
    Message: Not Found
    
* The above URIs work as expected, Not sure why this note appears. Maybe
  because it is a https address.
  
Warning messages:
1-50+ In parse_Rd("/RAQSAPI/man/.Rd", ... :
C://RAQSAPI/man/.Rd:78:
unknown macro '\lifecycle'
* This is an unsolved issue in either the lifecylce or spelling packages and not
  caused by this package.
  See https://github.com/r-lib/lifecycle/issues/19 for details.
