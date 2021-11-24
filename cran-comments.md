---
title: "cran-comments"
author: "Clinton Mccrowey"
date: "3/29/2021"
output: md_document
---

# cran-comments for RAQSAPI

## RAQSAPI 2.0.2
* This is a new release

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
* New list function aqs_sample_duration to retrieve a table of valid sample
    durations.
    
### Test environments
* local R installation, Windows 10 (Windows 10 Version 20H2 OS Build 19042.1348),
 R 4.1.1
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
