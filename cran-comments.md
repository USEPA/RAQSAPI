---
title: "cran-comments"
author: "Clinton Mccrowey"
date: "3/29/2021"
output: md_document
---

# cran-comments for RAQSAPI

## RAQSAPI 2.0.1
* This is a new release.

### Reason/Significant changes
* This version fixes issues with aqs_annual_summary_by_box.
* updates and corrects issues in README.
* add CI/CD (github actions) to git repository.
* separate AQSAPI.R into smaller more manageable files.
* devtools::package_coverage() now works
* lifecycle(maturing)

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
