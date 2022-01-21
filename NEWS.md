% News.md for RAQSAPI
% Clinton Mccrowey Physical Scientist  
 EPA Region III  
 Air and Radiation Division  
 Air Quality Analysis Branch
 
 
#RAQSAPI 2.0.2 (2021-11-29)

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

# RAQSAPI 2.0.1 (2021-03-30)

* add reverse dependency check, add cran-comments.Rmd, update README, prepare to
merge GitHub_action into main, update WORDLIST, add codecov badge
* correct the number of functional families and available services in the README,
addition of words to WORDLIST file.
* removed calls to library, devtools::unload and testthat::context from test
files that were causing build warnings and errors
* Fixed browser call left in aqs_annual_summary_by_box
* New Branch for GitHub actions, changed the lifecycle status of aqs_sign_up and
aqs_credentials functions from "experimental" to "stable", removed keyring from
suggests added GitHub_actions CI. added GitHub badge to README.
* first public release candidate


# RAQSAPI 2.0.0 (2021-02-16)

* First public release.
