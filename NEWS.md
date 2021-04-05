# RAQSAPI 2.0.1 (2021-03-30)

* add reverse dependency check, add cran-comments.Rmd, update README, prepare to
merge GitHub_action into main, update WORDLIST, add codecov badge
*correct the number of functional families and available services in the README,
addition of words to WORDLIST file.
*removed calls to library, devtools::unload and testthat::context from test
files that were causing build warnings and errors
*Fixed browser call left in aqs_annual_summary_by_box
*New Branch for GitHub actions, changed the lifecycle status of aqs_sign_up and
aqs_credentials functions from "experimental" to "stable", removed keyring from
suggests added GitHub_actions CI. added GitHub badge to README.
*first public release candidate


# RAQSAPI 2.0.0 (2021-02-16)

- First public release.
