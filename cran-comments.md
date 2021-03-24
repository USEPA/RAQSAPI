## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

More than 50 notes are printed in respect to unknown lifecycle macro. (one for
 each exported function)
# Warning messages:
# 1: In parse_Rd("<path to library>/RAQSAPI/man/<functionname>.Rd",  ... :
#   C:/<path to library>/RAQSAPI/man/<functionname>.Rd:78:
#      unknown macro '\lifecycle'
#   
# This is an unsolved issue in either the lifecylce or spelling packages and not
# caused by this package.
# See https://github.com/r-lib/lifecycle/issues/19 for details.
