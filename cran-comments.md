## Test environments
* local windows 10 install, R 3.5.1
* ubuntu 14.04 (on travis-ci)

## R CMD check results
* 0 errors | 0 warnings | 0 notes

## Release summary

- Fixes dependency issue with dev version of `tidypredict` by removing `as_parsed_model()`.  The function will be moved to `tidypredict` in its next version.

- Fixes warning given by CRAN check
