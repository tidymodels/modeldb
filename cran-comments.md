## Test environments
* local windows 10 install, R 3.5.2
* ubuntu 14.04 (on travis-ci)

## R CMD check results
* 0 errors | 0 warnings | 0 notes

## Release summary

- Removes pipes and other dplyr dependencies from internal `mlr()` function

- Consolidates duplicated database operations in `mlr()`

- Fixes an issue in `simple_kmeans_db()` when specifying variables

