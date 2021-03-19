# modeldb (development version)

* Re-licensed package from GPL-3 to MIT. All copyright holders are RStudio employees and give consent.

# modeldb 0.2.2

- Switches maintainer to Max Kuhn

# modeldb 0.2.1

- Uses `dplyr::tbl_vars()` for column name extraction to instead of `colnames()`

# modeldb 0.2.0

- Adds `as_parsed_model()` support for regression models

- Fixes compatability issues with `rlang` and `dplyr` 

# modeldb 0.1.2

- Removes pipes and other dplyr dependencies from internal `mlr()` function

- Consolidates duplicated database operations in `mlr()`

- Fixes an issue in `simple_kmeans_db()` when specifying variables

# modeldb 0.1.1

## Bug fixes

- Fixes dependency issue with `tidypredict` by removing `as_parsed_model()`.  The function will be moved to `tidypredict` in its next version.
