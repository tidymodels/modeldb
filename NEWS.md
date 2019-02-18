# modeldb 0.1.1.9000

- Removes pipes and other dplyr dependencies from internal `mlr()` function

- Consolidates duplicated database operations in `mlr()`

# modeldb 0.1.1

## Bug fixes

- Fixes dependency issue with `tidypredict` by removing `as_parsed_model()`.  The function will be moved to `tidypredict` in its next version.
