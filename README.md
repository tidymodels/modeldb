
# modeldb <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![R-CMD-check](https://github.com/tidymodels/modeldb/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/modeldb/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/modeldb)](https://CRAN.R-project.org/package=modeldb)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/modeldb/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/modeldb?branch=main)
[![Downloads](http://cranlogs.r-pkg.org/badges/modeldb)](https://CRAN.R-project.org/package=modeldb)

Fit models inside the database! **modeldb works with most database
back-ends** because it leverages [dplyr](https://dplyr.tidyverse.org/)
and [dbplyr](https://dbplyr.tidyverse.org/) for the final SQL
translation of the algorithm. It currently supports:

- K-means clustering

- Linear regression

## Installation

Install the CRAN version with:

``` r
install.packages("modeldb")
```

The development version is available from GitHub using remotes:

``` r
# install.packages("remotes")
remotes::install_github("tidymodels/modeldb")
```

## Linear regression

An easy way to try out the package is by creating a temporary SQLite
database, and loading `mtcars` to it.

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
RSQLite::initExtension(con)
dplyr::copy_to(con, mtcars)
```

``` r
library(dplyr)

tbl(con, "mtcars") %>%
  select(wt, mpg, qsec) %>%
  linear_regression_db(wt)
```

    ## # A tibble: 1 × 3
    ##   `(Intercept)`    mpg  qsec
    ##           <dbl>  <dbl> <dbl>
    ## 1          4.12 -0.156 0.125

The model output can be parsed by
[tidypredict](https://tidypredict.tidymodels.org/) to run the
predictions in the database. Please see the “Linear Regression” article
to learn more about how to use `linear_regression_db()`

## K Means clustering

To use the `simple_kmeans_db()` function, simply pipe the database back
end table to the function. This returns a list object that contains two
items:

- A sql query table with the final center assignment
- A local table with the information about the centers

``` r
km <- tbl(con, "mtcars") %>%
  simple_kmeans_db(mpg, wt)

colnames(km)
```

    ##  [1] "k_center" "k_mpg"    "k_wt"     "mpg"      "cyl"      "disp"    
    ##  [7] "hp"       "drat"     "wt"       "qsec"     "vs"       "am"      
    ## [13] "gear"     "carb"

The SQL statement from `tbl` can be extracted using dbplyr’s
`remote_query()`

``` r
dbplyr::remote_query(km)
```

    ## <SQL> SELECT
    ##   `LHS`.`k_center` AS `k_center`,
    ##   `k_mpg`,
    ##   `k_wt`,
    ##   `mpg`,
    ##   `cyl`,
    ##   `disp`,
    ##   `hp`,
    ##   `drat`,
    ##   `wt`,
    ##   `qsec`,
    ##   `vs`,
    ##   `am`,
    ##   `gear`,
    ##   `carb`
    ## FROM (
    ##   SELECT
    ##     `mpg`,
    ##     `cyl`,
    ##     `disp`,
    ##     `hp`,
    ##     `drat`,
    ##     `wt`,
    ##     `qsec`,
    ##     `vs`,
    ##     `am`,
    ##     `gear`,
    ##     `carb`,
    ##     `center` AS `k_center`
    ##   FROM (
    ##     SELECT
    ##       `q01`.*,
    ##       CASE
    ## WHEN (`center_1` >= `center_1` AND `center_1` < `center_2` AND `center_1` < `center_3`) THEN 'center_1'
    ## WHEN (`center_2` < `center_1` AND `center_2` >= `center_2` AND `center_2` < `center_3`) THEN 'center_2'
    ## WHEN (`center_3` < `center_1` AND `center_3` < `center_2` AND `center_3` >= `center_3`) THEN 'center_3'
    ## END AS `center`
    ##     FROM (
    ##       SELECT
    ##         `mtcars`.*,
    ##         SQRT(((20.6428571428571 - `mpg`) * (20.6428571428571 - `mpg`)) + ((3.07214285714286 - `wt`) * (3.07214285714286 - `wt`))) AS `center_1`,
    ##         SQRT(((14.4583333333333 - `mpg`) * (14.4583333333333 - `mpg`)) + ((4.05866666666667 - `wt`) * (4.05866666666667 - `wt`))) AS `center_2`,
    ##         SQRT(((30.0666666666667 - `mpg`) * (30.0666666666667 - `mpg`)) + ((1.873 - `wt`) * (1.873 - `wt`))) AS `center_3`
    ##       FROM `mtcars`
    ##     ) AS `q01`
    ##   ) AS `q01`
    ##   WHERE (NOT((`center` IS NULL)))
    ## ) AS `LHS`
    ## LEFT JOIN (
    ##   SELECT `center` AS `k_center`, AVG(`mpg`) AS `k_mpg`, AVG(`wt`) AS `k_wt`
    ##   FROM (
    ##     SELECT `mpg`, `wt`, `center`
    ##     FROM (
    ##       SELECT
    ##         `q01`.*,
    ##         CASE
    ## WHEN (`center_1` >= `center_1` AND `center_1` < `center_2` AND `center_1` < `center_3`) THEN 'center_1'
    ## WHEN (`center_2` < `center_1` AND `center_2` >= `center_2` AND `center_2` < `center_3`) THEN 'center_2'
    ## WHEN (`center_3` < `center_1` AND `center_3` < `center_2` AND `center_3` >= `center_3`) THEN 'center_3'
    ## END AS `center`
    ##       FROM (
    ##         SELECT
    ##           `mtcars`.*,
    ##           SQRT(((20.6428571428571 - `mpg`) * (20.6428571428571 - `mpg`)) + ((3.07214285714286 - `wt`) * (3.07214285714286 - `wt`))) AS `center_1`,
    ##           SQRT(((14.4583333333333 - `mpg`) * (14.4583333333333 - `mpg`)) + ((4.05866666666667 - `wt`) * (4.05866666666667 - `wt`))) AS `center_2`,
    ##           SQRT(((30.0666666666667 - `mpg`) * (30.0666666666667 - `mpg`)) + ((1.873 - `wt`) * (1.873 - `wt`))) AS `center_3`
    ##         FROM `mtcars`
    ##       ) AS `q01`
    ##     ) AS `q01`
    ##     WHERE (NOT((`center` IS NULL)))
    ##   ) AS `q01`
    ##   GROUP BY `center`
    ## ) AS `RHS`
    ##   ON (`LHS`.`k_center` = `RHS`.`k_center`)

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/modeldb/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code. Check out [this helpful article on how to create
  reprexes](https://dbplyr.tidyverse.org/articles/reprex.html) for
  problems involving a database.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
