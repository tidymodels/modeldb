[![Build Status](https://travis-ci.org/edgararuiz/modeldb.svg?branch=master)](https://travis-ci.org/edgararuiz/modeldb) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/modeldb)](http://cran.r-project.org/package=modeldb) [![Coverage Status](https://img.shields.io/codecov/c/github/edgararuiz/modeldb/master.svg)](https://codecov.io/github/edgararuiz/modeldb?branch=master)

modeldb
================

-   [Installation](#installation)
-   [Linear regression](#linear-regression)
-   [K Means clustering](#k-means-clustering)
-   [Supported models](#supported-models)

Fit models inside the database. **`modeldb` works with several databases back-ends** because it leverages `dplyr` and `dbplyr` for the final SQL translation of the algorithm. It currently supports:

-   K-means clustering

-   Linear regression

Installation
------------

Install the development version using `devtools` as follows:

``` r
devtools::install_github("edgararuiz/modeldb")
```

Linear regression
-----------------

An easy way to try out the package is by creating a temporary SQLite database, and loading `mtcars` to it

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
RSQLite::initExtension(con)
dplyr::copy_to(con, mtcars)
```

``` r
library(dplyr)

tbl(con, "mtcars") %>%
  select(wt, mpg, qsec) %>%
  linear_regression(wt)
```

    ## # A tibble: 1 x 3
    ##      mpg  qsec Intercept
    ##    <dbl> <dbl>     <dbl>
    ## 1 -0.156 0.125      4.12

The model output can be parsed by `tidypredict` to run the predictions in the database. Please see the `Linear Regression` article to learn more about how to use `linear_regression()`

K Means clustering
------------------

To use the `simple_kmeans()` function, simply pipe the database back end table to the function. This returns a list object that contains two items:

-   A sql query table with the final center assignment
-   A local table with the information about the centers

``` r
km <- tbl(con, "mtcars") %>%
  simple_kmeans(mpg, wt)

km$centers
```

    ## # A tibble: 3 x 2
    ##     mpg    wt
    ##   <dbl> <dbl>
    ## 1  20.6  3.07
    ## 2  14.5  4.06
    ## 3  30.1  1.87

To preview the record level center assignment use the `tbl` item

``` r
head(km$tbl)
```

    ## # Source:   lazy query [?? x 3]
    ## # Database: sqlite 3.22.0 []
    ##     mpg    wt center  
    ##   <dbl> <dbl> <chr>   
    ## 1  21.0  2.62 center_1
    ## 2  21.0  2.88 center_1
    ## 3  22.8  2.32 center_1
    ## 4  21.4  3.22 center_1
    ## 5  18.7  3.44 center_1
    ## 6  18.1  3.46 center_1

The SQL statement from `tbl` can be extracted using `dbplyr`'s `remote_query()`

``` r
dbplyr::remote_query(km$tbl)
```

    ## <SQL> SELECT `mpg`, `wt`, `center`
    ## FROM (SELECT `mpg`, `wt`, `center_1`, `center_2`, `center_3`, CASE
    ## WHEN (`center_1` >= `center_1` AND `center_1` < `center_2` AND `center_1` < `center_3`) THEN ('center_1')
    ## WHEN (`center_2` < `center_1` AND `center_2` >= `center_2` AND `center_2` < `center_3`) THEN ('center_2')
    ## WHEN (`center_3` < `center_1` AND `center_3` < `center_2` AND `center_3` >= `center_3`) THEN ('center_3')
    ## END AS `center`
    ## FROM (SELECT `mpg`, `wt`, SQRT(((20.6428571428571 - `mpg`) * (20.6428571428571 - `mpg`)) + ((3.07214285714286 - `wt`) * (3.07214285714286 - `wt`))) AS `center_1`, SQRT(((14.4583333333333 - `mpg`) * (14.4583333333333 - `mpg`)) + ((4.05866666666667 - `wt`) * (4.05866666666667 - `wt`))) AS `center_2`, SQRT(((30.0666666666667 - `mpg`) * (30.0666666666667 - `mpg`)) + ((1.873 - `wt`) * (1.873 - `wt`))) AS `center_3`
    ## FROM (SELECT *
    ## FROM (SELECT `mpg`, `wt`
    ## FROM `mtcars`)
    ## WHERE (NOT(((`mpg`) IS NULL)) AND NOT(((`wt`) IS NULL))))))
    ## WHERE (NOT(((`center`) IS NULL)))

More information can be found in the `KMeans Clustering` article.

Supported models
----------------

The following R models are currently supported. For more info please review the corresponding vignette:

-   [Linear Regression](http://modeldb.netlify.com/articles/linear_regression/) - `linear_regression()`
-   [K-means clustering](http://modeldb.netlify.com/articles/kmeans/) - `simple_kmenas()`
