test_that("Simple linear regression matches lm()", {
  expect_equal(
    lm(wt ~ mpg, data = mtcars) %>%
      coef() %>%
      as.numeric(),
    mtcars %>%
      select(wt, mpg) %>%
      linear_regression_db(wt) %>%
      as.numeric()
  )
})

test_that("Two variable linear regression matches lm()", {
  expect_equal(
    lm(wt ~ mpg + qsec, data = mtcars) %>%
      coef() %>%
      as.numeric(),
    mtcars %>%
      select(wt, mpg, qsec) %>%
      linear_regression_db(wt) %>%
      as.numeric()
  )
})

test_that("Multiple variable linear regression matches lm()", {
  expect_equal(
    lm(wt ~ mpg + qsec + hp, data = mtcars) %>%
      coef() %>%
      as.numeric(),
    mtcars %>%
      select(wt, mpg, qsec, hp) %>%
      linear_regression_db(wt, sample_size = 32) %>%
      as.numeric()
  )
})

test_that("MLR matches lm() with auto_count set to TRUE", {
  expect_equal(
    lm(wt ~ mpg + qsec + hp, data = mtcars) %>%
      coef() %>%
      as.numeric(),
    mtcars %>%
      select(wt, mpg, qsec, hp) %>%
      linear_regression_db(wt, auto_count = TRUE) %>%
      as.numeric()
  )
})

test_that("MLR failes when auto_count set to FALSE and no sample_size is passed", {
  expect_error(
    mtcars %>%
      select(wt, mpg, qsec, hp) %>%
      linear_regression_db(wt, auto_count = FALSE)
  )
  expect_error(
    mtcars %>%
      select(wt, mpg, qsec, hp) %>%
      linear_regression_db(wt)
  )
})

test_that("mlr with grouping matches lm()", {
  expect_equal(
    mtcars %>%
      select(wt, mpg, qsec, hp, am) %>%
      group_by(am) %>%
      linear_regression_db(wt, auto_count = TRUE) %>%
      transpose() %>%
      map(~ as.numeric(.x)),
    0:1 %>%
      map(~ {
        mtcars %>%
          filter(am == .x) %>%
          lm(wt ~ mpg + qsec + hp, data = .) %>%
          coef() %>%
          as.numeric() %>%
          c(.x, .)
      })
  )
})
