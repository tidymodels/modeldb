context("parsed model")

test_that("Simple linear regression matches lm()", {
  expect_is(
    as_parsed_model(
      linear_regression_db(mtcars, wt, sample_size = 32)
      ),
    "parsed_model"
  )
})
