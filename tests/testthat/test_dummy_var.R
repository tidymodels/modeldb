test_that("Function create the correct columns", {
  cols_expected <- c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl_6", "cyl_8")
  expect_equal(
    colnames(add_dummy_variables(mtcars, cyl, c(4, 6, 8))),
    cols_expected
  )
  expect_equal(
    colnames(add_dummy_variables(mtcars, cyl, auto_values = TRUE)),
    cols_expected
  )
})

test_that("Function fails when no values are passed and auto_values is FALSE", {
  expect_error(add_dummy_variables(mtcars, cyl))
  expect_error(add_dummy_variables(mtcars, cyl, auto_values = FALSE))
})

test_that("Has error when variable is missing", {
  expect_error(add_dummy_variables(mtcars, error))
})
