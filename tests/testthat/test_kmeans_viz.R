context("kmeans_viz")


test_that("plot_kmeans() returns a ggplot2 object", {
  expect_equal(
    class(plot_kmeans(mtcars, mpg, wt, group = am)),
    c("gg", "ggplot")
  )
})

test_that("plot_kmeans() returns error when no group is passed", {
  expect_error(
    plot_kmeans(mtcars, mpg, wt)
  )
})

test_that("Updating the resolution argument impacts the results", {
  expect_false(
    nrow(db_calculate_squares(mtcars,
      mpg, wt,
      group = am,
      resolution = 50
    )) ==
      nrow(db_calculate_squares(mtcars,
        mpg, wt,
        group = am,
        resolution = 30
      ))
  )
})
