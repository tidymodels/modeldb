context("kmeans_viz")


test_that("plot_kmeans() returns a ggplot2 object", {
  p <- plot_kmeans(mtcars, mpg, wt, group = am)
  # "ggplot" is for ggplot2 3.5.2 and lower
  # "ggplot2::ggplot" is for ggplot2 4.0.0 and above
  expect_true(inherits(p, c("ggplot", "ggplot2::ggplot")))
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
