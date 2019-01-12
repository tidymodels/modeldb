context("kmeans")

test_that("Specifying variables works", {
  expect_silent(
    simple_kmeans_db(mtcars, wt, mpg)
  )
})

test_that("Not specifying variables works", {
  expect_silent(
    select(mtcars, wt, mpg) %>%
      simple_kmeans_db()
  )
})

test_that("Centroid argument is accepted", {
  ik <- data.frame(
    wt = c(3.072143, 4.058667, 1.873000),
    mpg = c(20.64286, 14.45833, 30.06667)
  )
  
  expect_silent(
    simple_kmeans_db(mtcars, mpg, wt, initial_kmeans = ik)  
  )
})



