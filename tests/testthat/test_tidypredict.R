context("as_parsed_model() function")

test_that("Returns a tibble with the right dimensions", {
  pm <- mtcars %>%
    select(mpg, wt) %>%
    linear_regression_db(mpg) %>%
    as_parsed_model()
  
  expect_equal(class(pm), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(pm), 4)
  expect_equal(colnames(pm),
               c("labels", "estimate", "type","vals", "field_1")) 
  
  expect_equal(
    as.character(add_term(data.frame(), "Intercept", 0)[1, ]),
    c("(Intercept)","0", "term", "NA")
  )
})
