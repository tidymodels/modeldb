#' Linear Regression functions
#'
#' It uses 'tidyeval' and 'dplyr' to to apply the a simple 
#' linear regression formula with one or two independent
#' variables
#'
#' @param df A Local or remote data frame
#' @param y Dependent variable
#' @param x1 First independent variable
#' @param x2 Second independent variable
#' @param output The format of the output. Defaults to a 'tidypredict' parsed model
#'
#' @details
#' It currently only works with continous dependent variables,
#' no support for categorical variables yet.
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>% 
#'   two_variable_regression(mpg, wt, qsec)
#'  
#' mtcars %>% 
#'   simple_linear_regression(mpg, wt)
#'
#' @export
two_variable_regression <- function(df, y, x1, x2, output = "parsedmodel") {
  y <- enexpr(y)
  x1 <- enexpr(x1)
  x2 <- enexpr(x2)

  model <- df %>%
    summarise(
      x1y = sum(!! x1 * !! y) - (sum(!! x1) * sum(!! y) / n()),
      x2y = sum(!! x2 * !! y) - (sum(!! x2) * sum(!! y) / n()),
      x2x = sum(!! x2 * !! x2) - (sum(!! x2) * sum(!! x2) / n()),
      x1x = sum(!! x1 * !! x1) - (sum(!! x1) * sum(!! x1) / n()),
      all = sum(!! x1 * !! x2) - (sum(!! x1) * sum(!! x2) / n()),
      my = mean(!! y),
      mx1 = mean(!! x1),
      mx2 = mean(!! x2)
    ) %>%
    mutate(
      b1 = ((x2x * x1y) - (all * x2y)) / ((x1x * x2x) - (all * all)),
      b2 = ((x1x * x2y) - (all * x1y)) / ((x1x * x2x) - (all * all))
    ) %>%
    mutate(
      intercept = my - (b1 * mx1) - (b2 * mx2)
    ) %>%
    select(
      b1, b2, intercept
    )

  if (is.null(output)) output <- ""

  if (output == "parsedmodel") {
    model <- tibble(
      labels   = c("(Intercept)",   expr_text(x1), expr_text(x2), "labels",       "model"),
      estimate = c(model$intercept, model$b1,      model$b2,       0,              NA),
      type     = c("term",          "term",        "term",        "variable",     "variable"),
      field_1  = c(NA,              "{{:}}",       NA,            expr_text(x1),  NA),
      field_2  = c(NA,              NA,            "{{:}}",       expr_text(x2),  NA),
      vals     = c(NA,              NA,             NA,           NA,             "lm")
    )
  }
  model
}


#' @rdname two_variable_regression
#' @export
simple_linear_regression <- function(df, x, y, output = "parsedmodel"){
  
  x <- enexpr(x)
  y <- enexpr(y)
  
  model <- df %>%
    summarise(
      sx = sum(!!x),
      sy = sum(!!y),
      sxx = sum(!!x * !!x),
      syy = sum(!!y * !!y),
      sxy = sum(!!x * !!y),
      n = n()
    ) %>%
    mutate(
      coef = ((n * sxy) - (sx * sy)) / ((n * sxx) - (sx * sx))
    ) %>%
    mutate(
      intercept = ((1 / n) * sy) - (coef * (1/n) * sx)
    ) %>%
    select(coef, intercept) %>%
    collect()
  
  if(is.null(output)) output= ""
  
  if(output == "parsedmodel"){
    model <- tibble(
      labels = c("(Intercept)", expr_text(x), "labels", "model"),
      estimate = c(model$intercept, model$coef, 0, NA),
      type = c("term", "term", "variable", "variable"),
      field_1 = c(NA, "{{:}}", expr_text(x), NA),
      vals = c(NA, NA, NA, "lm")
    )
  }
  model
}
