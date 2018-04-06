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
two_variable_regression <- function(df, y, x1, x2) {
  y <- enexpr(y)
  x1 <- enexpr(x1)
  x2 <- enexpr(x2)

  df %>%
    summarise(
      x1y = sum(!!x1 * !!y, na.rm = TRUE) - (sum(!!x1, na.rm = TRUE) * sum(!!y, na.rm = TRUE) / n()),
      x2y = sum(!!x2 * !!y, na.rm = TRUE) - (sum(!!x2, na.rm = TRUE) * sum(!!y, na.rm = TRUE) / n()),
      x2x = sum(!!x2 * !!x2, na.rm = TRUE) - (sum(!!x2, na.rm = TRUE) * sum(!!x2, na.rm = TRUE) / n()),
      x1x = sum(!!x1 * !!x1, na.rm = TRUE) - (sum(!!x1, na.rm = TRUE) * sum(!!x1, na.rm = TRUE) / n()),
      all = sum(!!x1 * !!x2, na.rm = TRUE) - (sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE) / n()),
      my = mean(!!y, na.rm = TRUE),
      mx1 = mean(!!x1, na.rm = TRUE),
      mx2 = mean(!!x2, na.rm = TRUE)
    ) %>%
    mutate(
      !!x1 := ((x2x * x1y) - (all * x2y)) / ((x1x * x2x) - (all * all)),
      !!x2 := ((x1x * x2y) - (all * x1y)) / ((x1x * x2x) - (all * all))
    ) %>%
    mutate(
      intercept = my - (!!x1 * mx1) - (!!x2 * mx2)
    ) %>%
    select(!!x1, !!x2, intercept) %>%
    collect()
}


#' @rdname two_variable_regression
#' @export
simple_linear_regression <- function(df, x, y) {
  x <- enexpr(x)
  y <- enexpr(y)

  df %>%
    summarise(
      sx = sum(!!x, na.rm = TRUE),
      sy = sum(!!y, na.rm = TRUE),
      sxx = sum(!!x * !!x, na.rm = TRUE),
      syy = sum(!!y * !!y, na.rm = TRUE),
      sxy = sum(!!x * !!y, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(
      !!x := ((n * sxy) - (sx * sy)) / ((n * sxx) - (sx * sx))
    ) %>%
    mutate(
      intercept = ((1 / n) * sy) - (!!x * (1 / n) * sx)
    ) %>%
    select(!!x, intercept) %>%
    collect()
}

#' @rdname two_variable_regression
#' @export
mlr <- function(df, ..., y_var, sample_size = NULL, auto_count = FALSE) {
  y_var <- enexpr(y_var)
  x_vars <- exprs(...)

  if (length(x_vars) == 0) {
    x_vars <- df %>%
      select(-!!y_var) %>%
      colnames() %>%
      syms()
  }

  if (is.null(sample_size) & auto_count) {
    sample_size <- pull(tally(df))
  }

  ind_f <- function(x1, x2, n) {
    x1 <- enexpr(x1)
    x2 <- enexpr(x2)
    expr(sum(!!x1 * !!x2, na.rm = TRUE) - ((sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE)) / !!n))
  }

  all_vars <- c(x_vars, y_var)

  all <- all_vars %>%
    map(~ {
      y <- .x
      all_vars %>%
        map(~ ind_f(!!.x, !!y, sample_size))
    }) %>%
    flatten()

  nm <- all_vars %>%
    map(~ {
      y <- .x
      all_vars %>%
        map(~ paste0(.x, "_", y))
    }) %>%
    flatten()

  all <- set_names(all, nm)

  all_means <- all_vars %>%
    map(~ expr(mean(!!.x, na.rm = TRUE))) %>%
    set_names(~ paste0("mean_", all_vars))

  all <- c(all, all_means)

  ests <- df %>%
    summarise(
      !!!all
    ) %>%
    collect()

  xm <- ests %>%
    select(
      -contains(expr_text(y_var)),
      -contains("mean_")
    ) %>%
    map_dbl(~ .x) %>%
    matrix(nrow = length(x_vars))

  ym <- ests %>%
    select(contains(expr_text(y_var))) %>%
    select(1:length(x_vars)) %>%
    map_dbl(~ .x) %>%
    matrix(nrow = length(x_vars))

  coefs <- as.numeric(solve(xm, ym))

  ic <- seq_len(length(x_vars)) %>%
    map(~ expr((!!coefs[.x] * !!ests[, paste0("mean_", expr_text(x_vars[[.x]]))])))

  intercept <- c(ests[, paste0("mean_", expr_text(y_var))], ic) %>%
    reduce(function(l, r) expr(!!l - !!r)) %>%
    eval()

  if ("tbl_sql" %in% class(df)) intercept <- pull(intercept)

  bind_rows(
    tibble(
      var = x_vars %>% map_chr(~ expr_text(.x)),
      val = coefs
    ),
    tibble(
      var = "Intercept",
      val = intercept
    )
  ) %>%
    spread(var, val)
}
