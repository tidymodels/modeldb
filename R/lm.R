#' Fits a Linear Regression model
#'
#' It uses 'tidyeval' and 'dplyr' to create a linear
#' regression model.
#'
#' @param df A Local or remote data frame
#' @param y_var Dependent variable
#' @param sample_size Prevents a table count. It is only used for models 
#' with three or more independent variables
#' @param auto_count Serves as a safeguard in case sample_size is not
#' passed inadvertently.  Defaults to FALSE.  If it is ok for the
#' function to count how many records are in the sample, then set to
#' TRUE.  It is only used for models with three or more independent variables
#'
#' @details 
#' 
#' The linear_regression_db() function only calls one of three unexported functions. 
#' The function used is determined by the number of independent variables.  This is
#' so any model of one or two variables can use a simplier formula, which in turn
#' will have less SQL overhead.
#'
#' @examples
#' library(dplyr)
#' 
#' mtcars %>%
#'   select(mpg, wt, qsec) %>%
#'   linear_regression_db(mpg)
#'
#' @export
linear_regression_db <- function(df, y_var = NULL, sample_size = NULL, auto_count = FALSE){
  y_var <- enexpr(y_var)
  
  col_names <- colnames(df)
  grouped_count <- length(group_vars(df))
  n_cols <- length(col_names) - grouped_count
  
  x_vars <- col_names[col_names != expr_text(y_var)]
  if(grouped_count > 0) x_vars <- setdiff(x_vars, group_vars(df))
  
  if(n_cols == 2){
    m <- simple_linear_regression_db(
      df = df,
      x = !! sym(x_vars[1]),
      y = !! y_var
    )
  }
  if(n_cols == 3){
    m <- two_variable_regression(
      df = df,
      y = !! y_var,
      x1 = !! sym(x_vars[1]),
      x2 = !! sym(x_vars[2])
    )
  }
  if(n_cols > 3){
    m <- mlr(
      df = df,
      y_var = !! y_var,
      sample_size = sample_size,
      auto_count = auto_count
    )
  }
  m
}

two_variable_regression <- function(df, y, x1, x2) {
  y <- enexpr(y)
  x1 <- enexpr(x1)
  x2 <- enexpr(x2)

  vars <- group_vars(df)
  
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
      Intercept = my - (!!x1 * mx1) - (!!x2 * mx2)
    ) %>%
    select(!! vars, Intercept, !!x1, !!x2) %>%
    collect() %>%
    rename("(Intercept)" = Intercept) %>%
    as_tibble()
}

simple_linear_regression_db <- function(df, x, y) {
  x <- enexpr(x)
  y <- enexpr(y)

  vars <- group_vars(df)
  
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
      Intercept = ((1 / n) * sy) - (!!x * (1 / n) * sx)
    ) %>%
    select(!! vars, Intercept, !!x) %>%
    collect() %>%
    rename("(Intercept)" = Intercept) %>%
    as_tibble()
}

mlr <- function(df, ..., y_var, sample_size = NULL, auto_count = FALSE) {
  y_var <- enexpr(y_var)
  x_vars <- exprs(...)

  vars <- group_vars(df)
  vars_count <- length(vars)
  if(vars_count  == 0) vars <- "         "
  
  
  if (length(x_vars) == 0) {
    x_vars <- colnames(df)
    x_vars <- setdiff(x_vars, expr_text(y_var))
    x_vars <- setdiff(x_vars, vars)
    x_vars <- syms(x_vars)
  }

  if (is.null(sample_size)) {
    if(auto_count){
      sample_size <- pull(tally(df))  
    } else {
      stop("No sample size provided, and auto_count is set to FALSE") 
    }
  }

  ind_f <- function(x1, x2, n) {
    x1 <- enexpr(x1)
    x2 <- enexpr(x2)
    if(vars_count >= 1){
      expr(sum(!!x1 * !!x2, na.rm = TRUE) - ((sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE)) / n()))
    } else {
      expr(sum(!!x1 * !!x2, na.rm = TRUE) - ((sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE)) / !! n))
    }
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

  xm <- seq_len(nrow(ests)) %>% 
    map(~{
      ests[.x,] %>%
        select(
          -contains(expr_text(y_var)),
          -contains("mean_"),
          -contains(vars)
        ) %>%
        map_dbl(~.x) %>%
        matrix(nrow = length(x_vars))
        }) 

  ym <- seq_len(nrow(ests)) %>% 
    map(~{
      ests[.x,] %>%
        select(contains(expr_text(y_var))) %>%
        select(1:length(x_vars)) %>%
        map_dbl(~.x) %>%
        matrix(nrow = length(x_vars))
    }) 

  coefs <- seq_len(nrow(ests)) %>% 
    map(~as.numeric(solve(xm[[.x]], ym[[.x]])))


  ic <- seq_len(nrow(ests)) %>% 
    map(~{
      cr <- .x
      seq_len(length(x_vars)) %>%
        map(~ expr((!!coefs[[cr]][.x] * !!as.numeric(ests[cr, paste0("mean_", expr_text(x_vars[[.x]]))]))))
    })

  Intercept <- seq_len(nrow(ests)) %>% 
    map(~{ c(ests[.x, paste0("mean_", expr_text(y_var))], ic[[.x]]) %>%
        reduce(function(l, r) expr(!!l - !!r)) %>%
        eval()
    })

  coef_table <- transpose(coefs) %>% 
    set_names(x_vars %>% map_chr(~ expr_text(.x))) %>% 
    imap(~tibble(!!.y := as.numeric(!!.x))) %>% 
    bind_cols()
  
  intercept_table <- Intercept %>% 
    as.numeric() %>% 
    tibble("(Intercept)" = .)

  bind_cols(
    if(vars_count >= 1) select(ests, !!! vars),
    intercept_table,
    coef_table
  )
  
}
