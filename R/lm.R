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
  if (is.null(sample_size)) {
    if (auto_count) {
      sample_size <- pull(tally(df))
    } else {
      stop("No sample size provided, and auto_count is set to FALSE")
    }
  }

  y_var <- enquo(y_var)
  y_text <- as_label(y_var)

  grouping_vars <- group_vars(df)
  vars_count <- length(grouping_vars)

  x_vars <- colnames(df)
  x_vars <- x_vars[x_vars != y_text]
  if (vars_count > 0) x_vars <- map(grouping_vars, ~ x_vars[x_vars != .x])[[1]]
  x_vars <- syms(x_vars)

  all_vars <- c(x_vars, ensym(y_var))

  all_f_mapped <- map(
    all_vars, ~ {
      y <- .x
      map(
        all_vars, ~ {
          xy <- c(as_label(.x), as_label(y))
          list(
            f = ind_f(!!.x, !!y, sample_size, vars_count),
            name = paste0(xy[order(xy)], collapse = "_")
          )
        }
      )
    }
  )
  all_f <- flatten(all_f_mapped)
  all_f <- set_names(
    map(all_f, ~ .x$f),
    map(all_f, ~ .x$name)
  )

  # Deduping field combos, decreases number of calcs inside DB
  unique_f <- map(
    unique(names(all_f)),
    ~ all_f[names(all_f) == .x][[1]]
  )
  unique_f <- set_names(unique_f, unique(names(all_f)))

  all_means <- map(all_vars, ~ expr(mean(!!.x, na.rm = TRUE)))
  all_means <- set_names(all_means, ~ paste0("mean_", all_vars))

  all_fm <- c(unique_f, all_means)

  # Send all operations to the DB simultaneously
  ests_df <- summarise(df, !!!all_fm)
  ests_df <- collect(ests_df)

  ests_list <- as_list(ests_df)

  xm_names <- names(all_f)[!grepl(y_text, names(all_f))]
  xm <- map(xm_names, ~ ests_list[.x])
  xm <- flatten(xm)
  xm <- transpose(xm)
  xm <- map(xm, ~ matrix(as.numeric(.x), nrow = length(x_vars)))

  ym_names <- names(all_f)[grepl(y_text, names(all_f))]
  ym_names <- unique(ym_names)[1:length(x_vars)]
  ym <- map(ym_names, ~ ests_list[.x])
  ym <- flatten(ym)
  ym <- transpose(ym)
  ym <- map(ym, ~ matrix(as.numeric(.x), nrow = length(x_vars)))

  coefs <- map(
    seq_len(vars_count + 1),
    ~ as.numeric(solve(xm[[.x]], ym[[.x]]))
  )

  intercept <- map(
    seq_len(vars_count + 1), ~ {
      y <- .x
      x_f <- map(
        seq_len(length(x_vars)), ~ {
          x_name <- paste0("mean_", x_vars[.x])
          x_mean <- ests_list[names(ests_list) == x_name][[1]][y]
          expr((!!coefs[[y]][.x] * !!x_mean))
        }
      )
      y_name <- paste0("mean_", y_text)
      y_mean <- ests_list[names(ests_list) == y_name][[1]][y]
      int_f <- reduce(
        c(y_mean, x_f),
        function(l, r) expr(!!l - !!r)
      )
      eval(int_f)
    }
  )

  res <- transpose(coefs)
  res <- set_names(res, x_vars)
  res <- c(list("(Intercept)" = intercept), res)
  res <- map_df(transpose(res), ~.x)
  bind_cols(ests_df[, grouping_vars], res)
}
ind_f <- function(x1, x2, n, vars_count) {
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  if (vars_count >= 1) {
    expr(sum(!!x1 * !!x2, na.rm = TRUE) - ((sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE)) / n()))
  } else {
    expr(sum(!!x1 * !!x2, na.rm = TRUE) - ((sum(!!x1, na.rm = TRUE) * sum(!!x2, na.rm = TRUE)) / !!n))
  }
}