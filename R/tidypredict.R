#' Create a parsed_model table
#'
#' Converts the output of linear_regression() to tidypredict readable output
#'
#' @param df A Local or remote data frame
#' @param model_type the value passed to the model type. Defaults to 'lm'
#'
#' @examples
#' library(dplyr)
#' 
#' mtcars %>%
#'   select(mpg, wt) %>%
#'   linear_regression_db(mpg) %>%
#'   as_parsed_model()
#'
#' @export
as_parsed_model <- function(df, model_type = "lm") {
  df <- tidyr::gather(df, var, val)
  td <- start_parse(model_type = model_type)
  for (i in seq_len(nrow(df))) {
    td <- td %>%
      add_term(
        df$var[i],
        df$val[i]
      )
  }
  td
}

start_parse <- function(model_type = "lm") {
  bind_rows(
    tibble(
      labels = "labels",
      estimate = 0,
      type = "variable",
      vals = NA
    ),
    tibble(
      labels = "model",
      estimate = NA,
      type = "variable",
      vals = model_type
    )
  )
}

add_term <- function(df, names, estimate) {
  if (names == "Intercept") names <- "(Intercept)"
  df <- df %>%
    bind_rows(
      tibble(
        labels = !!names,
        estimate = !!estimate,
        type = "term",
        vals = NA
      )
    )

  if (names != "(Intercept)") {
    curr_field <- ncol(df) - 3
    field <- c(names, rep(NA, nrow(df) - 2), get_marker())
    new_field <- sym(paste0("field_", curr_field))
    df <- mutate(df, !!new_field := field)
  }
  df
}

get_marker <- function() "{{:}}"


