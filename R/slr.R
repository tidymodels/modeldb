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
