#' @importFrom tidypredict as_parsed_model
#' @export
tidypredict::as_parsed_model

#' Prepares parsed model object
#' 
#' @param x A parsed model object
#' 
#' @export
as_parsed_model.modeldb_lm <- function(x) {
  terms<- imap(
    x,
    ~{
      list(
        label = .y,
        coef = .x,
        is_intercept = ifelse(.y == "(Intercept)", 1, 0),
        fields = list(list(type = "ordinary", col = .y))
      )}
  )
  pm <- list(
    general = list(
      model = "modeldb_lm",
      version = 2,
      type = "regression",
      is_glm = 0
    ),
    terms = terms
  )
  as_parsed_model(pm)
}