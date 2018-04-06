start_parse <- function(){
  tibble(
    labels = "labels",
    estimate = 0,
    type = "variable"
  )
}

add_term <- function(df, names, estimate){
  
  if(names == "Intercept") names <- "(Intercept)"
  
  df <- df %>%
    bind_rows(
      tibble(
        labels = !! names,
        estimate = !! estimate,
        type = "variable"
      )
    )
  
  if(names != "(Intercept)") {
    curr_field <- ncol(df)  - 2
    field <- c(rep(NA, nrow(df) - 1), get_marker())
    
    new_field <- sym(paste0("field_", curr_field))
    df <- df %>%
      mutate(!! new_field := field)
    
    df <- df %>%
      mutate(!! new_field := ifelse(labels == "labels", names, NA))
  }
  
  df
}

get_marker <- function() "{:}"

get_marker_regx <- function() "\\{\\:\\}"

#' @export
tidypredict_parser <- function(df){
  td <- start_parse()
  for(i in seq_len(nrow(df))) {
    td <- td %>%
      add_term(
        df$var[i],
        df$val[i]
      )
  }
  
  td
}
