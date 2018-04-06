#' Creates dummy variables
#'
#' It uses 'tidyeval' and 'dplyr' to create dummy variables based for
#' categorical variables. 
#'
#' @param df A Local or remote data frame
#' @param x Categorical variable
#' @param values Possible known values of the categorical variable. If not passed
#' then the function will take an additional step to figure the unique values of
#' the variable.
#' @param auto_values Safeguard argument to prevent the function from figuring the
#' unique values if the values argument is empty.  If it is ok for this function
#' to obtain the unique values, set to TRUE.  Defaults to FALSE.
#' @param remove_original It removes the original variable from the returned table.
#' Defaults to TRUE.
#'
#' @examples
#' library(dplyr)
#' 
#' mtcars %>%
#'   add_dummy_variables(cyl, values = c(4, 6, 8))
#' 
#' mtcars %>%
#'   add_dummy_variables(cyl, auto_values = TRUE)
#'
#' @export
add_dummy_variables <- function(df, x, values = c(), 
                                auto_values = FALSE, remove_original = TRUE){
  
  x <- enexpr(x)
  
  var_found <- expr_text(x) %in% colnames(df)
  if(!var_found) stop("Variable not found")
  
  if(length(values) == 0){
    if(auto_values == TRUE){
      values <- df %>% 
        group_by(!! x) %>% 
        summarise() %>% 
        pull()
    } else {
      stop("No values provided and auto_values is set to FALSE")
    }

  }
  
 vals <- values %>%
   map(~expr(ifelse(!! x ==  !! .x, 1 ,0)))
  
 names <- values %>%
   map(~paste0(expr_text(x), "_", .x))
 
 vals <- vals %>%
   set_names(names)
 
 vals <- vals[2:length(vals)]
 
  df <- df %>%
    mutate(!!! vals)
  
  if(remove_original) df <- select(df, - !! x)
  
  df
}




