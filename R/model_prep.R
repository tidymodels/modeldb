#' @export
add_dummy_variables <- function(df, x, values = c(), 
                                auto_values = FALSE, remove_original = TRUE){
  
  x <- enexpr(x)
  
  var_found <- expr_text(x) %in% colnames(df)
  if(!var_found) stop("Variable not found")
  
  if(length(values) == 0 & auto_values == TRUE){
    values <- df %>% 
      group_by(!! x) %>% 
      summarise() %>% 
      pull()
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




