#' @export
simple_kmeans <- function(df,..., centers = 3, max_repeats = 100){
  
  vars <- exprs(...)
  
  df <- df %>%
    select(!!! vars)
  
  centroids <- df %>%
    head(centers) %>%
    collect()
  
  pb <- progress::progress_bar$new(total = max_repeats)
  for(iteration in 1:max_repeats){
    pb$tick()
    prev_centroids <- centroids
    
    new_centroids <- calculate_centers(df, centroids, centers)
    
    centroids <- new_centroids  %>%
      group_by(center) %>%
      summarise_all("mean", na.rm = TRUE) %>%
      select(-center) %>%
      collect()
    
    if(all(prev_centroids == centroids)) break()
    
  }
  list(
    tbl = new_centroids,
    centers = centroids
  )
}

calculate_centers <- function(df, center_df, centers){
  center_names <- paste0("center_", 1:centers)
  fields <- length(colnames(df))
  
  f_dist <- center_df %>%
    imap(~{
      map2(.x, .y, function(x,y)expr((!!x) - (!!sym(y))))
    }) 
  
  f_inside <- function(center){
    1:fields %>%
      map(~{
        f <- pluck(f_dist, .x, center) 
        expr(((!!f) * (!!f)))
      }) %>%
      reduce(function(l,r) expr((!!l ) + (!! r)))
  }
  
  km <- 1:centers %>%
    map(~expr(sqrt(!! f_inside(.x)))) %>%
    set_names(center_names)
  
  all <- center_names %>%
    map(~{
      comp <- map2(.x, center_names, function(x,y) 
        if(x !=y){ expr((!! sym(x)) <  (!!sym(y)))} else {expr((!!sym(x)) >= (!!sym(y)))
        }) %>%
        reduce(function(l,r) expr((!!l) & (!!r))) 
      c(comp, .x) %>%
        reduce(function(l,r) expr((!!l) ~ !!(r)))
    }) %>%
    flatten() 
  
  comp <- expr(case_when(!!! all))
  
  df %>%
    mutate(!!! km)  %>%
    mutate(center = !! comp) %>%
    select(-contains("center_")) 
}

