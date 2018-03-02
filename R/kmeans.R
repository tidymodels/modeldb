#' @export
simple_kmeans <- function(df,..., centers = 3, max_repeats = 100, 
                          initial_kmeans = NULL, safeguard = TRUE){
  
  vars <- exprs(...)
  
  
  
  df <- df %>%
    select(!!! vars) %>%
    filter_all(all_vars(!is.na(.)))
  
  if(!is.null(initial_kmeans)){
    centroids <- initial_kmeans
  } else {
    centroids <- df %>%
      head(centers) %>%
      collect()
  }
  
  pb <- progress::progress_bar$new(
    format = paste0(" Cycle :current of " , max_repeats, " max. [:bar] [:var][:elapsed]"),
    total = max_repeats, clear = TRUE, width= 80)
  
  for(iteration in 1:max_repeats){
    
    
    prev_centroids <- centroids
    
    new_centroids <- calculate_centers(df, centroids, centers)
    
    centroids <- new_centroids  %>%
      group_by(center) %>%
      summarise_all("mean", na.rm = TRUE) %>%
      select(-center) %>%
      collect()
    
    if(safeguard) current_kmeans <<- centroids
    
    variance <- (round(abs(sum(prev_centroids) - sum(centroids)) / sum(prev_centroids), digits = 4)*100)
    
    pb$tick(tokens = list(var = variance))
    
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
    filter(!is.na(center)) %>%
    select(-contains("center_")) 
}

