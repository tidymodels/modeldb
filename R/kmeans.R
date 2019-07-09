#' Simple kmeans routine that works in-database
#'
#' It uses 'tidyeval' and 'dplyr' to run multiple cycles of kmean
#' calculations, expressed in dplyr formulas until an the optimal
#' centers are found.
#'
#' @param df A Local or remote data frame
#' @param ... A list of variables to be used in the kmeans algorithm
#' @param centers The number of centers. Defaults to 3.
#' @param max_repeats The maximum number of cycles to run. Defaults to 100.
#' @param initial_kmeans A local dataframe with initial centroid values. Defaults to NULL.
#' @param safeguard_file Each cycle will update a file specified in this argument 
#' with the current centers.  Defaults to 'kmeans.csv'. Pass NULL if no file is 
#' desired.
#' @param verbose Indicates if the progress bar will be displayed during the model's fitting.
#'
#' @details
#' Because each cycle is an indiependent 'dplyr' operation, or SQL operation if using a remote source,
#' the latest centroid data frame is saved to the parent environment in case the process needs to be
#' canceled and then restarted at a later point.  Passing the `current_kmeans` as the `initial_kmeans`
#' will allow the operation to pick up where it left off. 
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   simple_kmeans_db(mpg, qsec, wt) %>%
#'   glimpse()
#'
#' @export
simple_kmeans_db <- function(df,
                          ...,
                          centers = 3,
                          max_repeats = 100,
                          initial_kmeans = NULL,
                          safeguard_file = "kmeans.csv",
                          verbose = TRUE) {
  vars <- enquos(...)
  
  if(length(vars) > 0) {
      f_df <- select(df, !!! vars)
    } else {
      vars <- syms(colnames(df))
      f_df <- df
    }
  
  f_df <- filter_all(f_df, all_vars(!is.na(.)))

  if (!is.null(initial_kmeans)) {
    centroids <- initial_kmeans
  } else {
    centroids <- head(f_df, centers)
    centroids <- collect(centroids)
  }
  
  if(verbose){
    pb <- progress::progress_bar$new(
      format = paste0(
        " Cycle :current of ", max_repeats, " max. [:bar] [:var][:elapsed]"
      ),
      total = max_repeats, clear = TRUE, width = 80
    )    
  }
  
  for (iteration in 1:max_repeats) {
    prev_centroids <- centroids
    new_centroids <- calculate_centers(df, centroids, centers, vars)

    centroids_db <- select(new_centroids, !!! vars, center)
    centroids_db <- group_by(centroids_db, center)
    centroids_db <- summarise_all(centroids_db, "mean", na.rm = TRUE) 
    
    centroids <- select(centroids_db, -center)
    centroids <- collect(centroids)

    if (!is.null(safeguard_file)){ 
      sfg <- file.path(tempdir(), safeguard_file)
      write.csv(centroids, sfg, row.names = FALSE) 
    }
    variance <- (
      round(
        abs(sum(prev_centroids) - sum(centroids)) / sum(prev_centroids),
        digits = 4
      ) * 100
    )
    if (verbose) pb$tick(tokens = list(var = variance))
    if (all(prev_centroids == centroids)) break()
  }
  centroids_db <- rename_all(centroids_db, ~paste0("k_", .))
  right_join(
    centroids_db,
    new_centroids, 
    by = c("k_center" = "center")
    )
}

calculate_centers <- function(df, center_df, centers, vars) {
  center_names <- paste0("center_", 1:centers)
  
  fields <- length(vars)

  f_dist <- imap(
    center_df, ~{
      map2(
        .x, .y,
        function(x, y) expr((!! x) - (!! sym(y)))
      )
    })

  f_inside <- function(curr_center) {
      fi <- map(1:fields, ~{
        f <- pluck(f_dist, .x, curr_center)
        expr((((!! f)) * ((!! f))))
      }) 
      reduce(fi, function(l, r) expr((!! l) + (!! r)))
  }

  km <- map(1:centers, ~expr(sqrt(!! f_inside(.x))))
  km <- set_names(km, center_names)

  all <- map(center_names, ~{
      comp <- map2(.x, center_names, function(x, y)
        if (x != y) {
          expr((!! sym(x)) < (!! sym(y)))
        } else {
          expr((!! sym(x)) >= (!! sym(y)))
        }) 
      comp <- reduce(comp, function(l, r) expr((!! l) & (!! r)))
      reduce(c(comp, .x) ,function(l, r) expr((!! l) ~ !! (r)))
    }) 
  all <- flatten(all)

  comp <- expr(case_when(!!! all))

  res <- mutate(df, !!! km) 
  res <- mutate(res, center = !! comp) 
  res <- filter(res, !is.na(center)) 
  select(res, -contains("center_"))
}