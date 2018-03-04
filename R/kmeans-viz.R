#' Visualize a KMeans Cluster with lots of data
#'
#' It uses 'ggplot2' to display the results of a KMeans routine. Instead
#' of a scatterplot, it uses a square grid that displays the concentration
#' of intersections per square.  The number of squares in the grid can 
#' be customized for more or less fine grain.
#'
#' @param df A Local or remote data frame with results of KMeans clustering
#' @param x A numeric variable for the x axis
#' @param y A numeric variable for the y axis
#' @param resolution The number of squares in the grid. Defaults to 50. 
#' Meaining a 50 x 50 grid.
#' @param group A discrete variable containing the grouping for the KMeans. It defaults to 'center'
#'
#' @details
#' For large result-sets in remote sources, downloading every intersection will 
#' be a long running, costly operation.  The approach of this function is to 
#' devide the x and y plane in a grid and have the remote source figure the 
#' total number of intersections, returned as a single number.  This reduces the
#' granularity of the visualization, but it speeds up the results.
#'
#' @examples
#' plot_kmeans(mtcars, mpg, wt, group = am)
#' @export
plot_kmeans <- function(df, x, y, resolution = 50, group = center) {
  x <- enexpr(x)
  y <- enexpr(y)
  group <- enexpr(group)

  squares <- db_calculate_squares(
    df = df,
    x = !! x,
    y = !! y,
    group = !! group,
    resolution = resolution
  )


  squares %>%
    mutate(x = !! x, y = !! y, Center = !! group, Count = n) %>%
    ggplot() +
    geom_rect(
      aes(
        xmin = x,
        ymin = y,
        xmax = xend,
        ymax = yend,
        color = Center
      ),
      fill = "transparent"
    ) +
    geom_rect(aes(
      xmin = x,
      ymin = y,
      xmax = xend,
      ymax = yend,
      fill = Center
      , alpha = Count
    )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.line = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Kmeans Cluters",
      subtitle = paste0(expr_text(y), " ~ ", expr_text(x)),
      x = expr_text(x),
      y = expr_text(y)
    )
}

db_calculate_squares <- function(df, x, y, group, resolution = 50) {
  x <- enexpr(x)
  y <- enexpr(y)
  group <- enexpr(group)
  segs <- df %>%
    group_by(
      !! x := !! db_bin(!! x, bins = resolution),
      !! y := !! db_bin(!! y, bins = resolution),
      !! group
    ) %>%
    tally() %>%
    ungroup() %>%
    collect()
  segs %>%
    mutate(
      xend = !! x + min_dif(segs, !! x),
      yend = !! y + min_dif(segs, !! y)
    )
}

min_dif <- function(df, field) {
  field <- enexpr(field)
  df %>%
    mutate(
      x1 = lag(!! field, 1),
      dif = !! field - x1
    ) %>%
    filter(dif != 0, dif > 0) %>%
    summarise(min(dif)) %>%
    pull()
}

db_bin <- function(var, bins = 30, binwidth = NULL) {
  var <- enexpr(var)
  range <- expr(max(!! var, na.rm = TRUE) - min(!! var, na.rm = TRUE))
  if (is.null(binwidth)) {
    binwidth <- expr((!! range) / (!! bins))
  } else {
    bins <- expr(as.integer((!! range) / (!! binwidth)))
  }
  bin_number <- expr(as.integer(floor(((!! var) - min(!! var, na.rm = TRUE)) / (!! binwidth))))
  expr(((!! binwidth) *
    ifelse((!! bin_number) == (!! bins), (!! bin_number) - 1,
      (!! bin_number)
    )) + min(!! var, na.rm = TRUE))
}