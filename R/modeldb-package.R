#' @import rlang
#' @import ggplot2
#' @import tibble
#' @importFrom dplyr mutate summarise left_join
#' @importFrom dplyr summarise_all filter_all
#' @importFrom dplyr pull collect arrange
#' @importFrom dplyr contains tally ungroup
#' @importFrom dplyr group_vars lag bind_cols
#' @importFrom dplyr all_vars group_by funs
#' @importFrom dplyr n as_tibble filter select
#' @importFrom dplyr case_when rename rename_all
#' @importFrom dplyr tbl_vars everything
#' @importFrom purrr map map2 map_df transpose
#' @importFrom purrr reduce imap pluck
#' @importFrom tidypredict as_parsed_model
#' @importFrom utils head
#' @importFrom utils write.csv
#' @keywords internal
#'
"_PACKAGE"
NULL
utils::globalVariables(c(
  ".", "Center", "Count", "Intercept", "center", "dif", "mx1",
  "mx2", "my", "sx", "sxx", "sxy", "sy", "val", "var", "x1",
  "x1x", "x1y", "x2x", "x2y", "xend", "yend"
))
