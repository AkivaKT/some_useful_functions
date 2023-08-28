#' Shuffle Columns in a Dataset
#'
#' This function shuffles specified columns in a dataset. It offers the flexibility
#' to shuffle columns individually or together, while preserving the order of
#' missing values. Additionally, an oversampling option is provided to augment the
#' dataset size before shuffling.
#'
#' @param dataset A data frame containing the dataset.
#' @param column_lists A list of lists specifying columns to be shuffled. Each
#'   inner list can contain either column names or column indices. If not
#'   provided, all columns will be shuffled individually.
#' @param oversample Logical. If TRUE, oversampling will be applied to the dataset
#'   before shuffling. Default is FALSE.
#' @param oversample_factor Numeric. The factor by which the dataset should be
#'   oversampled. It should be greater than 1. For example, an oversample_factor
#'   of 1.2 will result in a 20% increase in dataset size. Applicable only if
#'   oversample is TRUE. Default is 1.
#' @param seed An optional numeric value to set the random seed. When provided,
#'   the shuffling process will be deterministic. If not provided, the shuffling
#'   will use a different random seed each time, resulting in different shuffling.
#'
#' @return A shuffled dataset.
#'
#' @examples
#' data <- data.frame(
#'   A = c(1, 2, 3, 4, NA),
#'   B = c(5, 6, 7, 8, NA),
#'   C = c(9, 10, 11, 12, NA),
#'   D = c(13, 14, 15, 16, NA)
#' )
#'
#' # Shuffle columns A and B together, shuffle column C individually,
#' # shuffle column D individually
#' column_lists <- list(c("A", "B"), c("C"), c("D"))
#'
#' shuffled_data <- shuffle_columns(data, column_lists, oversample = TRUE, oversample_factor = 1.2, seed = 123)
#'
#' @import dplyr
#'
#' @export
shuffle_columns <- function(dataset, column_lists = NULL, oversample = FALSE, oversample_factor = 1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (is.null(column_lists)) {
    column_lists <- list(names(dataset))
  } else if (!is.list(column_lists)) {
    stop("column_lists should be a list.")
  }
  
  if (oversample) {
    oversampled_size <- ceiling(nrow(dataset) * oversample_factor)
    dataset <- dataset %>%
      sample_n(oversampled_size, replace = TRUE)
  }
  
  for (col_list in column_lists) {
    if (is.character(col_list)) {
      cols <- col_list
    } else if (is.numeric(col_list)) {
      cols <- names(dataset)[col_list]
    } else {
      stop("Each element in column_lists should be a list of column names or column indices.")
    }
    
    if (length(cols) == 1) {
      dataset <- dataset %>%
        mutate(across(cols, ~ ifelse(!is.na(.), sample(., length(.)), .)))
    } else if (length(cols) > 1) {
      temp_df <- dataset %>%
        select(all_of(cols))
      print(cols)
      temp_df <- temp_df %>%
        slice_sample(prop = 1)
        
      dataset <- dataset %>%
        mutate(across(cols, ~ temp_df[[cur_column()]]))
    }
  }
  
  return(dataset)
}
