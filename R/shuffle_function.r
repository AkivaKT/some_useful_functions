#' Shuffle Columns in a Dataset
#'
#' This function shuffles specified columns in a dataset while maintaining the order
#' of missing values. It offers the flexibility to shuffle columns individually or
#' together, and includes an option to oversample the dataset before shuffling.
#'
#' @param dataset A data frame containing the dataset.
#' @param column_lists A list of lists specifying columns to be shuffled. Each
#'   inner list can contain either column names or column indices. If not provided,
#'   all columns will be shuffled individually.
#' @param oversample Logical. If TRUE, oversampling is applied to the dataset before
#'   shuffling. Default is FALSE.
#' @param oversample_factor Numeric. The factor by which the dataset should be
#'   oversampled. For example, an oversample_factor of 1.2 increases the dataset
#'   size by 20%. Applicable only if oversample is TRUE. Default is 1.
#' @param seed An optional numeric value to set the random seed, ensuring deterministic
#'   shuffling if provided. If omitted, the shuffling uses different random seeds
#'   each time.
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
    oversampled_size <- ceiling(nrow(dataset) * (oversample_factor - 1))
    dataset <- dataset |>
      slice_sample(n = oversampled_size, replace = T) |>
      bind_rows(dataset)
  }

  for (col_list in column_lists) {
    if (is.character(col_list)) {
      cols <- col_list
    } else if (is.numeric(col_list)) {
      cols <- names(dataset)[col_list]
    } else {
      stop("Each element in column_lists should be a list of column names or column indices.")
    }
    non_missing_indices <- which(!is.na(dataset |> select(all_of(cols)) |> pull(cols[1])))
    shuffled_values <- slice(select(dataset, all_of(cols)), non_missing_indices) |>
      mutate(across(all_of(cols), sample))
    dataset[non_missing_indices, cols] <- shuffled_values[, cols]
  }
  return(dataset)
}
