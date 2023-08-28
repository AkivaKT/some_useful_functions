#' Find an approximate subset of a dataframe based on length and sum criteria.
#'
#' This function searches for a subset of a given dataframe that approximately matches
#' both the specified target length and target sum within provided tolerances.
#'
#' @param input_df The input dataframe from which to find the subset.
#' @param column_name The name of the column in the dataframe to be evaluated.
#' @param target_length The target length for the subset.
#' @param target_sum The target sum for the subset.
#' @param sum_tolerance The tolerance allowed for the difference between the subset sum and target sum.
#' @param max_iterations The maximum number of iterations for the search.
#' @param seed An optional seed for reproducibility.
#'
#' @return A dataframe subset that meets the length and sum criteria based on the specified column.
#'
#' @examples
#' # Generate sample dataframe
#' input_df <- data.frame(values = c(2, 5, 8, 3, 10, 6, 1, 4, 7))
#' column_name <- "values"
#' target_length <- 3
#' target_sum <- 16
#' sum_tolerance <- 0.2
#' max_iterations <- 1000
#'
#' # Find the approximate subset
#' subset_result <- approx_subset(input_df, column_name, target_length, target_sum, sum_tolerance, max_iterations)
#'
#' # Print the result
#' print("Approximate subset:")
#' print(subset_result)
#'
#' @import dplyr
#' @export
approx_subset <- function(input_df, column_name, target_length, target_sum, sum_tolerance = 0.1, max_iterations = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n <- nrow(input_df)
  best_subset <- NULL
  smallest_difference <- Inf
  tolerance_sum <- target_sum * sum_tolerance

  for (i in 1:max_iterations) {
    subset_indices <- sample(1:n, size = target_length)
    subset_sum <- sum(select(slice(input_df, subset_indices), {{column_name}}))

    if (between(subset_sum - target_sum, -tolerance_sum, tolerance_sum)) {
      curr_difference <- abs(subset_sum - target_sum)

      if (curr_difference < smallest_difference) {
        best_subset <- slice(input_df, subset_indices)
        smallest_difference <- curr_difference
        if(smallest_difference == 0) return(best_subset)
      }
    }
  }
  cat('Sum difference:', smallest_difference, '\n')
  return(best_subset)
}

