#' Construct a co-occurrence matrix
#'
#' @param data A tibble with numeric variables
#'
#' @return A co-occurrence matrix
#' 
co_occurrence <- function(data) {
  
  # create a vector of variable names
  data_names <- names(data)
  
  # create a p by p matrix
  co_occurence_matrix <- matrix(nrow = ncol(data), ncol = ncol(data))
  rownames(co_occurence_matrix) <- data_names
  colnames(co_occurence_matrix) <- data_names
  
  # iterate through the variables and assign the co-occurrences
  for (row_name in data_names) {
    
    for (col_name in data_names) {
      
      co_occurence_matrix[row_name, col_name] <- 
        mean(dplyr::pull(data, row_name) != 0 & dplyr::pull(data, col_name) != 0)
      
    }
    
  }
  
  return(co_occurence_matrix)
  
}
