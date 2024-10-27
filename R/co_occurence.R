#' Construct a co-occurrence matrix
#'
#' @param data A tibble with numeric variables
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @return A co-occurrence matrix
#' 
co_occurrence <- function(data, na.rm = FALSE) {
  
  # create a vector of variable names
  data_names <- names(data)
  
  # create a p by p matrix
  co_occurence_matrix <- matrix(nrow = ncol(data), ncol = ncol(data))
  rownames(co_occurence_matrix) <- data_names
  colnames(co_occurence_matrix) <- data_names
  
  # iterate through the variables and assign the co-occurrences
  for (row_name in data_names) {
    
    for (col_name in data_names) {
      
      row_var <- dplyr::pull(data, row_name)
      col_var <- dplyr::pull(data, col_name)

      if (na.rm) {
        
        # remove missing values
        na_lgl <- !is.na(row_var) & !is.na(col_var)
        row_var <- row_var[na_lgl]
        col_var <- col_var[na_lgl]
        
      } 
      
      co_occurence_matrix[row_name, col_name] <- 
        mean(row_var != 0 & col_var != 0)
      
    }
    
  }
  
  return(co_occurence_matrix)
  
}
