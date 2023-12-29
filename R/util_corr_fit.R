#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param postsynth A postsynth object from tidysynthesis or a tibble
#' @param data an original (observed) data set.
#' @param group_by 
#'
#' @return A `list` of fit metrics:
#'  - `correlation_original`: correlation matrix of the original data.
#'  - `correlation_synthetic`: correlation matrix of the synthetic data.
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the correlation matrix.
#'  
#' @family utility functions
#'
#' @export

util_corr_fit <- function(postsynth,
                          data, 
                          group_by = NULL) {
  
  if (is_postsynth(postsynth)) {
  
    synthetic_data <- postsynth$synthetic_data
  
  } else {
    
    synthetic_data <- postsynth
  }
  
   
  synthetic_data <- dplyr::select(synthetic_data, where(is.numeric), {{ group_by }})
  data <- dplyr::select(data, where(is.numeric), {{ group_by }})

  # reorder data names (this appears to check if the variables are the same)
  data <- dplyr::select(data, names(synthetic_data))
  
  if(!rlang::quo_is_null(enquo(group_by))){
    
    levels <- data %>% dplyr::distinct({{ group_by }}) %>% dplyr::pull()
    
    correlation_data <- data.frame()
    correlation_fit = c()
    correlation_difference_mae = c()
    correlation_difference_rmse = c()
 
   for(level in levels) {
       data_sub <- data %>% dplyr::filter({{ group_by }} == level)
       
       # get the results for the subgroup/level
       result <- util_corr_fit(postsynth = synthetic_data, data = data_sub)
       
       df <- result$correlation_data
       fit <- result$correlation_fit
       mae <- result$correlation_difference_mae
       rmse <- result$correlation_difference_rmse
       
       # add the results to a growing list of results for each subgroup/level 
       correlation_data <- dplyr::bind_rows(correlation_data, cbind(level, df))
       correlation_fit = c(correlation_fit, fit)
       correlation_difference_mae = c(correlation_difference_mae, mae)
       correlation_difference_rmse = c(correlation_difference_rmse, rmse)
   }
    
    return(
      list(
        correlation_data = correlation_data,
        correlation_fit = correlation_fit,
        correlation_difference_mae = correlation_difference_mae,
        correlation_difference_rmse = correlation_difference_rmse
        )
    )
  }
  
  # helper function to find a correlation matrix with the upper tri set to zeros
  lower_triangle <- function(x) {
    
    # find the linear correlation matrix of numeric variables from a data set
    correlation_matrix <-
      x %>%
      dplyr::select_if(is.numeric) %>%
      stats::cor()
    
    # set the values in the upper triangle to zero to avoid double counting
    correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA
    
    return(correlation_matrix)
  }
  
  # find the lower triangle of the original data linear correlation matrix and return a df
  original_lt <- data.frame(lower_triangle(data))
  
  # adding variable 2 column to the original df
  original_lt$var2 <- colnames(original_lt)
  
  # restructuring the correlation matrix so the cols are var1, var2, original
  original_lt <- original_lt %>%
    tidyr::pivot_longer(cols = !var2, names_to = "var1", values_to = "original") %>%
    dplyr::filter(!is.na(original)) %>%
    dplyr::arrange(var1) %>%
    dplyr::select(var1, var2, original)
  
  # find the lower triangle of the synthetic data linear correlation matrix and return a df
  synthetic_lt <- data.frame(lower_triangle(synthetic_data))
  
  # adding variable 2 column to the synthetic df
  synthetic_lt$var2 <- colnames(synthetic_lt)
  
  # restructuring the correlation matrix so the cols are var1, var2, synthetic
  synthetic_lt <- synthetic_lt %>%
    tidyr::pivot_longer(cols = !var2, names_to = "var1", values_to = "synthetic") %>%
    dplyr::filter(!is.na(synthetic)) %>%
    dplyr::arrange(var1) %>%
    dplyr::select(var1, var2, synthetic)
  
  # find the difference between the original correlations and the synthetic 
  correlation_data <- original_lt %>%
    dplyr::left_join(synthetic_lt, by = c("var1","var2")) %>%
    dplyr::mutate(difference = original - synthetic,
           proportion_difference = .data$difference / .data$original)
  
  # find the length of the nonzero values in the matrices
  n <- choose(ncol(correlation_data), 2)

  # calculate the correlation fit and divide by n
  correlation_fit <- sqrt(sum(correlation_data$difference ^ 2, na.rm = TRUE)) / n
  
  difference_vec <- as.numeric(correlation_data$difference)
  
  # mean absolute error
  correlation_difference_mae <- difference_vec %>%
    abs() %>%
    mean()

  # root mean square error
  correlation_difference_rmse <- difference_vec ^ 2 %>%
    mean() %>%
    sqrt()
  
  
  return(
    list(
      correlation_data = correlation_data,
      correlation_fit = correlation_fit,
      correlation_difference_mae = correlation_difference_mae,
      correlation_difference_rmse = correlation_difference_rmse
    )
  )
  
}