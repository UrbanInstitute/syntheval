#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param postsynth A postsynth object from tidysynthesis or a tibble
#' @param data an original (observed) data set.
#' @param group_var 
#' @param level
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
                          group_var = NULL,
                          level = NULL) {
  
  if (is_postsynth(postsynth)) {
  
    synthetic_data <- postsynth$synthetic_data
  
  } else {
    
    synthetic_data <- postsynth
  }
  
  synthetic_data <- dplyr::select(synthetic_data, where(is.numeric), {{ group_var }})
  data <- dplyr::select(data, where(is.numeric), {{ group_var }})

  # reorder data names (this appears to check if the variables are the same)
  data <- dplyr::select(data, names(synthetic_data))
  
  # issue: if group_var = NULL is passed into the function, this runs 

  if(!missing(group_var) & !is.null(group_var)){
    
    group_var <- as.name(group_var)
    
    levels <- data %>% dplyr::distinct({{ group_var }}) %>% pull()
    
    correlation_data <- data.frame()
 
   for(level in levels) {
       data_sub <- data %>% dplyr::filter({{ group_var }} == level)
       
       df <- util_corr_fit(postsynth = synthetic_data, data = data_sub, level = level)$correlation_data
       
       correlation_data <- dplyr::bind_rows(correlation_data, df)
   }
    
    return(correlation_data)
  }
  
  print("group_var does not exist")
  
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
    pivot_longer(cols = !var2, names_to = "var1", values_to = "original") %>%
    filter(!is.na(original)) %>%
    arrange(var1) %>%
    select(var1, var2, original)
  
  # find the lower triangle of the synthetic data linear correlation matrix and return a df
  synthetic_lt <- data.frame(lower_triangle(synthetic_data))
  
  # adding variable 2 column to the synthetic df
  synthetic_lt$var2 <- colnames(synthetic_lt)
  
  # restructuring the correlation matrix so the cols are var1, var2, synthetic
  synthetic_lt <- synthetic_lt %>%
    pivot_longer(cols = !var2, names_to = "var1", values_to = "synthetic") %>%
    filter(!is.na(synthetic)) %>%
    arrange(var1) %>%
    select(var1, var2, synthetic)
  
  # find the difference between the original correlations and the synthetic 
  correlation_data <- original_lt %>%
    left_join(synthetic_lt, by = c("var1","var2")) %>%
    mutate(difference = original - synthetic,
           proportion_difference = .data$difference / .data$original)
  
  # add level (if level is not null)
  if(!is.null(level)){
    correlation_data <- cbind(level, correlation_data)
  }
  
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

  
   # compare names
  # if (any(rownames(original_lt) != rownames(synthetic_lt))) {
  #   stop("ERROR: rownames are not identical")
  # }
  # 
  # if (any(colnames(original_lt) != colnames(synthetic_lt))) {
  #   stop("ERROR: colnames are not identical")
  # }
  # 
  # # find the difference between the matrices
  # difference_lt <- synthetic_lt - original_lt
  # 
  # # find the length of the nonzero values in the matrices
  # n <- choose(ncol(difference_lt), 2)
  
  # calculate the correlation fit and divide by n
  # correlation_fit <- sqrt(sum(difference_lt ^ 2, na.rm = TRUE)) / n
  # 
  # difference_vec <- as.numeric(difference_lt)[!is.na(difference_lt)]
  # 
  # # mean absolute error
  # correlation_difference_mae <- difference_vec %>%
  #   abs() %>%
  #   mean()
  # 
  # # root mean square error
  # correlation_difference_rmse <- 
  #   difference_vec ^ 2%>%
  #   mean() %>%
  #   sqrt()
  
  return(
    list(
      correlation_data = correlation_data,
      # correlation_original = original_lt,
      # correlation_synthetic = synthetic_lt
      # correlation_difference = difference_lt,
      correlation_fit = correlation_fit,
      correlation_difference_mae = correlation_difference_mae,
      correlation_difference_rmse = correlation_difference_rmse
    )
  )
  
}
