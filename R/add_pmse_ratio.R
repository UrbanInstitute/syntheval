#' Add pMSE ratio to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#' @param prop The proportion of data to be retained for modeling/analysis in 
#' the training/testing split. The sampling is stratified by the original and
#' synthetic data.
#' @param group A set of variables to group the pmse and null pmse by
#' @param times The number of bootstrap samples.
#'
#' @return A discrimination with pMSE
#' 
#' @family Utility metrics
#' 
#' @export
add_pmse_ratio <- function(discrimination, split = TRUE, prop = 3 / 4, group = c(), times) {
  
  if (is.null(discrimination$pmse)) {
    
    stop("Error: discrimination must have a pmse. Use add_pmse() before add_pmse_ratio()")
    
  }
  
  calc_pmse <- function(propensities) {
    
    # calculate the expected propensity
    prop_synthetic <- propensities %>%
      dplyr::group_by(across(all_of(group))) %>%
      dplyr::summarize("prop_synthetic" = list(c(sum(.data$.source_label == "synthetic")/dplyr::n()))) %>%
      dplyr::pull(prop_synthetic)
    
    propensities_vec <- propensities %>%
      dplyr::group_by(across(all_of(group))) %>%
      dplyr::summarise(".pred_synthetic" = list(c(.pred_synthetic))) %>%
      dplyr::pull(.pred_synthetic)
    
    # function for pmse
    pmse_func <- function(propensities_vec, prop_synthetic){
      mean((propensities_vec - prop_synthetic)^2)
    }
    # calculate the observed pMSE
    pmse <- mapply(pmse_func, propensities_vec, prop_synthetic)
    
    return(pmse)
  }
  # function to sample 2x size of grouped data, by group, 
  # for the dataset with both confidential and synthetic data
  # group_resample <- function(data){
  #   # split by grouping variables
  #   split_data = dplyr::group_split(data %>% dplyr::ungroup() %>% dplyr::group_by(across(all_of(group))))
  #   bootstrap_sample = list()
  #   for (elem in split_data){ # iterate through grouped dataset
  #     # in each group, sample twice as much data
  #     bootstrap_sample <- append(bootstrap_sample, list(dplyr::bind_cols(
  #       elem %>%
  #         dplyr::filter(.data$.source_label == "original") %>%
  #         dplyr::group_by(across(all_of(group))) %>%
  #         dplyr::slice_sample(n = nrow(elem), replace = TRUE) %>%
  #         dplyr::select(-".source_label"),
  #       elem %>%
  #         dplyr::select(".source_label"))
  #     ))
  #   }
  #   bootstrap_sample = dplyr::bind_rows(bootstrap_sample)
  # }
  
  group_resample <- function(data){
    # split by grouping variables
    split_data = dplyr::group_split(data %>% dplyr::ungroup() %>% dplyr::group_by(across(all_of(group))))
    bootstrap_sample = vector("list", length = length(split_data))
    for (i in 1:length(split_data)){ # iterate through grouped dataset
      # in each group, sample twice as much data
      bootstrap_sample[[i]] <- list(dplyr::bind_cols(
        split_data[[i]] %>%
          dplyr::filter(.data$.source_label == "original") %>%
          dplyr::group_by(across(all_of(group))) %>%
          dplyr::slice_sample(n = nrow(split_data[[i]]), replace = TRUE) %>%
          dplyr::select(-".source_label"),
        split_data[[i]] %>%
          dplyr::select(".source_label"))
      )
    }
    bootstrap_sample = dplyr::bind_rows(bootstrap_sample)
  }
  
  # matrix instead of vector, where each entry is a simulation, containing a vector with groups
  #pmse_null_overall <- c()
  #pmse_null_training <- c()
  #pmse_null_testing <- c()
  
  pmse_null_overall <- rep(NA, times)
  pmse_null_training <- rep(NA, times)
  pmse_null_testing <- rep(NA, times)
  
  for (a in 1:times) {
    # bootstrap sample original observations to equal the size of the combined 
    # data, with a vector of one set of observations per grouping variable (?)
    # append the original labels so the proportions match
    bootstrap_sample <- group_resample(discrimination$combined_data)
    
    if (split) {
      
      # make training/testing split 
      data_split <- rsample::initial_split(
        data = bootstrap_sample,
        prop = prop,
        strata = ".source_label" # NOTE: Should this also be stratified by group?
      )
      
      # fit the model from the pMSE on the bootstrap sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator, 
        data = rsample::training(data_split)
      )
      
      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      ) %>%
        dplyr::mutate(
          .sample = dplyr::if_else(
            dplyr::row_number() %in% data_split$in_id, 
            true = "training", 
            false = "testing"
          )
        )
      
      # calculate the pmse for each bootstrap
      # pmse_null_overall <- append(pmse_null_overall, calc_pmse(propensities_df))
      # pmse_null_training <- append(pmse_null_training, propensities_df %>%
      #                                dplyr::filter(.data$.sample == "training") %>%
      #                                calc_pmse())
      # pmse_null_testing <- append(pmse_null_testing, propensities_df %>%
      #                               dplyr::filter(.data$.sample == "testing") %>%
      #                               calc_pmse())
      pmse_null_overall[a] <- calc_pmse(propensities_df)
      pmse_null_training[a] <- propensities_df %>%
                                     dplyr::filter(.data$.sample == "training") %>%
                                     calc_pmse()
      pmse_null_testing[a] <- propensities_df %>%
                                    dplyr::filter(.data$.sample == "testing") %>%
                                    calc_pmse()
    } else {
      
      # fit the model from the pMSE on the bootstrap sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator, 
        data = bootstrap_sample
      )
      
      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      )
      
      # calculate the pmse for each bootstrap
      pmse_null_overall[a] <- calc_pmse(propensities_df)
      
    }
    
  }
  
  # find the mean of the bootstrapped pMSEs
  mean_null_pmse_overall <- colMeans(t(matrix(pmse_null_overall, ncol = times))) # each row is a new sample
  if (split){
    mean_null_pmse_training <- colMeans(t(matrix(pmse_null_training, ncol = times)))
    mean_null_pmse_testing <- colMeans(t(matrix(pmse_null_testing, ncol= times)))
  }
  
  # calculate the ratio for the training/testing split or overall data
  if (all(c("training", "testing") %in% discrimination$pmse$.source)) {
    
    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = c(mean_null_pmse_training, mean_null_pmse_testing))
    ) %>%
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
    
  } else {
    
    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = mean_null_pmse_overall)
    ) %>%
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
    
  }
  
  discrimination$pmse <- pmse
  
  return(discrimination)
  
}