#' Add pMSE to discrimination object, with option to assess separately on groups
#' indicated by a grouping variable
#'
#' @param discrimination A discrimination object with propensities (likely 
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#' @param group A set of variables to group the pmse by
#'
#' @return A discrimination object with propensities (likely added using 
#' add_propensities()) with a pMSE
#' 
#' @family Utility metrics
#' 
#' @export
#' 

add_pmse <- function(discrimination, group = c(), split = TRUE) {
  
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
  
  if (split) {
    
    pmse_training <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "training") %>%
      calc_pmse()
    
    pmse_testing <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "testing") %>%
      calc_pmse()
    
    if (length(group)==0){ # original case
      pmse <- tibble::tibble(
        .source = factor(c("training", "testing"), levels = c("training", "testing")),
        .pmse = c(pmse_training, pmse_testing)
      )
    }
    else{ # we have passed a list of grouping variables
      groups <- discrimination$propensities %>%
        dplyr::group_by(across(all_of(group))) %>%
        group_keys()
      groups <- rbind(groups, groups) # make 2 copies for train/test
      
      pmse <- tibble::tibble(
        groups,
        .source = factor(c(rep("training", length(pmse_training)), rep("testing", length(pmse_testing))), levels = c("training", "testing")),
        .pmse = c(pmse_training, pmse_testing)
      )
    }
    
  } else {
    
    pmse_overall <- discrimination$propensities %>%
      calc_pmse()
    
    if (length(group)==0){ # original case
      pmse <- tibble::tibble(
        .source = factor("overall", levels = "overall"),
        .pmse = pmse_overall
      )
    }
    else{ # we have passed a list of grouping variables
      groups <- discrimination$propensities %>%
        dplyr::group_by(across(all_of(group))) %>%
        group_keys()
      
      pmse <- tibble::tibble(
        groups,
        .source = factor(c(rep("overall", length(pmse_overall))), levels = c("overall")),
        .pmse = pmse_overall
      )
    }
  }
  
  discrimination$pmse <- pmse
  
  return(discrimination)
  
}