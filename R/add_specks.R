#' Add SPECKS to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#' @param group A list of variable names to group by
#'
#' @family Utility metrics
#'
#' @return A discrimination with SPECKS
#' 
#' @export
add_specks <- function(discrimination, group = c(), split = TRUE) {
  
  calc_specks <- function(propensities) {
    
    propensities_original <- propensities %>%
      dplyr::group_by(across(all_of(group))) %>%
      dplyr::filter(.data$.source_label == "original") %>%
      dplyr::summarize(".pred_synthetic" = list(c(.data$.pred_synthetic))) %>%
      dplyr::pull(".pred_synthetic")
    
    propensities_synthetic <- propensities %>%
      dplyr::group_by(across(all_of(group))) %>%
      dplyr::filter(.data$.source_label == "synthetic") %>%
      dplyr::summarize(".pred_synthetic" = list(c(.data$.pred_synthetic))) %>%
      dplyr::pull(".pred_synthetic")
    
    specks_func <- function(original, synthetic){
      suppressWarnings(
        stats::ks.test(
          original, 
          synthetic, 
          exact = FALSE)$statistic
      )
    }
    # Calculate KS Distance of the original and synthetic ECDFS
    #specks <- suppressWarnings(
    # stats::ks.test(
    #    propensities_original, 
    #    propensities_synthetic, 
    #    exact = FALSE)$statistic
    #)
    # calculate the observed pMSE
    specks <- mapply(specks_func, propensities_original, propensities_synthetic)
    
    specks <- unname(specks)
    
    return(specks)
    
  }
  
  if (split) {
    
    specks_training <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "training") %>%
      calc_specks()
    
    specks_testing <- discrimination$propensities %>%
      dplyr::filter(.data$.sample == "testing") %>%
      calc_specks()
    
    if (length(group)==0){ # original case
      specks <- tibble::tibble(
        .source = factor(c("training", "testing"), levels = c("training", "testing")),
        .specks = c(specks_training, specks_testing)
      )
    }
    else{ # we have passed a list of grouping variables
      groups <- discrimination$propensities %>%
        dplyr::group_by(across(all_of(group))) %>%
        group_keys()
      groups <- rbind(groups, groups) # make 2 copies for train/test
      
      specks <- tibble::tibble(
        groups,
        .source = factor(c(rep("training", length(specks_training)), rep("testing", length(specks_testing))), levels = c("training", "testing")),
        .specks = c(specks_training, specks_testing)
      )
    }
    
  } else {
    
    specks_overall <- discrimination$propensities %>%
      calc_specks()
    
    if (length(group)==0){ # original case
      specks <- tibble::tibble(
        .source = factor("overall", levels = "overall"),
        .specks = specks_overall
      )
    }
    else{ # we have passed a list of grouping variables
      groups <- discrimination$propensities %>%
        dplyr::group_by(across(all_of(group))) %>%
        group_keys()
      
      specks <- tibble::tibble(
        groups,
        .source = factor(c(rep("overall", length(specks_overall))), levels = c("overall")),
        .specks = specks_overall
      )
    }
    
  }
  
  discrimination$specks <- specks
  
  return(discrimination)
  
}