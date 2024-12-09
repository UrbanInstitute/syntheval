#'
#' Check whether na.rm is compatible with univariate utilty metrics
#'
#' @param combined_data A data frame or tibble
#' @param na.rm A boolean for whether to ignore missing values
#' @param drop_zeros A boolean for whether to ignore zero values in utility metrics
#' @param drop_zeros_exclude An optional set of unquoted columns on which to drop zeros 
#'
#' @return A data frame or tibble with missing and/or zero values set to NA
#' 
#' @export
#' 
prep_combined_data_for_na.rm <- function(
    combined_data,
    na.rm = FALSE,
    drop_zeros = FALSE,
    drop_zeros_exclude = NULL) {
  
  # raise warning if missing values present
  if (na.rm == FALSE) {
    
    na_cols <- combined_data %>% 
      purrr::map_lgl(.f = ~ any(is.na(.x)))
    
    if (any(na_cols)) {
      
      message(
        paste(
          "Some variables contain missing data: ",
          paste(names(combined_data)[na_cols], collapse=", ")
        )
      )
      
      # stop if drop_zeros incompatible with keeping NAs
      if (drop_zeros) {
        
        stop("Cannot set na.rm == FALSE and drop_zeros == TRUE with missing data")
        
      }
      
    }
    
  } 
  
  if (drop_zeros) {
    
    if (is.null(drop_zeros_exclude)) {
      
      combined_data[combined_data == 0] <- NA
      
    }

    else {
      
      combined_data <- combined_data %>%
        dplyr::mutate(
          dplyr::across(
            -dplyr::all_of(drop_zeros_exclude),
            \(x) {
              dplyr::if_else(x == 0, NA, x)
            }
          )
        )
      
    }

  }
  
  return(combined_data)
  
}  

#'
#' Check whether na.rm is compatible with univariate utilty metrics
#'
#' @param combined_data A data frame or tibble
#' @param na.rm A boolean for whether to ignore missing values
#' @param drop_zeros A boolean for whether to ignore zero values in utility metrics
#' @param drop_zeros_exclude An optional set of quoted columns on which to drop zeros 
#'
#' @return A data frame or tibble with missing and/or zero values set to NA
#' 
.prep_combined_data_for_na.rm_q <- function(
    combined_data,
    na.rm = FALSE,
    drop_zeros = FALSE,
    drop_zeros_exclude = NULL) {
  
  # raise warning if missing values present
  if (na.rm == FALSE) {
    
    na_cols <- combined_data %>% 
      purrr::map_lgl(.f = ~ any(is.na(.x)))
    
    if (any(na_cols)) {
      
      message(
        paste(
          "Some variables contain missing data: ",
          paste(names(combined_data)[na_cols], collapse=", ")
        )
      )
      
      # stop if drop_zeros incompatible with keeping NAs
      if (drop_zeros) {
        
        stop("Cannot set na.rm == FALSE and drop_zeros == TRUE with missing data")
        
      }
      
    }
    
  } 
  
  if (drop_zeros) {
    
    if (is.null(drop_zeros_exclude)) {
      
      combined_data[combined_data == 0] <- NA
      
    }
    
    else {
      
      combined_data <- combined_data %>%
        dplyr::mutate(
          dplyr::across(
            -dplyr::any_of(drop_zeros_exclude),
            \(x) {
              dplyr::if_else(x == 0, NA, x)
            }
          )
        )
      
    }
    
  }
  
  return(combined_data)
  
}  


#'
#' Convert `NA` values to `"NA"` for categorical variables
#'
#' @param data A data frame or tibble
#'
#' @return A data frame or tibble with `NA` converted to `"NA"`
#' 
#' @export
#' 
convert_na_to_level <- function(data) {
  
  na_to_level <- function(x) {
    
    # do nothing if x isn't a character or factor
    if (!pillar::type_sum(x) %in% c("chr", "ord", "fct")) return(x) 
    
    # test if NA is already a level
    if (sum(x == "NA", na.rm = TRUE) > 0) {
      stop("ERROR: can't convert NA to 'NA' because 'NA' already exists")
    }
    
    # replace `NA` with `"NA"`
    if (all(!is.na(x))) {
      
      return(x)
      
    } else if (pillar::type_sum(x) == "chr") {
      
      x <- tidyr::replace_na(data = x, replace = "NA")
      
    } else if (pillar::type_sum(x) %in% c("ord", "fct")) {
      
      ordinal_flag <- pillar::type_sum(x) == "ord"
      
      # store the factor levels
      x_levels <- c(levels(x), "NA")
      
      # convert to character and replace the NA
      x_chr <- as.character(x)
      
      x_chr <- tidyr::replace_na(data = x_chr, replace = "NA")
      
      # convert back to a factor
      x <- factor(x_chr, levels = x_levels, ordered = ordinal_flag)
      
    }
    
    return(x)
    
  }
  
  data_converted <- data %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = na_to_level))
  
  return(data_converted)
  
}



#'
#' Convert `NA` values to `"NA"` for categorical variables
#'
#' @param data A data frame or tibble
#'
#' @return A data frame or tibble with `NA` converted to `"NA"`
#' 
#' @export
#' 
convert_na_to_level <- function(data) {
  
  na_to_level <- function(x) {
    
    # do nothing if x isn't a character or factor
    if (!pillar::type_sum(x) %in% c("chr", "ord", "fct")) return(x) 
    
    # test if NA is already a level
    if (sum(x == "NA", na.rm = TRUE) > 0) {
      stop("ERROR: can't convert NA to 'NA' because 'NA' already exists")
    }
    
    # replace `NA` with `"NA"`
    if (all(!is.na(x))) {
      
      return(x)
      
    } else if (pillar::type_sum(x) == "chr") {
      
      x <- tidyr::replace_na(data = x, replace = "NA")
      
    } else if (pillar::type_sum(x) %in% c("ord", "fct")) {
      
      ordinal_flag <- pillar::type_sum(x) == "ord"
      
      # store the factor levels
      x_levels <- c(levels(x), "NA")
      
      # convert to character and replace the NA
      x_chr <- as.character(x)
      
      x_chr <- tidyr::replace_na(data = x_chr, replace = "NA")
      
      # convert back to a factor
      x <- factor(x_chr, levels = x_levels, ordered = ordinal_flag)
      
    }
    
    return(x)
    
  }
  
  data_converted <- data %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = na_to_level))
  
  return(data_converted)
  
}
