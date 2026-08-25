#' 
#' Summarize the size distribution of equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available), `n_classes` (the number of 
#' observed equivalence classes), `min_size`, `mean_size`, `median_size`, and 
#' `max_size` (summary statistics of the observed equivalence class sizes).
#' 
#' @export
#' 
scan_equivalence_class_size <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_size <- .class_size_summary(
    attribute_scan$confidential$equivalence_classes
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_size <- .class_size_summary(
    attribute_scan$synthetic$equivalence_classes
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_size, synth_size)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_size <- .class_size_summary(
      attribute_scan$holdout$equivalence_classes
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_size))
    
  }
  
  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(
      dplyr::all_of(c("source", "n_classes", "min_size", "mean_size", "median_size", "max_size"))
    )
  
  return(result)
  
}

#' 
#' Compute summary statistics of observed equivalence class sizes
#' 
#' @param equivalence_classes An equivalence classes tibble, as stored in an 
#' `attribute_scan` object (see `.aggregate_qid()`).
#' 
#' @return A tibble with columns `n_classes`, `min_size`, `mean_size`, 
#' `median_size`, and `max_size`.
#' 
.class_size_summary <- function(equivalence_classes) {
  
  # raw_n > 0 excludes classes completed by .drop = FALSE but never observed
  observed_sizes <- equivalence_classes$raw_n[equivalence_classes$raw_n > 0]
  
  result <- tibble::tibble(
      n_classes = length(observed_sizes),
      min_size = min(observed_sizes),
      mean_size = mean(observed_sizes),
      median_size = stats::median(observed_sizes),
      max_size = max(observed_sizes)
    )

  return(result)
  
}
