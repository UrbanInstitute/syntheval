#' 
#' Calculate the minimum equivalence class size defined by the quasi-identifiers
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available) and `k` (the minimum observed 
#' equivalence class size).
#' 
#' @export
#' 
scan_k_anonymity <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_eq_classes <- attribute_scan$confidential$equivalence_classes
  synth_eq_classes <- attribute_scan$synthetic$equivalence_classes
  
  result <- tibble::tibble(
    source = c("confidential", "synthetic"),
    k = c(
      min(conf_eq_classes$raw_n[conf_eq_classes$raw_n > 0]),
      min(synth_eq_classes$raw_n[synth_eq_classes$raw_n > 0])
    )
  )
  
  # holdout data is optional; only add a holdout row when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_eq_classes <- attribute_scan$holdout$equivalence_classes
    
    result <- dplyr::bind_rows(
      result,
      tibble::tibble(
        source = "holdout",
        k = min(holdout_eq_classes$raw_n[holdout_eq_classes$raw_n > 0])
      )
    )
    
  }
  
  return(result)
  
}
