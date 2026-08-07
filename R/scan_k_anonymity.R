#' 
#' Calculate the minimum equivalence class size defined by the quasi-identifiers
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"` or `"synthetic"`) and 
#' `value` (the minimum observed equivalence class size).
#' 
#' @export
#' 
scan_k_anonymity <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_eq_classes <- attribute_scan$confidential$equivalence_classes
  synth_eq_classes <- attribute_scan$synthetic$equivalence_classes
  
  result <- tibble::tibble(
    source = c("confidential", "synthetic"),
    value = c(
      min(conf_eq_classes$raw_n[conf_eq_classes$raw_n > 0]),
      min(synth_eq_classes$raw_n[synth_eq_classes$raw_n > 0])
    )
  )
  
  return(result)
  
}
