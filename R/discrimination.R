#' Combine synthetic data and data for a discriminant based metric
#'
#' @param eval_data An `eval_data` object.
#'
#' @return A list of class `discrimination` with slots `combined_data`,
#' `propensities`, `discriminator`, `discriminator_auc`, `pmse`, and `specks`.
#' Slots other than `combined_data` are `NULL` until populated by the
#' corresponding `add_*()` function. `print()` reports which slots are
#' populated and `summary()` returns all computed metrics as a tibble with
#' columns `.metric`, `.sample`, and `.value`.
#' 
#' @family Utility metrics
#' 
#' @export
#'
discrimination <- function(eval_data) {
  
  stopifnot(is_eval_data(eval_data)) 
  
  if (eval_data$n_rep > 1 ) {
    
    synthetic_data <- eval_data[["synth_data"]][[1]]
    message("Creating discriminator object using 1 synthetic data replicate.")
    
  } else {
    
    synthetic_data <- eval_data[["synth_data"]]
    
  }
  data <- eval_data[["conf_data"]]
  
  
  mismatched_variables <- c(
    setdiff(names(synthetic_data), names(data)),
    setdiff(names(data), names(synthetic_data))
  )
  
  if (length(mismatched_variables) != 0) {
    
    message(
      paste(
        paste(mismatched_variables, collapse = ", "),
        "exists in one data set but not the other. discrimination() will only use common variables for modeling propensities."
      )
    )
    
  }

  ## combine original and synthetic data and add group indicator
  combined_data <- dplyr::bind_rows(
    original = dplyr::select(data, dplyr::any_of(colnames(synthetic_data))),
    synthetic = dplyr::select(synthetic_data, dplyr::any_of(colnames(data))),
    .id = ".source_label"
  ) |>
    dplyr::mutate(.source_label = factor(.data$.source_label, levels = c("synthetic", "original")))
  
  discrimination <- list(
    combined_data = combined_data,
    propensities = NULL,
    discriminator = NULL,
    discriminator_auc = NULL,
    pmse = NULL,
    specks = NULL
  )
  
  discrimination <- structure(discrimination, class = "discrimination")
  
  return(discrimination)
  
}

is_discrimination <- function(x) {
  inherits(x, "discrimination")
}

#' Validate a discrimination object and its required components
#'
#' @param x An object to validate.
#' @param requires A named character vector. Names are slots of the discrimination
#' object that must be non-NULL; values are the functions the user should run to
#' populate them, used in the error message.
#'
#' @return `x`, invisibly.
#'
.validate_discrimination <- function(x, requires = NULL) {

  if (!is_discrimination(x)) {

    stop("`discrimination` must be a discrimination object created by discrimination().")

  }

  for (slot in names(requires)) {

    if (is.null(x[[slot]])) {

      stop(paste0("`discrimination` has no ", slot, ". Run ", requires[[slot]], " first."))

    }

  }

  return(invisible(x))

}

#' Print a discrimination object
#'
#' @param x A `discrimination` object.
#' @param ... Additional arguments passed to or from other methods. Unused.
#'
#' @return `x`, invisibly.
#'
#' @export
print.discrimination <- function(x, ...) {

  n_source <- table(x$combined_data$.source_label)

  status <- function(computed) {

    return(if (computed) "computed" else "not computed")

  }

  cat("Discrimination object\n")

  cat(
    "Combined data:", nrow(x$combined_data), "rows x",
    ncol(x$combined_data) - 1, "variables\n"
  )

  cat("  original:", n_source[["original"]], "  synthetic:", n_source[["synthetic"]], "\n")

  cat(
    "Discriminator:",
    if (is.null(x$discriminator)) "not fitted" else class(x$discriminator)[1],
    "\n"
  )

  cat("pMSE:", status(!is.null(x$pmse)), "\n")
  cat("pMSE ratio:", status(".pmse_ratio" %in% names(x$pmse)), "\n")
  cat("SPECKS:", status(!is.null(x$specks)), "\n")
  cat("Discriminator AUC:", status(!is.null(x$discriminator_auc)), "\n")

  return(invisible(x))

}


#' Summarise the metrics computed on a discrimination object
#'
#' @param object A `discrimination` object.
#' @param ... Additional arguments passed to or from other methods. Unused.
#'
#' @return A `tibble` with columns `.metric`, `.sample`, and `.value`, with one
#' row per computed metric and sample split. Metrics are `pmse`, `null_pmse`,
#' `pmse_ratio`, `specks`, and `discriminator_auc`. The tibble has zero rows
#' if no metrics have been computed.
#'
#' @export
summary.discrimination <- function(object, ...) {

  out <- tibble::tibble(
    .metric = character(),
    .sample = character(),
    .value = double()
  )

  if (!is.null(object$pmse)) {

    pmse_long <- object$pmse |>
      dplyr::mutate(.sample = as.character(.data$.source)) |>
      dplyr::select(-".source") |>
      tidyr::pivot_longer(
        cols = -".sample",
        names_to = ".metric",
        values_to = ".value"
      ) |>
      dplyr::mutate(.metric = sub("^\\.", "", .data$.metric))

    out <- dplyr::bind_rows(out, pmse_long)

  }

  if (!is.null(object$specks)) {

    specks_long <- tibble::tibble(
      .metric = "specks",
      .sample = as.character(object$specks$.source),
      .value = object$specks$.specks
    )

    out <- dplyr::bind_rows(out, specks_long)

  }

  if (!is.null(object$discriminator_auc)) {

    auc_long <- tibble::tibble(
      .metric = "discriminator_auc",
      .sample = as.character(object$discriminator_auc$.sample),
      .value = object$discriminator_auc$.estimate
    )

    out <- dplyr::bind_rows(out, auc_long)

  }

  out <- dplyr::select(out, ".metric", ".sample", ".value")

  return(out)

}
