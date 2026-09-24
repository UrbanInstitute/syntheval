#' Combine synthetic data and data for a discriminant based metric
#'
#' @param eval_data An `eval_data` object.
#'
#' @return A list of class `discrimination` with elements `combined_data`,
#' `propensities`, `discriminator`, `discriminator_auc`, `pmse`, and `specks`.
#' Elements other than `combined_data` are `NULL` until populated by the
#' corresponding `add_*()` function. `print()` shows the size of the combined
#' data, the fitted discriminator, and a table of every computed metric.
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

#' Check if object is `discrimination`
#'
#' @param x An object.
#'
#' @return A boolean.
#'
#' @export
is_discrimination <- function(x) {

  return(inherits(x, "discrimination"))

}

#' Validate a discrimination object before an `add_*()` step
#'
#' The discriminant-based metrics are built up in steps: `discrimination()`
#' creates the object, `add_propensities()` fills the `propensities` element,
#' and `add_pmse()`, `add_specks()`, and `add_discriminator_auc()` each read
#' that element to fill their own. `add_pmse_ratio()` in turn reads the `pmse`
#' element. Every element starts as `NULL`, so calling a step before the one it
#' depends on used to fail deep inside dplyr with a message about a missing
#' column. This helper runs first in each `add_*()` function and turns that
#' into an error naming the step the user needs to run.
#'
#' @param x The object passed as `discrimination` to an `add_*()` function.
#' @param requires A named character vector describing the elements this step
#' depends on. Each name is an element that must be non-`NULL` (for example
#' `"propensities"`); each value is the function that fills it (for example
#' `"add_propensities()"`), which is only used in the error message. `NULL`
#' means the step needs nothing beyond a valid `discrimination` object.
#'
#' @return `x`, invisibly, so the call can sit at the top of a function
#' without affecting its return value.
#'
.validate_discrimination <- function(x, requires = NULL) {

  # every add_*() function needs a discrimination object
  if (!is_discrimination(x)) {

    stop("`discrimination` must be a discrimination object created by discrimination().")

  }

  # check each required element in turn; names(requires) are the elements, the
  # values are the hints for the error message
  for (element in names(requires)) {

    if (is.null(x[[element]])) {

      stop(paste0("`discrimination` has no ", element, ". Run ", requires[[element]], " first."))

    }

  }

  return(invisible(x))

}

#' Collect the computed discriminant metrics into one tibble
#'
#' The metrics live in three elements with different shapes: `pmse` and `specks`
#' use a `.source` column for the training/testing split, `discriminator_auc`
#' uses `.sample`, and `add_pmse_ratio()` adds `.null_pmse` and `.pmse_ratio`
#' as extra columns on the `pmse` element rather than an element of its own. This
#' helper normalises all of them to `.metric`, `.sample`, `.value` so that
#' `print.discrimination()` can lay them out in a single table.
#'
#' @param x A `discrimination` object.
#'
#' @return A `tibble` with columns `.metric`, `.label`, `.sample`, and `.value`,
#' one row per computed metric and sample split, in display order. `.label` is
#' the metric name as shown by `print()`. Zero rows if nothing has been
#' computed.
#'
.discrimination_metrics <- function(x) {

  # display order and labels for the discriminant metrics. The order follows
  # the README's discriminant-based metrics example (AUC, SPECKS, pMSE, pMSE
  # ratio), so the printed table does not depend on which add_*() the user
  # ran first
  labels <- c(
    discriminator_auc = "Discriminator AUC",
    specks = "SPECKS",
    pmse = "pMSE",
    null_pmse = "null pMSE",
    pmse_ratio = "pMSE ratio"
  )

  pieces <- list()

  if (!is.null(x$discriminator_auc)) {

    pieces$discriminator_auc <- tibble::tibble(
      .metric = "discriminator_auc",
      .sample = as.character(x$discriminator_auc$.sample),
      .value = x$discriminator_auc$.estimate
    )

  }

  if (!is.null(x$specks)) {

    pieces$specks <- tibble::tibble(
      .metric = "specks",
      .sample = as.character(x$specks$.source),
      .value = x$specks$.specks
    )

  }
  if (!is.null(x$pmse)) {

    # x$pmse has one row per split and one column per pMSE metric: .pmse
    # always, plus .null_pmse and .pmse_ratio once add_pmse_ratio() has run
    pieces$pmse <- x$pmse |>
      # rename the split column .source to .sample to match the AUC element
      dplyr::mutate(.sample = as.character(.data$.source)) |>
      dplyr::select(-".source") |>
      # turn each metric column into rows, however many add_*() steps have
      # added so far
      tidyr::pivot_longer(
        cols = -".sample",
        names_to = ".metric",
        values_to = ".value"
      ) |>
      # column names carry a leading dot (.pmse); metric names do not
      dplyr::mutate(.metric = sub("^\\.", "", .data$.metric))

  }

  # start from an empty tibble with the four columns already present, so the
  # result has the same columns even when nothing has been computed yet.
  # Bind as one flat list: passing the named list `pieces` directly would be
  # read as columns, and unname() stops the list names becoming an .id column
  empty <- tibble::tibble(
    .metric = character(), .label = character(), .sample = character(), .value = double()
  )
  metrics <- dplyr::bind_rows(c(list(empty), unname(pieces)))

  # set the display order: metrics as in `labels`, training before testing
  # within each metric; attach the display label for print()
  metrics <- metrics |>
    dplyr::mutate(
      .metric = factor(.data$.metric, levels = names(labels)),
      .sample = factor(.data$.sample, levels = c("training", "testing", "overall"))
    ) |>
    dplyr::arrange(.data$.metric, .data$.sample) |>
    dplyr::mutate(
      .metric = as.character(.data$.metric),
      .label = unname(labels[.data$.metric]),
      .sample = as.character(.data$.sample)
    ) |>
    dplyr::select(".metric", ".label", ".sample", ".value")

  return(metrics)

}

#' Print a discrimination object
#'
#' Shows the size of the combined data, which discriminator has been fitted,
#' and a table of every metric computed so far with one column per sample
#' split (`training`/`testing`, or `overall` when the metrics were computed
#' with `split = FALSE`). Metrics that have not been computed are omitted.
#'
#' @param x A `discrimination` object.
#' @param ... Additional arguments passed to or from other methods. Unused.
#'
#' @return `x`, invisibly.
#'
#' @export
print.discrimination <- function(x, ...) {

  n_source <- table(x$combined_data$.source_label)

  cat("Discrimination\n")

  cat(
    "Combined data: ", nrow(x$combined_data), " rows x ",
    ncol(x$combined_data) - 1, " columns",
    " (original: ", n_source[["original"]],
    ", synthetic: ", n_source[["synthetic"]], ")\n",
    sep = ""
  )

  # the discriminator element holds a fitted workflow once add_propensities() or
  # add_propensities_tuned() has run; report the model type and engine
  if (is.null(x$discriminator)) {

    cat("Discriminator: not fitted\n")

  } else if (inherits(x$discriminator, "workflow")) {

    spec <- workflows::extract_spec_parsnip(x$discriminator)
    cat("Discriminator: ", class(spec)[1], " (", spec$engine, "), fitted\n", sep = "")

  } else {

    cat("Discriminator: ", class(x$discriminator)[1], "\n", sep = "")

  }

  metrics <- .discrimination_metrics(x)

  if (nrow(metrics) == 0) {

    cat(
      "Metrics: none computed. Use add_discriminator_auc(), add_specks(),",
      "add_pmse(), or add_pmse_ratio().\n"
    )

  } else {

    # one row per metric, one column per sample split, labelled for display;
    # values are formatted to three significant digits each so the columns do
    # not get padded to a common number of decimals
    table <- metrics |>
      dplyr::select(-".metric") |>
      tidyr::pivot_wider(names_from = ".sample", values_from = ".value") |>
      dplyr::mutate(
        dplyr::across(-".label", \(v) formatC(v, digits = 3, format = "g"))
      ) |>
      as.data.frame()

    rownames(table) <- table$.label
    table$.label <- NULL

    cat("\n")
    print(table, right = TRUE)

  }

  return(invisible(x))

}
