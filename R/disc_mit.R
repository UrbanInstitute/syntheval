#' Nearest synthetic record distance for each blended record
#'
#' @param blended_data Confidential and holdout records, with a `source` column.
#' @param synth_data A data frame with synthetic data.
#' @param blocked_variables Optional character vector of variables that must
#' match exactly. They are excluded from the Gower distance.
#' @return A numeric vector with one Gower distance per row of `blended_data`.
#' A record whose block has no synthetic records gets the maximum distance, 1.
#'
#' @noRd
.nn_mi_distances <- function(blended_data, synth_data, blocked_variables = NULL) {

  blended_data <- dplyr::select(blended_data, -source)

  if (is.null(blocked_variables)) {

    distances <- gower::gower_topn(x = blended_data, y = synth_data, n = 1)$distance[1, ]

  } else {

    anchors <- .nn_mi_range_anchors(blended_data, synth_data, blocked_variables)

    # nesting by the blocked columns pairs each blended block with the synthetic
    # block that has identical values; NA only matches NA
    # and no separator can make two different values look the same
    blocks <- blended_data |>
      dplyr::mutate(.row = dplyr::row_number()) |>
      dplyr::nest_by(dplyr::pick(dplyr::all_of(blocked_variables)), .key = "blended") |>
      dplyr::left_join(
        dplyr::nest_by(synth_data, dplyr::pick(dplyr::all_of(blocked_variables)), .key = "synth"),
        by = blocked_variables
      )

    block_distances <- purrr::map2(
      .x = blocks$blended, 
      .y = blocks$synth, 
      .f = .nn_mi_block_distances,
      anchors = anchors
    )

    # nest_by() changes the order of the rows, so sorting on .row restores the
    # original row order that the scoring step expects
    distances <- block_distances |>
      dplyr::bind_rows() |>
      dplyr::arrange(.row) |>
      dplyr::pull(distance)

  }

  # Gower can return non-finite distances when no variables have usable ranges.
  distances[!is.finite(distances)] <- 1

  distances

}

#' Two rows holding the global minimum and maximum of each numeric variable
#'
#' @param blended_data Blended records, without the `source` column.
#' @param synth_data A data frame with synthetic data.
#' @param blocked_variables Character vector of variables used for blocking.
#'
#' @return A two-row data frame of the numeric variables used for Gower
#' distance: the first row is each variable's minimum, the second its maximum.
#'
#' @noRd
.nn_mi_range_anchors <- function(blended_data, synth_data, blocked_variables) {

  variables <- setdiff(intersect(names(blended_data), names(synth_data)), blocked_variables)

  # the range has to cover every record, not only those in one block
  dplyr::bind_rows(
    dplyr::select(blended_data, dplyr::all_of(variables)),
    dplyr::select(synth_data, dplyr::all_of(variables))
  ) |>
    dplyr::reframe(dplyr::across(dplyr::where(is.numeric), \(x) range(x, na.rm = TRUE)))

}

#' Nearest synthetic record distance within one block
#'
#' @param blended Blended records in the block, with a `.row` column.
#' @param synth Synthetic records in the block, or `NULL` if there are none.
#' @param anchors Rows from `.nn_mi_range_anchors()`.
#'
#' @return A tibble with one row per row of `blended`: its `.row` and its
#' nearest-neighbor `distance`.
#'
#' @noRd
.nn_mi_block_distances <- function(blended, synth, anchors) {

  # blocked variables were removed by nest_by(), so only the variables used for
  # Gower distance remain; .row is bookkeeping and must not enter the distance
  shared <- intersect(setdiff(names(blended), ".row"), names(synth))

  if (is.null(synth)) {

    # no synthetic records share this block
    distance <- rep(1, nrow(blended))

  } else if (length(shared) == 0) {

    # every variable was used for blocking, so the block matches exactly
    distance <- rep(0, nrow(blended))

  } else {

    # gower_topn() scales numeric variables by the range in each call, so the
    # anchor rows give every block the same global range; their distances are dropped
    x <- dplyr::bind_rows(dplyr::select(anchors, dplyr::any_of(shared)), blended[shared])

    distance <- gower::gower_topn(x = x, y = synth[shared], n = 1)$distance[1, ]
    distance <- utils::tail(distance, n = nrow(blended))

  }

  tibble::tibble(.row = blended$.row, distance = distance)

}

#' Score nearest-neighbor distances as a membership inference test
#'
#' @param blended_data Confidential and holdout records, with a `source` column,
#' one row per element of `distances`.
#' @param distances A numeric vector of nearest-neighbor distances.
#' @param threshold_percentile A number in [0, 1].
#' @param keep_row_level Boolean for whether to also return the row-level
#' results and ROC curve.
#'
#' @return See [disc_mit()].
#'
#' @noRd
.nn_mi_score <- function(blended_data, distances, threshold_percentile, keep_row_level) {

  # convert distances into predictions for if the record from the blended data
  # was used to train the synthetic data
  threshold <- stats::quantile(distances, probs = threshold_percentile)

  prediction <- ifelse(distances <= threshold, "training", "holdout")

  max_distance <- max(distances)
  # a tie at zero has no ranking information, so all records receive the same score.
  pseudo_probabilities <- if (max_distance == 0) {
    rep(0.5, length(distances))
  } else {
    1 - (distances / max_distance)
  }

  blended_data <- dplyr::bind_cols(
    blended_data,
    distance = distances,
    pseudo_probability = pseudo_probabilities,
    prediction = prediction
  ) |>
    dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))

  membership_metrics <- list(
    precision = yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate,
    recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
    auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
    conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
  )

  # both are as large as the number of blended records, so they are opt-in
  if (keep_row_level) {

    membership_metrics$results <- blended_data

    membership_metrics$roc <- yardstick::roc_curve(
      data = blended_data,
      truth = tidyselect::all_of("source"),
      tidyselect::all_of("pseudo_probability")
    )

  }

  membership_metrics

}

#' Run a nearest-neighbor membership inference test 
#'
#' @param eval_data An `eval_data` object.
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data. If the 
#' threshold_percentile is not provided, the function calculates it with the 
#' following formula: `nrow(data) / (nrow(data) + nrow(holdout_data))`
#' @param keep_row_level Boolean. If `FALSE`, the default, only the summary
#' metrics are returned. If `TRUE`, the row-level `results` tibble and the `roc`
#' curve points are also returned; both have one row per scored record, so they
#' can be large.
#' @param blocked_variables An optional character vector of variable names that
#' must match exactly. Each confidential or holdout record is only compared to
#' synthetic records with the same values of these variables, and the variables
#' are excluded from the Gower distance. A record with no matching synthetic
#' records is assigned the maximum distance of 1.
#'
#' @return A list with `precision`, `recall`, `auc`, and `conf_mat`. If
#' `keep_row_level = TRUE`, the list also contains `results`, a tibble of each
#' record's distance, pseudo-probability, and prediction, and `roc`, a tibble of
#' ROC curve points.
#' 
#' @family Disclosure risk metrics
#' 
#' @export
#'
disc_mit <- function(eval_data, 
                     threshold_percentile = NULL,
                     keep_row_level = FALSE,
                     blocked_variables = NULL) {

  conf_data <- eval_data[["conf_data"]]
  holdout_data <- eval_data[["holdout_data"]]

  # ROC and threshold-based metrics require examples from both classes.
  if (nrow(conf_data) == 0) {
    stop("error: disc_mit() requires nonempty conf_data", call. = FALSE)
  }

  if (is.null(holdout_data) || nrow(holdout_data) == 0) {
    stop("error: disc_mit() requires nonempty holdout_data", call. = FALSE)
  }
  
  # calculate threshold percentile for when the data are imbalanced
  if (!is.null(threshold_percentile)) {

    # test the threshold percentile
    if (threshold_percentile < 0 || threshold_percentile > 1) {

      stop("error: threshold_percentile must be in [0, 1]")

    }

  } else {

    threshold_percentile <- (
      nrow(eval_data[["conf_data"]]) / (
        nrow(eval_data[["conf_data"]]) + nrow(eval_data[["holdout_data"]])
      )
    )

  }

  # treat a single synthetic dataset as one replicate
  if (eval_data[["n_rep"]] == 1) {

    synths <- list(eval_data[["synth_data"]])

  } else {

    synths <- eval_data[["synth_data"]]

    # identifies each record across the replicates in the disaggregated results
    conf_data <- tibble::rowid_to_column(conf_data, "nn_mi_id")
    holdout_data <- tibble::rowid_to_column(holdout_data, "nn_mi_id")

  }

  # blocking needs the variable in the confidential data and in every replicate
  missing_vars <- setdiff(
    blocked_variables,
    purrr::reduce(c(list(names(conf_data)), purrr::map(synths, names)), intersect)
  )

  if (length(missing_vars) > 0) {

    stop(
      "error: blocked_variables must be present in conf_data and synth_data: ",
      paste(missing_vars, collapse = ", ")
    )

  }

  # combine records from the training data and holdout data
  blended_data <- dplyr::bind_rows(
    training = conf_data,
    holdout = holdout_data,
    .id = "source"
  ) |>
    dplyr::mutate(source = factor(source, levels = c("training", "holdout")))

  # for each record in the blended data, calculate the distance to the closest
  # record in each synthetic dataset
  distances <- purrr::map(
    .x = synths,
    .f = \(synth) .nn_mi_distances(blended_data, synth, blocked_variables)
  ) |>
    unlist(use.names = FALSE)

  # the blended data is repeated once per replicate to match the distances
  blended_data <- dplyr::bind_rows(rep(list(blended_data), length(synths)))

  .nn_mi_score(blended_data, distances, threshold_percentile, keep_row_level)

}


