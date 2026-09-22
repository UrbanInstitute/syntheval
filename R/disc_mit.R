#' Find the nearest synthetic record for each blended record using Gower distance
#'
#' @param blended_features A data frame of the variables used for distance
#' calculation from the combined training/holdout data.
#' @param synth_features A data frame of the variables used for distance
#' calculation from the synthetic data.
#' @param chunk_size An optional integer. If provided, `blended_features` is
#' split into row batches of this size so that peak memory use is bounded.
#' Chunking does not change the returned distances.
#'
#' @return A numeric vector of nearest-neighbor Gower distances, one per row
#' of `blended_features`.
#'
#' @noRd
.nn_mi_nearest_distances <- function(blended_features, synth_features, chunk_size = NULL) {
  
  # distance refactor: single call shared by nn_membership_inference() and
  # disc_mit()'s multi-replicate path, replacing their previously duplicated
  # gower_topn() logic
  if (is.null(chunk_size) || nrow(blended_features) <= chunk_size) {
    
    # no chunking requested (or data already small enough): one gower_topn() call
    distances <- gower::gower_topn(
      x = blended_features,
      y = synth_features,
      n = 1
    )
    
    return(distances$distance[1, ])
    
  }
  
  # exact chunking: prepend fixed range anchor rows to every chunk so gower's
  # per-call numeric range normalization matches the unchunked calculation
  range_anchor_rows <- .nn_mi_range_anchor_rows(blended_features, synth_features)
  n_range_anchor_rows <- nrow(range_anchor_rows)
  
  # split into row batches so a single gower_topn() call never sees all rows
  row_chunks <- split(
    seq_len(nrow(blended_features)),
    ceiling(seq_len(nrow(blended_features)) / chunk_size)
  )
  
  unlist(
    purrr::map(
      .x = row_chunks,
      .f = \(idx) {
        
        x <- dplyr::bind_rows(
          range_anchor_rows,
          blended_features[idx, , drop = FALSE]
        )
        
        distances <- gower::gower_topn(
          x = x,
          y = synth_features,
          n = 1
        )
        
        # drop the anchor rows' distances, keeping only this chunk's rows
        distances$distance[1, seq.int(n_range_anchor_rows + 1, ncol(distances$distance))]
        
      }
    ),
    use.names = FALSE
  )
  
}

#' Create numeric range anchors for chunked Gower distance calls
#'
#' @param blended_features A data frame of the variables used for distance
#' calculation from the combined training/holdout data.
#' @param synth_features A data frame of the variables used for distance
#' calculation from the synthetic data.
#'
#' @return A two-row data frame containing the combined minimum and maximum of
#' each numeric column.
#'
#' @noRd
.nn_mi_range_anchor_rows <- function(blended_features, synth_features) {
  
  # gower_topn() rescales each numeric column by the min/max seen in that call,
  # so a chunk missing the global extremes would be normalized differently
  # than the unchunked call; anchor rows pin every chunk to the same range
  numeric_columns <- names(blended_features)[
    vapply(blended_features, is.numeric, logical(1)) &
      vapply(synth_features, is.numeric, logical(1))
  ]
  
  if (length(numeric_columns) == 0) {
    
    return(blended_features[0, , drop = FALSE])
    
  }
  
  anchors <- blended_features[rep(1, 2), , drop = FALSE]
  
  for (column in numeric_columns) {
    
    combined_values <- c(blended_features[[column]], synth_features[[column]])
    combined_range <- range(combined_values, na.rm = TRUE)
    
    if (all(is.finite(combined_range))) {
      
      anchors[[column]] <- combined_range
      
    }
    
  }
  
  anchors
  
}

#' Resolve the variables used for membership inference distance calculation
#'
#' @param conf_data A data frame with a subset of the original data.
#' @param synth_data A data frame with synthetic data.
#' @param variables An optional character vector of variable names to restrict
#' the Gower distance calculation to (for example, quasi-identifiers). Defaults
#' to all variables common to `conf_data` and `synth_data`.
#' @param blocked_variables A character vector of variable names (possibly
#' `character(0)`) that are matched exactly instead of being fed into the
#' Gower distance calculation.
#'
#' @return A character vector of variable names.
#'
#' @noRd
.nn_mi_select_variables <- function(conf_data, synth_data, variables = NULL, blocked_variables = character(0)) {
  
  # variable selection: restricting to a subset (e.g. quasi-identifiers) keeps
  # gower_topn() from paying for columns that aren't relevant to the test
  if (is.null(variables)) {
    
    resolved_variables <- intersect(names(conf_data), names(synth_data))
    
  } else {
    
    missing_vars <- union(
      setdiff(variables, names(conf_data)),
      setdiff(variables, names(synth_data))
    )
    
    if (length(missing_vars) > 0) {
      
      stop(
        "error: variables must be present in both conf_data and synth_data: ",
        paste(missing_vars, collapse = ", ")
      )
      
    }
    
    resolved_variables <- variables
    
  }
  
  # blocked_variables are matched exactly, so they're dropped here to avoid
  # also feeding them into the Gower distance calculation
  setdiff(resolved_variables, blocked_variables)
  
}

#' Validate that blocked variables are present in the confidential and synthetic data
#'
#' @param conf_data A data frame with a subset of the original data.
#' @param synth_data A data frame with synthetic data.
#' @param blocked_variables An optional character vector of variable names that
#' must match exactly before two records are compared with Gower distance.
#'
#' @return A character vector of blocked variable names, or `character(0)` if
#' `blocked_variables` is `NULL`.
#'
#' @noRd
.nn_mi_validate_blocked_variables <- function(conf_data, synth_data, blocked_variables = NULL) {
  
  if (is.null(blocked_variables)) {
    
    return(character(0))
    
  }
  
  missing_vars <- union(
    setdiff(blocked_variables, names(conf_data)),
    setdiff(blocked_variables, names(synth_data))
  )
  
  if (length(missing_vars) > 0) {
    
    stop(
      "error: blocked_variables must be present in both conf_data and synth_data: ",
      paste(missing_vars, collapse = ", ")
    )
    
  }
  
  blocked_variables
  
}

#' Build an exact-match block key from a set of blocking variables
#'
#' @param data A data frame containing `blocked_variables`.
#' @param blocked_variables A character vector of variable names that must
#' match exactly before two records are compared with Gower distance.
#'
#' @return A character vector, one block key per row of `data`.
#'
#' @noRd
.nn_mi_block_key <- function(data, blocked_variables) {
  
  # paste() renders NA as the literal string "NA", so records missing the same
  # blocked_variables still match each other rather than a non-missing value;
  # the \r separator keeps adjacent column values from being confused with
  # each other (e.g. c("1", "23") vs. c("12", "3"))
  do.call(paste, c(dplyr::select(data, dplyr::all_of(blocked_variables)), sep = "\r"))
  
}

#' Find the nearest synthetic record for each blended record within exact-match blocks
#'
#' @param blended_features A data frame of the (non-blocked) variables used
#' for distance calculation from the combined training/holdout data.
#' @param synth_features A data frame of the (non-blocked) variables used for
#' distance calculation from the synthetic data.
#' @param blended_block_key A character vector of block keys, one per row of
#' `blended_features`, from `.nn_mi_block_key()`.
#' @param synth_block_key A character vector of block keys, one per row of
#' `synth_features`, from `.nn_mi_block_key()`.
#' @param chunk_size An optional integer passed through to
#' `.nn_mi_nearest_distances()`.
#'
#' @return A numeric vector of nearest-neighbor Gower distances, one per row
#' of `blended_features`, in the original row order. A blended record whose
#' block has no matching synthetic records is assigned the maximum possible
#' Gower distance (`1`), since no valid within-block comparison exists.
#'
#' @noRd
.nn_mi_blocked_nearest_distances <- function(blended_features,
                                              synth_features,
                                              blended_block_key,
                                              synth_block_key,
                                              chunk_size = NULL) {
  
  if (ncol(blended_features) == 0) {
    
    # every distance variable was consumed by blocking, so records in the
    # same block are an exact match (distance 0) and otherwise maximally
    # distant (distance 1)
    return(as.numeric(blended_block_key %in% synth_block_key))
    
  }
  
  distances <- rep(NA_real_, nrow(blended_features))
  
  # restrict the nearest-neighbor search to synthetic records sharing the
  # same exact blocked_variables values, one block at a time
  for (block in unique(blended_block_key)) {
    
    query_idx <- which(blended_block_key == block)
    match_idx <- which(synth_block_key == block)
    
    if (length(match_idx) == 0) {
      
      # no synthetic record shares this exact block: treat as maximally distant
      distances[query_idx] <- 1
      next
      
    }
    
    distances[query_idx] <- .nn_mi_nearest_distances(
      blended_features = blended_features[query_idx, , drop = FALSE],
      synth_features = synth_features[match_idx, , drop = FALSE],
      chunk_size = chunk_size
    )
    
  }
  
  distances
  
}

#' Validate that sampling strata variables are present in the blended data
#'
#' @param blended_data A data frame combining confidential and holdout records.
#' @param sampling_strata An optional character vector of variable names.
#'
#' @return Invisibly returns `sampling_strata`.
#'
#' @noRd
.nn_mi_validate_sampling_strata <- function(blended_data, sampling_strata) {
  
  if (is.null(sampling_strata)) {
    
    return(invisible(sampling_strata))
    
  }
  
  missing_vars <- setdiff(sampling_strata, names(blended_data))
  
  if (length(missing_vars) > 0) {
    
    stop(
      "error: sampling_strata must be present in conf_data/holdout_data: ",
      paste(missing_vars, collapse = ", ")
    )
    
  }
  
  invisible(sampling_strata)
  
}

#' Take a stratified sample of blended confidential/holdout records
#'
#' @param blended_data A data frame combining confidential and holdout records,
#' with a `source` column identifying which.
#' @param sampling_strata An optional character vector of variable names (in
#' addition to `source`) whose proportions should be preserved in the sample.
#' @param sample_prop A number in (0, 1] giving the proportion of rows to keep
#' within each stratum.
#' @param sampling_seed An optional integer seed for reproducible sampling.
#'
#' @return A data frame with the same columns as `blended_data`, subset to the
#' sampled rows.
#'
#' @noRd
.nn_mi_stratified_sample <- function(blended_data, sampling_strata, sample_prop, sampling_seed = NULL) {
  
  if (sample_prop <= 0 || sample_prop > 1) {
    
    stop("error: sample_prop must be in (0, 1]")
    
  }
  
  # always stratify by source so sampling doesn't distort the training/holdout
  # ratio that the default threshold_percentile is based on
  strata_columns <- union("source", sampling_strata)
  
  if (!is.null(sampling_seed)) {
    
    # snapshot the caller's RNG state so this call has no side effects on
    # unrelated code elsewhere that relies on set.seed()
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    })
    
    set.seed(sampling_seed)
    
  }
  
  blended_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata_columns))) %>%
    dplyr::slice_sample(prop = sample_prop) %>%
    dplyr::ungroup()
  
}

#' Perform a nearest-neighbor membership inference test on one synthetic dataset
#'
#' @param synth_data A dataframe with synthetic data generated from the data input
#' @param conf_data A data frame with a subset of the original data
#' @param holdout_data A dataframe with observations similar to the original but
#' not used to train the synthesizer. The data should have the same variables as
#' postsynth.
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data. If the 
#' threshold_percentile is not provided, the function calculates it with the 
#' following formula: `nrow(data)/(nrow(data) + nrow(holdout_data))`
#' @param summary Boolean if TRUE, returns summary statistics, if FALSE, returns 
#' two disaggregated dataframes of individual distances and ROC curve points.
#' @param variables An optional character vector of variable names (for example,
#' quasi-identifiers) used to calculate Gower distance. Defaults to all
#' variables common to `conf_data`/`holdout_data` and `synth_data`.
#' @param blocked_variables An optional character vector of variable names that
#' must match exactly before two records are compared. Blended records are only
#' compared to synthetic records sharing the same `blocked_variables` values,
#' and these variables are excluded from the Gower distance calculation. A
#' blended record whose block has no matching synthetic records is assigned
#' the maximum possible Gower distance (`1`).
#' @param sampling_strata An optional character vector of variable names (in
#' addition to `source`) whose proportions are preserved when `sample_prop` is
#' used to subsample the blended data.
#' @param sample_prop An optional number in (0, 1]. If provided, only this
#' proportion of blended records (stratified by `source` and
#' `sampling_strata`) are scored, which reduces the number of Gower distance
#' comparisons at the cost of an approximate result.
#' @param sampling_seed An optional integer seed so `sample_prop` sampling is
#' reproducible. Has no effect if `sample_prop` is `NULL`.
#' @param chunk_size An optional integer controlling how many blended records
#' are compared to the synthetic data per `gower::gower_topn()` call. Lowering
#' this bounds peak memory use for large data without changing the result.
#'
#' @return Either a list with precision, recall, the confusion matrix, and ROC AUC 
#' or a list with two disaggregated dataframes (if summary = FALSE). 
#'
nn_membership_inference <- function(
    synth_data, 
    conf_data, 
    holdout_data, 
    threshold_percentile = NULL,
    summary = TRUE,
    variables = NULL,
    blocked_variables = NULL,
    sampling_strata = NULL,
    sample_prop = NULL,
    sampling_seed = NULL,
    chunk_size = NULL
    ) {

  # calculate threshold percentile for when the data are imbalanced
  if (!is.null(threshold_percentile)) {
    
    # test the threshold percentile
    if (threshold_percentile < 0 || threshold_percentile > 1) {
      
      stop("error: threshold_percentile must be in [0, 1]")
      
    }
    
  } else {
    
    threshold_percentile <- nrow(conf_data) / (nrow(conf_data) + nrow(holdout_data))
    
  }
  
  # combine records from the training data and holdout data
  blended_data <- dplyr::bind_rows(
    training = conf_data,
    holdout = holdout_data,
    .id = "source"
  ) %>%
    dplyr::mutate(source = factor(source, levels = c("training", "holdout")))

  # optional stratified sample: scores fewer blended records while keeping
  # source (and any requested sampling_strata) proportions intact
  if (!is.null(sample_prop)) {
    
    .nn_mi_validate_sampling_strata(blended_data, sampling_strata)
    
    blended_data <- .nn_mi_stratified_sample(
      blended_data = blended_data,
      sampling_strata = sampling_strata,
      sample_prop = sample_prop,
      sampling_seed = sampling_seed
    )
    
  }

  # blocked_variables must match exactly, so they're excluded from the Gower
  # distance calculation and instead used to restrict which records are compared
  blocked_variables <- .nn_mi_validate_blocked_variables(conf_data, synth_data, blocked_variables)

  # resolve which variables feed the Gower distance calculation
  variables <- .nn_mi_select_variables(conf_data, synth_data, variables, blocked_variables)

  # for each record in the blended data, calculate the distance to the closest 
  # record in the synthetic data
  if (length(blocked_variables) > 0) {
    
    # restrict comparisons to synthetic records sharing the same exact block
    nearest_distances <- .nn_mi_blocked_nearest_distances(
      blended_features = dplyr::select(blended_data, dplyr::all_of(variables)),
      synth_features = dplyr::select(synth_data, dplyr::all_of(variables)),
      blended_block_key = .nn_mi_block_key(blended_data, blocked_variables),
      synth_block_key = .nn_mi_block_key(synth_data, blocked_variables),
      chunk_size = chunk_size
    )
    
  } else {
    
    nearest_distances <- .nn_mi_nearest_distances(
      blended_features = dplyr::select(blended_data, dplyr::all_of(variables)),
      synth_features = dplyr::select(synth_data, dplyr::all_of(variables)),
      chunk_size = chunk_size
    )
    
  }

  # convert distances into predictions for if the record from the blended data
  # was used to train the synthetic data
  threshold <- stats::quantile(nearest_distances, probs = threshold_percentile)

  prediction <- ifelse(nearest_distances <= threshold, "training", "holdout")

  pseudo_probabilities <- 1 - (nearest_distances / max(nearest_distances))

  blended_data <- dplyr::bind_cols(
    blended_data,
    distance = nearest_distances,
    pseudo_probability = pseudo_probabilities,
    prediction = prediction
  ) %>%
    dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))

  if (summary) {
    
    # calculate metrics
    membership_metrics <- list(
      precision = yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate,
      recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
      auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
      conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
    )
    
    return(membership_metrics)
    
  } else {
    
    # calculate complete ROC 
    roc <- yardstick::roc_curve(
      data = blended_data,
      truth = tidyselect::all_of("source"),
      tidyselect::all_of("pseudo_probability")
    )
    
    return(
      list(
        "results" = blended_data,
        "roc" = roc
      )
    )
    
  }
  
}

#' Run a nearest-neighbor membership inference test 
#'
#' @param eval_data An `eval_data` object.
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data. If the 
#' threshold_percentile is not provided, the function calculates it with the 
#' following formula: `nrow(data) / (nrow(data) + nrow(holdout_data))`
#' @param summary Boolean if TRUE, returns summary statistics, if FALSE, returns 
#' two disaggregated dataframes of individual distances and ROC curve points.
#' @param variables An optional character vector of variable names (for example,
#' quasi-identifiers) used to calculate Gower distance. Defaults to all
#' variables common to the confidential/holdout data and the synthetic data.
#' @param blocked_variables An optional character vector of variable names that
#' must match exactly before two records are compared. Blended records are only
#' compared to synthetic records sharing the same `blocked_variables` values,
#' and these variables are excluded from the Gower distance calculation. A
#' blended record whose block has no matching synthetic records is assigned
#' the maximum possible Gower distance (`1`).
#' @param sampling_strata An optional character vector of variable names (in
#' addition to `source`) whose proportions are preserved when `sample_prop` is
#' used to subsample the blended data.
#' @param sample_prop An optional number in (0, 1]. If provided, only this
#' proportion of blended records (stratified by `source` and
#' `sampling_strata`) are scored, which reduces the number of Gower distance
#' comparisons at the cost of an approximate result.
#' @param sampling_seed An optional integer seed so `sample_prop` sampling is
#' reproducible. Has no effect if `sample_prop` is `NULL`.
#' @param chunk_size An optional integer controlling how many blended records
#' are compared to the synthetic data per `gower::gower_topn()` call. Lowering
#' this bounds peak memory use for large data without changing the result.
#'
#' @return A list with precision, recall, the confusion matrix, and ROC AUC
#' 
#' @family Disclosure risk metrics
#' 
#' @export
#'
disc_mit <- function(eval_data, 
                     threshold_percentile = NULL,
                     summary = TRUE,
                     variables = NULL,
                     blocked_variables = NULL,
                     sampling_strata = NULL,
                     sample_prop = NULL,
                     sampling_seed = NULL,
                     chunk_size = NULL) {
  
  # if single replicate supplied
  if (eval_data[["n_rep"]] == 1) {
    
    return(
      nn_membership_inference(
        synth_data = eval_data[["synth_data"]], 
        conf_data = eval_data[["conf_data"]], 
        holdout_data = eval_data[["holdout_data"]], 
        threshold_percentile = threshold_percentile,
        summary = summary,
        variables = variables,
        blocked_variables = blocked_variables,
        sampling_strata = sampling_strata,
        sample_prop = sample_prop,
        sampling_seed = sampling_seed,
        chunk_size = chunk_size
      )
    ) 
    
  # if multiple replicates supplied
  } else {
    
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
    
    # concatenate synthetic data and add synthesis id
    synths <- purrr::imap(
      .x = eval_data[["synth_data"]],
      .f = \(x, idx) {
        dplyr::mutate(x, synth_id = idx)
      }
    ) 
    
    conf_data_id <- eval_data[["conf_data"]] %>% 
      tibble::rowid_to_column("nn_mi_id")
    
    holdout_data_id <- eval_data[["holdout_data"]] %>%
      tibble::rowid_to_column("nn_mi_id")
    
    blended_data <- dplyr::bind_rows(
      training = conf_data_id,
      holdout = holdout_data_id,
      .id = "source"
    ) %>%
      dplyr::mutate(source = factor(source, levels = c("training", "holdout")))
    
    # optional stratified sample: scores fewer blended records while keeping
    # source (and any requested sampling_strata) proportions intact
    if (!is.null(sample_prop)) {
      
      .nn_mi_validate_sampling_strata(blended_data, sampling_strata)
      
      blended_data <- .nn_mi_stratified_sample(
        blended_data = blended_data,
        sampling_strata = sampling_strata,
        sample_prop = sample_prop,
        sampling_seed = sampling_seed
      )
      
    }
    
    # blocked_variables must match exactly, so they're excluded from the Gower
    # distance calculation and instead used to restrict which records are compared
    blocked_variables <- .nn_mi_validate_blocked_variables(
      eval_data[["conf_data"]],
      eval_data[["synth_data"]][[1]],
      blocked_variables
    )
    
    # resolve which variables feed the Gower distance calculation
    variables <- .nn_mi_select_variables(
      eval_data[["conf_data"]],
      eval_data[["synth_data"]][[1]],
      variables,
      blocked_variables
    )
    
    blended_features <- dplyr::select(blended_data, dplyr::all_of(variables))
    
    # a blended record's block key is the same for every synthetic replicate
    blended_block_key <- if (length(blocked_variables) > 0) {
      .nn_mi_block_key(blended_data, blocked_variables)
    } else {
      NULL
    }
    
    # for each record in the blended data, calculate the distance to the closest 
    # record in each synthetic dataset
    synth_distances <- purrr::map(
      .x = synths, 
      .f = \(.x) { 
        
        if (length(blocked_variables) > 0) {
          
          # restrict comparisons to synthetic records sharing the same exact block
          .nn_mi_blocked_nearest_distances(
            blended_features = blended_features,
            synth_features = dplyr::select(.x, dplyr::all_of(variables)),
            blended_block_key = blended_block_key,
            synth_block_key = .nn_mi_block_key(.x, blocked_variables),
            chunk_size = chunk_size
          )
          
        } else {
          
          .nn_mi_nearest_distances(
            blended_features = blended_features,
            synth_features = dplyr::select(.x, dplyr::all_of(variables)),
            chunk_size = chunk_size
          )
          
        }
        
      }
    ) 
    
    all_distances <- unlist(synth_distances, use.names = FALSE)
    
    pseudo_probabilities <- 1 - (all_distances / max(all_distances))
    
    # convert distances into predictions for if the record from the blended data
    # was used to train the synthetic data
    threshold <- stats::quantile(all_distances, probs = threshold_percentile)
    prediction <- ifelse(all_distances <= threshold, "training", "holdout")
    
    blended_data <- dplyr::bind_cols(
      dplyr::bind_rows(rep(list(blended_data), eval_data[["n_rep"]])),
      distance = all_distances,
      pseudo_probability = pseudo_probabilities,
      prediction = prediction
    ) %>%
      dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))
    
    if (summary) {
      
      # calculate metrics
      membership_metrics <- list(
        precision = yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate,
        recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
        auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
        conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
      )
      
      return(membership_metrics)
      
    } else {
      
      # calculate complete ROC 
      roc <- yardstick::roc_curve(
        data = blended_data,
        truth = tidyselect::all_of("source"),
        tidyselect::all_of("pseudo_probability")
      )
      
      return(
        list(
          "results" = blended_data,
          "roc" = roc
        )
      )
      
    }
    
  }
  
}


