#' Run a membership inference test
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param holdout_data A data frame with holdout data
#' @param threshold_percentile Distances below the value associated with this 
#' percentile will be predicted as in the training data
#'
#' @return A list with precision, recall, the confusion matrix, and ROC AUC
#' 
#' @export
#'
membership_inference_test <- function(postsynth, data, holdout_data, threshold_percentile = NULL) {

  if (is_postsynth(postsynth)) {

    synthetic_data <- postsynth$synthetic_data

  } else {

    synthetic_data <- postsynth

  }

  # calculate threshold percentile for when the data are imbalanced
  if (is.null(threshold_percentile)) {
    
    threshold_percentile <- nrow(data) / (nrow(data) + nrow(holdout_data))
    
  }

  # combine records from the training data and holdout data
  blended_data <- dplyr::bind_rows(
    training = data,
    holdout = holdout_data,
    .id = "source"
  ) |>
    dplyr::mutate(source = factor(source, levels = c("training", "holdout")))

  # for each record in the blended data, calculate the distance to the closest 
  # record in the synthetic data
  distances <- gower::gower_topn(
    x = dplyr::select(blended_data, -source), 
    y = synthetic_data,
    n = 1
  )

  # convert distances into predictions for if the record from the blended data
  # was used to train the synthetic data
  threshold <- quantile(distances$distance, probs = threshold_percentile)

  prediction <- ifelse(distances$distance[1, ] <= threshold, "training", "holdout")

  pseudo_probabilities <- 1 - (distances$distance[1, ] / max(distances$distance[1, ]))

  blended_data <- dplyr::bind_cols(
    blended_data,
    prediction = prediction,
    pseudo_probability = pseudo_probabilities
  ) |>
    dplyr::mutate(prediction = factor(prediction, levels = c("training", "holdout")))

  precision <- yardstick::precision(blended_data, truth = source, estimate = prediction)$.estimate

  # calculate metrics
  list(
    precision = precision,
    recall = yardstick::recall(blended_data, truth = source, estimate = prediction)$.estimate,
    auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
    conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
  )

}