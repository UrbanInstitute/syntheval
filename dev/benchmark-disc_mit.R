# Benchmark disc_mit() distance calculation: unchunked vs. chunked, and the
# effect of restricting `variables`. Not part of the package or test suite;
# run interactively with `source("dev/benchmark-disc_mit.R")` from the repo
# root after loading the package (e.g. via `devtools::load_all()`).

set.seed(20240607)

make_mixed_data <- function(n, n_numeric = 4, n_categorical = 4, n_levels = 6) {

  numeric_cols <- as.data.frame(
    matrix(rnorm(n * n_numeric), nrow = n, ncol = n_numeric)
  )
  names(numeric_cols) <- paste0("num_", seq_len(n_numeric))

  categorical_cols <- as.data.frame(
    replicate(
      n_categorical,
      sample(paste0("lvl_", seq_len(n_levels)), n, replace = TRUE)
    )
  )
  names(categorical_cols) <- paste0("cat_", seq_len(n_categorical))

  tibble::as_tibble(cbind(numeric_cols, categorical_cols))

}

benchmark_disc_mit <- function(n_conf, n_holdout, n_synth, chunk_sizes = c(NULL, 500, 2000)) {

  conf_data <- make_mixed_data(n_conf)
  holdout_data <- make_mixed_data(n_holdout)
  synth_data <- make_mixed_data(n_synth)

  ed <- eval_data(conf_data = conf_data, synth_data = synth_data, holdout_data = holdout_data)

  results <- lapply(chunk_sizes, \(chunk_size) {

    timing <- system.time(
      out <- disc_mit(ed, summary = FALSE, chunk_size = chunk_size)
    )

    list(
      chunk_size = if (is.null(chunk_size)) NA_integer_ else chunk_size,
      elapsed = unname(timing[["elapsed"]]),
      n_rows = nrow(out$results)
    )

  })

  do.call(rbind.data.frame, results)

}

# example usage:
# devtools::load_all(".")
# print(benchmark_disc_mit(n_conf = 2000, n_holdout = 2000, n_synth = 2000))
#
# to compare the effect of restricting `variables`, time a full-column run
# against a run restricted to a subset of quasi-identifying columns, e.g.:
# ed <- eval_data(conf_data = conf_data, synth_data = synth_data, holdout_data = holdout_data)
# system.time(disc_mit(ed, summary = FALSE))
# system.time(disc_mit(ed, summary = FALSE, variables = c("num_1", "cat_1")))
