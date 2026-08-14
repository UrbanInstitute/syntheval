#' @title Select the variable combinations for the k-marginals metric
#'
#' @description Enumerates all unique k-combinations of the supplied
#' variables and, when the total exceeds `n_marginals`, samples the
#' combinations down to the cap while always retaining combinations that
#' contain a priority variable.
#'
#' @param shared_vars Character vector of variables available for marginals.
#' @param k Scalar order of the k-marginal.
#' @param n_marginals Single integer target maximum for the number of
#' variable combinations, or `Inf` for no cap. All priority combinations are
#' retained even when they alone exceed the target.
#' @param priority_vars Optional character vector of variable names whose
#' combinations always survive sampling. Defaults to `NULL`.
#'
#' @return A character matrix with one row per selected combination and `k`
#' columns.
#'
.select_k_marginal_combos <- function(
  shared_vars,
  k,
  n_marginals,
  priority_vars = NULL
) {
  kmarginals_vars <- t(utils::combn(x = shared_vars, m = k))

  # sample combinations down to n_marginals, always keeping combinations that
  # contain a priority variable
  if (nrow(kmarginals_vars) > n_marginals) {
    is_priority <- apply(
      X = kmarginals_vars,
      MARGIN = 1,
      FUN = \(vars) any(vars %in% priority_vars)
    )

    n_sampled <- min(max(n_marginals - sum(is_priority), 0), sum(!is_priority))

    sampled_rows <- sample(x = which(!is_priority), size = n_sampled)

    kmarginals_vars <- kmarginals_vars[
      sort(c(which(is_priority), sampled_rows)), ,
      drop = FALSE
    ]
  }

  return(kmarginals_vars)
}
