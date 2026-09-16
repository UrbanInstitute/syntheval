I want to add metrics for comparing missing data in the confidential, synthetic and holdout data. I want to break this into three questions:

1. How much missing data is there?
2. How is missingness clustered?
3. How does missingness related to other variables?

I propose three functions:

1. `util_na_amount()` to compare the percentage of values missing in each variable.
2. `util_na_cluster()` to compare missingness matrices.
3. `util_na_relate()` to run t tests or logistic regression to see how missingness relates to other variables. 

I will include functionality to allow users to specify a value other than `NA` as a missing value.

## Implementation plan

All three functions will follow the existing `util_*` convention used by
`util_corr_fit()`, `util_co_occurrence()`, and `util_ci_overlap()`: an exported
wrapper `util_na_*(eval_data, ...)` that validates `is_eval_data(eval_data)` and
dispatches over `eval_data$n_rep` (single list vs. `purrr::map` list-of-lists over
synthetic replicates), calling an internal `.util_na_*(conf_data, synth_data,
holdout_data = NULL, ...)` helper that operates on raw data frames. Holdout support
is optional/additive, matching how `attribute_scan()`/`attribute_target()` treat
`eval_data$holdout_data`.

### Phase A - shared helper for custom missing values

- Add `.recode_custom_na(data, na_values = NULL)` to `R/util_na_helper.R`. If
  `na_values` is supplied (scalar or vector), replace matching values with real
  `NA` across all columns (e.g. `dplyr::across(everything(), \(x) replace(x, x %in% na_values, NA))`);
  otherwise return `data` unchanged. All three new functions call this on
  `conf_data`, `synth_data`, and `holdout_data` (if present) before computing
  anything, so a user-specified sentinel (e.g. `-99`, `"Unknown"`) is treated as
  missing consistently.
- Add a short roxygen block (`@param`, `@return`) since this is an internal
  helper shared across the three metrics - no `@export`.
- Add `tests/testthat/test-util_na_helper.R` (or extend it if it already covers
  `convert_na_to_level()`) with a case for a custom `na_values` sentinel and a
  no-op case (`na_values = NULL` returns data unchanged).

### Phase B - `util_na_amount()` (new `R/util_na_amount.R`)

- `.util_na_amount(conf_data, synth_data, holdout_data = NULL, na_values = NULL)`:
  recode custom NAs, compute per-column % missing (using `across()`) for each
  source, and return output as two long tibbles instead of one wide tibble, so
  adding holdout (or more sources later) doesn't add columns:
  - `na_pct`: one row per `variable` x `source` (`source` is
    `"original"`/`"synthetic"`/`"holdout"`), with a single `na_pct` column. Built
    by computing each source's per-column % missing separately and
    `dplyr::bind_rows()`-ing with a `source` column, rather than joining into wide
    columns.
  - `na_pct_diff`: one row per `variable` x `comparison` (`comparison` is
    `"synthetic_v_original"`, and `"holdout_v_original"` when holdout is
    supplied), with a `na_pct_difference` column. Summary scalars
    `na_pct_difference_mae`/`na_pct_difference_rmse` are computed per
    `comparison` and returned as columns on this same tibble (or a small third
    summary tibble keyed by `comparison`), mirroring `util_corr_fit()`'s
    `*_mae`/`*_rmse` convention without duplicating them per variable row.
- `util_na_amount(eval_data, na_values = NULL)`: validate + dispatch as described
  above.
- Add roxygen to `util_na_amount()` following the `util_corr_fit()` template:
  `@param eval_data`, `@param na_values`, `@return` (describe the two output
  tibbles), `@family utility metrics`, `@examples` using
  `eval_data(conf_data = ..., synth_data = ...)`, `@export`. Do not hand-edit
  `NAMESPACE`/`man/*.Rd` - run `devtools::document()` after adding.
- Add `tests/testthat/test-util_na_amount.R` with small inline toy data frames
  (matching `util_corr_fit()`'s test style: identical conf/synth data implies
  `na_pct_difference` is 0 everywhere), a case with injected `NA`s, and a case
  with a custom `na_values` sentinel. Run `devtools::test()` after adding.

### Phase C - `util_na_cluster()` (new `R/util_na_cluster.R`)

Considered a row x variable black/white missingness matrix (like `naniar::vis_miss()`)
for this phase, but rows aren't aligned between confidential/synthetic/holdout (no
record correspondence, and `n` can differ), so two matrices can't be diffed
cell-by-cell into a single `*_difference`/`*_mae`/`*_rmse` metric the way the other
`util_*` functions do. Splitting into a quantitative metric plus a separate plot
(matching how `create_cormat_plot()` in `R/util_plots.R` is a plotting helper
distinct from the numeric `util_corr_fit()` metric) keeps both use cases:

- **Metric** - represents "how is missingness clustered" as co-occurrence of
  missingness across variables. Reuse the approach in `R/util_co_ocurrence.R`, but
  applied to a binary missingness-indicator frame (`dplyr::mutate(across(everything(), is.na))`)
  instead of raw values.
  - `.util_na_cluster(conf_data, synth_data, holdout_data = NULL, na_values = NULL)`:
    build indicator matrices per source, compute a pairwise co-occurrence measure
    for conf and synth (+holdout), then `*_difference`, `*_mae`, `*_rmse` following
    the same naming scheme as `util_corr_fit()`. Only include columns with at
    least one missing value in conf or synth (skip fully-observed columns to avoid
    degenerate all-zero rows/columns) and document this in `@return`.
  - Open question: should the co-occurrence measure be a simple joint-missingness
    proportion (matches `util_co_occurrence()`'s existing style) or a phi
    coefficient (correlation between binary missingness indicators, more standard
    for "clustering" and consistent with `util_corr_fit()`'s correlation framing)?
    Decide before implementing.
  - `util_na_cluster(eval_data, na_values = NULL)`: validate + dispatch as above.
  - Add roxygen (`@param`, `@return` describing the co-occurrence
    matrices/difference/mae/rmse, `@family utility metrics`, `@examples`,
    `@export`) following the `util_corr_fit()` template; run
    `devtools::document()` after adding.
  - Add `tests/testthat/test-util_na_cluster.R`: identical conf/synth toy data
    implies zero difference, plus a case with two columns always missing
    together to confirm co-occurrence is detected, and a custom `na_values`
    case. Run `devtools::test()` after adding.
- **Plot (new `R/util_plots.R` additions)** - the row/column black-and-white view
  for visual inspection, mirroring `create_cormat_plot()` + its eval_data wrapper
  at line 232:
  - `create_na_matrix_plot(data, na_values = NULL)`: recode custom NAs, build a
    long tibble of `row_id`/`variable`/`is_na`, and render with
    `ggplot2::geom_tile()` (white/black fill by `is_na`), one plot per source.
  - `plot_na_matrix(eval_data, na_values = NULL)`: calls `create_na_matrix_plot()`
    on `conf_data`/`synth_data`(/`holdout_data`) and arranges them side-by-side
    with titles ("Confidential data", "Synthetic data", "Holdout data"), same
    pattern as the existing eval_data correlation-matrix plot wrapper.
  - Add roxygen (`@param`, `@return A ggplot2 plot`, `@family utility plots`,
    `@export`) matching `create_cormat_plot()`'s doc style; run
    `devtools::document()` after adding. No `tests/testthat` case needed since
    existing plot helpers in `R/util_plots.R` aren't unit tested.

### Phase D - `util_na_relate()` (new `R/util_na_relate.R`)

- Addresses "how does missingness relate to other variables": for each variable
  with any missingness, create a missingness indicator and test its association
  with every other variable - a t-test (`stats::t.test`, Welch) if the other
  variable is numeric, or logistic regression (`stats::glm(indicator ~ other_var,
  family = binomial)`) if categorical.
- `.util_na_relate(conf_data, synth_data, holdout_data = NULL, na_values = NULL)`:
  returns a long tibble with columns `target_var` (the variable with
  missingness), `related_var`, `method` (`"t_test"`/`"logistic"`),
  `statistic_original`, `statistic_synthetic`, `statistic_difference` (+ holdout
  columns when supplied). Skip `target_var == related_var` and any pair where a
  source has zero or all missing values (can't fit); wrap `t.test`/`glm` calls in
  `tryCatch`, returning `NA` with a `message()` on failure rather than erroring -
  similar to how `util_ci_overlap()` handles model-fit failures.
- Open question: should `statistic_*` report p-values, effect sizes (standardized
  coefficient / Cohen's d), or both? Reporting both lets users distinguish
  "significant" from "large" - decide before implementing.
- `util_na_relate(eval_data, na_values = NULL)`: validate + dispatch as above.
- Add roxygen (`@param`, `@return` describing the long-tibble columns,
  `@family utility metrics`, `@examples`, `@export`) following the
  `util_corr_fit()` template; run `devtools::document()` after adding.
- Add `tests/testthat/test-util_na_relate.R`: a toy case with a clear
  numeric-variable association (t-test path), a categorical-variable
  association (logistic path), and a custom `na_values` case. Run
  `devtools::test()` after adding.




