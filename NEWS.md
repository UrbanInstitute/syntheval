# syntheval 0.1.0 (development)

- Add `attribute_target()` functionality for target attribute inference metrics.
- Add `attribute_scan()` functionality for discovery attribute inference metrics.
* Add `util_k_marginals()` to calculate the k-marginals metric for 1-, 2-, and 3-way marginals, with worst-marginal and worst-cell output, marginal sampling with priority variables, sample weights, a `synth_vars` flag to restrict marginals to synthesized variables, `na.rm` handling of missing values, `group_by` stratification with per-stratum scores, and discretization of numeric variables. (#20)
* Add optional parallelization to the bootstrapped null pMSEs in `add_pmse_ratio()` via `furrr`; enable it with `future::plan()`. Results for a given seed change relative to earlier versions because bootstrap iterations now use parallel-safe L'Ecuyer-CMRG streams. (#113)

# syntheval 0.0.5 (release)

* Remove `util_tails()`
* Update deprecated dplyr code. 
* Move all functions to use `eval_data` (#106).

# syntheval 0.0.4

* Add empirical disclosure risk metrics.
* Add comparison visualization utilities.
* Add `na.rm` functionality to most functions to handle `NA` values.
* Add families to roxygen2 headers.
* Ensure that all functions return ungrouped output.

# syntheval 0.0.3

* Add a README with examples.
* Add flexible system for calculating discriminant-based metrics for utility including the pMSE, pMSE ratio, SPECKS, and discriminator AUC. The code can use most functionality from `library(tidymodels)` to generate propensities.
* Add `common_vars` and `synth_vars` arguments to most utility functions. These control the variables included in the utility metrics. 

# syntheval 0.0.2

## New Features

* Add `util_co_occurrence()` for comparing co-occurrence matrices from the original and synthetic data.
* Add `util_ks_distance()` for calculating the KS distance (D) for each numeric variable in the original and synthetic data.

## Improvements

* Use `reframe()` in `util_percentiles()` instead of `summarize()` to minimize warnings.

# syntheval 0.0.1

## New Features

* Create first numbered version of `library(syntheval)`!
* Add `util_proportions()`, `util_totals()`, `util_moments()`, and `util_percentiles()` for comparing univariate statistics in the original and synthetic data.
* Add `util_corr_fit()` for comparing the correlation matrices from the original and synthetic data.
* Add `util_ci_overlap()` for calculating regression confidence interval overlap.
* Add `pmse()`, `null_pmse()`, and `pmse_ratio()` for calculating one version of the pMSE ratio. 
* Add `disc_mit()` for a basic membership inference test.
