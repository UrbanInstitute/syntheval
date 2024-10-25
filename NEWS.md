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
