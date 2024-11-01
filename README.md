

# `syntheval`

<!-- badges: start -->
<!-- badges: end -->

`syntheval` makes it simple to evaluate the utility and disclosure risks
of synthetic data. The package is designed to work any `data.frame`
objects or `postsynth` objects from `tidysynthesis`.

# Package Status and Documentation

This package is still under active development before we release our
first major version, 0.1.0. This will involve API changes and new
functionality. You can keep track of our work in the following issues:

- [Version 0.0.5](https://github.com/UrbanInstitute/syntheval/issues/77)
- [Version 0.0.5](https://github.com/UrbanInstitute/syntheval/issues/78)
- [Version
  0.1.0](https://github.com/UrbanInstitute/syntheval/issues/107)

For detailed documentation, you can see our [documentation
website](https://ui-research.github.io/tidysynthesis-documentation/).

**Note:** `library(tidysynthesis)` is currently under private
development but will be made public in Q1 of 2025.

## Navigation

- [Installation](#installation)
- [Utility Metrics](#utility-metrics)
  - [Proportions](#proportions)
  - [Means and totals](#means-and-totals)
  - [Percentiles](#percentiles)
  - [KS Distance](#ks-distance)
  - [Co-Occurrence](#co-occurrence)
  - [Correlations](#correlations)
  - [Coefficient overlap](#coefficient-overlap)
  - [Discriminant-based metrics](#discriminant-based-metrics)
- [Additional Functionality](#additional-functionality)
  - [Grouping](#grouping)
  - [Weighting](#weighting)

## Installation

``` r
install.packages("remotes")
remotes::install_github("UrbanInstitute/syntheval")
```

## Utility Metrics

``` r
library(tidyverse)
library(syntheval)
```

### Setup

The following examples demonstrate utility and disclosure risk metrics
using synthetic data based on the [Palmer
Penguins](https://allisonhorst.github.io/palmerpenguins/) dataset.
`library(syntheval)` contains three built-in data sets:

- `penguins_conf`: Pre-processed `penguins` data that were passed into
  the synthesizer.
- `penguins_postsynth`: A `postsynth` object synthesized from `penguins`
  using `library(tidysynthesis)`.
- `penguins_syn_df`: A data frame pulled from `penguins_postsynth`. This
  is used to demonstrate how `library(syntheval)` works with output from
  a synthesizer different than `library(tidysynthesis)`.

Functions like `util_proportions()` and `util_moments()` have different
behaviors for `postsynth` objects and data frames. By default, they only
show synthesized variables for `postsynth` objects and show all common
variables for data frames. The `common_vars` and `synth_vars` arguments
can change this behavior.

### Proportions

`util_proportions()` compares the proportions of classes from
categorical variables in the original data and synthetic data.

``` r
util_proportions(
  postsynth = penguins_postsynth, 
  data = penguins_conf
)
```

    # A tibble: 2 × 5
      variable class  synthetic original difference
      <chr>    <fct>      <dbl>    <dbl>      <dbl>
    1 sex      female     0.529    0.495     0.0330
    2 sex      male       0.471    0.505    -0.0330

All common variables are shown when using a data frame.

``` r
util_proportions(
  postsynth = penguins_syn_df, 
  data = penguins_conf
)
```

    # A tibble: 8 × 5
      variable class     synthetic original difference
      <chr>    <fct>         <dbl>    <dbl>      <dbl>
    1 island   Biscoe        0.465    0.489    -0.0240
    2 island   Dream         0.414    0.369     0.0450
    3 island   Torgersen     0.120    0.141    -0.0210
    4 sex      female        0.529    0.495     0.0330
    5 sex      male          0.471    0.505    -0.0330
    6 species  Adelie        0.459    0.438     0.0210
    7 species  Chinstrap     0.234    0.204     0.0300
    8 species  Gentoo        0.306    0.357    -0.0511

### Means and Totals

`util_moments()` compares the counts, means, standard deviations,
skewnesses, and kurtoses of the original data and synthetic data.

``` r
util_moments(
  postsynth = penguins_postsynth, 
  data = penguins_conf
)
```

    # A tibble: 20 × 6
       variable        statistic original synthetic difference proportion_difference
       <fct>           <fct>        <dbl>     <dbl>      <dbl>                 <dbl>
     1 bill_length_mm  count      3.33e+2  333          0                    0      
     2 bill_length_mm  mean       4.40e+1   43.5       -0.502               -0.0114 
     3 bill_length_mm  sd         5.47e+0    5.54       0.0723               0.0132 
     4 bill_length_mm  skewness   4.51e-2    0.0646     0.0195               0.432  
     5 bill_length_mm  kurtosis  -8.88e-1   -0.948     -0.0598               0.0674 
     6 bill_depth_mm   count      3.33e+2  333          0                    0      
     7 bill_depth_mm   mean       1.72e+1   17.3        0.122                0.00712
     8 bill_depth_mm   sd         1.97e+0    1.89      -0.0762              -0.0387 
     9 bill_depth_mm   skewness  -1.49e-1   -0.278     -0.129                0.867  
    10 bill_depth_mm   kurtosis  -8.97e-1   -0.742      0.155               -0.172  
    11 flipper_length… count      3.33e+2  333          0                    0      
    12 flipper_length… mean       2.01e+2  199.        -1.70                -0.00847
    13 flipper_length… sd         1.40e+1   13.9       -0.135               -0.00961
    14 flipper_length… skewness   3.59e-1    0.611      0.253                0.705  
    15 flipper_length… kurtosis  -9.65e-1   -0.704      0.261               -0.270  
    16 body_mass_g     count      3.33e+2  333          0                    0      
    17 body_mass_g     mean       4.21e+3 4162.       -45.0                 -0.0107 
    18 body_mass_g     sd         8.05e+2  783.       -22.3                 -0.0277 
    19 body_mass_g     skewness   4.70e-1    0.655      0.185                0.394  
    20 body_mass_g     kurtosis  -7.40e-1   -0.388      0.353               -0.477  

`util_totals()` is similar to `util_moments()` but looks at counts and
totals.

``` r
util_totals(
  postsynth = penguins_postsynth, 
  data = penguins_conf
)
```

    # A tibble: 8 × 6
      variable         statistic original synthetic difference proportion_difference
      <fct>            <fct>        <dbl>     <dbl>      <dbl>                 <dbl>
    1 bill_length_mm   count         333       333         0                 0      
    2 bill_length_mm   total       14650.    14483.     -167.               -0.0114 
    3 bill_depth_mm    count         333       333         0                 0      
    4 bill_depth_mm    total        5716.     5757.       40.7               0.00712
    5 flipper_length_… count         333       333         0                 0      
    6 flipper_length_… total       66922     66355      -567                -0.00847
    7 body_mass_g      count         333       333         0                 0      
    8 body_mass_g      total     1400950   1385950    -15000                -0.0107 

### Percentiles

`util_percentiles()` compares percentiles from the original data and
synthetic data. The default percentiles are `c(0.1, 0.5, 0.9)` and can
be easily overwritten.

``` r
util_percentiles(
  postsynth = penguins_postsynth, 
  data = penguins_conf,
  probs = c(0.5, 0.8)
)
```

    # A tibble: 8 × 6
          p variable          original synthetic difference proportion_difference
      <dbl> <fct>                <dbl>     <dbl>      <dbl>                 <dbl>
    1   0.5 bill_length_mm        44.5      43.5    -1                   -0.0225 
    2   0.8 bill_length_mm        49.5      49.1    -0.440               -0.00889
    3   0.5 bill_depth_mm         17.3      17.5     0.200                0.0116 
    4   0.8 bill_depth_mm         18.9      19.0     0.0600               0.00317
    5   0.5 flipper_length_mm    197       195      -2                   -0.0102 
    6   0.8 flipper_length_mm    215       214      -1                   -0.00465
    7   0.5 body_mass_g         4050      3950    -100                   -0.0247 
    8   0.8 body_mass_g         4990      4850    -140                   -0.0281 

The functions are designed to work well with `library(ggplot2)`.

``` r
util_percentiles(
  postsynth = penguins_postsynth, 
  data = penguins_conf,
  probs = seq(0.01, 0.99, 0.01)
) |>
  pivot_longer(
    cols = c(original, synthetic),
    names_to = "source",
    values_to = "value"
  ) |>
  ggplot(aes(x = p, y = value, color = source)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free")
```

![](README_files/figure-commonmark/unnamed-chunk-8-1.png)

### KS Distance

`util_ks_distance()` shows the Kolmogorov-Smirnov distance between the
original distribution and synthetic distribution for numeric variables.
The function also returns the point(s) of the maximum distance.

``` r
util_ks_distance(
  postsynth = penguins_syn_df, 
  data = penguins_conf
)
```

    # A tibble: 14 × 3
       variable           value      D
       <chr>              <dbl>  <dbl>
     1 bill_length_mm      38.7 0.0601
     2 bill_depth_mm       16.7 0.0511
     3 bill_depth_mm       16.7 0.0511
     4 bill_depth_mm       16.8 0.0511
     5 bill_depth_mm       16.8 0.0511
     6 flipper_length_mm  196.  0.0781
     7 flipper_length_mm  196.  0.0781
     8 flipper_length_mm  197.  0.0781
     9 flipper_length_mm  197.  0.0781
    10 flipper_length_mm  197.  0.0781
    11 body_mass_g       4359.  0.0480
    12 body_mass_g       4370.  0.0480
    13 body_mass_g       4381.  0.0480
    14 body_mass_g       4392.  0.0480

### Co-Occurrence

`util_co_occurrence()` differences the lower triangles of co-occurrence
matrices calculated on numeric variables in the original data and
synthetic data.

``` r
co_occurrence <- util_co_occurrence(
  postsynth = penguins_postsynth, 
  data = penguins_conf
)

co_occurrence$co_occurrence_difference
```

                      bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    bill_length_mm                NA            NA                NA          NA
    bill_depth_mm                  0            NA                NA          NA
    flipper_length_mm              0             0                NA          NA
    body_mass_g                    0             0                 0          NA

The function returns the MAE for co-occurrences, which provides a sense
of the median error between the original and synthetic data. The
function also returns the RMSE for co-occurrences, which provides a
sense of the average error between the original data and synthetic data.

``` r
co_occurrence$co_occurrence_difference_mae
```

    [1] 0

``` r
co_occurrence$co_occurrence_difference_rmse
```

    [1] 0

All observations have non-zero `bill_length_mm`, `bill_depth_mm`,
`flipper_length_mm`, and `body_mass_g`. `util_co_occurrence()` is most
useful for economic variables like income and wealth where `0` is a
common value.

### Correlations

`util_corr_fit()` differences the lower triangles of correlation
matrices calculated on numeric variables in the original data and
synthetic data.

``` r
corr_fit <- util_corr_fit(
  postsynth = penguins_postsynth, 
  data = penguins_conf
)

round(corr_fit$correlation_difference, digits = 3)
```

                      bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    bill_length_mm                NA            NA                NA          NA
    bill_depth_mm             -0.069            NA                NA          NA
    flipper_length_mm         -0.003        -0.048                NA          NA
    body_mass_g                0.031        -0.011             -0.08          NA

The function returns the MAE for correlation coefficients, which
provides a sense of the median error between the original and synthetic
data. The function also returns the RMSE for the correlation
coefficients, which provides a sense of the average error between the
original synthetic data.

``` r
corr_fit$correlation_difference_mae
```

    [1] 0.04034565

``` r
corr_fit$correlation_difference_rmse
```

    [1] 0.04922324

### Coefficient Overlap

`util_ci_overlap()` compares a linear regression models estimated on the
original data and synthetic data. `formula` specifies the functional
form of the regression model.

``` r
ci_overlap <- util_ci_overlap(
  postsynth = penguins_postsynth, 
  data = penguins_conf,
  formula = body_mass_g ~ bill_length_mm  + sex
)
```

`$ci_overlap()` summarizes each coefficient including how much the
confidence intervals overlap, if the signs match, and if the statistical
significance matches.

``` r
ci_overlap$ci_overlap 
```

    # A tibble: 3 × 8
      term    overlap coef_diff std_coef_diff sign_match significance_match ss_match
      <chr>     <dbl>     <dbl>         <dbl> <lgl>      <lgl>              <lgl>   
    1 (Inter…   0.963   -27.8         -0.0978 TRUE       TRUE               TRUE    
    2 bill_l…   0.965     0.917        0.138  TRUE       TRUE               TRUE    
    3 sexmale   0.951   -14.0         -0.192  TRUE       TRUE               TRUE    
    # ℹ 1 more variable: sso_match <lgl>

`$coefficient` provides detail for each coefficient and is useful for
data visualization.

``` r
ci_overlap$coefficient
```

    # A tibble: 6 × 8
      source    term        estimate std.error statistic  p.value conf.low conf.high
      <chr>     <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    1 original  (Intercept)    746.     285.        2.62 9.22e- 3    186.     1306. 
    2 original  bill_lengt…     74.0      6.67     11.1  1.51e-24     60.9      87.1
    3 original  sexmale        405.      72.8       5.57 5.43e- 8    262.      548. 
    4 synthetic (Intercept)    718.     264.        2.72 6.77e- 3    200.     1237. 
    5 synthetic bill_lengt…     74.9      6.25     12.0  8.98e-28     62.7      87.2
    6 synthetic sexmale        391.      69.2       5.65 3.42e- 8    255.      527. 

``` r
ci_overlap$coefficient |>
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, color = source)) +
  geom_pointrange(alpha = 0.5, position = position_dodge(width = 0.5)) +
  labs(
    title = "The Synthesizer Recreates the Point Estimates and Confidence Intervals",
    subtitle = "Regression Confidence Interval Overlap"
  )
```

![](README_files/figure-commonmark/unnamed-chunk-16-1.png)

### Discriminant-Based Metrics

Discriminant-based metrics build models to predict if an observation is
original or synthetic and then evaluate those model predictions.
Ideally, it should be difficult for a model to distinguish, or
discriminate, between original observations and synthetic observations.

- Any classification model that generates probabilities from
  `library(tidymodels)` can be used to generate propensities (the
  estimated probability than observation is synthetic).
- By default, the code creates a training/testing split and returns
  separate metrics for each split. This can be turned off with
  `split = FALSE`.
- The code can handle hyperparameter tuning.
- After calculating propensities, the code can calculate ROC AUC,
  SPECKS, pMSE, and pMSE ratio.

#### Example Using Decision Trees

Discriminant-based metrics are built a `discrimination` object created
by `discrimination()`.

``` r
disc1 <- discrimination(postsynth = penguins_postsynth, data = penguins_conf)
```

Next, we use `library(tidymodels)` to specify a model. We recommend the
[tidymodels tutorial](https://www.tidymodels.org/start/) to learn more.

``` r
library(tidymodels)

rpart_rec <- recipe(
  .source_label ~ ., 
  data = disc1$combined_data
)

rpart_mod <- decision_tree(cost_complexity = 0.01) |>
  set_mode(mode = "classification") |>
  set_engine(engine = "rpart")
```

Next, we fit the model to the data to generate predicted probabilities.

``` r
disc1 <- disc1 |>
  add_propensities(
    recipe = rpart_rec,
    spec = rpart_mod
  ) 
```

At this point, we can use

- `add_discriminator_auc()` to add the ROC AUC for the predicted
  probabilities
- `add_specks()` to add SPECKS for the predicted probabilities
- `add_pmse()` to add pMSE for the predicted probabilities
- `add_pmse_ratio(times = 25)` to add the pMSE ratio using the pMSE
  model and 25 bootstrap samples

``` r
disc1 |>
  add_discriminator_auc() |>
  add_specks() |>
  add_pmse() |>
  add_pmse_ratio(times = 25)
```

    $combined_data
    # A tibble: 666 × 8
       .source_label species island    sex    bill_length_mm bill_depth_mm
       <fct>         <fct>   <fct>     <fct>           <dbl>         <dbl>
     1 original      Adelie  Torgersen male             39.1          18.7
     2 original      Adelie  Torgersen female           39.5          17.4
     3 original      Adelie  Torgersen female           40.3          18  
     4 original      Adelie  Torgersen female           36.7          19.3
     5 original      Adelie  Torgersen male             39.3          20.6
     6 original      Adelie  Torgersen female           38.9          17.8
     7 original      Adelie  Torgersen male             39.2          19.6
     8 original      Adelie  Torgersen female           41.1          17.6
     9 original      Adelie  Torgersen male             38.6          21.2
    10 original      Adelie  Torgersen male             34.6          21.1
    # ℹ 656 more rows
    # ℹ 2 more variables: flipper_length_mm <dbl>, body_mass_g <dbl>

    $propensities
    # A tibble: 666 × 10
       .pred_synthetic .source_label .sample  species island    sex   bill_length_mm
                 <dbl> <fct>         <chr>    <fct>   <fct>     <fct>          <dbl>
     1           0.154 original      training Adelie  Torgersen male            39.1
     2           0.154 original      training Adelie  Torgersen fema…           39.5
     3           0.369 original      training Adelie  Torgersen fema…           40.3
     4           0.596 original      testing  Adelie  Torgersen fema…           36.7
     5           0.154 original      training Adelie  Torgersen male            39.3
     6           0.154 original      training Adelie  Torgersen fema…           38.9
     7           0.714 original      training Adelie  Torgersen male            39.2
     8           0.369 original      training Adelie  Torgersen fema…           41.1
     9           0.596 original      testing  Adelie  Torgersen male            38.6
    10           0.4   original      training Adelie  Torgersen male            34.6
    # ℹ 656 more rows
    # ℹ 3 more variables: bill_depth_mm <dbl>, flipper_length_mm <dbl>,
    #   body_mass_g <dbl>

    $discriminator
    ══ Workflow [trained] ══════════════════════════════════════════════════════════
    Preprocessor: Recipe
    Model: decision_tree()

    ── Preprocessor ────────────────────────────────────────────────────────────────
    0 Recipe Steps

    ── Model ───────────────────────────────────────────────────────────────────────
    n= 498 

    node), split, n, loss, yval, (yprob)
          * denotes terminal node

      1) root 498 249 synthetic (0.50000000 0.50000000)  
        2) bill_depth_mm>=16.65 332 153 synthetic (0.53915663 0.46084337)  
          4) bill_length_mm< 34.2 10   1 synthetic (0.90000000 0.10000000) *
          5) bill_length_mm>=34.2 322 152 synthetic (0.52795031 0.47204969)  
           10) bill_length_mm>=42.6 128  49 synthetic (0.61718750 0.38281250)  
             20) flipper_length_mm< 194.5 48  11 synthetic (0.77083333 0.22916667) *
             21) flipper_length_mm>=194.5 80  38 synthetic (0.52500000 0.47500000)  
               42) bill_length_mm< 52.45 73  32 synthetic (0.56164384 0.43835616)  
                 84) bill_length_mm>=44.25 64  25 synthetic (0.60937500 0.39062500)  
                  168) body_mass_g>=4175 27   6 synthetic (0.77777778 0.22222222) *
                  169) body_mass_g< 4175 37  18 original (0.48648649 0.51351351)  
                    338) bill_length_mm< 45.65 8   2 synthetic (0.75000000 0.25000000) *
                    339) bill_length_mm>=45.65 29  12 original (0.41379310 0.58620690) *
                 85) bill_length_mm< 44.25 9   2 original (0.22222222 0.77777778) *
               43) bill_length_mm>=52.45 7   1 original (0.14285714 0.85714286) *
           11) bill_length_mm< 42.6 194  91 original (0.46907216 0.53092784)  
             22) bill_length_mm< 39.65 129  62 synthetic (0.51937984 0.48062016)  
               44) flipper_length_mm>=180.5 121  56 synthetic (0.53719008 0.46280992)  
                 88) bill_length_mm>=36.1 96  41 synthetic (0.57291667 0.42708333)  
                  176) island=Biscoe 29   9 synthetic (0.68965517 0.31034483) *
                  177) island=Dream,Torgersen 67  32 synthetic (0.52238806 0.47761194)  
                    354) bill_length_mm< 38.75 47  19 synthetic (0.59574468 0.40425532) *
                    355) bill_length_mm>=38.75 20   7 original (0.35000000 0.65000000)  
                      710) flipper_length_mm>=190.5 7   2 synthetic (0.71428571 0.28571429) *
                      711) flipper_length_mm< 190.5 13   2 original (0.15384615 0.84615385) *
                 89) bill_length_mm< 36.1 25  10 original (0.40000000 0.60000000) *
               45) flipper_length_mm< 180.5 8   2 original (0.25000000 0.75000000) *
             23) bill_length_mm>=39.65 65  24 original (0.36923077 0.63076923) *
        3) bill_depth_mm< 16.65 166  70 original (0.42168675 0.57831325)  
          6) bill_length_mm>=51.35 10   2 synthetic (0.80000000 0.20000000) *
          7) bill_length_mm< 51.35 156  62 original (0.39743590 0.60256410)  
           14) body_mass_g>=3125 149  62 original (0.41610738 0.58389262)  
             28) body_mass_g< 4387.5 34  15 synthetic (0.55882353 0.44117647)  
               56) bill_depth_mm>=13.95 25   8 synthetic (0.68000000 0.32000000) *
               57) bill_depth_mm< 13.95 9   2 original (0.22222222 0.77777778) *
             29) body_mass_g>=4387.5 115  43 original (0.37391304 0.62608696)  
               58) body_mass_g>=4612.5 104  42 original (0.40384615 0.59615385)  
                116) bill_depth_mm< 14.15 18   6 synthetic (0.66666667 0.33333333) *
                117) bill_depth_mm>=14.15 86  30 original (0.34883721 0.65116279) *
               59) body_mass_g< 4612.5 11   1 original (0.09090909 0.90909091) *
           15) body_mass_g< 3125 7   0 original (0.00000000 1.00000000) *

    $discriminator_auc
    # A tibble: 2 × 4
      .sample  .metric .estimator .estimate
      <fct>    <chr>   <chr>          <dbl>
    1 training roc_auc binary         0.742
    2 testing  roc_auc binary         0.425

    $pmse
    # A tibble: 2 × 4
      .source   .pmse .null_pmse .pmse_ratio
      <fct>     <dbl>      <dbl>       <dbl>
    1 training 0.0466     0.0320        1.46
    2 testing  0.0437     0.0327        1.34

    $specks
    # A tibble: 2 × 2
      .source  .specks
      <fct>      <dbl>
    1 training   0.390
    2 testing    0.143

    attr(,"class")
    [1] "discrimination"

Finally, we can look at variable importance and the decision tree from
our discriminator.

``` r
library(vip)
library(rpart.plot)

disc1$discriminator |> 
  extract_fit_parsnip() |> 
  vip()
```

![](README_files/figure-commonmark/unnamed-chunk-21-1.png)

``` r
disc1$discriminator$fit$fit$fit |>
  prp()
```

    Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    To silence this warning:
        Call prp with roundint=FALSE,
        or rebuild the rpart model with model=TRUE.

![](README_files/figure-commonmark/unnamed-chunk-21-2.png)

#### Example Using Regularized Regression

Let’s repeat the workflow from above with LASSO logistic regression and
hyperparameter tuning.

``` r
# create discrimination
disc2 <- discrimination(postsynth = penguins_postsynth, data = penguins_conf)

# create a recipe that includes 2nd-degree polynomials, dummy variables, and
# standardization
lasso_rec <- recipe(
  .source_label ~ ., 
  data = disc2$combined_data
) |>
  step_poly(all_numeric_predictors(), degree = 2) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

# create the model
lasso_mod <- logistic_reg(
  penalty = tune(), 
  mixture = 1
) |>
  set_engine(engine = "glmnet") |>
  set_mode(mode = "classification")

# create a tuning grid
lasso_grid <- grid_regular(penalty(), levels = 10)

# add the propensities
disc2 <- disc2 |>
  add_propensities_tuned(
    recipe = lasso_rec,
    spec = lasso_mod,
    grid = lasso_grid
  ) 

# calculate metrics
disc2 |>
  add_discriminator_auc() |>
  add_specks() |>
  add_pmse() |>
  add_pmse_ratio(times = 25)
```

    $combined_data
    # A tibble: 666 × 8
       .source_label species island    sex    bill_length_mm bill_depth_mm
       <fct>         <fct>   <fct>     <fct>           <dbl>         <dbl>
     1 original      Adelie  Torgersen male             39.1          18.7
     2 original      Adelie  Torgersen female           39.5          17.4
     3 original      Adelie  Torgersen female           40.3          18  
     4 original      Adelie  Torgersen female           36.7          19.3
     5 original      Adelie  Torgersen male             39.3          20.6
     6 original      Adelie  Torgersen female           38.9          17.8
     7 original      Adelie  Torgersen male             39.2          19.6
     8 original      Adelie  Torgersen female           41.1          17.6
     9 original      Adelie  Torgersen male             38.6          21.2
    10 original      Adelie  Torgersen male             34.6          21.1
    # ℹ 656 more rows
    # ℹ 2 more variables: flipper_length_mm <dbl>, body_mass_g <dbl>

    $propensities
    # A tibble: 666 × 10
       .pred_synthetic .source_label .sample  species island    sex   bill_length_mm
                 <dbl> <fct>         <chr>    <fct>   <fct>     <fct>          <dbl>
     1           0.409 original      training Adelie  Torgersen male            39.1
     2           0.455 original      training Adelie  Torgersen fema…           39.5
     3           0.360 original      testing  Adelie  Torgersen fema…           40.3
     4           0.496 original      training Adelie  Torgersen fema…           36.7
     5           0.420 original      training Adelie  Torgersen male            39.3
     6           0.457 original      training Adelie  Torgersen fema…           38.9
     7           0.539 original      training Adelie  Torgersen male            39.2
     8           0.354 original      training Adelie  Torgersen fema…           41.1
     9           0.483 original      training Adelie  Torgersen male            38.6
    10           0.681 original      testing  Adelie  Torgersen male            34.6
    # ℹ 656 more rows
    # ℹ 3 more variables: bill_depth_mm <dbl>, flipper_length_mm <dbl>,
    #   body_mass_g <dbl>

    $discriminator
    ══ Workflow [trained] ══════════════════════════════════════════════════════════
    Preprocessor: Recipe
    Model: logistic_reg()

    ── Preprocessor ────────────────────────────────────────────────────────────────
    3 Recipe Steps

    • step_poly()
    • step_dummy()
    • step_normalize()

    ── Model ───────────────────────────────────────────────────────────────────────

    Call:  glmnet::glmnet(x = maybe_matrix(x), y = y, family = "binomial",      alpha = ~1) 

       Df %Dev   Lambda
    1   0 0.00 0.036370
    2   3 0.07 0.033140
    3   3 0.18 0.030200
    4   3 0.27 0.027520
    5   3 0.35 0.025070
    6   3 0.41 0.022840
    7   5 0.50 0.020820
    8   5 0.63 0.018970
    9   5 0.75 0.017280
    10  5 0.86 0.015750
    11  5 0.95 0.014350
    12  5 1.02 0.013070
    13  4 1.08 0.011910
    14  4 1.13 0.010850
    15  5 1.16 0.009889
    16  5 1.29 0.009010
    17  6 1.40 0.008210
    18  7 1.50 0.007481
    19  8 1.58 0.006816
    20  8 1.65 0.006210
    21  9 1.72 0.005659
    22 10 1.79 0.005156
    23 10 1.86 0.004698
    24 10 1.91 0.004281
    25 10 1.96 0.003900
    26 10 2.00 0.003554
    27 10 2.04 0.003238
    28 10 2.06 0.002950
    29 10 2.09 0.002688
    30 10 2.11 0.002450
    31 10 2.12 0.002232
    32 11 2.14 0.002034
    33 11 2.16 0.001853
    34 11 2.18 0.001688
    35 11 2.19 0.001538
    36 11 2.20 0.001402
    37 11 2.21 0.001277
    38 11 2.22 0.001164
    39 11 2.22 0.001060
    40 11 2.23 0.000966
    41 11 2.23 0.000880
    42 12 2.24 0.000802
    43 12 2.24 0.000731
    44 12 2.24 0.000666
    45 12 2.24 0.000607
    46 13 2.25 0.000553

    ...
    and 6 more lines.

    $discriminator_auc
    # A tibble: 2 × 4
      .sample  .metric .estimator .estimate
      <fct>    <chr>   <chr>          <dbl>
    1 training roc_auc binary         0.601
    2 testing  roc_auc binary         0.475

    $pmse
    # A tibble: 2 × 4
      .source    .pmse .null_pmse .pmse_ratio
      <fct>      <dbl>      <dbl>       <dbl>
    1 training 0.00732    0.00736       0.996
    2 testing  0.00829    0.00745       1.11 

    $specks
    # A tibble: 2 × 2
      .source  .specks
      <fct>      <dbl>
    1 training   0.157
    2 testing    0.167

    attr(,"class")
    [1] "discrimination"

``` r
# look at variable importance
library(vip)

disc2$discriminator |> 
  extract_fit_parsnip() |> 
  vip()
```

![](README_files/figure-commonmark/unnamed-chunk-22-1.png)

## Additional Functionality

### Grouping

Many utility metrics include a `group_by` argument to group the metrics
by group during calculation. For example, this code calculates moments
by species.

``` r
util_moments(
  postsynth = penguins_postsynth, 
  data = penguins_conf,
  group_by = species
)
```

    # A tibble: 60 × 7
       species   variable       statistic original synthetic difference
       <fct>     <fct>          <fct>        <dbl>     <dbl>      <dbl>
     1 Adelie    bill_length_mm count      146       153          7    
     2 Chinstrap bill_length_mm count       68        78         10    
     3 Gentoo    bill_length_mm count      119       102        -17    
     4 Adelie    bill_length_mm mean        38.8      38.5       -0.294
     5 Chinstrap bill_length_mm mean        48.8      47.3       -1.56 
     6 Gentoo    bill_length_mm mean        47.6      48.0        0.475
     7 Adelie    bill_length_mm sd           2.66      2.93       0.267
     8 Chinstrap bill_length_mm sd           3.34      3.07      -0.273
     9 Gentoo    bill_length_mm sd           3.11      3.40       0.299
    10 Adelie    bill_length_mm skewness     0.156     0.505      0.349
    # ℹ 50 more rows
    # ℹ 1 more variable: proportion_difference <dbl>

### Weighting

Many utility metrics include a `weight_var` argument to use weighted
statistics during calculation. For example, this code weights the
moments by the body weight of the penguins.

``` r
util_moments(
  postsynth = penguins_postsynth, 
  data = penguins_conf,
  weight_var = body_mass_g
)
```

    # A tibble: 20 × 6
       variable        statistic original synthetic difference proportion_difference
       <fct>           <fct>        <dbl>     <dbl>      <dbl>                 <dbl>
     1 bill_length_mm  count      1.40e+6   1.39e+6   -1.5 e+4              -0.0107 
     2 bill_length_mm  mean       4.46e+1   4.41e+1   -4.71e-1              -0.0106 
     3 bill_length_mm  sd         5.38e+0   5.54e+0    1.60e-1               0.0297 
     4 bill_length_mm  skewness  -7.39e-2  -5.74e-2    1.64e-2              -0.222  
     5 bill_length_mm  kurtosis  -7.89e-1  -9.15e-1   -1.26e-1               0.160  
     6 bill_depth_mm   count      1.40e+6   1.39e+6   -1.5 e+4              -0.0107 
     7 bill_depth_mm   mean       1.70e+1   1.71e+1    1.28e-1               0.00754
     8 bill_depth_mm   sd         2.01e+0   1.94e+0   -7.07e-2              -0.0351 
     9 bill_depth_mm   skewness   1.05e-2  -1.37e-1   -1.47e-1             -14.0    
    10 bill_depth_mm   kurtosis  -1.00e+0  -9.17e-1    8.70e-2              -0.0867 
    11 flipper_length… count      1.40e+6   1.39e+6   -1.5 e+4              -0.0107 
    12 flipper_length… mean       2.03e+2   2.01e+2   -1.97e+0              -0.00970
    13 flipper_length… sd         1.44e+1   1.46e+1    1.60e-1               0.0111 
    14 flipper_length… skewness   1.48e-1   3.99e-1    2.51e-1               1.70   
    15 flipper_length… kurtosis  -1.14e+0  -1.04e+0    1.08e-1              -0.0940 
    16 body_mass_g     count      1.40e+6   1.39e+6   -1.5 e+4              -0.0107 
    17 body_mass_g     mean       4.36e+3   4.31e+3   -5.19e+1              -0.0119 
    18 body_mass_g     sd         8.26e+2   8.17e+2   -9.84e+0              -0.0119 
    19 body_mass_g     skewness   2.69e-1   4.64e-1    1.95e-1               0.724  
    20 body_mass_g     kurtosis  -9.70e-1  -7.26e-1    2.44e-1              -0.251  

Most commonly, `weight_var` is used when synthesizing data from surveys.

## Getting Help

Contact <a href="mailto:awilliams@urban.org">Aaron R. Williams</a> with
feedback or questions.
