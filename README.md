
# syntheval

<!-- badges: start -->
<!-- badges: end -->

The goal of syntheval is to make it simple to evaluate the utility and
disclosure risk of synthetic data. The package works with
`library(tidysynthesis)`.

## Installation

You can install the development version of syntheval by cloning this
respository and building the package in RStudio.

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
    # Groups:   variable [1]
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
    # Groups:   variable [3]
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

`util_totals()` is similar to `util_moments()`, but looks at counts and
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

![](README_files/figure-commonmark/unnamed-chunk-7-1.png)

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
significances matche.

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

![](README_files/figure-commonmark/unnamed-chunk-15-1.png)

### Discriminant Metrics

Discriminant-based metrics build models to predict if an observation is
original or synthetic and then evaluate those model predictions.
Ideally, it should be difficult for a model to distinguish, or
discriminate, between original observations and synthetic observations.

- Any classification model that generates probabilities from
  `library(tidymodels)` can be used to generate propensities (the
  estimated probability than observation is synthetic).
- By default, the code creates a training/testing split. This can be
  turned off with `split = FALSE`.
- The code can handle hyperparameter tuning.
- After calculating propensities, the code can calculate ROC AUC,
  SPECKS, pMSE, and pMSE ratio.

#### Decision Tree

Discriminant-based metrics are built a `discrimination` object created
by `discrimination()`.

``` r
disc1 <- discrimination(postsynth = penguins_postsynth, data = penguins_conf)
```

Next, we use `library(tidymodels)` to specify a model. We recommend the
[tidymodels tutorial](https://www.tidymodels.org/start/) to leanr more.

``` r
library(tidymodels)

rpart_rec <- recipe(
  .source_label ~ ., 
  data = disc1$combined_data
)

rpart_mod <- decision_tree(cost_complexity = 0.01) %>%
  set_mode(mode = "classification") %>%
  set_engine(engine = "rpart")
```

Next, we fit the model to the data to generate predicted probabilities.

``` r
disc1 <- disc1 %>%
  add_propensities(
    recipe = rpart_rec,
    spec = rpart_mod
  ) 
```

At this point, we can use

- `add_discriminator_auc()` to add the ROC AUC for the predicted
  probabilites
- `add_specks()` to add SPECKS for the predicted probabilities
- `add_pmse()` to add pMSE for the predicted probabilitieis
- `add_pmse_ratio(times = 25)` to add the pMSE ratio using the pMSE
  model and 25 bootstrap samples

``` r
disc1 %>%
  add_discriminator_auc() %>%
  add_specks() %>%
  add_pmse() %>%
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
     1           0.765 original      testing  Adelie  Torgersen male            39.1
     2           0.411 original      training Adelie  Torgersen fema…           39.5
     3           0.590 original      training Adelie  Torgersen fema…           40.3
     4           0.679 original      training Adelie  Torgersen fema…           36.7
     5           0.411 original      testing  Adelie  Torgersen male            39.3
     6           0.765 original      testing  Adelie  Torgersen fema…           38.9
     7           0.292 original      training Adelie  Torgersen male            39.2
     8           0.411 original      testing  Adelie  Torgersen fema…           41.1
     9           0.411 original      training Adelie  Torgersen male            38.6
    10           0.35  original      training Adelie  Torgersen male            34.6
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

        1) root 498 249 synthetic (0.5000000 0.5000000)  
          2) bill_length_mm< 52.35 482 237 synthetic (0.5082988 0.4917012)  
            4) body_mass_g< 4962.5 398 188 synthetic (0.5276382 0.4723618)  
              8) body_mass_g>=2912.5 390 182 synthetic (0.5333333 0.4666667)  
               16) bill_length_mm< 34.55 13   3 synthetic (0.7692308 0.2307692) *
               17) bill_length_mm>=34.55 377 179 synthetic (0.5251989 0.4748011)  
                 34) body_mass_g< 3062.5 10   2 synthetic (0.8000000 0.2000000) *
                 35) body_mass_g>=3062.5 367 177 synthetic (0.5177112 0.4822888)  
                   70) flipper_length_mm>=179 359 171 synthetic (0.5236769 0.4763231)  
                    140) flipper_length_mm< 181.5 17   4 synthetic (0.7647059 0.2352941) *
                    141) flipper_length_mm>=181.5 342 167 synthetic (0.5116959 0.4883041)  
                      282) bill_length_mm>=36.1 322 154 synthetic (0.5217391 0.4782609)  
                        564) bill_length_mm< 37.4 28   9 synthetic (0.6785714 0.3214286) *
                        565) bill_length_mm>=37.4 294 145 synthetic (0.5068027 0.4931973)  
                         1130) bill_length_mm>=42.65 175  79 synthetic (0.5485714 0.4514286)  
                           2260) body_mass_g>=3712.5 133  55 synthetic (0.5864662 0.4135338) *
                           2261) body_mass_g< 3712.5 42  18 original (0.4285714 0.5714286)  
                             4522) bill_length_mm< 49.2 33  16 synthetic (0.5151515 0.4848485)  
                               9044) bill_length_mm< 45.35 8   2 synthetic (0.7500000 0.2500000) *
                               9045) bill_length_mm>=45.35 25  11 original (0.4400000 0.5600000)  
                                18090) flipper_length_mm>=195.5 8   2 synthetic (0.7500000 0.2500000) *
                                18091) flipper_length_mm< 195.5 17   5 original (0.2941176 0.7058824) *
                             4523) bill_length_mm>=49.2 9   1 original (0.1111111 0.8888889) *
                         1131) bill_length_mm< 42.65 119  53 original (0.4453782 0.5546218)  
                           2262) body_mass_g< 4262.5 95  46 original (0.4842105 0.5157895)  
                             4524) flipper_length_mm>=191.5 39  16 synthetic (0.5897436 0.4102564) *
                             4525) flipper_length_mm< 191.5 56  23 original (0.4107143 0.5892857) *
                           2263) body_mass_g>=4262.5 24   7 original (0.2916667 0.7083333) *
                      283) bill_length_mm< 36.1 20   7 original (0.3500000 0.6500000) *
                   71) flipper_length_mm< 179 8   2 original (0.2500000 0.7500000) *
              9) body_mass_g< 2912.5 8   2 original (0.2500000 0.7500000) *
            5) body_mass_g>=4962.5 84  35 original (0.4166667 0.5833333)  
             10) bill_length_mm>=51.2 11   2 synthetic (0.8181818 0.1818182) *
             11) bill_length_mm< 51.2 73  26 original (0.3561644 0.6438356) *
          3) bill_length_mm>=52.35 16   4 original (0.2500000 0.7500000) *

    $discriminator_auc
    # A tibble: 2 × 4
      .sample  .metric .estimator .estimate
      <fct>    <chr>   <chr>          <dbl>
    1 training roc_auc binary         0.693
    2 testing  roc_auc binary         0.471

    $pmse
    # A tibble: 2 × 4
      .source   .pmse .null_pmse .pmse_ratio
      <fct>     <dbl>      <dbl>       <dbl>
    1 training 0.0299     0.0341       0.876
    2 testing  0.0304     0.0342       0.890

    $specks
    # A tibble: 2 × 2
      .source  .specks
      <fct>      <dbl>
    1 training  0.309 
    2 testing   0.0952

    attr(,"class")
    [1] "discrimination"

Finally, we can look at variable importance and the decision tree from
our discriminator.

``` r
library(vip)
library(rpart.plot)

disc1$discriminator %>% 
  extract_fit_parsnip() %>% 
  vip()
```

![](README_files/figure-commonmark/unnamed-chunk-20-1.png)

``` r
disc1$discriminator$fit$fit$fit %>%
  prp()
```

    Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    To silence this warning:
        Call prp with roundint=FALSE,
        or rebuild the rpart model with model=TRUE.

![](README_files/figure-commonmark/unnamed-chunk-20-2.png)

#### Regularized Regression

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
) %>%
  step_poly(all_numeric_predictors(), degree = 2) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_predictors())

# create the model
lasso_mod <- logistic_reg(
  penalty = tune(), 
  mixture = 1
) %>%
  set_engine(engine = "glmnet") %>%
  set_mode(mode = "classification")

# create a tuning grid
lasso_grid <- grid_regular(penalty(), levels = 10)

# add the propensities
disc2 <- disc2 %>%
  add_propensities_tuned(
    recipe = lasso_rec,
    spec = lasso_mod,
    grid = lasso_grid
  ) 

# calculate metrics
disc2 %>%
  add_discriminator_auc() %>%
  add_specks() %>%
  add_pmse() %>%
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
     1             0.5 original      training Adelie  Torgersen male            39.1
     2             0.5 original      training Adelie  Torgersen fema…           39.5
     3             0.5 original      training Adelie  Torgersen fema…           40.3
     4             0.5 original      testing  Adelie  Torgersen fema…           36.7
     5             0.5 original      training Adelie  Torgersen male            39.3
     6             0.5 original      testing  Adelie  Torgersen fema…           38.9
     7             0.5 original      training Adelie  Torgersen male            39.2
     8             0.5 original      training Adelie  Torgersen fema…           41.1
     9             0.5 original      training Adelie  Torgersen male            38.6
    10             0.5 original      training Adelie  Torgersen male            34.6
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

       Df %Dev    Lambda
    1   0 0.00 0.0242000
    2   1 0.03 0.0220500
    3   2 0.06 0.0200900
    4   4 0.09 0.0183100
    5   5 0.15 0.0166800
    6   5 0.20 0.0152000
    7   5 0.25 0.0138500
    8   5 0.29 0.0126200
    9   5 0.32 0.0115000
    10  6 0.35 0.0104800
    11  7 0.52 0.0095460
    12  7 0.66 0.0086980
    13  7 0.79 0.0079250
    14  7 0.89 0.0072210
    15  8 0.99 0.0065800
    16  8 1.08 0.0059950
    17  8 1.15 0.0054630
    18  8 1.21 0.0049770
    19  8 1.26 0.0045350
    20  8 1.30 0.0041320
    21  8 1.34 0.0037650
    22  9 1.37 0.0034310
    23  9 1.39 0.0031260
    24  9 1.41 0.0028480
    25 10 1.43 0.0025950
    26 10 1.45 0.0023650
    27 10 1.46 0.0021550
    28 10 1.47 0.0019630
    29 10 1.48 0.0017890
    30 10 1.49 0.0016300
    31 10 1.49 0.0014850
    32 10 1.50 0.0013530
    33 11 1.50 0.0012330
    34 11 1.50 0.0011230
    35 11 1.51 0.0010240
    36 12 1.51 0.0009326
    37 12 1.51 0.0008498
    38 12 1.51 0.0007743
    39 12 1.52 0.0007055
    40 13 1.52 0.0006428
    41 13 1.52 0.0005857
    42 13 1.52 0.0005337
    43 13 1.52 0.0004863

    $discriminator_auc
    # A tibble: 2 × 4
      .sample  .metric .estimator .estimate
      <fct>    <chr>   <chr>          <dbl>
    1 training roc_auc binary           0.5
    2 testing  roc_auc binary           0.5

    $pmse
    # A tibble: 2 × 4
      .source     .pmse .null_pmse .pmse_ratio
      <fct>       <dbl>      <dbl>       <dbl>
    1 training 1.23e-32   1.23e-32           1
    2 testing  1.23e-32   1.23e-32           1

    $specks
    # A tibble: 2 × 2
      .source   .specks
      <fct>       <dbl>
    1 training 4.86e-17
    2 testing  6.94e-17

    attr(,"class")
    [1] "discrimination"

``` r
# look at variable importance
library(vip)

disc2$discriminator %>% 
  extract_fit_parsnip() %>% 
  vip()
```

![](README_files/figure-commonmark/unnamed-chunk-21-1.png)

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

Contact [Aaron R. Williams](awilliams@urban.org) with feedback or
questions.
