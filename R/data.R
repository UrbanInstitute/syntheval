#' American Community Survey microdata
#'
#' An extract constructed from the 2019 American Community Survey containing a 
#' random sample of n = 1000 Nebraska respondents.
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_conf`
#' A data frame with 1,000 rows and 11 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{hcovany}{fct, health insurance status}
#'   \item{empstat}{fct, employment status}
#'   \item{classwkr}{fct, employment kind (ex: self-employed, etc.)}
#'   \item{age}{dbl, age (in years)}
#'   \item{famsize}{dbl, household/family size}
#'   \item{transit_time}{dbl, transit time to work (in minutes)}
#'   \item{inctot}{dbl, annual income}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_conf"

#' American Community Survey lower-risk synthetic data
#'
#' A list of 30 samples of synthetic data derived from `acs_conf`, 
#' generated using noise infusion for both categorical and numeric random variables. 
#' These are referred to as "lower-risk" relative to the "higher-risk" synthetic data
#' also available in this package; the synthetic data is purely for testing purposes.
#' 
#' Categorical random variables are selected by resampling from a mixture of the 
#' original multivariate cell proportions and a uniform mixture. Numeric random 
#' variables are first modelled using regression trees, and new sampled values
#' each have additional discrete two-sided geometric noise added to them. 
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_lr_synths`
#' A list of 30 data frames with 1,000 rows and 11 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{hcovany}{fct, health insurance status}
#'   \item{empstat}{fct, employment status}
#'   \item{classwkr}{fct, employment kind (ex: self-employed, etc.)}
#'   \item{age}{dbl, age (in years)}
#'   \item{famsize}{dbl, household/family size}
#'   \item{transit_time}{dbl, transit time to work (in minutes)}
#'   \item{inctot}{dbl, annual income}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_lr_synths"


#' American Community Survey higher-risk synthetic data
#'
#' A list of 30 samples of partial synthetic data derived from `acs_conf`, 
#' generated using models that intentionally overfit to the confidential data. 
#' These are referred to as "higher-risk" relative to the "lower-risk" synthetic 
#' data also available in this package; the synthetic data is purely for testing purposes.
#' 
#' Categorical variables are primarily kept "as-is" in this partially synthetic data,
#' with a small proportion of categorical records resampled from the data. Numeric
#' variables are resampled from decision tree models that are intentionally designed
#' to overfit to the confidential data.
#' 
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_hr_synths`
#' A list of 30 data frames with 1,000 rows and 11 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{hcovany}{fct, health insurance status}
#'   \item{empstat}{fct, employment status}
#'   \item{classwkr}{fct, employment kind (ex: self-employed, etc.)}
#'   \item{age}{dbl, age (in years)}
#'   \item{famsize}{dbl, household/family size}
#'   \item{transit_time}{dbl, transit time to work (in minutes)}
#'   \item{inctot}{dbl, annual income}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_hr_synths"