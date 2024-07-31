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
#' A list of 30 samples of lower-risk synthetic data derived from `acs_conf`, 
#' generated using noise infusion for both categorical and numeric random variables.
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
#' A list of 30 samples of higher-risk synthetic data derived from `acs_conf`, 
#' generated using weakly variable resampling and models overfit to the confidential data.
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