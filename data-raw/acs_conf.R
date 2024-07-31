library(ipumsr)

set.seed(20240726)

ddi <- ipumsr::read_ipums_ddi("./data-raw/acs_extract.xml")
data <- ipumsr::read_ipums_micro(ddi, 
                                 data_file = "./data-raw/acs_extract.dat.gz")

acs_conf <- data %>%
  dplyr::transmute(
    county = haven::as_factor(COUNTYFIP) %>%
      forcats::fct_recode(
        "Douglas" = "55", 
        "Lancaster" = "109", 
        "Sarpy" = "153", 
        `NA` = "County not identifiable from public-use data (1950-onward)"
      ), 
    gq = haven::as_factor(GQ) %>%
      forcats::fct_collapse(
        "Household" = c("Households under 1970 definition",
                        "Additional households under 1990 definition",
                        "Additional households under 2000 definition"),
        "Institution" = c("Group quarters--Institutions"),
        "Other GQ" = c("Other group quarters", "Vacant unit", "Fragment")
      ),
    sex = haven::as_factor(SEX) %>% 
      forcats::fct_drop(only = "Missing/blank"),
    marst = haven::as_factor(MARST) %>% 
      forcats::fct_collapse(
        "Married" = c("Married, spouse present", 
                      "Married, spouse absent"),
        "Divorced" = c("Divorced"),
        "Separated" = c("Separated"),
        "Widowed" = c("Widowed"), 
        "Single" = c("Never married/single")
      ) %>%
      forcats::fct_drop(only = "Blank, missing"),
    hcovany = haven::as_factor(HCOVANY),
    empstat = haven::as_factor(EMPSTAT) %>%
      forcats::fct_collapse(
        "Employed" = c("Employed"), 
        "Unemployed" = c("Unemployed"), 
        "Not in labor force" = c("Not in labor force", "N/A")
      ) %>% 
      forcats::fct_drop(only = "Unknown/Illegible"),
    classwkr = haven::as_factor(CLASSWKR) %>%
      forcats::fct_drop(only = "Unknown"),
    age = as.double(AGE),
    famsize = as.double(FAMSIZE),
    transit_time = as.double(TRANTIME),
    inctot = dplyr::if_else(INCTOT == 9999999, NA, as.double(INCTOT)) 
    ) %>% 
  dplyr::slice_sample(n = 1000)
  
usethis::use_data(acs_conf, overwrite = TRUE)
