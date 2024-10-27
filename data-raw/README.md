# Test Data Replication

The scripts in this folder reproduce the test data sets written to file
in the `./data` folder.

## Penguins

The objects `penguins_*.rda` are derived from the
`library(palmerpenguins)` dataset, available in its original form on
`CRAN`.

## American Community Survey

The objects `acs_*.rda` are derived from IPUMS extracts from the 2019
American Community Survey (ACS). Citation is available in the `acs_conf`
docstring. To replicate the raw data, you need an IPUMS API key, which
can be requested for free at https://account.ipums.org/api_keys and set
using

    ipumsr::set_ipums_api_key(api_key)
