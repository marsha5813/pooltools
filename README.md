# pooltools

`pooltools` provides functions for significance to pool a set of estimates and margins of error such as from Census tabulations.

## Installation

```r
install.packages("devtools")
devtools::install_github("marsha5813/pooltools")
```

## Example
```r
library(tidycensus)
library(tidyverse)
library(pooltools)

# Download ACS data
# Include the variable for the number males and number of males in sales and service occupations
acs <- tidycensus::get_acs(geography = "county", 
                          variables = c("B01001_002", "B24022_028E"), 
                          output = "wide",
                          state = "MD")

# Get list of fips codes that border the coast
coastal_fips <- c("24003", "24005", "24009", "24011", "24015", "24017", 
                 "24019", "24025", "24029", "24035", "24039", "24037", 
                 "24041", "24045", "24047")

# Create a "coastal" variable for the ACS data
acs <- acs %>% 
 mutate(coastal = ifelse(GEOID %in% coastal_fips, 1, 0))

# Test whether counties that border the coast have a higher percentage of males in sales and service upations
poolandcompare(
 data = acs,
 count_var = "B24022_028E", 
 moe_var = "B24022_028M", 
 total_var = "B01001_002E", 
 moe_total = "B01001_002M", 
 group_var = "coastal"
)
```