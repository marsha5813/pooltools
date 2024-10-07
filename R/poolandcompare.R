#' Pool counts and MOEs to compare two groups
#'
#' This function pools a set of estimates and MOEs (such as from Census tabulations), splits them via a grouping variable, calculates percentages, and compares the percentages with significant test. 
#'
#' @param data A dataframe containing the data.
#' @param count_var A character string representing the column name for the count variable.
#' @param moe_var A character string representing the column name for the margin of error (MOE) of the count variable.
#' @param total_var A character string representing the column name for the total count variable (denominator).
#' @param moe_total A character string representing the column name for the margin of error of the total count variable, or 0 if the total is a Census count. Default is 0.
#' @param group_var A character string for a variable that splits the data into two groups. Must be binary and code for 0 and 1.
#'
#' @return A list containing the percent values, z-score, p-value, and significance of the test.
#' @import dplyr
#' @examples
#' library(tidycensus)
#' library(tidyverse)
#' library(pooltools)
#' 
#' # Download ACS data
#' # Include the variable for the number males and number of males in sales and service occupations
#' acs <- tidycensus::get_acs(geography = "county", 
#'                            variables = c("B01001_002", "B24022_028E"), 
#'                            output = "wide",
#'                            state = "MD")
#' 
#' # Get list of fips codes that border the coast
#' coastal_fips <- c("24003", "24005", "24009", "24011", "24015", "24017", 
#'                   "24019", "24025", "24029", "24035", "24039", "24037", 
#'                   "24041", "24045", "24047")
#' 
#' # Create a "coastal" variable for the ACS data
#' acs <- acs %>% 
#'   mutate(coastal = ifelse(GEOID %in% coastal_fips, 1, 0))
#' 
#' # Test whether counties that border the coast have a higher percentage of males in sales and service occupations
#' poolandcompare(
#'   data = acs,
#'   count_var = "B24022_028E", 
#'   moe_var = "B24022_028M", 
#'   total_var = "B01001_002E", 
#'   moe_total = "B01001_002M", 
#'   group_var = "coastal"
#' )
#' 
#' @export
poolandcompare <- function(data, count_var, moe_var, total_var, moe_total = 0, group_var) {

  # Check if moe_total is a column name or zero
  if (moe_total == 0) {
    data$moe_total <- 0
  } else {
    data$moe_total <- data[[moe_total]]
  }
  
  # Summarize the data to get counts, MOEs, and percentages
  summary_data <- data |> 
    group_by(!!sym(group_var)) |> 
    summarize(
      count = sum(!!sym(count_var), na.rm = TRUE),
      se = sqrt(sum((!!sym(moe_var) / 1.645)^2)),  # Calculate SE for the sum
      moe = se * 1.645,  # Calculate MOE for the sum
      se_total = sqrt(sum((moe_total / 1.645)^2)),  
      total = sum(!!sym(total_var), na.rm = TRUE),
      percent = (count / total) * 100,
      se_percent = (1 / total) * sqrt(se^2 - (count^2 / total^2) * se_total^2) * 100,
      moe_percent = se_percent * 1.645,
      .groups = "drop"
    )
  
  # Separate the results into two variables for easy reference
  group_nd <- summary_data |> filter(!!sym(group_var) == 0)
  group_d <- summary_data |> filter(!!sym(group_var) == 1)
  
  # Calculate the difference in percentages
  diff_percent <- group_d$percent - group_nd$percent
  
  # Calculate the standard error of the difference using pooled variance
  se_diff <- sqrt(group_nd$se_percent^2 + group_d$se_percent^2)
  
  # Calculate the z-score for the difference in percentages
  z_score <- diff_percent / se_diff
  
  # Calculate p-value
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Add results to the summary data
  summary_data <- summary_data |>
    mutate(
      z_score = round(z_score, 3),
      p_value = round(p_value, 3)
    )
  
  # Return results as a list
  return(list(
    group_nd_percent = round(group_nd$percent, 1),
    group_d_percent = round(group_d$percent, 1),
    diff_percent = round(diff_percent, 1),
    se_diff = round(se_diff, 3),
    moe_diff = round(se_diff * 1.645, 3),
    z_score = round(z_score, 3),
    p_value = round(p_value, 3),
    summary_data = summary_data,
    significance = ifelse(p_value < 0.05, "Significant", "Not Significant")
  ))
}
