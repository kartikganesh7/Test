# ----------------------------------------------------
# TITLE: Hurricanes and Construction Wages in Texas
# AUTHOR: Kartik Ganesh
# PURPOSE: Estimate causal impact of Hurricane Harvey on construction wages
# ----------------------------------------------------

# 1. Load libraries
library(dplyr)
library(readr)
library(fixest)
library(ggplot2)

# 2. Load 2017 QCEW dataset (all U.S. counties)
qcew <- read_csv("/home/kartik/Downloads/2017.q1-q4.singlefile.csv")

# 3. Filter for Texas counties, Construction sector (NAICS 23), Total ownership (own_code 5)
qcew_tx <- qcew %>%
  filter(substr(area_fips, 1, 2) == "48",
         industry_code == "23",
         own_code == 5)

# 4. Clean & compute additional variables
qcew_tx <- qcew_tx %>%
  mutate(
    year_qtr = paste(year, qtr, sep = "Q"),
    avg_weekly_wage_calc = total_qtrly_wages / (avg_emplvl * 13),
    log_wage = log(avg_weekly_wage_calc),
    county_name = toupper(gsub(" County, TX", "", area_title))
  )

# 5. Define treatment: Counties hit by Hurricane Harvey (August 2017)
treated_counties <- c("ARANSAS", "BRAZORIA", "CALHOUN", "CHAMBERS", "FORT BEND",
                      "GALVESTON", "HARRIS", "JACKSON", "JEFFERSON", "LIBERTY",
                      "MATAGORDA", "NUECES", "ORANGE", "REFUGIO", "SAN PATRICIO",
                      "VICTORIA", "WHARTON")

qcew_tx <- qcew_tx %>%
  mutate(
    treated = ifelse(county_name %in% treated_counties, 1, 0),
    post = ifelse((year == 2017 & qtr >= 3), 1, 0)
  )

# 6. Run DiD regression with fixed effects for county and time
did_model <- feols(log_wage ~ treated * post | area_fips + year_qtr, data = qcew_tx)
summary(did_model)

# 7. Aggregate for visualization
plot_data <- qcew_tx %>%
  group_by(year_qtr, treated) %>%
  summarise(mean_log_wage = mean(log_wage, na.rm = TRUE), .groups = "drop")

# 8. Plot treated vs. control wage trends
ggplot(plot_data, aes(x = year_qtr, y = mean_log_wage, color = factor(treated))) +
  geom_line(size = 1.2) +
  labs(title = "Construction Wages Before and After Hurricane Harvey",
       subtitle = "Treated (Harvey-Affected) vs. Control Counties in Texas",
       x = "Quarter", y = "Log Weekly Wage",
       color = "Treated Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Run the difference-in-differences regression
did_model <- feols(log_wage ~ treated * post | area_fips + year_qtr, data = qcew_tx)

summary(did_model)
