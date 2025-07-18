library(data.table)
library(dplyr)
library(ggplot2)
library(fixest)

files <- c("~/Downloads/2016.q1-q4.singlefile.csv",
           "~/Downloads/2017.q1-q4.singlefile.csv",
           "~/Downloads/2018.q1-q4.singlefile.csv",
           "~/Downloads/2019.q1-q4.singlefile.csv")

list_dfs <- lapply(files, function(f) {
  dt <- fread(f, select = c("area_fips", "industry_code", "own_code", "year", "qtr",
                            "month1_emplvl", "month2_emplvl", "month3_emplvl", "avg_wkly_wage"))
  dt <- dt[industry_code == "23" & own_code == 5]
  dt[, area_fips := as.character(area_fips)]
  dt[, industry_code := as.character(industry_code)]
  return(dt)
})

qcew <- rbindlist(list_dfs)

qcew[, avg_empl := (month1_emplvl + month2_emplvl + month3_emplvl) / 3]

qcew[, log_wage := log(avg_wkly_wage)]
qcew[, log_empl := log(avg_empl)]

treated_counties <- c("48241", "48351", "48373", "48457", "48403", "48407", "48199", 
                      "48245", "48361", "48015", "48039", "48071", "48089", "48157", 
                      "48167", "48201", "48291", "48321", "48339", "48471", "48473", 
                      "48481", "48185", "48021", "48055", "48149", "48287", "48057", 
                      "48123", "48175", "48177", "48239", "48285", "48469", "48255", 
                      "48007", "48025", "48273", "48355", "48391", "48409")

control_counties <- c(
  "40001", "05077","05047", "05067", "05071", "05065", "05055", "05115", "05117",
  "05051", "05041", "05059", "05075", "05061", "05133","40105", "05057", "05135",
  "40085", "05063", "40115", "40129", "05121", "40013", "05129", "05107", "05079", 
  "40117", "05011", "40015", "40137", "05139", "40107", "40133", "05011","40003",
  "05101", "40121", "05103", "05123", "40011", "40017", "05111","05109", "05131", 
  "05009", "40111", "40009", "05105", "40099", "40095", "40087", "40019", "40113", 
  "05119", "40131", "40139", "40091", "40109", "05173", "05007", "05063", "40099"
)

qcew[, treated := ifelse(area_fips %in% treated_counties, 1, 0)]

qcew <- qcew %>%
  filter(area_fips %in% c(treated_counties, control_counties))

qcew <- qcew %>%
  mutate(time_to_treat = (year - 2017) * 4 + (qtr - 3),
         year_qtr = paste0(year, "Q", qtr))

qcew <- qcew[year == 2016 & qtr >= 3 | year == 2017 | year == 2018 | year == 2019]

qcew <- qcew %>%
  filter(avg_wkly_wage > 100, avg_wkly_wage < 2000)

event_model_wages <- feols(
  log_wage ~ i(time_to_treat, treated, ref = 0) | area_fips + year_qtr,
  cluster = ~area_fips,
  data = qcew
)

summary(event_model_wages)

iplot(event_model_wages,
      main = "Hurricane Harvey Impact on Log Wages (2016–2019)",
      xlab = "Quarters Since Harvey (Q3 2017 = 0)",
      ylab = "Difference in Log Wages (Treated vs Control)")

event_model_employment <- feols(
  log_empl ~ i(time_to_treat, treated, ref = 0) | area_fips + year_qtr,
  cluster = ~area_fips,
  data = qcew
)

summary(event_model_employment)

iplot(event_model_employment,
      main = "Hurricane Harvey Impact on Log Employment (2016–2019)",
      xlab = "Quarters Since Harvey (Q3 2017 = 0)",
      ylab = "Difference in Log Employment (Treated vs Control)")