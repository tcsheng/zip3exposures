devtools::load_all()
library(dplyr)
library(lubridate)

# Create a dataset to test the package
individuals <- tibble(
  id = 1:5,
  zip3 = c("900", "902", "474", "477", "466"),
  diagnosis_date = as.Date(c(
    "2017-06-15",
    "2017-03-20",
    "2017-08-25",
    "2017-04-27",
    "2017-10-30")))

# Load PM2.5 for a year
pm25_2017 <- download_pm25_year(2017)

# Aggregate to ZIP3
pm25_zip3 <- aggregate_pm25_zip3(pm25_2017)

# Compute exposures
final1 <- pm25_before_dx(
  zip3_pm25   = pm25_zip3,
  individuals = individuals,
  window_days = 30)

final1

