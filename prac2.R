library(tsibble)
library(dplyr)
library(fable)
library(feasts)   # <- this provides portmanteau_tests

mauna <- as_tsibble(co2) |> 
  rename(year_month = index, concentration = value)

mauna |> features(concentration, portmanteau_tests)

mauna_lm <- mauna |> model(lm = TSLM(concentration ~ trend()))

# some summaries
tidy(mauna_lm)
glance(mauna_lm)

# get fitted, residuals, ... and augment to raw data
mauna_fit <- augment(mauna_lm)
