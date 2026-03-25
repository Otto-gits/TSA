library(tsibble)
library(dplyr)
library(ggplot2)
library(fable)
library(feasts)
mauna <- as_tsibble(co2) |> 
  # use meaningful names
  rename(year_month = index, concentration = value) 

mauna_model_cs <- mauna |> 
  model(
    lm_cs = TSLM(concentration ~ trend() + I(trend()^2) + I(trend()^3) + season()),
  )

tidy(mauna_model_cs)
augment(mauna_model_cs) |> autoplot(.fitted) + 
  geom_point(aes(x=year_month,y=concentration), colour = "red") # add raw data

res <- augment(mauna_model_cs)
res |> 
  ACF(.resid) |> 
  autoplot()

res |> 
  PACF(.resid) |> 
  autoplot()
res |> 
  features(.resid, portmanteau_tests)
mauna_model_cs |> gg_tsresiduals()

res

# I would say this model is a bad fit for the data as the p value is 0 suggesting that the fit is autocorrelated. 

mauna <- mauna |> mutate(D_concentration = difference(concentration, lag = 1))

mauna |> autoplot(D_concentration)

difference(series, lag = s)
mauna <- mauna |> 
  mutate(
    S_concentration = difference(concentration, lag = 12)
  )

mauna |> autoplot(S_concentration)
mauna <- mauna |> mutate(D_concentration = difference(concentration, lag = 1))
mauna <- mauna |> 
  mutate(
    DS_concentration = difference(difference(concentration, lag = 12), lag = 1)
  )

mauna |> autoplot(SD_concentration)
mauna |> 
  ACF(S_concentration) |> 
  autoplot()

mauna |> 
  ACF(SD_concentration) |> 
  autoplot()

mauna |> autoplot(DS_concentration)

mauna |> ACF(DS_concentration) |> autoplot()
mauna |> PACF(DS_concentration) |> autoplot()

mauna |> features(DS_concentration, portmanteau_tests)
