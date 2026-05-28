pacman::p_load(fpp3, skimr, tsibble, dplyr, readr) 
BFR <- read_csv("BirthsAndFertilityRatesAnnual.csv")
```


BFR <- BFR |>   
  filter(DataSeries %in% c("Total Fertility Rate (TFR)", "Total Live-Births")) |> 
  mutate(DataSeries = 
           recode(DataSeries,
                  "Total Fertility Rate (TFR)" = "TFR",
                  "Total Live-Births" = "TLB"  )) |>   
  select(DataSeries, `1960`:`2024`) |>   
  mutate(across(`1960`:`2024`, as.numeric)) |>   
  pivot_longer(     
    cols = `1960`:`2024`,     
    names_to = "Year",     
    values_to = "Value") |>   
  mutate(Year = as.integer(Year)) |>   
  pivot_wider(     
    names_from = DataSeries,     
    values_from = Value   ) |>   
  as_tsibble(index = Year)

train <- BFR |> filter(Year <= 2012)
test  <- BFR |> filter(Year >= 2013)


BFR |>
  features(TFR, unitroot_kpss)

BFR |>
  features(TLB, unitroot_kpss)
BFR |>   
  ACF(TFR) |>   
  autoplot() 
BFR |>   
  ACF(TLB) |>   
  autoplot()

BFR |>   
  PACF(TFR) |>   
  autoplot() 
BFR |>   
  PACF(TLB) |>   
  autoplot()

BFR_diff <- BFR |>
  mutate(
    d_TFR = difference(TFR),
    d_TLB = difference(TLB)
  )

BFR_diff |>
  features(d_TFR, unitroot_kpss)

BFR_diff |>
  features(d_TLB, unitroot_kpss)

BFR_diff |>   
  ACF(d_TFR) |>   
  autoplot() 
BFR_diff |>   
  ACF(d_TLB) |>   
  autoplot()

BFR_diff |>   
  PACF(d_TFR) |>   
  autoplot() 
BFR_diff |>   
  PACF(d_TLB) |>   
  autoplot()

BFR |>
  features(log(TFR), unitroot_kpss)

BFR |>
  features(log(TLB), unitroot_kpss) 

BFR_diff2 <- BFR |>
  mutate(
    dd_TFR = difference(TFR, differences = 2),
    dd_TLB = difference(TLB, differences = 2)
  )


BFR_diff2 |>
  features(dd_TFR, unitroot_kpss)

BFR_diff2 |>
  features(dd_TLB, unitroot_kpss) 


BFR_diff2 |>   
  ACF(dd_TFR) |>   
  autoplot() 
BFR_diff2 |>   
  ACF(dd_TLB) |>   
  autoplot()

BFR_diff2 |>   
  PACF(dd_TFR) |>   
  autoplot() 
BFR_diff2 |>   
  PACF(dd_TLB) |>   
  autoplot()


BFR_logdiff <- BFR |>
  mutate(
    d_log_TFR = difference(log(TFR), differences = 1),
    d_log_TLB = difference(log(TLB), differences = 1)
  )

BFR_logdiff |>
  features(d_log_TFR, unitroot_kpss)

BFR_logdiff |>
  features(d_log_TLB, unitroot_kpss)

library(patchwork)

p1 <- BFR_logdiff |>
  ACF(d_log_TFR) |>
  autoplot() +
  labs(title = "ACF of differenced log TFR")

p2 <- BFR_logdiff |>
  ACF(d_log_TLB) |>
  autoplot() +
  labs(title = "ACF of differenced log TLB")

p3 <- BFR_logdiff |>
  PACF(d_log_TFR) |>
  autoplot() +
  labs(title = "PACF of differenced log TFR")

p4 <- BFR_logdiff |>
  PACF(d_log_TLB) |>
  autoplot() +
  labs(title = "PACF of differenced log TLB")

p1 + p3 + p2 + p4

BFR_logdiff |>
  autoplot(d_log_TFR)

BFR_logdiff |>
  autoplot(d_log_TLB)

library(skimr)

skimr::skim(BFR_logdiff)



fit_tfr_base <- train |>
  model(
    tfr_auto_log = ARIMA(log(TFR)),
    tfr_011_log  = ARIMA(log(TFR) ~ pdq(0, 1, 1)),
    tfr_111_log  = ARIMA(log(TFR) ~ pdq(1, 1, 1)),
    tfr_211_log  = ARIMA(log(TFR) ~ pdq(2, 1, 1)),
    tfr_112_log  = ARIMA(log(TFR) ~ pdq(1, 1, 2))
  )

fit_tlb_base <- train |>
  model(
    tlb_auto_log = ARIMA(log(TLB)),
    tlb_011_log  = ARIMA(log(TLB) ~ pdq(0, 1, 1)),
    tlb_111_log  = ARIMA(log(TLB) ~ pdq(1, 1, 1)),
    tlb_211_log  = ARIMA(log(TLB) ~ pdq(2, 1, 1)),
    tlb_112_log  = ARIMA(log(TLB) ~ pdq(1, 1, 2))
  )
report(fit_tfr_base)
report(fit_tlb_base)
fit_tfr_base |>
  glance() |>
  arrange(AICc)

fit_tlb_base |>
  glance() |>
  arrange(AICc)
fit_tfr_base |>
  augment() |>
  features(.innov, ljung_box, lag = 12, dof = 2)

fit_tlb_base |>
  augment() |>
  features(.innov, ljung_box, lag = 12, dof = 2)
fc_tfr_base <- fit_tfr_base |>
  forecast(h = 12)

fc_tlb_base <- fit_tlb_base |>
  forecast(h = 12)

fc_tfr_base |>
  accuracy(test) |>
  arrange(RMSE)
fc_tlb_base |>
  accuracy(test) |>
  arrange(RMSE)

fc_tfr_base |>
  autoplot(BFR, level = NULL) +
  autolayer(test, TFR) +
  labs(title = "Baseline log-ARIMA forecasts for TFR",
       y = "TFR")


fc_tlb_base |>
  autoplot(BFR, level = NULL) +
  autolayer(test, TLB) +
  labs(title = "Baseline log-ARIMA forecasts for TLB",
       y = "TLB")

fit_tfr_base |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residual ACFs for baseline TFR models")


fit_tfr_high <- train |>
  model(
    # Higher-lag ARIMA models on log scale, no constant
    tfr_ar11_log = ARIMA(log(TFR) ~ 0 + pdq(11, 1, 0),
                         order_constraint = TRUE),
    
    tfr_ar12_log = ARIMA(log(TFR) ~ 0 + pdq(12, 1, 0),
                         order_constraint = TRUE),
    
    tfr_ar13_log = ARIMA(log(TFR) ~ 0 + pdq(13, 1, 0),
                         order_constraint = TRUE),
    
    tfr_ar12ma1_log = ARIMA(log(TFR) ~ 0 + pdq(12, 1, 1),
                            order_constraint = TRUE),
    
    tfr_ar13ma1_log = ARIMA(log(TFR) ~ 0 + pdq(13, 1, 1),
                            order_constraint = TRUE),
    
    # SARIMA-style models with period 12, no constant
    tfr_sar_011_100_log = ARIMA(log(TFR) ~ 0 + pdq(0, 1, 1) + PDQ(1, 0, 0, period = 12),
                                order_constraint = TRUE),
    
    tfr_sar_011_001_log = ARIMA(log(TFR) ~ 0 + pdq(0, 1, 1) + PDQ(0, 0, 1, period = 12),
                                order_constraint = TRUE),
    
    tfr_sar_111_100_log = ARIMA(log(TFR) ~ 0 + pdq(1, 1, 1) + PDQ(1, 0, 0, period = 12),
                                order_constraint = TRUE),
    
    tfr_sar_111_001_log = ARIMA(log(TFR) ~ 0 + pdq(1, 1, 1) + PDQ(0, 0, 1, period = 12),
                                order_constraint = TRUE)
  )




fit_tlb_high <- train |>
  model(
    # Higher-lag ARIMA models on log scale, no constant
    tlb_ar11_log = ARIMA(log(TLB) ~ 0 + pdq(11, 1, 0),
                         order_constraint = TRUE),
    
    tlb_ar12_log = ARIMA(log(TLB) ~ 0 + pdq(12, 1, 0),
                         order_constraint = TRUE),
    
    tlb_ar13_log = ARIMA(log(TLB) ~ 0 + pdq(13, 1, 0),
                         order_constraint = TRUE),
    
    tlb_ar12ma1_log = ARIMA(log(TLB) ~ 0 + pdq(12, 1, 1),
                            order_constraint = TRUE),
    
    tlb_ar13ma1_log = ARIMA(log(TLB) ~ 0 + pdq(13, 1, 1),
                            order_constraint = TRUE),
    
    # SARIMA-style models with period 12, no constant
    tlb_sar_011_100_log = ARIMA(log(TLB) ~ 0 + pdq(0, 1, 1) + PDQ(1, 0, 0, period = 12),
                                order_constraint = TRUE),
    
    tlb_sar_011_001_log = ARIMA(log(TLB) ~ 0 + pdq(0, 1, 1) + PDQ(0, 0, 1, period = 12),
                                order_constraint = TRUE),
    
    tlb_sar_111_100_log = ARIMA(log(TLB) ~ 0 + pdq(1, 1, 1) + PDQ(1, 0, 0, period = 12),
                                order_constraint = TRUE),
    
    tlb_sar_111_001_log = ARIMA(log(TLB) ~ 0 + pdq(1, 1, 1) + PDQ(0, 0, 1, period = 12),
                                order_constraint = TRUE)
  )

report(fit_tfr_high)
report(fit_tlb_high)


fit_tlb_high |>
  glance() |>
  arrange(AICc)

fit_tlb_high |>
  glance() |>
  arrange(AICc)

fit_tfr_high |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residual ACFs for higher-lag TFR models")

fit_tlb_high |>
  augment() |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residual ACFs for higher-lag TLB models")

fit_tfr_high |>
  select(tfr_ar12_log) |>
  gg_tsresiduals()

fit_tfr_high |>
  select(tfr_sar_111_100_log) |>
  gg_tsresiduals()

fit_tlb_high |>
  select(tlb_ar12_log) |>
  gg_tsresiduals()

fit_tlb_high |>
  select(tlb_sar_111_100_log) |>
  gg_tsresiduals()

fit_tfr_high |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 0) |>
  arrange(lb_pvalue)


fit_tlb_high |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 0) |>
  arrange(lb_pvalue)

fc_tfr_high <- fit_tfr_high |>
  forecast(h = 12)

fc_tlb_high <- fit_tlb_high |>
  forecast(h = 12)

fc_tfr_high |>
  accuracy(test) |>
  arrange(RMSE)

fc_tlb_high |>
  accuracy(test) |>
  arrange(RMSE)

fc_tfr_high |>
  autoplot(BFR, level = NULL) +
  autolayer(test, TFR) +
  labs(
    title = "Higher-lag log-ARIMA and SARIMA forecasts for TFR",
    y = "TFR"
  )

fc_tlb_high |>
  autoplot(BFR, level = NULL) +
  autolayer(test, TLB) +
  labs(
    title = "Higher-lag log-ARIMA and SARIMA forecasts for TLB",
    y = "TLB"
  )

tfr_model_comparison <- bind_rows(
  fit_tfr_base |>
    glance() |>
    mutate(model_group = "Baseline"),
  fit_tfr_high |>
    glance() |>
    mutate(model_group = "Higher-lag / SARIMA")
) |>
  arrange(AICc)

tfr_model_comparison

tlb_model_comparison <- bind_rows(
  fit_tlb_base |>
    glance() |>
    mutate(model_group = "Baseline"),
  fit_tlb_high |>
    glance() |>
    mutate(model_group = "Higher-lag / SARIMA")
) |>
  arrange(AICc)

tlb_model_comparison

tfr_accuracy_comparison <- bind_rows(
  fc_tfr_base |>
    accuracy(test) |>
    mutate(model_group = "Baseline"),
  fc_tfr_high |>
    accuracy(test) |>
    mutate(model_group = "Higher-lag / SARIMA")
) |>
  arrange(RMSE)

tfr_accuracy_comparison

tlb_accuracy_comparison <- bind_rows(
  fc_tlb_base |>
    accuracy(test) |>
    mutate(model_group = "Baseline"),
  fc_tlb_high |>
    accuracy(test) |>
    mutate(model_group = "Higher-lag / SARIMA")
) |>
  arrange(RMSE)

tlb_accuracy_comparison


