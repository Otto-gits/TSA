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

BFR_logdiff |>
  ACF(d_log_TFR) |>
  autoplot()

BFR_logdiff |>
  ACF(d_log_TLB) |>
  autoplot()















