
## Falsification Test: FX as Negative-Control Outcome


library(readxl)
library(dplyr)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)
library(scales)
library(purrr)




fx_start_date <- start_date
fx_end_date <- end_date



parse_excel_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXct") | inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  x_num <- suppressWarnings(as.numeric(x))
  as.Date(x_num, origin = "1899-12-30")
}


fx_monthly <- read_data(
  date_col = 18,
  value_col = 19,
  start_row = 4
) |>
  mutate(
    Date = parse_excel_date(date_raw),
    Exchange_Rate = num(value_raw),
    Year = year(Date),
    Month = month(Date),
    YearMonth = yearmonth(Date),
    MonthNumber = Year * 12 + Month,
    MonthLabel = format(Date, "%Y %b")
  ) |>
  filter(
    !is.na(Date),
    !is.na(Exchange_Rate),
    Date >= fx_start_date,
    Date <= fx_end_date
  ) |>
  group_by(YearMonth) |>
  summarise(
    Date = first(Date),
    Year = first(Year),
    Month = first(Month),
    MonthNumber = first(MonthNumber),
    MonthLabel = first(MonthLabel),
    Exchange_Rate = mean(Exchange_Rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    step_paynow = as.integer(
      MonthNumber >= intervention_mnum
    ),
    ramp_paynow = ifelse(
      MonthNumber >= intervention_mnum,
      MonthNumber - intervention_mnum + 1,
      0
    )
  ) |>
  arrange(YearMonth) |>
  as_tsibble(index = YearMonth) |>
  mutate(
    time_trend = row_number() - 1
  )




fx_plot <- ggplot(fx_monthly, aes(x = Date, y = Exchange_Rate)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = intervention_date,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = intervention_date,
    y = max(fx_monthly$Exchange_Rate, na.rm = TRUE),
    label = "PayNow",
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 3.5
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  labs(
    title = "Monthly SGD/USD Exchange Rate",
    subtitle = "Negative-control outcome for PayNow intervention analysis",
    x = "Year",
    y = "Exchange rate, SGD per USD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

fx_plot



## Fit baseline SARIMA model


fit_fx_nco <- fx_monthly |>
  model(
    fx_step_model= ARIMA(
      Exchange_Rate ~ 1 + step_paynow + pdq(1, 1, 1) + PDQ(1, 0, 1, period = 12)
    )
  )




# Model reports

fit_fx_nco |>
  select(
    fx_step_model
  ) |>
  report()




# Coefficients

fx_nco_coeff <- fit_fx_nco |>
  tidy()

print(fx_nco_coeff, n = Inf, width = Inf)



# Residual analysis

fx_nco_resid_plot <- fit_fx_nco |>
  select(
    fx_step_model
  ) |>
  gg_tsresiduals(
    type = "innovation",
    plot_type = "histogram",
    lag_max = 36
  )


fx_nco_resid_plot





# Ljung-Box test

fx_nco_augmented <- fit_fx_nco |>
  select(
    fx_step_model
  ) |>
  augment()

fx_nco_resid <- fx_nco_augmented |>
  filter(
    is.finite(.innov)
  ) |>
  pull(
    .innov
  )

T_fx <- length(fx_nco_resid)

lb_lag_fx <- min(24, T_fx/5)

fx_nco_ljung_test <- Box.test(
  fx_nco_resid,
  lag = lb_lag_fx,
  type = "Ljung-Box",
  fitdf = 3
) 


fx_nco_ljung_results <- tibble(
  LB_statistic = as.numeric(
    fx_nco_ljung_test$statistic
  ),
  p_value = as.numeric(
    fx_nco_ljung_test$p.value
  )
)


fx_nco_ljung_results






