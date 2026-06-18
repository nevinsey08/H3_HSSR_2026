



library(dplyr)
library(tsibble)
library(fable)
library(feasts)
library(tseries)
library(ggplot2)
library(purrr)



## Stationarity test 

SARIMA_stationarity <- CIC_GDP_ts |>
  arrange(YearMonth) |>
  mutate(
    diff1 = CIC_GDP_Ratio - lag(CIC_GDP_Ratio, 1),
    diff2 = diff1 - lag(diff1, 1),
    seasonal = CIC_GDP_Ratio - lag(CIC_GDP_Ratio, 12),
    diff1_seasonal = diff1 - lag(diff1, 12),
    diff2_seasonal = diff2 - lag(diff2, 12)
  )


SARIMA_stationarity


stationarity_test <- function(x, series_name) {
  x <- na.omit(
    as.numeric(x)
  )
  adf_result <- adf.test(x)
  kpss_result <- kpss.test(
    x,
    null = "Level"
  )
  data.frame(
    Series = series_name,
    N = length(x),
    ADF_stat = as.numeric(adf_result$statistic),
    ADF_p_value = as.numeric(adf_result$p.value),
    KPSS_stat = as.numeric(kpss_result$statistic),
    KPSS_p_value = as.numeric(kpss_result$p.value)
  )
}


stationarity_level <- stationarity_test(
  SARIMA_stationarity$CIC_GDP_Ratio,
  "Level"
)

stationarity_diff1 <- stationarity_test(
  SARIMA_stationarity$diff1,
  "First difference"
)

stationarity_diff2 <- stationarity_test(
  SARIMA_stationarity$diff2,
  "Second difference"
)

stationarity_seasonal <- stationarity_test(
  SARIMA_stationarity$seasonal,
  "Seasonal difference"
)

stationarity_diff1_seasonal <- stationarity_test(
  SARIMA_stationarity$diff1_seasonal,
  "First + seasonal difference"
)


stationarity_diff2_seasonal <- stationarity_test(
  SARIMA_stationarity$diff2_seasonal,
  "Second + seasonal difference"
)

SARIMA_stationarity_results <- bind_rows(
  stationarity_level,
  stationarity_diff1,
  stationarity_diff2,
  stationarity_seasonal,
  stationarity_diff1_seasonal,
  stationarity_diff2_seasonal
)

SARIMA_stationarity_results




## ACF and PACF analysis

acf_pacf_SARIMA_level <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = CIC_GDP_Ratio,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: Level Series"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_level


acf_pacf_SARIMA_diff1 <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = diff1,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: First Difference"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_diff1


acf_pacf_SARIMA_diff2 <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = diff2,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: Second Difference"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_diff2


acf_pacf_SARIMA_seasonal <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = seasonal,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: Seasonal Difference"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_seasonal


acf_pacf_SARIMA_diff1_seasonal <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = diff1_seasonal,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: First and Seasonal Difference"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_diff1_seasonal


acf_pacf_SARIMA_diff2_seasonal <- SARIMA_stationarity |>
  gg_tsdisplay(
    y = diff2_seasonal,
    plot_type = "partial",
    lag_max = 48
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio: Second and Seasonal Difference"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

acf_pacf_SARIMA_diff2_seasonal





## SARIMA model specifications

manual_model <- c(
  "011_001", "110_001", "111_001", "011_100", "110_100", "111_100", "011_101", "110_101", "111_101"
)


manual_pdq <- c(
  "(0,1,1)", "(1,1,0)", "(1,1,1)", "(0,1,1)", "(1,1,0)", "(1,1,1)", "(0,1,1)", "(1,1,0)", "(1,1,1)"
)


manual_PDQ <- c(
  "(0,0,1)[12]", "(0,0,1)[12]", "(0,0,1)[12]", "(1,0,0)[12]", "(1,0,0)[12]", "(1,0,0)[12]", "(1,0,1)[12]", "(1,0,1)[12]", "(1,0,1)[12]"
)



SARIMA_model_specs <- tibble(
  .model = c(
    "step_auto",
    paste0("step_", manual_model),
    "ramp_auto",
    paste0("ramp_", manual_model)
  ),
  Intervention = c(
    "Step PayNow",
    rep("Step PayNow", 9),
    "Ramp PayNow",
    rep("Ramp PayNow", 9)
  ),
  pdq_order = c(
    "Auto-selected, d = 1",
    manual_pdq,
    "Auto-selected, d = 1",
    manual_pdq
  ),
  PDQ_order = c(
    "Auto-selected, D = 0",
    manual_PDQ,
    "Auto-selected, D = 0",
    manual_PDQ
  )
)




## Fit SARIMA models

fit_SARIMA <- CIC_GDP_ts |>
  model(
    step_auto = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(p = 0:5, d = 1, q = 0:5) + PDQ(P = 0:5, D = 0, Q = 0:5, period = 12),
      stepwise = FALSE,
      approximation = FALSE
    ),
    step_011_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(0,1,1) + PDQ(0,0,1, period = 12)
    ),
    step_110_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 0) + PDQ(0, 0, 1, period = 12)
    ),
    step_111_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 1) + PDQ(0, 0, 1, period = 12)
    ),
    step_011_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(0, 1, 1) + PDQ(1, 0, 0, period = 12)
    ),
    step_110_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 0) + PDQ(1, 0, 0, period = 12)
    ),
    step_111_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 1) + PDQ(1, 0, 0, period = 12)
    ),
    step_011_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(0, 1, 1) + PDQ(1, 0, 1, period = 12)
    ),
    step_110_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 0) + PDQ(1, 0, 1, period = 12)
    ),
    step_111_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 1) + PDQ(1, 0, 1, period = 12)
    ),
    ramp_auto = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(p = 0:5, d = 1, q = 0:5) + PDQ(P = 0:5, D = 0, Q = 0:5, period = 12),
      stepwise = FALSE,
      approximation = FALSE
    ),
    ramp_011_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(0, 1, 1) + PDQ(0, 0, 1, period = 12)
    ),
    ramp_110_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 0) + PDQ(0, 0, 1, period = 12)
    ),
    ramp_111_001 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 1) + PDQ(0, 0, 1, period = 12)
    ),
    ramp_011_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(0, 1, 1) + PDQ(1, 0, 0, period = 12)
    ),
    ramp_110_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 0) + PDQ(1, 0, 0, period = 12)
    ),
    ramp_111_100 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 1) + PDQ(1, 0, 0, period = 12)
    ),
    ramp_011_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(0, 1, 1) + PDQ(1, 0, 1, period = 12)
    ),
    ramp_110_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 0) + PDQ(1, 0, 1, period = 12)
    ),
    ramp_111_101 = ARIMA(
      CIC_GDP_Ratio ~ 1 + ramp_paynow + pdq(1, 1, 1) + PDQ(1, 0, 1, period = 12)
    )
  )



## Compare candidate models

SARIMA_comparison <- fit_SARIMA |>
  glance() |>
  left_join(
    SARIMA_model_specs,
    by = ".model"
  )


print(
  SARIMA_comparison,
  n = Inf,
  width = Inf
)



## Identify successful and failed models

SARIMA_models <- SARIMA_model_specs |>
  select(.model) |>
  left_join(
    SARIMA_comparison |>
      select(
        .model,
        AICc
      ),
    by = ".model"
  ) |>
  filter(
    !is.na(AICc)
  ) |>
  pull(.model)


SARIMA_models



SARIMA_failed_models <- SARIMA_model_specs |>
  select(.model) |>
  left_join(
    SARIMA_comparison |>
      select(
        .model,
        AICc
      ),
    by = ".model"
  ) |>
  filter(
    is.na(AICc)
  ) |>
  pull(.model)


SARIMA_failed_models





## Model reports

SARIMA_reports <- list()


for (m in SARIMA_models) {
  one_model <- fit_SARIMA |>
    select(
      all_of(m)
    )
  SARIMA_reports[[m]] <- report(
    one_model
  )
  cat("\n\n====================================================\n")
  cat("Model:", m, "\n")
  cat("====================================================\n")
  print(
    SARIMA_reports[[m]]
  )
}





## Full coefficient results

options(
  tibble.print_max = Inf,
  tibble.width = Inf
)


SARIMA_coeff <- fit_SARIMA |>
  tidy() |>
  left_join(
    SARIMA_model_specs,
    by = ".model"
  ) |>
  mutate(
    .model = factor(
      .model,
      levels = SARIMA_models
    )
  ) |>
  arrange(
    .model,
    term
  ) |>
  mutate(
    .model = as.character(.model)
  )


print(
  SARIMA_coeff,
  n = Inf,
  width = Inf
)






## Residual diagnostic plots

SARIMA_resid_plots <- list()


for (m in SARIMA_models) {
  one_resid_plot <- fit_SARIMA |>
    select(
      all_of(m)
    ) |>
    gg_tsresiduals() +
    labs(
      title = paste0(
        "Residual Diagnostics: ",
        m
      )
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    )
  SARIMA_resid_plots[[m]] <- one_resid_plot
  print(
    SARIMA_resid_plots[[m]]
  )
}


SARIMA_resid_plots[["step_111_101"]]




## Ljung-Box tests


SARIMA_fitdf <- fit_SARIMA |>
  tidy() |>
  filter(
    .model %in% SARIMA_models
  ) |>
  mutate(
    Is_ARMA_parameter = grepl(
      "^(ar|ma|sar|sma)[0-9]+$",
      term
    )
  ) |>
  group_by(
    .model
  ) |>
  summarise(
    Model_fitdf = sum(
      Is_ARMA_parameter
    ),
    .groups = "drop"
  )


SARIMA_ljung_list <- list()

for (m in SARIMA_models) {
  resid <- fit_SARIMA |>
    select(
      all_of(m)
    ) |>
    augment() |>
    pull(.resid)
  resid <- na.omit(
    resid
  )
  T <- length(resid)
  model_fitdf <- SARIMA_fitdf |>
    filter(
      .model == m
    ) |>
    pull(
      Model_fitdf
    )
  lb_lag <- min(24, T/5)
  lb_test <- Box.test(
    resid,
    lag = lb_lag,
    type = "Ljung-Box",
    fitdf = model_fitdf
  )
  SARIMA_ljung_list[[m]] <- tibble(
    .model = m,
    LB_statistic = as.numeric(
      lb_test$statistic
    ),
    LB_p_value = as.numeric(
      lb_test$p.value
    )
  )
}


SARIMA_ljung_results <- bind_rows(
  SARIMA_ljung_list
) |>
  left_join(
    SARIMA_model_specs,
    by = ".model"
  ) |>
  mutate(
    .model = factor(
      .model,
      levels = SARIMA_models
    )
  ) |>
  arrange(
    .model
  ) |>
  mutate(
    .model = as.character(
      .model
    )
  )


print(
  SARIMA_ljung_results,
  n = Inf,
  width = Inf
)









## Invertibility check for step_111_101

step_111_101_coef <- fit_SARIMA |>
  select(
    step_111_101
  ) |>
  tidy()


ma1 <- step_111_101_coef |>
  filter(
    term == "ma1"
  ) |>
  pull(estimate)


sma1 <- step_111_101_coef |>
  filter(
    term == "sma1"
  ) |>
  pull(estimate)


ma_poly <- c(
  1,
  ma1,
  rep(0, 10),
  sma1,
  ma1 * sma1
)


ma_roots <- polyroot(
  ma_poly
)

ma_inverse_roots <- 1 / ma_roots

ma_root_table <- tibble(
  Root_Number = seq_along(ma_roots),
  Root_Real = Re(ma_roots),
  Root_Imaginary = Im(ma_roots),
  Root_Modulus = Mod(ma_roots),
  Inverse_Root_Real = Re(ma_inverse_roots),
  Inverse_Root_Imaginary = Im(ma_inverse_roots),
  Inverse_Root_Modulus = Mod(ma_inverse_roots)
)


print(
  ma_root_table,
  n = Inf,
  width = Inf
)



## Plot inverse MA roots

unit_circle <- tibble(
  angle = seq(
    0,
    2 * pi,
    length.out = 500
  ),
  x = cos(angle),
  y = sin(angle)
)

inverse_root_plot <- ggplot() +
  geom_path(
    data = unit_circle,
    aes(
      x = x,
      y = y
    ),
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.5
  ) +
  geom_point(
    data = ma_root_table,
    aes(
      x = Inverse_Root_Real,
      y = Inverse_Root_Imaginary
    ),
    size = 3
  ) +
  coord_equal() +
  labs(
    title = "Inverse MA Roots for SARIMA step_111_101",
    x = "Real component",
    y = "Imaginary component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    panel.grid.minor = element_blank()
  )


inverse_root_plot




