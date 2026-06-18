
# Counterfactual Analysis


library(dplyr)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)
library(scales)





CIC_cf_data <- CIC_GDP_ts |>
  arrange(YearMonth)


# Baseline intervention model

fit_counterfactual <- CIC_cf_data |>
  model(
    counterfactual_model = ARIMA(
      CIC_GDP_Ratio ~ 1 + step_paynow + pdq(1, 1, 1) + PDQ(1, 0, 1, period = 12)
    )
  )

fit_counterfactual |> report()


cf_model_info <- fit_counterfactual |>
  glance()

cf_model_info


cf_model_coeff <- fit_counterfactual |>
  tidy()

print(cf_model_coeff, n = Inf, width = Inf)




# Residual Analysis 

cf_resid_plot <- fit_counterfactual |>
  gg_tsresiduals() +
  labs(
    title = "Residual Diagnostics: Baseline step-intervention model"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

cf_resid_plot



cf_resid <- fit_counterfactual |>
  augment() |>
  pull(.resid)

cf_resid <- na.omit(cf_resid)

cf_resid_sd <- sd(cf_resid)




# Build counterfactual model without PayNow effect

beta_step <- cf_model_coeff |>
  filter(term == "step_paynow") |>
  pull(estimate)


cf_table <- fit_counterfactual |>
  augment() |>
  as_tibble() |>
  left_join(
    CIC_cf_data |>
      as_tibble() |>
      select(
        YearMonth,
        Date,
        Year,
        Month,
        MonthNumber,
        MonthLabel,
        step_paynow
      ),
    by = "YearMonth"
  ) |>
  mutate(
    counterfactual = .fitted - beta_step * step_paynow,
    cf_lower_90 = counterfactual - qnorm(0.95) * cf_resid_sd,
    cf_upper_90 = counterfactual + qnorm(0.95) * cf_resid_sd,
    cf_lower_95 = counterfactual - qnorm(0.975) * cf_resid_sd,
    cf_upper_95 = counterfactual + qnorm(0.975) * cf_resid_sd,
    actual = CIC_GDP_Ratio,
    effect = actual - counterfactual,
    pct_effect = effect / counterfactual * 100,
    period = case_when(
      MonthNumber < intervention_mnum ~ "Pre-PayNow",
      MonthNumber >= intervention_mnum ~ "Post-PayNow",
      TRUE ~ "Other"
    )
  )

cf_table



# Plot counterfactual 

actual_plot_data <- CIC_cf_data |>
  as_tibble() |>
  select(Date, CIC_GDP_Ratio)


cf_plot_data <- cf_table |>
  filter(MonthNumber >= intervention_mnum) |>
  select(
    Date,
    counterfactual,
    cf_lower_90,
    cf_upper_90,
    cf_lower_95,
    cf_upper_95
  )


cf_plot <- ggplot() +
  geom_ribbon(
    data = cf_plot_data,
    aes(
      x = Date,
      ymin = cf_lower_95,
      ymax = cf_upper_95,
      fill = "95% band"
    ),
    alpha = 0.35
  ) +
  geom_ribbon(
    data = cf_plot_data,
    aes(
      x = Date,
      ymin = cf_lower_90,
      ymax = cf_upper_90,
      fill = "90% band"
    ),
    alpha = 0.50
  ) +
  geom_line(
    data = actual_plot_data,
    aes(
      x = Date,
      y = CIC_GDP_Ratio,
      colour = "Actual"
    ),
    linewidth = 1
  ) +
  geom_line(
    data = cf_plot_data,
    aes(
      x = Date,
      y = counterfactual,
      colour = "Counterfactual no PayNow"
    ),
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = intervention_date, linetype = "PayNow"),
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = intervention_date,
    y = max(CIC_cf_data$CIC_GDP_Ratio, na.rm = TRUE),
    label = "PayNow",
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 3.5
  ) +
  scale_colour_manual(
    name = "Series",
    values = c(
      "Actual" = "grey40",
      "Counterfactual no PayNow" = "red"
    )
  ) +
  scale_fill_manual(
    name = "Residual-based uncertainty bands",
    values = c(
      "95% band" = "#7f86e8",
      "90% band" = "#5f6af2"
    )
  ) +
  scale_linetype_manual(
    name = "Intervention line",
    values = c(
      "PayNow" = "dotted"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "PayNow Counterfactual: Monthly CIC/GDP Ratio",
    subtitle = "Step intervention model with SARIMA(1,1,1)(1,0,1)[12]",
    x = "Year",
    y = "CIC/GDP Ratio"
  ) +
  guides(
    colour = guide_legend(order = 1),
    fill = guide_legend(order = 2),
    linetype = "none"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

cf_plot




# Zoomed plot from PayNow onwards

actual_zoom_data <- actual_plot_data |>
  filter(Date >= intervention_date)

cf_zoom_data <- cf_plot_data |>
  filter(Date >= intervention_date)


cf_plot_zoom <- ggplot() +
  geom_ribbon(
    data = cf_zoom_data,
    aes(
      x = Date,
      ymin = cf_lower_95,
      ymax = cf_upper_95,
      fill = "95% band"
    ),
    alpha = 0.35
  ) +
  geom_ribbon(
    data = cf_zoom_data,
    aes(
      x = Date,
      ymin = cf_lower_90,
      ymax = cf_upper_90,
      fill = "90% band"
    ),
    alpha = 0.50
  ) +
  geom_line(
    data = actual_zoom_data,
    aes(
      x = Date,
      y = CIC_GDP_Ratio,
      colour = "Actual"
    ),
    linewidth = 1
  ) +
  geom_line(
    data = cf_zoom_data,
    aes(
      x = Date,
      y = counterfactual,
      colour = "Counterfactual no PayNow"
    ),
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = intervention_date, linetype = "PayNow"),
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = intervention_date,
    y = max(actual_zoom_data$CIC_GDP_Ratio, na.rm = TRUE),
    label = "PayNow",
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 3.5
  ) +
  scale_colour_manual(
    name = "Series",
    values = c(
      "Actual" = "grey40",
      "Counterfactual no PayNow" = "red"
    )
  ) +
  scale_fill_manual(
    name = "Residual-based uncertainty bands",
    values = c(
      "95% band" = "#7f86e8",
      "90% band" = "#5f6af2"
    )
  ) +
  scale_linetype_manual(
    name = "Intervention line",
    values = c(
      "PayNow" = "dotted"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "PayNow Counterfactual: Monthly CIC/GDP Ratio",
    x = "Year",
    y = "CIC/GDP Ratio"
  ) +
  guides(
    colour = guide_legend(order = 1),
    fill = guide_legend(order = 2),
    linetype = "none"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

cf_plot_zoom






## Post-intervention check


cf_post_table <- cf_table |>
  filter(
    MonthNumber >= intervention_mnum
  )


cf_90_band_summary <- cf_post_table |>
  summarise(
    n_months = n(),
    months_below_90_band = sum(
      actual < cf_lower_90,
      na.rm = TRUE
    ),
    share_below_90_band = mean(
      actual < cf_lower_90,
      na.rm = TRUE
    ) * 100,
    months_within_90_band = sum(
      actual >= cf_lower_90 &
        actual <= cf_upper_90,
      na.rm = TRUE
    ),
    share_within_90_band = mean(
      actual >= cf_lower_90 &
        actual <= cf_upper_90,
      na.rm = TRUE
    ) * 100,
    months_above_90_band = sum(
      actual > cf_upper_90,
      na.rm = TRUE
    ),
    share_above_90_band = mean(
      actual > cf_upper_90,
      na.rm = TRUE
    ) * 100
  )


cf_95_band_summary <- cf_post_table |>
  summarise(
    n_months = n(),
    months_below_95_band = sum(
      actual < cf_lower_95,
      na.rm = TRUE
    ),
    share_below_95_band = mean(
      actual < cf_lower_95,
      na.rm = TRUE
    ) * 100,
    months_within_95_band = sum(
      actual >= cf_lower_95 &
        actual <= cf_upper_95,
      na.rm = TRUE
    ),
    share_within_95_band = mean(
      actual >= cf_lower_95 &
        actual <= cf_upper_95,
      na.rm = TRUE
    ) * 100,
    months_above_95_band = sum(
      actual > cf_upper_95,
      na.rm = TRUE
    ),
    share_above_95_band = mean(
      actual > cf_upper_95,
      na.rm = TRUE
    ) * 100
  )



cf_counterfactual_summary <- cf_post_table |>
  summarise(
    n_months = n(),
    months_below_counterfactual = sum(
      actual < counterfactual,
      na.rm = TRUE
    ),
    share_below_counterfactual = mean(
      actual < counterfactual,
      na.rm = TRUE
    ) * 100,
    months_above_counterfactual = sum(
      actual > counterfactual,
      na.rm = TRUE
    ),
    share_above_counterfactual = mean(
      actual > counterfactual,
      na.rm = TRUE
    ) * 100,
    months_equal_counterfactual = sum(
      actual == counterfactual,
      na.rm = TRUE
    ),
    share_equal_counterfactual = mean(
      actual == counterfactual,
      na.rm = TRUE
    ) * 100
  )



cf_direction_summary <- tibble(
  Comparison = c(
    "Central counterfactual",
    "90% uncertainty band",
    "95% uncertainty band"
  ),
  Total_months = c(
    cf_counterfactual_summary$n_months,
    cf_90_band_summary$n_months,
    cf_95_band_summary$n_months
  ),
  Months_below = c(
    cf_counterfactual_summary$months_below_counterfactual,
    cf_90_band_summary$months_below_90_band,
    cf_95_band_summary$months_below_95_band
  ),
  Share_below = c(
    cf_counterfactual_summary$share_below_counterfactual,
    cf_90_band_summary$share_below_90_band,
    cf_95_band_summary$share_below_95_band
  ),
  Months_within = c(
    cf_counterfactual_summary$months_equal_counterfactual,
    cf_90_band_summary$months_within_90_band,
    cf_95_band_summary$months_within_95_band
  ),
  Share_within = c(
    cf_counterfactual_summary$share_equal_counterfactual,
    cf_90_band_summary$share_within_90_band,
    cf_95_band_summary$share_within_95_band
  ),
  Months_above = c(
    cf_counterfactual_summary$months_above_counterfactual,
    cf_90_band_summary$months_above_90_band,
    cf_95_band_summary$months_above_95_band
  ),
  Share_above = c(
    cf_counterfactual_summary$share_above_counterfactual,
    cf_90_band_summary$share_above_90_band,
    cf_95_band_summary$share_above_95_band
  )
)


cf_direction_summary







