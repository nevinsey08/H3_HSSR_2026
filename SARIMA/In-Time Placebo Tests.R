
# In-Time Placebo Tests


library(dplyr)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)
library(scales)
library(purrr)


placebo_dates <- data.frame(
  placebo_label = c("Nov 2016", "Dec 2016", "Jan 2017", "Feb 2017", "Mar 2017", "Apr 2017", "May 2017"),
  placebo_year = c(2016,2016,2017,2017,2017,2017,2017),
  placebo_month = c(11,12,1,2,3,4,5)
) |>
  mutate(
    placebo_mnum =
      placebo_year * 12 +
      placebo_month,
    placebo_date = as.Date(
      paste0(
        placebo_year,
        "-",
        sprintf("%02d", placebo_month),
        "-01"
      )
    )
  )


placebo_data_list <- purrr::pmap(
  placebo_dates |>
    select(
      placebo_mnum,
      placebo_date,
      placebo_label
    ),
  function(placebo_mnum, placebo_date, placebo_label) {
    CIC_GDP_ts |>
      filter(MonthNumber < intervention_mnum) |>
      mutate(
        placebo_step = as.integer(
          MonthNumber >= placebo_mnum
        )
      ) |>
      arrange(YearMonth)
  }
)

names(placebo_data_list) <- placebo_dates$placebo_label




## Fit baseline model

placebo_fits <- purrr::map(
  placebo_data_list,
  function(placebo_data) {
    placebo_data |>
      model(
        placebo_model = ARIMA(
          CIC_GDP_Ratio ~ 1 + placebo_step + pdq(1, 1, 1) + PDQ(1, 0, 1, period = 12)
        )
      )
  }
)


names(placebo_fits) <- placebo_dates$placebo_label




## Models information

placebo_model_info <- purrr::imap_dfr(
  placebo_fits,
  function(placebo_fit, placebo_label) {
    placebo_date <- placebo_dates |>
      filter(placebo_label == .env$placebo_label) |>
      pull(placebo_date)
    model_info <- placebo_fit |>
      glance()
    valid_model <- (
      nrow(model_info) > 0 &&
        "AICc" %in% names(model_info) &&
        !is.na(model_info$AICc[1])
    )
    if (!valid_model) {
      return(
        tibble(
          placebo_label = placebo_label,
          placebo_date = placebo_date,
          sigma2 = NA_real_,
          log_lik = NA_real_,
          AIC = NA_real_,
          AICc = NA_real_,
          BIC = NA_real_,
          model_status = "Failed"
        )
      )
    }
    model_info |>
      transmute(
        placebo_label = placebo_label,
        placebo_date = placebo_date,
        sigma2,
        log_lik,
        AIC,
        AICc,
        BIC,
        model_status = "Estimated"
      )
  }
)


placebo_model_info




## Successful and failed placebo models

successful_placebo_labels <- placebo_model_info |>
  filter(model_status == "Estimated") |>
  pull(placebo_label)


failed_placebo_labels <- placebo_model_info |>
  filter(model_status == "Failed") |>
  pull(placebo_label)


successful_placebo_labels

failed_placebo_labels


placebo_model_info_success <- placebo_model_info |>
  filter(model_status == "Estimated") |>
  arrange(AICc)


placebo_model_info_failed <- placebo_model_info |>
  filter(model_status == "Failed")


placebo_model_info_success

placebo_model_info_failed




## Model coefficients

placebo_coefficients_success <- purrr::map_dfr(
  successful_placebo_labels,
  function(placebo_label) {
    placebo_date <- placebo_dates |>
      filter(placebo_label == .env$placebo_label) |>
      pull(placebo_date)
    placebo_fits[[placebo_label]] |>
      tidy() |>
      mutate(
        placebo_label = placebo_label,
        placebo_date = placebo_date
      ) |>
      select(
        placebo_label,
        placebo_date,
        term,
        estimate,
        std.error,
        statistic,
        p.value
      )
  }
)

placebo_coefficients <- bind_rows(
  placebo_coefficients_success
) |>
  mutate(
    placebo_label = factor(
      placebo_label,
      levels = placebo_dates$placebo_label
    )
  ) |>
  arrange(
    placebo_label,
    term
  ) |>
  mutate(
    placebo_label = as.character(placebo_label)
  )


print(
  placebo_coefficients,
  n = Inf,
  width = Inf
)



## Residual diagnostic plots

placebo_resid_plots <- purrr::map(
  successful_placebo_labels,
  function(placebo_label) {
    placebo_fits[[placebo_label]] |>
      select(placebo_model) |>
      gg_tsresiduals() +
      labs(
        title = paste0(
          "Residual Diagnostics: Placebo ",
          placebo_label
        )
      ) +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5
        )
      )
  }
)


names(placebo_resid_plots) <- successful_placebo_labels


purrr::walk(
  successful_placebo_labels,
  
  function(placebo_label) {
    print(
      placebo_resid_plots[[placebo_label]]
    )
  }
)


placebo_resid_plots[["Mar 2017"]]
placebo_resid_plots[["Dec 2016"]]
placebo_resid_plots[["Feb 2017"]]
placebo_resid_plots[["Apr 2017"]]
placebo_resid_plots[["May 2017"]]






## Ljung-Box tests


placebo_ljung_success <- purrr::map_dfr(
  successful_placebo_labels,
  function(placebo_label) {
    placebo_resid <- placebo_fits[[placebo_label]] |>
      augment() |>
      pull(.resid)
    placebo_resid <- na.omit(placebo_resid)
    T <- length(placebo_resid)
    lb_lag <- min(24, T/5)
    lb_test <- Box.test(
      placebo_resid,
      lag = lb_lag,
      type = "Ljung-Box",
      fitdf = 3
    )
    tibble(
      placebo_label = placebo_label,
      LB_statistic = as.numeric(
        lb_test$statistic
      ),
      p_value = as.numeric(
        lb_test$p.value
      )
    )
  }
)


placebo_ljung_success






















