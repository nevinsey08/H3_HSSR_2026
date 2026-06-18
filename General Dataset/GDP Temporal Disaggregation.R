
## Temporal Disaggregation of Quarterly GDP to Monthly GDP using Litterman method


library(readxl)
library(dplyr)
library(lubridate)
library(tsibble)
library(tempdisagg)
library(ggplot2)
library(scales)



td_file_path <- "C:/Users/user/Downloads/HSSR H3/HSSR H3/General Dataset/GDP Temporal Disaggregation Indicators.xlsx"

td_start_date <- as.Date("2014-01-01")
td_end_date <- as.Date("2019-12-31")



num <- function(x) {
  suppressWarnings(as.numeric(x))
}


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






GDP_quarterly_raw <- read_excel(
  td_file_path,
  sheet = "My Series",
  range = cellranger::cell_limits(
    c(4, 10),
    c(NA, 11)
  ),
  col_names = FALSE
)


names(GDP_quarterly_raw) <- c(
  "date_raw",
  "GDP_SGD_million"
)


GDP_quarterly <- GDP_quarterly_raw |>
  mutate(
    Date = parse_excel_date(date_raw),
    GDP_SGD_million = num(GDP_SGD_million),
    GDP_SGD = GDP_SGD_million * 1e6,
    Year = year(Date),
    Quarter = quarter(Date),
    YearQuarter = yearquarter(Date)
  ) |>
  filter(
    !is.na(Date),
    !is.na(GDP_SGD),
    Date >= td_start_date,
    Date <= td_end_date
  ) |>
  group_by(YearQuarter) |>
  summarise(
    Date = first(Date),
    Year = first(Year),
    Quarter = first(Quarter),
    GDP_SGD_million = first(GDP_SGD_million),
    GDP_SGD = first(GDP_SGD),
    .groups = "drop"
  ) |>
  arrange(YearQuarter)




indicator_raw <- read_excel(
  td_file_path,
  sheet = "My Series",
  range = cellranger::cell_limits(
    c(30, 1),
    c(NA, 6)
  ),
  col_names = FALSE
)


names(indicator_raw) <- c(
  "date_raw",
  "IPI",
  "Retail_Sales_Index",
  "Domestic_Exports",
  "Visitor_Arrivals",
  "Unemployment_Rate"
)


indicators_monthly <- indicator_raw |>
  mutate(
    Date = parse_excel_date(date_raw),
    Year = year(Date),
    Month = month(Date),
    YearMonth = yearmonth(Date),
    IPI = num(IPI),
    Retail_Sales_Index = num(Retail_Sales_Index),
    Domestic_Exports = num(Domestic_Exports),
    Visitor_Arrivals = num(Visitor_Arrivals),
    Unemployment_Rate = num(Unemployment_Rate)
  ) |>
  filter(
    !is.na(Date),
    Date >= td_start_date,
    Date <= td_end_date
  ) |>
  group_by(YearMonth) |>
  summarise(
    Date = first(Date),
    Year = first(Year),
    Month = first(Month),
    IPI = mean(IPI, na.rm = TRUE),
    Retail_Sales_Index = mean(Retail_Sales_Index, na.rm = TRUE),
    Domestic_Exports = mean(Domestic_Exports, na.rm = TRUE),
    Visitor_Arrivals = mean(Visitor_Arrivals, na.rm = TRUE),
    Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(YearMonth)



indicators_monthly_clean <- indicators_monthly |>
  mutate(
    z_log_IPI = as.numeric(
      scale(log(IPI))
    ),
    z_log_Retail_Sales_Index = as.numeric(
      scale(log(Retail_Sales_Index))
    ),
    z_log_Domestic_Exports = as.numeric(
      scale(log(Domestic_Exports))
    ),
    z_log_Visitor_Arrivals = as.numeric(
      scale(log(Visitor_Arrivals))
    ),
    z_Unemployment_Rate = as.numeric(
      scale(Unemployment_Rate)
    )
  ) |>
  filter(
    !is.na(z_log_IPI),
    !is.na(z_log_Retail_Sales_Index),
    !is.na(z_log_Domestic_Exports),
    !is.na(z_log_Visitor_Arrivals),
    !is.na(z_Unemployment_Rate)
  )




gdp_start_year <- first(GDP_quarterly$Year)
gdp_start_quarter <- first(GDP_quarterly$Quarter)

indicator_start_year <- first(indicators_monthly_clean$Year)
indicator_start_month <- first(indicators_monthly_clean$Month)



GDP_quarterly_ts <- ts(
  GDP_quarterly$GDP_SGD,
  start = c(
    gdp_start_year,
    gdp_start_quarter
  ),
  frequency = 4
)


IPI_ts <- ts(
  indicators_monthly_clean$z_log_IPI,
  start = c(
    indicator_start_year,
    indicator_start_month
  ),
  frequency = 12
)


Retail_ts <- ts(
  indicators_monthly_clean$z_log_Retail_Sales_Index,
  start = c(
    indicator_start_year,
    indicator_start_month
  ),
  frequency = 12
)


Exports_ts <- ts(
  indicators_monthly_clean$z_log_Domestic_Exports,
  start = c(
    indicator_start_year,
    indicator_start_month
  ),
  frequency = 12
)


Visitors_ts <- ts(
  indicators_monthly_clean$z_log_Visitor_Arrivals,
  start = c(
    indicator_start_year,
    indicator_start_month
  ),
  frequency = 12
)


Unemployment_ts <- ts(
  indicators_monthly_clean$z_Unemployment_Rate,
  start = c(
    indicator_start_year,
    indicator_start_month
  ),
  frequency = 12
)


## Litterman Temporal Disaggregation



GDP_td_litterman <- td(
  GDP_quarterly_ts ~ IPI_ts + Retail_ts + Exports_ts + Visitors_ts + Unemployment_ts,
  to = "monthly",
  conversion = "sum",
  method = "litterman-maxlog"
)


summary(GDP_td_litterman)


Monthly_GDP_ts <- predict(
  GDP_td_litterman
)





monthly_gdp_dates <- seq(
  from = td_start_date,
  by = "month",
  length.out = length(Monthly_GDP_ts)
)


Monthly_GDP <- data.frame(
  Date = monthly_gdp_dates,
  Monthly_GDP_SGD = as.numeric(Monthly_GDP_ts)
) |>
  mutate(
    Year = year(Date),
    Month = month(Date),
    YearMonth = yearmonth(Date),
    YearQuarter = yearquarter(Date),
    Monthly_GDP_SGD_million = Monthly_GDP_SGD / 1e6,
    Annualised_Monthly_GDP_SGD = Monthly_GDP_SGD * 12,
    Annualised_Monthly_GDP_SGD_million = Annualised_Monthly_GDP_SGD / 1e6
  ) |>
  filter(
    Date >= td_start_date,
    Date <= td_end_date
  )


Monthly_GDP





## Plot temporally disaggregated monthly GDP

monthly_gdp_plot <- ggplot(
  Monthly_GDP,
  aes(
    x = Date,
    y = Monthly_GDP_SGD_million
  )
) +
  geom_line(
    linewidth = 1
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Temporally Disaggregated Monthly GDP",
    subtitle = "Litterman method",
    x = "Month",
    y = "Monthly GDP, SGD million"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    panel.grid.minor = element_blank()
  )


monthly_gdp_plot



## Plot original quarterly GDP

quarterly_gdp_plot <- ggplot(
  GDP_quarterly,
  aes(
    x = Date,
    y = GDP_SGD_million
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Original Quarterly GDP",
    x = "Quarter",
    y = "Quarterly GDP, SGD million"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    panel.grid.minor = element_blank()
  )


quarterly_gdp_plot





## Fernández Temporal Disaggregation

GDP_td_fernandez <- td(
  GDP_quarterly_ts ~ IPI_ts + Retail_ts + Exports_ts + Visitors_ts + Unemployment_ts,
  to = "monthly",
  conversion = "sum",
  method = "fernandez"
)


summary(GDP_td_fernandez)


Monthly_GDP_Fernandez_ts <- predict(
  GDP_td_fernandez
)






## Chow-Lin Temporal Disaggregation

GDP_td_ChowLin <- td(
  GDP_quarterly_ts ~ IPI_ts + Retail_ts + Exports_ts + Visitors_ts + Unemployment_ts,
  to = "monthly",
  conversion = "sum",
  method = "chow-lin-maxlog"
)


summary(GDP_td_ChowLin)


Monthly_GDP_ChowLin_ts <- predict(
  GDP_td_ChowLin
)






## Compare Litterman, Fernández and Chow-Lin 

Monthly_GDP_Comparison <- data.frame(
  Date = monthly_gdp_dates,
  GDP_ChowLin_SGD = as.numeric(
    Monthly_GDP_ChowLin_ts
  ),
  GDP_Fernandez_SGD = as.numeric(
    Monthly_GDP_Fernandez_ts
  ),
  GDP_Litterman_SGD = as.numeric(
    Monthly_GDP_ts
  )
) |>
  mutate(
    Year = year(Date),
    Month = month(Date),
    YearMonth = yearmonth(Date),
    YearQuarter = yearquarter(Date),
    GDP_ChowLin_SGD_million =
      GDP_ChowLin_SGD / 1e6,
    GDP_Fernandez_SGD_million =
      GDP_Fernandez_SGD / 1e6,
    GDP_Litterman_SGD_million =
      GDP_Litterman_SGD / 1e6,
    diff_Fernandez_vs_Litterman = GDP_Fernandez_SGD - GDP_Litterman_SGD,
    diff_ChowLin_vs_Litterman = GDP_ChowLin_SGD - GDP_Litterman_SGD,
    pct_diff_Fernandez_vs_Litterman = diff_Fernandez_vs_Litterman / GDP_Litterman_SGD * 100,
    pct_diff_ChowLin_vs_Litterman = diff_ChowLin_vs_Litterman / GDP_Litterman_SGD * 100
  ) |>
  filter(
    Date >= td_start_date,
    Date <= td_end_date
  )


Monthly_GDP_Comparison










