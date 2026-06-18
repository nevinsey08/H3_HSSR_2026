
## Creating CIC/GDP Ratio 


library(readxl)
library(dplyr)
library(lubridate)
library(tsibble)
library(ggplot2)
library(scales)



file_path <- "C:/Users/user/Downloads/HSSR H3/HSSR H3/General Dataset/HSSR_H3_Dataset.xlsx"


start_date <- as.Date("2014-04-01")
end_date <- as.Date("2019-12-31")

start_year <- year(start_date)
end_year <- year(end_date)


intervention_year <- 2017
intervention_month <- 8
intervention_mnum <- intervention_year * 12 + intervention_month
intervention_date <- as.Date(sprintf("%04d-%02d-01", intervention_year, intervention_month))



read_data <- function(date_col, value_col, start_row = 5, end_row = NA) {
  raw <- read_excel(
    file_path,
    sheet = "CIC-GDP Ratio",
    range = cell_limits(c(start_row, date_col), c(end_row, value_col)),
    col_names = FALSE
  )
  data.frame(
    date_raw = raw[[1]],
    value_raw = raw[[ncol(raw)]]
  )
}


num <- function(x) {
  suppressWarnings(as.numeric(x))
}


parse_month_date <- function(x) {
  x <- gsub(" ", "", as.character(x))
  x <- gsub("Sept", "Sep", x)
  as.Date(parse_date_time(x, orders = c("Yb", "YB")))
}



CIC_monthly <- read_data(date_col = 2, value_col = 3) |>
  mutate(
    Date = parse_month_date(date_raw),
    CIC_SGD_million = num(value_raw),
    CIC_SGD = CIC_SGD_million * 1e6,
    Year = year(Date),
    Month = month(Date),
    YearMonth = yearmonth(Date),
    MonthNumber = Year * 12 + Month,
    MonthLabel = format(Date, "%Y %b")
  ) |>
  filter(
    !is.na(Date),
    !is.na(CIC_SGD),
    Date >= start_date,
    Date <= end_date
  )



CIC_GDP_monthly <- CIC_monthly |>
  left_join(
    Monthly_GDP |>
      select(
        YearMonth,
        Monthly_GDP_SGD,
        Monthly_GDP_SGD_million,
        Annualised_Monthly_GDP_SGD,
        Annualised_Monthly_GDP_SGD_million
      ),
    by = "YearMonth"
  ) |>
  mutate(
    CIC_GDP_Ratio = CIC_SGD / Annualised_Monthly_GDP_SGD
  ) |>
  filter(
    !is.na(CIC_GDP_Ratio)
  ) |>
  arrange(YearMonth)



CIC_GDP_ts <- CIC_GDP_monthly |>
  mutate(
    TimeIndex_M = row_number(),
    step_paynow = as.integer(
      MonthNumber >= intervention_mnum
    ),
    ramp_paynow = ifelse(
      MonthNumber >= intervention_mnum,
      MonthNumber - intervention_mnum + 1, 
      0
    )
  ) |>
  select(
    YearMonth,
    TimeIndex_M,
    Date,
    Year,
    Month,
    MonthNumber,
    MonthLabel,
    CIC_SGD_million,
    CIC_SGD,
    Monthly_GDP_SGD,
    Monthly_GDP_SGD_million,
    Annualised_Monthly_GDP_SGD,
    Annualised_Monthly_GDP_SGD_million,
    CIC_GDP_Ratio,
    step_paynow,
    ramp_paynow
  ) |>
  as_tsibble(index = YearMonth)


print(
  CIC_GDP_ts,
  n = Inf,
  width = Inf
)



# Plot monthly CIC/GDP ratio 

interventions <- data.frame(
  Event = c("PayNow"),
  Date = as.Date(c("2017-08-01"))
)

max_y <- max(CIC_GDP_ts$CIC_GDP_Ratio, na.rm = TRUE)

graph_CIC_GDP <- ggplot(CIC_GDP_ts, aes(x = Date, y = CIC_GDP_Ratio)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = interventions,
    aes(xintercept = Date),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_text(
    data = interventions,
    aes(x = Date, y = max_y, label = Event),
    angle = 90,
    vjust = -0.5,
    hjust = 1,
    size = 3.5
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Monthly CIC/GDP Ratio in Singapore",
    x = "Year",
    y = "CIC/GDP Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

graph_CIC_GDP




# Box plots for trend and seasonality

boxplot_data <- CIC_GDP_ts |>
  as_tibble() |>
  mutate(
    Month_name = factor(month.abb[Month], levels = month.abb),
    Year_factor = factor(Year)
  )


boxplot_deviation_data <- boxplot_data |>
  group_by(Year) |>
  mutate(
    CIC_GDP_year_mean = mean(CIC_GDP_Ratio, na.rm = TRUE),
    CIC_GDP_pct_deviation = 100 * (CIC_GDP_Ratio / CIC_GDP_year_mean - 1)
  ) |>
  ungroup()



monthly_boxplot_deviation <- ggplot(
  boxplot_deviation_data,
  aes(x = Month_name, y = CIC_GDP_pct_deviation)
) +
  geom_boxplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  labs(
    title = "Monthly Box Plot of CIC/GDP Ratio Relative to Yearly Average",
    x = "Month",
    y = "Deviation from yearly average, %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

monthly_boxplot_deviation


yearly_boxplot <- ggplot(
  boxplot_data,
  aes(x = Year_factor, y = CIC_GDP_Ratio)
) +
  geom_boxplot() +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Yearly Box Plot of CIC/GDP Ratio",
    x = "Year",
    y = "CIC/GDP Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

yearly_boxplot






