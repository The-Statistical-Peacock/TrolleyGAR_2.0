#helpers.R

medmodus <- read_csv("data/medmodus.csv") %>% 
  select(c(1:22)) %>% 
  mutate(Date = dmy(Date)) %>% 
  rename("Health Region" = "Hospital Group Name")

# Pre-calculate unique regions and hospitals, and their mapping

all_regions <- sort(unique(medmodus$`Health Region`))

# Create a mapping for easy lookup (Region -> Hospitals)
region_hospital_map <- medmodus %>%
  distinct(`Health Region`, Hospital) %>%
  arrange(`Health Region`, Hospital) %>%
  group_by(`Health Region`) %>%
  summarise(Hospitals = list(Hospital)) %>%
  tibble::deframe() # Converts to a named list

# A mapping for Hospital -> Region (for reverse lookup)
hospital_region_map <- medmodus %>%
  distinct(Hospital, `Health Region`) %>%
  tibble::deframe()


#---------------Functions---------------#


# Total Trolleys
current_time_total <- function(data, column_name) {
  
  data %>%
    filter(Date == max(Date)) %>%
    pull({{ column_name }}) %>% 
    sum(na.rm = TRUE)
  
}

# Avg Trolleys YTD
avg__ytd <- function(data, column_name) {
  
  data %>%
    filter(year(Date) == 2025) %>%
    pull({{ column_name }}) %>% 
    mean(na.rm = TRUE) %>% 
    round(2)
  
}

# Under 9hrs
under_9hrs <- function(data, col1, col2, denom_col) {

  latest_data <- data %>%
    filter(Date == max(Date))
  
  numo <- latest_data %>%
    pull({{ col1 }}) %>%
    sum(na.rm = TRUE) +
    latest_data %>%
    pull({{ col2 }}) %>%
    sum(na.rm = TRUE)
  
  
  denom <- latest_data %>%
    pull({{ denom_col }}) %>%
    sum(na.rm = TRUE)
  
  # Calculate the percentage
  if (denom == 0) {
    return(NA) 
  } else {
    percentage <- (numo / denom) * 100
    return(sprintf("%.1f%%", percentage))
  }
}

#------------------ Plots ----------------------#

plot_trolley_trend <- function(data, value_column) {
  library(dplyr)
  library(lubridate)
  library(zoo)
  library(plotly)
  
  plot_data <- data %>%
    filter(year(Date) == 2025) %>% # Data is already filtered for a single year
    group_by(Date) %>%
    summarise(Total_8am_Trolleys = sum(.data[[value_column]], na.rm = TRUE), .groups = "drop") %>%
    arrange(Date) %>%
    mutate(
      RollingAvg_7Day = zoo::rollmean(Total_8am_Trolleys, k = 7, fill = NA, align = "right"),
      RollingAvg_30Day = zoo::rollmean(Total_8am_Trolleys, k = 30, fill = NA, align = "right"),
      DayOfWeek = weekdays(Date)
    )
  
  if (nrow(plot_data %>% filter(!is.na(RollingAvg_7Day))) == 0) {
    p <- plotly_empty() %>%
      add_annotations(
        text = "No data to calculate rolling average.",
        x = 0.5, y = 0.5,
        showarrow = FALSE,
        font = list(size = 18)
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
    return(p)
  }
  
  p <- plot_data %>%
    plot_ly(
      x = ~Date,
      y = ~RollingAvg_7Day,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#0048A8', shape = "spline", smoothing = 1.3),
      text = ~sprintf(
        "Date: %s<br>Day: %s<br>7-Day Avg: %.0f",
        strftime(Date, format = "%d %b"),
        DayOfWeek,
        RollingAvg_7Day
      ),
      hoverinfo = "text",
      name = "7-Day Avg"
    ) %>%
    add_trace(
      y = ~RollingAvg_30Day,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#4FA7AF', shape = "spline", smoothing = 1.3, dash = 'dash'),
      text = ~sprintf(
        "Date: %s<br>Day: %s<br>30-Day Avg: %.0f",
        strftime(Date, format = "%d %b"),
        DayOfWeek,
        RollingAvg_30Day
      ),
      hoverinfo = "text",
      name = "30-Day Avg"
    ) %>%
    layout(
      title = "",
      xaxis = list(
        title = "",               
        type = "date",
        tickformat = "%b",          
        dtick = "M1",               
        ticklabelmode = "period",  
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Rolling Average Trolleys",
        showgrid = FALSE
      ),
      margin = list(l = 50, r = 50, b = 50, t = 50),
      showlegend = TRUE
    )
  
  return(p)
}






