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
  
  plot_data <- data %>%
    filter(year(Date) == 2025) %>%
    group_by(Date) %>%
    summarise(Total_8am_Trolleys = sum(.data[[value_column]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Date) %>%
    mutate(
      DayOfWeek = weekdays(Date, abbreviate = FALSE),
      IsWeekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
    )
  
  if (nrow(plot_data) == 0) {
    p <- plotly_empty() %>%
      add_annotations(
        text = "No data for 2025 with current filters.",
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
  
  colors_for_bars <- c("Weekday" = "#9BAAB3", "Weekend" = "#DF8234")
  
  p <- plot_data %>%
    plot_ly(
      x = ~Date,
      y = ~Total_8am_Trolleys,
      type = 'bar',
      color = ~IsWeekend,
      colors = colors_for_bars,
      # --- NEW: Use text aesthetic to format hover info directly ---
      text = ~sprintf("Date: %s<br>Day: %s<br>Trolleys: %s",
                      strftime(Date, format = "%d %b"), # Format date for display
                      DayOfWeek, # Directly use DayOfWeek
                      Total_8am_Trolleys),
      hoverinfo = "text" # Tell Plotly to use the 'text' aesthetic for hover
      # --- END NEW ---
      # customdata = ~DayOfWeek # No longer needed if we build hover with 'text'
    ) %>%
    layout(
      xaxis = list(title = "", type = "date", tickformat = "%d %b"),
      yaxis = list(title = "Total Trolleys"),
      margin = list(l = 50, r = 50, b = 50, t = 50),
      showlegend = TRUE
    )
  
  return(p)
}




