# app.R

# Load the shiny and bslib libraries
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyverse) 
library(plotly)

# Source the authentication module
source("auth_module.R")

# Source your helpers file to load data and functions
source("helpers.R") 


# Define the UI for the application
ui <- page_fluid(
  title = "TrolleyGAR 2.0",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly", # This is the DEFAULT/LIGHT theme

  ),
  
  conditionalPanel(
    condition = "!output.auth_authenticated",
    loginUI("auth")
  ),
  
  conditionalPanel(
    condition = "output.auth_authenticated",
    layout_sidebar(
      sidebar = sidebar(
        title = "Dashboard Controls",
          input_dark_mode(
            id = "dark_mode_toggle",
            mode = "light"
          )
        , 
        h3("Region and Hospitals"),
        
        selectInput(
          inputId = "selected_region",
          label = "Select Health Region:",
          choices = c("All" = "", all_regions),
          selected = ""
        ),
        
        selectInput(
          inputId = "selected_hospital",
          label = "Select Hospital:",
          choices = c("All" = "", sort(unique(medmodus$Hospital))), 
          selected = ""
        )
        

      ),
      layout_column_wrap(
        width = 1/1,
        
        # --- Row 1: Key Performance Indicators ---
        card(
          card_header("8am Trolleys - Key Metrics"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              # Metric 1: Current 8am Trolleys
              card(
                class = "text-center",
                card_header(h5("Current 8am")),
                card_body(
                  h2(textOutput("current_8am_metric"))
                )
              ),
              # Metric 2: Average 8am Trolleys
              card(
                class = "text-center",
                card_header(h5("Average 8am YTD (2025)")), 
                card_body(
                  h2(textOutput("average_8am_metric"))
                )
              ),
              # Metric 3: Under 9 Hours 8am Trolleys
              card(
                class = "text-center",
                card_header(h5("% Under 9 Hrs")), 
                card_body(
                  h2(textOutput("under_9hrs_8am_metric"))
                )
              )
            ),
            card(
              card_header("Row 1: Additional 8am Details (e.g., trend)"),
              card_body(
                plotlyOutput("plot_8am_trolley_trend_plot")
              )
            )
          )
        ),
        # --- End Row 1 ---
        
        # Row 2 Placeholder (remains as is)
        card(
          card_header("2pm Trolleys"),
          card_body(
            p("Content for the second row goes here. This might be a larger plot or table."),
            plotOutput("distPlot")
          )
        )
        ,
        
        # Row 3 Placeholder (remains as is)
        card(
          card_header("8pm Trolleys"),
          card_body(
            p("Content for the third row goes here. Perhaps more detailed tables or another set of filters.")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  authenticated_status <- loginServer("auth")
  
  output$auth_authenticated <- reactive({
    authenticated_status()
  })
  outputOptions(output, "auth_authenticated", suspendWhenHidden = FALSE)
  
  
  # Reactive expression to filter the data based on current selections
  filtered_medmodus <- reactive({
    req(authenticated_status())
    
    temp_data <- medmodus
    
    if (input$selected_region != "") {
      temp_data <- temp_data %>%
        filter(`Health Region` == input$selected_region)
    }
    
    if (input$selected_hospital != "") {
      temp_data <- temp_data %>%
        filter(Hospital == input$selected_hospital)
    }
    return(temp_data)
  })
  
  
  # Observer for when a Health Region is selected (one-way dependency)
  observeEvent(input$selected_region, {
    if (input$selected_region != "") {
      hospitals_in_region <- region_hospital_map[[input$selected_region]]
      updateSelectInput(
        session,
        "selected_hospital",
        choices = c("All" = "", sort(hospitals_in_region)),
        selected = ""
      )
    } else {
      updateSelectInput(
        session,
        "selected_hospital",
        choices = c("All" = "", sort(unique(medmodus$Hospital))),
        selected = ""
      )
    }
  }, ignoreNULL = FALSE)
  
  
  # Display selected filters (for debugging/confirmation)
  output$current_region <- renderText({
    paste("Selected Region:", ifelse(input$selected_region == "", "All", input$selected_region))
  })
  
  output$current_hospital <- renderText({
    paste("Selected Hospital:", ifelse(input$selected_hospital == "", "All", input$selected_hospital))
  })
  
  
  # --- Outputs for Row 1 Metrics (Updated Function Calls) ---
  output$current_8am_metric <- renderText({
    req(authenticated_status(), nrow(filtered_medmodus()) > 0)
    value <- current_time_total(filtered_medmodus(), "TimeTotal_8am")
    return(as.character(value))
  })
  
  output$average_8am_metric <- renderText({
    req(authenticated_status(), nrow(filtered_medmodus()) > 0)
    value <- avg__ytd(filtered_medmodus(), "TimeTotal_8am")
    return(as.character(value))
  })
  
  output$under_9hrs_8am_metric <- renderText({
    req(authenticated_status(), nrow(filtered_medmodus()) > 0)
    value <- under_9hrs(filtered_medmodus(), "Point_8am_0_6", "Point_8am_6_9", "TimeTotal_8am")
    return(value) # under_9hrs returns a formatted string directly
  })
  
  output$plot_8am_trolley_trend_plot <- renderPlotly({
    req(authenticated_status())
    plot_trolley_trend(filtered_medmodus(), "TimeTotal_8am")
  })
  
  # --- End Outputs for Row 1 Metrics ---
  
  
  # --- Dashboard Server Logic (runs only when authenticated) ---
  
  output$distPlot <- renderPlot({
    req(authenticated_status())
    
    current_data <- filtered_medmodus()
    
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = 30 + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         main = paste("Old Faithful (Filtered Data Rows:", nrow(current_data), ")"))
  })
  
  # --- End Dashboard Server Logic ---
}

# Run the application
shinyApp(ui = ui, server = server)