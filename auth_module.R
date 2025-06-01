# auth_module.R

# Load necessary libraries for database connection
library(shiny)
library(bslib)
library(DBI)
library(duckdb) # Ensure this is installed: install.packages("duckdb")

# UI for the login module (remains the same)
loginUI <- function(id) {
  ns <- NS(id) # Create a namespace for the module
  
  # Conditional panel to show the login page
  conditionalPanel(
    condition = paste0("!", ns("authenticated")), # Condition based on server output
    style = "background-color: rgb(0, 68, 57); color: white; min-height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center;",
    div(
      style = "text-align: center; padding: 20px;",
      h1("Welcome to TrolleyGAR 2.0", style = "color: white; font-size: 3.5rem; margin-bottom: 20px;"),
      card(
        card_header("Please Enter Login Credentials", style = "color: white; font-size: 1.2rem; background-color: rgb(0, 72, 168);"),
        card_body(
          div(
            style = "display: flex; flex-direction: column; align-items: center;",
            textInput(ns("username"), "Username:", ""), # Namespace inputs
            passwordInput(ns("password"), "Password:", ""), # Namespace inputs
            actionButton(ns("login_button"), "Login", class = "btn-primary") # Namespace inputs
          )
        ),
        width = "350px"
      ),
      h3("brought to you by Planning & Performance", style = "color: white; margin-bottom: 30px; font-family: monospace; font-size: 1.5rem;")
    ),
    div(
      style = "position: absolute; top: 20px; left: 40px;",
      img(src = "hse_logo_white.png", alt = "HSE Logo", style = "width: 175px; height: 150px;")
    )
  )
}

# Server logic for the login module
loginServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to track authentication status
    authenticated <- reactiveVal(FALSE)
    
    # Output to control conditional panels
    output$authenticated <- reactive({
      authenticated()
    })
    
    # Make the output accessible to the UI
    outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
    
    # Observe the login button click
    observeEvent(input$login_button, {
      # Get entered credentials
      entered_username <- input$username
      entered_password <- input$password
      
      con <- NULL # Initialize connection to NULL
      tryCatch({
        # --- LOCAL DUCKDB DATABASE CONNECTION ---
        # Define the path to your existing local DuckDB database file.
        # Ensure 'auth_users.duckdb' is in the same directory as your app.R or auth_module.R.
        local_db_path <- "Auth_Users.duckdb" 
        
        # Connect to the local DuckDB database file
        con <- dbConnect(duckdb::duckdb(), dbdir = local_db_path)
        
        # Query your local database for user credentials
        # IMPORTANT: Using 'user' and 'password' as your column names, and 'USERS' as table name.
        quoted_username <- dbQuoteString(con, entered_username)
        
        query <- paste0(
          "SELECT password FROM USERS WHERE username = ", # Column 'user' for username
          quoted_username
        )
        
        user_data <- dbGetQuery(con, query)
        # --- END LOCAL DUCKDB DATABASE CONNECTION ---
        
        # Check if a user was found and if the password matches
        if (nrow(user_data) == 1 && user_data$password == entered_password) {
          authenticated(TRUE) # Set authenticated status to TRUE
        } else {
          # Display an error message if login fails
          showModal(modalDialog(
            title = "Login Failed",
            "Invalid username or password.",
            easyClose = TRUE
          ))
        }
      }, error = function(e) {
        # Handle database connection or query errors
        showModal(modalDialog(
          title = "Database Error",
          paste0("An error occurred: ", e$message, ". Please ensure the '", local_db_path, "' file is accessible and contains a 'USERS' table with 'user' and 'password' columns."),
          easyClose = TRUE
        ))
        print(paste0("Database connection or query error: ", e$message)) # Log the error for debugging
      }, finally = {
        # Disconnect from the database
        if (!is.null(con)) {
          dbDisconnect(con)
        }
      })
    })
    
    # Return the authentication status
    return(authenticated)
  })
}