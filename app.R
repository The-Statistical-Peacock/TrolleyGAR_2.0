# Load the shiny and bslib libraries
library(shiny)
library(bslib) # We'll use bslib for styling and layout

# Define the UI for the application
ui <- page_fluid( # Use page_fluid from bslib for a modern layout
  title = "TrolleyGAR 2.0", # Set the page title

  # Apply a bslib theme (you can choose different themes)
  theme = bs_theme(version = 5, bootswatch = "cosmo"), # Example theme: Cosmo

  # Conditional panel to show the login page
  conditionalPanel(
    condition = "!output.authenticated", # Condition based on server output
    # Apply custom styles for the background color and centering
    style = "background-color: rgb(0, 68, 57); color: white; min-height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center;", # Centering the main content
    
    div( # Use a div to group login elements and apply padding
      style = "text-align: center; padding: 20px;",
      
      # Add the title
      h1("Welcome to TrolleyGAR 2.0", style = "color: white; font-size: 3.5rem; margin-bottom: 20px;"),
      # Reverted h3 font size
      
      
      # Use card from bslib for a nice container for the login form
      card(
        # Updated card_header text
        card_header("Please Enter Login Credentials", style = "color: white; font-size: 1.2rem; background-color: rgb(0, 72, 168);"), # Set header text color to black for contrast
        card_body(
          div(
            style = "display: flex; flex-direction: column; align-items: center;", # Use flexbox to center items vertically
            textInput("username", "Username:", ""),
            passwordInput("password", "Password:", ""),
            actionButton("login_button", "Login", class = "btn-primary") # Add a Bootstrap class
          ) 
        ),
        width = "350px" # Increased width slightly for better spacing
      ),
      
      h3("brought to you by Planning & Performance", style = "color: white; margin-bottom: 30px; font-family: monospace; font-size: 1.5rem;") # Added margin below title
    ),
    div(
      style = "position: absolute; top: 20px; left: 40px;", # Position logo in top left
      img(src = "hse_logo_white.png", alt = "HSE Logo", style = "width: 175px; height: 150px;") # Adjusted width for top corner and updated src
    )
  ),

  # Conditional panel to show the dashboard content after successful login
  conditionalPanel(
    condition = "output.authenticated", # Condition based on server output
    layout_sidebar( # Use layout_sidebar for a simple layout structure
      sidebar = sidebar( # Define a sidebar (can be empty or contain controls)
        title = "Dashboard Controls",
        # You could add controls here if needed, e.g., another slider
        p("Welcome to the Dashboard!")
      ),
      # Main content area
      card( # Use card for the plot and slider
        card_header("Old Faithful Geyser Data"),
        card_body(
          layout_columns( # Arrange elements in columns
            col_widths = c(4, 8), # 4 units for slider, 8 for plot (out of 12 total)
            # Input control
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            # Plot output
            plotOutput("distPlot")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {

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
    # Hardcoded credentials for demonstration purposes
    valid_username <- "user"
    valid_password <- "password"

    # Check if entered credentials match
    if (input$username == valid_username && input$password == valid_password) {
      authenticated(TRUE) # Set authenticated status to TRUE
    } else {
      # Display an error message if login fails
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password.",
        easyClose = TRUE
      ))
    }
  })

  # --- Dashboard Server Logic (runs only when authenticated) ---

  # Render the plot for the Old Faithful example
  output$distPlot <- renderPlot({
    # Ensure this code only runs if the user is authenticated
    req(authenticated())

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  # --- End Dashboard Server Logic ---
}

# Run the application
shinyApp(ui = ui, server = server)
