# auth_module.R

# UI for the login module
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
      # Hardcoded credentials (Change To DB or FlatFILE later)
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
    
    # Return the authentication status
    return(authenticated)
  })
}