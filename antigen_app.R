library(shiny)
library(rsconnect)

antigens <- c(
  D = 0.85,
  C = 0.68,
  c = 0.8,
  E = .29,
  e = 0.98,
  k = 0.99,
  K = 0.09,   
  Jka = 0.77,
  Jkb = 0.74,
  S = 0.55,
  s = 0.89,
  Fya = 0.66,
  Fyb = 0.83,
  Cw = 0.02,
  M = .78
)

neg_antigens <- 1 - antigens 

# Function to calculate units needed to screen based on negative antigen frequencies
units_to_screen <- function(n_needed, neg_antigens){
    units <- n_needed/prod(neg_antigens)
    print(units)
}


# UI
ui <- fluidPage(
  titlePanel("Units to Screen Calculator"),
  
  tags$style(HTML("
    #result {
      color: blue;
      font-weight: bold;
      font-size: 18px;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("units_needed", "Number of antigen-negative units needed:", 1, min = 1),
      selectizeInput("selected_antigens",
                     "Select antigens to be negative for:",
                     choices = names(neg_antigens),
                     multiple = TRUE)
    ),
    
    mainPanel(
      verbatimTextOutput("result"),
      br(),
      h4("Negative Frequencies of Selected Antigens"),
      tableOutput("neg_table")
    )
  )
)


# Server
server <- function(input, output) {
  output$result <- renderPrint({
    req(input$selected_antigens)
    selected_neg_freqs <- neg_antigens[input$selected_antigens]
    units_to_screen <- input$units_needed / prod(selected_neg_freqs)
    cat("You will need to screen", ceiling(units_to_screen), "units.")
  })

  output$neg_table <- renderTable({
    req(input$selected_antigens)
    
    freqs <- round(neg_antigens[input$selected_antigens] * 100, 1)
    freqs_pct <- paste0(freqs, "%")
    
    data.frame(
      Antigen = input$selected_antigens,
      `Negative Frequencies` = freqs_pct,
      check.names = FALSE
    )
  })
}


shinyApp(ui, server)
