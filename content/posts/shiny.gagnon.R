library(shiny)
library(MASS)

sim_class <- function(n,p,G)
{
  mean1 <- rep(0, p)
  mean2 <- rep(2, p)
  cov1 <- diag(p)
  cov2 <- diag(p)
  class1 <- mvrnorm(n/2, mean1, cov1)
  class2 <- mvrnorm(n/2, mean2, cov2)
  data <- data.frame(rbind(cbind(class1, rep(1, n/2)),
                           cbind(class2, rep(2, n/2))))
  return(data)
}

sim_reg <- function(n,p)
{
  set.seed(123)
  mu <- rep(0, p)
  Sigma <- diag(p)
  x <- mvrnorm(n, mu, Sigma)
  return(x)
}

# Define UI for application
ui <- fluidPage(
  
  # App title
  titlePanel("Zach's Shiny App"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      # Radio button input for learner type
      radioButtons("learner_type", "Choose learner type:",
                   choices = c("Regression", "Classification"),
                   selected = "Regression"),
      
      # Select input for data set type
      selectInput("data", "Choose data set:",
                  choices = c("Data from disk (csv required)", "Simulated data"),
                  selected = "Data from disk"),
      
      # Conditional panel for data from user disk
      conditionalPanel(
        condition = "input.data == 'Data from disk'",
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = ".csv")
      ),
      
      # Conditional panel for simulated data inputs
      conditionalPanel(
        condition = "input.data == 'Simulated data'",
        numericInput("n", "Sample size:", value = 100),
        numericInput("p", "Dimension of x:", value = 2),
        numericInput("G", "Number of classes (if classification):", value = 2)
      ),
      
      #Select response variable
      selectInput("response_var", "Select response variable:",
                  choices = colnames(data)),
      
      # Numeric input for number of stochastic holdout replications
      numericInput("n_reps", "Number of stochastic holdout replications:", value = 10)
      
      
    ),
    
    # Main panel
    mainPanel(
      
      # Output(s)
      verbatimTextOutput("output")
      
    )
  )
)

server <- function(input, output, session) {

  # Load data set from disk or generate simulated data
  data <- reactive({
    if (input$data == "Data from disk")
    {
      return(read.csv(input$file1$datapath))
    }
    else
    {
      if(input$learner_type == "Regression")
      {
        sim_data <- sim_reg(input$n, input$p)
        return(sim_data)
      }
      else
      {
        return(sim_class(input$n, input$p, input$G))
      }
    }
  })
  
  response <- reactive({
    data[, input$response_var, drop = FALSE]
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "response_var",
                      choices = colnames(data()))
  })
  
}

shinyApp(ui = ui, server = server)
