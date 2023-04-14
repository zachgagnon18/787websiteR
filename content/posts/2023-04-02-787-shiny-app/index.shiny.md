---
title: "787 Shiny App"
author: "Zach Gagnon"
date: 2023-04-02
slug: []
categories: []
tags: []
---

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
  )
    
    # Main panel
  mainPanel(
    tabsetPanel(
      tabPanel("Results", 
               fluidRow(
                 column(width = 6, 
                        plotlyOutput("boxplot"), 
                        plotOutput("response_plot")),
                 column(width = 6, 
                        DT::DTOutput("corrplot"),
                        plotOutput("anova_plot"))
               ),
               tags$hr(),
               verbatimTextOutput("summary")
      ),
      tabPanel("About",
               tags$h3("Data Description"),
               verbatimTextOutput("data_description"),
               tags$h3("Author Description"),
               verbatimTextOutput("author_description"),
               tags$h3("Relevant Info"),
               verbatimTextOutput("relevant_info")
      )
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
  
    # Train and test models
  results <- reactive({
    if (input$task == "Regression") {
      metric <- "RMSE"
      trainControl <- trainControl(method = "repeatedcv", number = 5, repeats = input$replications)
      models <- list(
        KNN = train(Y ~ ., data = dataset(), method = "knn", trControl = trainControl),
        CART = train(Y ~ ., data = dataset(), method = "rpart", trControl = trainControl),
        SVM = train(Y ~ ., data = dataset(), method = "svmRadial", trControl = trainControl)
      )
    }
     else {
      metric <- "Accuracy"
      trainControl <- trainControl(method = "repeatedcv", number = 5, repeats = input$replications, classProbs = TRUE, summaryFunction = twoClassSummary)
      models <- list(
        KNN = train(Y ~ ., data = dataset(), method = "knn", trControl = trainControl),
        CART = train(Y ~ ., data = dataset(), method = "rpart", trControl = trainControl),
        SVM = train(Y ~ ., data = dataset(), method = "svmRadial", trControl = trainControl)
      )
    }
    models
  })
  
        #Output summary
      output$summary <- renderPrint({
        print(summary(results()))
      })
      
      #Output boxplot
      output$boxplot <- renderPlotly({
        data <- lapply(results(), function(x) data.frame(model = x$method, error = x$results$Mean))
        data <- do.call(rbind, data)
        gg <- ggplot(data, aes(x = model, y = error)) +
          geom_boxplot() +
          xlab("Model") +
          ylab(paste0("Test ", tolower(metric)))
        ggplotly(gg)
      })
      
      #Output response plot
      output$response_plot <- renderPlot({
        if (input$response != "") {
          ggplot(dataset(), aes_string(input$response)) +
            geom_density(fill = "blue") +
            ggtitle(input$response)
        }
      })
      
      #Output correlation plot
      output$corrplot <- renderPlot({
        corrplot(cor(dataset()[, -ncol(dataset())]), method = "circle")
      })
      
      #Output ANOVA plot
      output$anova_plot <- renderPlot({
        if (input$anova) {
          p_values <- sapply(results(), function(x) {
            res <- pairwise.t.test(x$results$Accuracy, x$method, p.adjust.method = "bonferroni")
            res$p.value
          })
          data <- data.frame(model = names(results()), p_value = p_values)
          ggplot(data, aes(x = model, y = p_value)) +
            geom_bar(stat = "identity", fill = "blue") +
            geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
            xlab("Model") +
            ylab("P-value") +
            ggtitle("ANOVA Comparison")
        }
      })
      
      #Output data description
      output$data_description <- renderPrint({
        if (!is.null(input$file)) {
          cat(paste0("Data set name: ", input$file$name, "\n"))
          cat(paste0("Number of observations: ", nrow(dataset()), "\n"))
          cat(paste0("Number of variables: ", ncol(dataset()) - 1, "\n"))
        } else {
          cat(paste0("Simulated data set\n"))
          cat(paste0("Sample size: ", input$n, "\n"))
          cat(paste0("Dimension of predictor variables: ", input$p, "\n"))
          cat(paste0("Number of classes: ", input$G, "\n"))
        }
      })
      
      #Output author description
      output$author_description <- renderText({
        "This Shiny app was created by Zach Gagnon."
      })
      
      #Output relevant info
      output$relevant_info <- renderText({
        "This Shiny app compares the performance of several machine learning 
        models for either regression or classification tasks. The user can 
        either choose a data set from disk or simulate one. The user can also 
        choose a response variable to analyze and select the number of 
        stochastic holdout replications. The user can choose from several 
        machine learning models to be compared, and can optionally display an 
        ANOVA comparison of the test errors. The app displays a summary of the 
        results, boxplots of the test errors by model, a density plot of the 
        response variable, a correlation matrix of the predictor variables, and 
        an ANOVA comparison plot (if selected)."
      })
  
}

shinyApp(ui = ui, server = server)
