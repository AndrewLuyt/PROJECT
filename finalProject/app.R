#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(shiny)
library(caret)  # unified modeling frameworkb

tc <- trainControl(method = 'cv', number = 5)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Comparison of Statistical Models"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a dataset. Three regression models will be created and their training performance plotted."),
      shiny::selectInput(inputId = 'dataset', label = "Choose dataset",
                         choices = c('foo', 'mtcars', 'swiss'),
                         selected = 'mtcars', multiple = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="main",
                  tabPanel("summary",
                           plotOutput('rmsePlot'),
                           plotOutput("predictPlot")),
                  tabPanel("linear",
                           plotOutput('linear.residuals'),
                           helpText("Model summary:"),
                           verbatimTextOutput('lm.summary'),
                           verbatimTextOutput("lm.longsummary"),
                           plotOutput('linear.diagnostics')),
                  tabPanel("knn",
                           plotOutput('knn.residuals'),
                           helpText("Model summary:"),
                           verbatimTextOutput('knn.summary')),
                  tabPanel("rf",
                           plotOutput('rf.residuals'),
                           helpText("Model summary:"),
                           verbatimTextOutput('rf.summary')))
    )
  )
)


server <- function(input, output) {
  set.seed(1712)

  # df()[, 1] is always the response Y
  df <- reactive({
    switch (input$dataset,
      foo = foo,
      mtcars = mtcars,
      swiss = cbind(swiss[, 6, drop=F], swiss[, 1:5])
    )
  })

  modelFormula <- reactive({
    switch(input$dataset,
           mtcars = as.formula(mpg ~ .),
           swiss = as.formula(Infant.Mortality ~ .))
  })

  linear.model <- reactive({
    fit <- train(modelFormula(), df(), method='lm', trControl = tc)
  })

  knn.model <- reactive({
    fit <- train(modelFormula(), df(), method='knn', trControl = tc)
  })

  rf.model <- reactive({
    fit <- train(modelFormula(), df(), method='rf', trControl = tc)
  })

  predictions <- reactive({
    tibble(linear.model = predict(linear.model()),
           knn.model = predict(knn.model()),
           rf.model = predict(rf.model()),
           y = df()[, 1])
  })

  output$predictPlot <- renderPlot({
    predictions() %>%
      pivot_longer(cols = c('linear.model', 'knn.model', 'rf.model'),
                   names_to = 'model', values_to = 'prediction') %>%
      ggplot(aes(y, prediction, col=model)) +
      geom_abline(intercept=0, slope=1, lty=2, lwd=1) +
      geom_line(lwd=0.8) +
      labs(title = 'Prediction Performance', y = 'Predicted', x='Actual')
  })

  output$rmsePlot <- renderPlot({
    tibble(linear = postResample(predict(linear.model()), df()[, 1])[1],
           knn = postResample(predict(knn.model()), df()[, 1])[1],
           randomForest = postResample(predict(rf.model()), df()[, 1])[1]) %>%
      pivot_longer(cols = everything(), names_to = 'model', values_to = 'RMSE') %>%
      ggplot(aes(model, RMSE)) +
      geom_col() +
      labs(title = "Prediction error (lower is better)")
  })

  #output$linear.table <- renderTable(predictions())
  output$linear.residuals <- renderPlot(hist(resid(linear.model()), main="Residuals"))
  output$knn.residuals <- renderPlot(hist(resid(knn.model()), main="Residuals"))
  output$rf.residuals <- renderPlot(hist(resid(rf.model()), main="Residuals"))

  output$lm.summary <- renderPrint({
    postResample(predictions()$linear.model, df()[, 1])
  })
  output$lm.longsummary <- renderPrint({
    summary(linear.model())
  })
  output$linear.diagnostics <- renderPlot({
    par(mfrow=c(2,2))
    plot(linear.model()$finalModel)
  })
  output$knn.summary <- renderPrint({
    postResample(predictions()$knn.model, df()[, 1])
  })
  # output$rf.summary <- renderPrint(postResample(predict(rf.model()), df()[, 1]))
  # output$rf.summary <- renderPrint("Random forest summary???")
  output$rf.summary <- renderPrint({
    postResample(predictions()$rf.model, df()[, 1])
  })
}

# Run the application
shinyApp(ui = ui, server = server)
