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
library(stringr)
library(tidyr)
library(shiny)
library(randomForest)
library(caret)  # unified modeling frameworkb
library(DT)
library(doMC)
registerDoMC(6)

set.seed(1712)
data(diamonds)
diamonds = slice_sample(diamonds, prop = 0.02)

PLOTHEIGHT = "250px"
PLOTHEIGHT2 = "350px"
vars.mtcars = names(mtcars)[c(2,4,5,7,8,9,10,11)]
vars.swiss = names(swiss)[c(2,4,5)]
vars.diamonds = names(diamonds)[c(2:6)]
tc <- trainControl(method = 'none', seeds=1712, allowParallel = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Comparison of Statistical Models"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a dataset. Three regression models will be created and their training performance plotted.",
               "The", code("diamonds"), "dataset takes longer to train."),
      shiny::selectInput(inputId = 'dataset', label = "Choose dataset",
                         choices = c('mtcars', 'swiss', 'diamonds'),
                         selected = 'mtcars', multiple = FALSE),
      conditionalPanel(condition="input.dataset=='mtcars'",
                       helpText("The", code("miles per gallon"), "of cars will be predicted."),
                       helpText("The variables", code('wt'), 'and', code('disp'), 'will always be used.'),
                       checkboxGroupInput("mtcars.vars", label = "Select Extra Variables:",
                                          choices = vars.mtcars,
                                          selected = vars.mtcars)),
      conditionalPanel(condition="input.dataset=='swiss'",
                       helpText(code("Infant mortality"), "of Swiss provinces in 1888 will be predicted."),
                       helpText("The variables", code('Fertility'), 'and', code('Examination'), 'will always be used.'),
                       checkboxGroupInput("swiss.vars", label = "Select Extra Variables:",
                                          choices = vars.swiss,
                                          selected = vars.swiss)),
      conditionalPanel(condition="input.dataset=='diamonds'",
                       helpText(code("Price"), "of diamonds will be predicted."),
                       helpText("The variables", code('carat, x, y, z'), 'will always be used.'),
                       checkboxGroupInput("diamonds.vars", label = "Select Extra Variables:",
                                          choices = vars.diamonds,
                                          selected = vars.diamonds)),
      # if you want to show tab-specific helpers
      # conditionalPanel(condition="input.mainPanel==2",),
      helpText("Click on the tabs to see detailed information on each model.",
               "To optimize a model, try removing variables with low importance.",
               "Different models will find different variables to be important!")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="mainPanel",
                  tabPanel("summary", value=1,
                           h3("Modeling as: ", code(textOutput('modelformula'))),
                           plotOutput('rmsePlot', height = PLOTHEIGHT),
                           plotOutput("predictPlot", height = PLOTHEIGHT2)),
                  tabPanel("linear", value=2,
                           h3("Diagnostics for Linear Model"),
                           plotOutput('linear.residuals', height = PLOTHEIGHT),
                           h4("Performance:"),
                           verbatimTextOutput('lm.summary'),
                           h4("Variable Importance:"),
                           plotOutput("lm.varimp.plot", height = PLOTHEIGHT2),
                           verbatimTextOutput("lm.importance"),
                           h4("Linear Model Diagnostic Plots"),
                           plotOutput('linear.diagnostics'),
                           h4("Detailed Summary"),
                           verbatimTextOutput("lm.longsummary")),
                  tabPanel("knn", value=3,
                           h3("Diagnostics for KNN Model"),
                           plotOutput('knn.residuals', height = PLOTHEIGHT),
                           h4("Performance:"),
                           verbatimTextOutput('knn.summary'),
                           h4("Variable Importance:"),
                           plotOutput("knn.varimp.plot", height = PLOTHEIGHT2),
                           verbatimTextOutput('knn.importance')),
                  tabPanel("rf", value=4,
                           h3("Diagnostics for Random Forest Model"),
                           plotOutput('rf.residuals', height = PLOTHEIGHT),
                           h4("Performance:"),
                           verbatimTextOutput('rf.summary'),
                           h4("Variable Importance"),
                           plotOutput("rf.varimp.plot", height = PLOTHEIGHT2),
                           verbatimTextOutput('rf.importance')))
    )
  )
)


server <- function(input, output) {
  set.seed(1712)
  ggplot2::theme_set(theme_bw())

  # df()[, 1] must be the response Y
  df <- reactive({
    switch (input$dataset,
      mtcars = mtcars,
      swiss = cbind(swiss[, 6, drop=F], swiss[, 1:5]),
      diamonds = cbind(diamonds[, 7, drop=F], diamonds[, c(1:6, 8:10)])
    )
  })

  modelFormula <- reactive({
    switch(input$dataset,
           mtcars = as.formula(paste0('mpg ~ ', mtcars.vars(), '+ wt + disp')),
           swiss = as.formula(paste0('Infant.Mortality ~', swiss.vars(), "+ Fertility + Examination")),
           diamonds = as.formula(paste0('price ~', diamonds.vars(), " + carat + x + y + z")))
  })

  linear.model <- reactive({
    set.seed(1712)
    fit <- train(modelFormula(), df(), method='lm', trControl = tc)
  })

  knn.model <- reactive({
    set.seed(1712)
    fit <- train(modelFormula(), df(), method='knn', trControl = tc)
  })

  rf.model <- reactive({
    set.seed(1712)
    fit <- train(modelFormula(), df(), method='rf', trControl = tc, proximity=TRUE)
  })

  predictions <- reactive({
    tibble(linear.model = predict(linear.model()),
           knn.model = predict(knn.model()),
           rf.model = predict(rf.model()),
           y = df()[, 1])
  })

  # create string representation of selected variables separated by +
  mtcars.vars <- reactive({
    do.call(str_c, as.list(c(input$mtcars.vars, sep='+')))
  })
  swiss.vars <- reactive({
    do.call(str_c, as.list(c(input$swiss.vars, sep='+')))
  })
  diamonds.vars <- reactive({
    do.call(str_c, as.list(c(input$diamonds.vars, sep='+')))
  })

  output$mtv <- renderText(mtcars.vars())

  output$modelformula <- renderText({
    f <- modelFormula()
    paste(f[2], f[1], f[3])
  })

  output$predictPlot <- renderPlot({
    predictions() %>%
      pivot_longer(cols = c('linear.model', 'knn.model', 'rf.model'),
                   names_to = 'model', values_to = 'prediction') %>%
      ggplot(aes(y, prediction, col=model)) +
      geom_abline(intercept=0, slope=1, lty=1, lwd=0.5) +
      geom_line(lwd=0.8) +
      labs(title = 'Prediction Performance (black line represents perfection)', y = 'Predicted', x='Actual')
  })

  output$rmsePlot <- renderPlot({
    tibble(linear = postResample(predict(linear.model()), df()[, 1])[1],
           knn = postResample(predict(knn.model()), df()[, 1])[1],
           randomForest = postResample(predict(rf.model()), df()[, 1])[1]) %>%
      pivot_longer(cols = everything(), names_to = 'model', values_to = 'RMSE') %>%
      ggplot(aes(model, RMSE, fill=model)) +
      geom_col() +
      labs(title = "Prediction error (lower is better)")
  })

  #output$linear.table <- renderTable(predictions())
  output$linear.residuals <- renderPlot(hist(resid(linear.model()), main="Residuals", breaks=8))
  output$knn.residuals <- renderPlot(hist(resid(knn.model()), main="Residuals", breaks=8))
  output$rf.residuals <- renderPlot(hist(resid(rf.model()), main="Residuals", breaks=8))

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
  output$lm.importance <- renderPrint({
    varImp(linear.model())
  })
  output$lm.varimp.plot <- renderPlot({
    dotPlot(varImp(linear.model()))
  })
  output$knn.summary <- renderPrint({
    postResample(predictions()$knn.model, df()[, 1])
  })
  output$knn.importance <- renderPrint({
    varImp(knn.model())
  })
  output$knn.varimp.plot <- renderPlot({
    dotPlot(varImp(knn.model()))
  })
  output$rf.summary <- renderPrint({
    postResample(predictions()$rf.model, df()[, 1])
  })
  output$rf.importance <- renderPrint({
    varImp(rf.model())
  })
  output$rf.varimp.plot <- renderPlot({
    varImpPlot(rf.model()$finalModel, main=NULL, bg='skyblue')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
