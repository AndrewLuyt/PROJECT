#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      shiny::selectInput(inputId = 'dataset', label = "Choose dataset",
                         choices = c('foo', 'mtcars', 'Orange'),
                         selected = 'Orange', multiple = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="main",
                  tabPanel("summary",
                           plotOutput("distPlot"),
                           verbatimTextOutput('lm.summary'),
                           verbatimTextOutput('knn.summary'),
                           verbatimTextOutput('rf.summary')),
                  tabPanel("linear",
                           tableOutput('linear.table')),
                  tabPanel("knn"),
                  tabPanel("randomforest"))


    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # df()[, 1] always the response Y
  df <- reactive({
    switch (input$dataset,
      foo = foo,
      mtcars = mtcars,
      Orange = data.frame(circumference = Orange$circumference, age = Orange$age)
    )
  })

  modelFormula <- reactive({
    switch(input$dataset,
           mtcars = as.formula(mpg ~ .),
           Orange = as.formula(circumference ~ age))
  })

  linearModel <- reactive({
    fit <- train(modelFormula(), df(), method='lm', trControl = tc)$finalModel
  })

  knnModel <- reactive({
    fit <- train(modelFormula(), df(), method='knn', trControl = tc)$finalModel
  })

  rfModel <- reactive({
    fit <- train(modelFormula(), df(), method='rf', trControl = tc)$finalModel
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- df()[, 1]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  output$linear.table <- renderTable(df())

  output$lm.summary <- renderPrint(postResample(predict(linearModel()), df()[, 1]))
  output$knn.summary <- renderPrint(postResample(predict(knnModel(), df()[, 1]), df()[, 1]))
  output$rf.summary <- renderPrint(postResample(predict(rfModel()), df()[, 1]))
}

# Run the application
shinyApp(ui = ui, server = server)
