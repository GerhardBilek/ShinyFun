library(shiny)
library(utils)
library(gridExtra)

swiss <- swiss[,-3]    # remove "Examination" from Dataset

##definition for the scatterplot
#------------------------------------------------------------------------------------------------
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#-------------------------------------------------------------------------------------

ui <- fluidPage(
  navbarPage(title = 'Swiss Data',
    tabPanel('Exploration', tags$h3("Data Exploration: Distribution of Swiss datasets "), #tableOutput("rawdata_swiss"),
             sidebarLayout
              (
                
                sidebarPanel
                (
                  radioButtons("dataset", "Select a Dataset", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" ))
                ),
                
              mainPanel(tags$h4("Data and Visualizations:"),
                  tabsetPanel(
                    tabPanel("All data", tableOutput("rawdata_swiss") ),
                    tabPanel("Summary", verbatimTextOutput("summary") ),
                    tabPanel("Histogram & Boxplot", plotOutput("hist"), plotOutput("boxplot")),
                    tabPanel("QQ-Plot", plotOutput("qqplot")),
                    tabPanel("Scatterplot", plotOutput("scatter"))
                  )
                )
              )),
    tabPanel('Correlation', "TEXT"),
    tabPanel('Linear Model', tags$h4("Enter your dependent and independent variables")),
            sidebarLayout(
              sidebarPanel(
                selectInput("regressand", "Dependent Variable", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Fertility"),
                selectInput("regressor1", "Independent Variable 1", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality", "NONE"), selected = "Agriculture"),
                selectInput("regressor2", "Independent Variable 2", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Education"),
                selectInput("regressor3", "Independent Variable 3", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Catholic"),
                selectInput("regressor4", "Independent Variable 4", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Infant.Mortality"),
                #checkbox group statt der drop downs? verbesserung für modellanpassung?
                checkboxGroupInput("checkbox", "check independent variables", choiceNames = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), choiceValues = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"))

              ),
              mainPanel(tags$h4("Possible linear models:"),
                        verbatimTextOutput("stepmodel"),
                        actionButton("analysis","Analyze!"),
                        verbatimTextOutput("modelFormula"),
                        verbatimTextOutput("modelSummary"),
                        verbatimTextOutput("value"),
                        tableOutput("data1") 
              )
            )
    ))

  
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Fertility" = swiss$Fertility,
           "Agriculture" = swiss$Agriculture,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Infant.Mortality" = swiss$Infant.Mortality
           )
  })
  output$rawdata_swiss <- renderTable({
    dataset <- swiss
    dataset})  # head(dataset)
   
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    hist(dataset)})
  
  output$boxplot <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)})
  
  output$qqplot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset); qqline(dataset, col=2)})
  
  output$scatter <- renderPlot({
    dataset <- datasetInput()
    pairs(swiss, lower.panel = panel.smooth, upper.panel = panel.cor,
          gap=0, row1attop=FALSE, main = "Scatterplot")})
  
  output$stepmodel <- renderPrint({
    fit <- lm(swiss[,input$regressand] ~ swiss[,input$regressor1] + swiss[,input$regressor2] + swiss[,input$regressor3] + swiss[,input$regressor4])
    names(fit$coefficients) <- c("Intercept", input$regressor1, input$regressor2, input$regressor3)
    step(fit)})
  
  myformula <- reactive({
    expln <- paste(input$checkbox, collapse = "+")
    as.formula(paste(input$regressand, " ~ ", expln))
  })
  
  mod <- eventReactive(input$analysis, {
    lm(myformula(), data = swiss)
  })
  
  output$modelFormula <- renderPrint({
    myformula()
  })
  
  output$modelSummary <- renderPrint({
    summary(mod())
  })


  #output$data1 <- renderTable({
   # swiss[, c("Fertility", input$checkbox), drop = F] # input$checkbox ist eine function, type closure
  #}, rownames = T)
  
  #output$stepmodel <- renderPrint({
   # fit <- lm(swiss[,input$regressand] ~ input$checkbox)
    #names(fit$coefficients) <- c("Intercept", input$regressor1, input$regressor2, input$regressor3)
    #step(fit)})
}


shinyApp(ui = ui, server = server)
