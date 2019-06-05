library(shiny)
library(utils)
library(gridExtra)
library(ggplot2) 

swiss <- swiss[,-3]    # remove "Examination" from Dataset
#snames <- colnames(s)

##definition for scatterplot
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
             #tabPanel('swiss raw data', "Main Text"),
             tabPanel('Exploration', tags$h3("Data Exploration: Distribution of Swiss datasets "), #tableOutput("rawdata_swiss"),
                      sidebarLayout
                      (
                        
                        sidebarPanel
                        (
                          radioButtons("dataset", "Select a Dataset", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" ))
                          #selectInput("dataset", "Pick a variable", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
                          
                        ),
                        
                        mainPanel(tags$h4("Data and Visualizations:"),
                                  #tableOutput("rawdata_swiss"),
                                  #verbatimTextOutput("summary"),
                                  #plotOutput("hist"),
                                  #plotOutput("boxplot"),
                                  tabsetPanel(
                                    tabPanel("All data", tableOutput("rawdata_swiss") ),
                                    tabPanel("Summary", verbatimTextOutput("summary") ),
                                    tabPanel("Histogram & Boxplot", plotOutput("hist"), plotOutput(outputId = "boxplot", brush = brushOpts(direction = "y", id= "plot_brush_"))),
                                    tabPanel("QQ-Plot", plotOutput("qqplot")),
                                    tabPanel("Scatterplot", plotOutput("scatter"))
                                  )
                        )
                      )),
             tabPanel('Correlation', "TEXT"),
             tabPanel('Linear Model', tags$h3("Enter your dependent and independent variables")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("regressand", "Dependent Variable", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
                 selectInput("regressor1", "Independent Variable 1", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
                 selectInput("regressor2", "Independent Variable 2", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
                 selectInput("regressor3", "Independent Variable 3", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" ))
                 
               ),
               mainPanel(tags$h4("Modelle:"),
                         actionButton("analysis","Analyze!"),
                         verbatimTextOutput("stepmodel"),
                         verbatimTextOutput("modelFormula"),
                         verbatimTextOutput("modelSummary")
               )
             )
  ))


server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Fertility" = swiss$Fertility,
           "Agriculture" = swiss$Agriculture,
           "Examination" = swiss$Examination,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Fertility" = swiss$Fertility,
           "Infant.Mortality" = swiss$Infant.Mortality
    )
  })
  
  #--------Definition for Boxplot with interactive area 
  # dinput = input$dataset
  # dinput$Fertility = as.factor(dinput$Fertility)
  
  rds <- reactiveValues(data=swiss)
  
  #------------------------------------------------
  
  
  output$rawdata_swiss <- renderTable({
    dataset <- swiss
    dataset})  # head(dataset)
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    hist(dataset)})
  
  #---interactive boxplot---somehow we need to take feature into the ggplot- for now it is hard coded with only fertility--------------------------------------------------
  output$boxplot <- renderPlot({
    feature <- switch(input$dataset,
                      "Fertility" = swiss$Fertility,
                      "Agriculture" = swiss$Agriculture,
                      "Examination" = swiss$Examination,
                      "Education" = swiss$Education,
                      "Catholic" = swiss$Catholic,
                      "Fertility" = swiss$Fertility,
                      "Infant.Mortality" = swiss$Infant.Mortality
    )
    
    ggplot(rds$data, aes(y = Fertility)) + geom_boxplot(outlier.colour = "red") +guides(color=guide_legend(),size=guide_legend())
  })
  #observe function to make the plot reactive 
  observe({
    df = brushedPoints(rds$data, brush = input$plot_brush_, allRows = TRUE) 
    rds$data = df[df$selected_== FALSE,]
  })
  #--------------------------------------------------------------------------------
  
  output$qqplot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset); qqline(dataset, col=2)})
  
  output$scatter <- renderPlot({
    dataset <- datasetInput()
    pairs(swiss, lower.panel = panel.smooth, upper.panel = panel.cor,
          gap=0, row1attop=FALSE, main = "Scatterplot")})
  
  output$stepmodel <- renderPrint({
    fit <- lm(swiss[,input$regressand] ~ swiss[,input$regressor1] + swiss[,input$regressor2] + swiss[,input$regressor3])
    names(fit$coefficients) <- c("Intercept", input$regressor1, input$regressor2, input$regressor3)
    step(fit)})
  
  myformula <- reactive({
    expln <- paste(c(input$regressor1, input$regressor2, input$regressor3), collapse = "+")
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
}


shinyApp(ui = ui, server = server)