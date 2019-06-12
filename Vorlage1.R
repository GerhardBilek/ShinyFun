library(shiny)
library(utils)
library(gridExtra)
library(ggplot2) 

#remove "Examination" from Dataset
#snames <- colnames(s)
swiss <- swiss[,-3]

##predefinition for Correlation "Scatterplot"----------------------------------------------
panel.cor <- function(x,
                      y,
                      digits = 2,
                      prefix = "",
                      cex.cor,
                      ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if (missing(cex.cor))
    cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#----------------------------------------------------------------------------------------------------------
#Definition of the UserInterface
ui <- fluidPage(navbarPage(
  tags$h2(p(code('Swiss Data'))),
  
  #1st tab Panel--------------------------------------------------------------------------------------  
  tabPanel(tags$h3(p(em('All data'))), tags$h2("Overview of data"), hr(), 
           mainPanel(tableOutput("rawdata_swiss"))),
  
  #2nd tab Panel------------------------------------------------------------------------------------------
  tabPanel(tags$h3(p(em('Exploration'))),
           tags$h3("Data Exploration: Distribution of Swiss datasets "), hr(), br(),
           
           sidebarLayout
           (
             sidebarPanel
             (radioButtons(
               "dataset",
               "Select a Dataset",
               choices = c(
                 "Fertility",
                 "Agriculture",
                 "Education",
                 "Catholic",
                 "Infant.Mortality"
               )
             )),
             
             mainPanel(tags$h4("Data and Visualizations:"),
                       #tableOutput("rawdata_swiss"),
                       #verbatimTextOutput("summary"),
                       #plotOutput("hist"),
                       #plotOutput("boxplot"),
                       tabsetPanel(
                         tabPanel("Summary", verbatimTextOutput("summary")),
                         tabPanel("Histogram & Boxplot", plotOutput("hist"), plotOutput(outputId = "boxplot", brush = brushOpts(direction = "x", id= "plot_brush_"))),
                         tabPanel("QQ-Plot", plotOutput("qqplot"))#,
                         #tabPanel("Scatterplot", plotOutput("scatter"))
                       )
             )
           )),
  
  
  #3rdtabpanel-----------------------------------------------------------------------------------------------------------
  tabPanel(tags$h3(p(em('Correlations'))), tags$h2("Overview all Correlations"),hr(),
           mainPanel(plotOutput("scatter"))
  ),
  
  #4th tabpanel-------------------------------------------------------------------------------------------------------------
  tabPanel(tags$h3(p(em('Linear Model'))), tags$h3("Enter your dependent and independent Variables"),hr(),
           sidebarLayout(
             sidebarPanel(
               selectInput("regressand", "Dependent Variable", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
               checkboxGroupInput("checkbox", "Check independent variables", choiceNames = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), choiceValues = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality")),
               radioButtons("transformation", "Apply this transformation", choices = c("No Transformation", "Log(X)", "Log(Y)", "Log/Log", "Standardisation", "Polynom?"))
             ),
             mainPanel(tags$h4("Possible linear Models:"), hr(),
                       tabsetPanel(
                         tabPanel("Step (AIC)", verbatimTextOutput("stepmodel"),
                                  actionButton("analysis","I have chosen my independents and want to ANALYSE")
                         ),
                         tabPanel("Formel und Modell", verbatimTextOutput("modelFormula"),
                                  verbatimTextOutput("modelSummary")
                         ),
                         tabPanel("Residuenplots", plotOutput("model_plot")
                                  
                         )
                       )
             )
           )
  )
  
))
#-----------------------------------------------------------------------------------------------------------------

#---server-------------------------------------------------------------------------------------------
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Fertility" = swiss$Fertility,
           "Agriculture" = swiss$Agriculture,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Fertility" = swiss$Fertility,
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
  
  #---interactive boxplot---somehow we need to take feature into the ggplot- for now it is hard coded with only fertility--------------------------------------------------
  #--------Definition for Boxplot with interactive area 
  # dinput = input$dataset
  # dinput$Fertility = as.factor(dinput$Fertility)
  rds <- reactiveValues(data=swiss)
  
  output$boxplot <- renderPlot({
    feature <- switch(input$dataset,
                      "Fertility" = swiss$Fertility,
                      "Agriculture" = swiss$Agriculture,
                      "Education" = swiss$Education,
                      "Catholic" = swiss$Catholic,
                      "Infant.Mortality" = swiss$Infant.Mortality
    )
    
    ggplot(rds$data, aes(y = Fertility)) + geom_boxplot(outlier.colour = "red")+ coord_flip() +guides(color=guide_legend(),size=guide_legend())
  }) #  + coord_flip() in the ggplot would make the boxplot horizontal
  
  
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
  
  #Lineares Modell --------------------------------------------------------------------------------
  myformula <- reactive({
    expln <- paste(input$checkbox, collapse = "+")
    as.formula(paste(input$regressand, "~", expln))
    # einen haufen buttons für diverse transf. iwo muss sich formel ändern
    # if? wenn input$transformation == "LOGX" dann as.formula(paste(input$regressand), "~", log(expln)) etc
    #if (input$transformation == "Log(Y)") {
      #expln <- paste(input$checkbox, collapse = "+") # Error in log: non-numeric argument to mathematical function
      #as.formula(paste(log(input$regressand), "~", expln))
      #}
  })
  
  mod <- eventReactive(input$analysis, {
    lm(myformula(), data = swiss)
  })
  
  output$modelFormula <- renderPrint({
    myformula()
  })
  
  output$stepmodel <- renderPrint({
    fit = lm(myformula(), data=swiss)
    step(fit)
  })
  
  output$modelSummary <- renderPrint({
    summary(mod())
  })
  
  output$model_plot <- renderPlot ({
    fit = lm(myformula(), data=swiss)
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  
  #  button fürs logarithmieren von X, Y oder X&Y. residuenplots müssen das iwie observen? button bei plots
  #  wohin? zum modell? neues modell anzeigen?
  
  # welche arten von transformationen sollen wir einbinden? polynom raw=TRUE (grad der polyn. zum eingeben), log, standardisieren. 
  # logistische Regression?
  # prüfungsstoff? als gruppe präsentieren. 
  # anderer cooks plot  mfrow
  # ausreißer nur für cooks distance mit groupcheckboxtool
  # auswahl variablen (nur unkorrelierte variablen) / modellselektion (welche variablen hab ich nach modellselektion:  
  
#Lineares Modell Ende --------------------------------------------------------------------------------
}


shinyApp(ui = ui, server = server)