library(shiny)
library(utils)
library(gridExtra)
library(ggplot2) 

#remove "Examination" from Dataset
#snames <- colnames(s)

swiss <- swiss[,-3]

awesomeData <- swiss

# versuch, alles im vorhinein zu normalisieren und dann unten mit den normalisierten daten zu arbeiten



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
ui <- fluidPage(
  
  navbarPage(
    tags$h2(p(code('Swiss Data'))),
    
    
    
    #1st tab Panel--------------------------------------------------------------------------------------  
    tabPanel(tags$h3(p(em('All data'))), tags$h2("Overview of data"), hr(), 
             mainPanel( tableOutput("rawdata_swiss"))),
    
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
                         tabsetPanel(
                           tabPanel("Summary", verbatimTextOutput("summary")),
                           tabPanel("Histogram & Boxplot", plotOutput("hist"), h4(textOutput("caption")),plotOutput("boxplot")),
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
                 checkboxGroupInput("checkbox", "Check independent variables", 
                                    choiceNames = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), 
                                    choiceValues = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), 
                                    selected = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality")),
                 radioButtons("standardize", "Which data to use ...", choices = c("regular data", "standardized data")),
                 radioButtons("transformation", "Apply this transformation", choices = c("No Transformation", "Log(X)", "Log(Y)", "Log/Log", "Polynom")),
                 checkboxGroupInput("checkGroup", label = h4("Remove Outlier: "), choices = c(rownames(swiss)),  selected = c(rownames(swiss)))
                 
               ),
               mainPanel(tags$h4("Possible linear Models:"), hr(),
                         tabsetPanel(
                           tabPanel("Step (AIC)", verbatimTextOutput("stepmodel"),
                                    actionButton("analysis","I have chosen my independents and want to ANALYSE")
                           ),
                           tabPanel("Formel und Modell", verbatimTextOutput("modelFormula"),
                                    verbatimTextOutput("modelSummary")
                           ),
                           tabPanel("Residuenplots",
                                    conditionalPanel(condition = "input.transformation == 'Polynom'",
                                    numericInput("a2", "a2", value = 0),
                                    numericInput("a1", "a1", value = 1),
                                    numericInput("a0", "a0", value = 0)),
                                    plotOutput("model_plot")
                                    
                           )
                         )
               )
             )
    )
    
  ))


#---SERVER-------------------------------------------------------------------------------------------
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
  
  #----------------Beschriftung------------- 
  output$caption<-renderText({
    switch(input$dataset,
           "Fertility" = "Fertility", 
           "Agriculture" = "Agriculture",
           "Education" = "Education",
           "Catholic" = "Catholic",
           "Fertility" = "Fertility",
           "Infant.Mortality" = "Infant.Mortality"
           
    )
  })
  
  
  output$value <- renderText({output$caption})
  
  output$rawdata_swiss <- renderTable({
    dataset <- swiss
    dataset})  # head(dataset)
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    ggplot(swiss, aes(x=dataset)) + geom_histogram(binwidth = 1, aes(y= ..density.., fill = ..count..))+geom_density(fill="red", alpha = 0.4)   + labs(x="")
    #hist(dataset)
  })
  
  output$boxplot <- renderPlot({
    dataset <- datasetInput()
    
    #boxplot(dataset)
    ggplot(swiss, aes(y = dataset)) + geom_boxplot(outlier.colour = "red")+ coord_flip() +guides(color=guide_legend(),size=guide_legend())  + labs(y="")
  })
  
  #rds <- reactiveValues(data=swiss)
  # dinput$Fertility = as.factor(dinput$Fertility)
  #observe function to make the plot reactive 
  #observe({
  # df = brushedPoints(rds$data, brush = input$plot_brush_, allRows = TRUE)
  #  rds$data = df[df$selected_== FALSE,]
  #})
  
  
  
  #-----------------QQPLot---------------------------------------------------------------
  
  output$qqplot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset); qqline(dataset, col=2)})
  
  output$scatter <- renderPlot({
    dataset <- datasetInput()
    pairs(swiss, lower.panel = panel.smooth, upper.panel = panel.cor,
          gap=0, row1attop=FALSE, main = "Scatterplot")})
    
  #Lineares Modell --------------------------------------------------------------------------------
  # "No Transformation", "Log(X)", "Log(Y)", "Log/Log", "Standardisation", "Polynom"
  
  myformula <- reactive({
    if (input$transformation == "No Transformation") {
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
    
    } else if (input$transformation == "Log(X)") {
      expln <- paste("log(", input$checkbox, ")", collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
      
    } else if (input$transformation == "Log(Y)"){
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste("log(",input$regressand, ")", "~", expln))
      
    } else if (input$transformation == "Log/Log") {
      expln <- paste("log(", input$checkbox, ")", collapse = "+")
      as.formula(paste("log(",input$regressand, ")", "~", expln))

    }    
      else if (input$transformation == "Polynom") {
        expln <- paste(input$checkbox, collapse = "+")
        
        as.formula(paste(input$a2,"*(",input$regressand, "^2) + ",
                         input$a1,"*(",input$regressand,") + ",
                         input$a0, "~", expln))
        
    } else if (input$standardize == "regular data") {
      #rm(swiss)
      #swiss_norm <- swiss
      
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
      
    } else if (input$standardize == "standardized data"){
      #rm(awesomeData)
      Fertility <- scale(awesomeData$Fertility, center = TRUE, scale = TRUE)
      Education <- scale(awesomeData$Education, center = TRUE, scale = TRUE)
      Agriculture <- scale(awesomeData$Agriculture, center = TRUE, scale = TRUE)
      Infant.Mortality <- scale(awesomeData$Infant.Mortality, center = TRUE, scale = TRUE)
      Catholic <- scale(awesomeData$Catholic, center = TRUE, scale = TRUE)
      swiss_scale <- cbind(Fertility, Education, Agriculture, Infant.Mortality, Catholic)
      #colnames(swiss_scale) <- c("scale_Fertility", "scale_Education", "scale_Agriculture", "scale_Infant.Mortality", "scale_Catholic")
      colnames(swiss_scale) <- c("Fertility", "Education", "Agriculture", "Infant.Mortality", "Catholic")
      
      swiss_scale <- as.data.frame(swiss_scale)
      #swiss <- swiss_scale
      
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
    }

    
  })
  
  mod <- eventReactive(input$analysis, {
    if (input$standardize == "standardized data") { 
      # lm(myformula(), data = swiss_scale[c(input$checkGroup),]) # das sich input$checkGroup nicht auf swiss scale bezieht, geht diese zeile nicht
      lm(myformula(), data = swiss_scale)
    } else {
      lm(myformula(), data = swiss[c(input$checkGroup),])
    }
  })
  
  output$modelFormula <- renderPrint({
    myformula()
    #head(swiss)
    #temp <- swiss[,input$regressand]
    #temp <- log(temp)
    #temp
  })
  
  output$stepmodel <- renderPrint({
    #fit = lm(myformula(), data=swiss)
    fit = lm(myformula(), data=swiss[c(input$checkGroup),]) # ??ndert nix; bei mir funktioniert es (Gerry)
    step(fit)
  })
  
  output$modelSummary <- renderPrint({
    summary(mod())
  })
  
  output$model_plot <- renderPlot ({
    fit = lm(myformula(), data=swiss[c(input$checkGroup),])  # Remove Outlier via Checkbox
    #fit = lm(myformula(), data=swiss)
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  
  #  button f??rs logarithmieren von X, Y oder X&Y. residuenplots m??ssen das iwie observen? button bei plots
  #  wohin? zum modell? neues modell anzeigen?
  
  # welche arten von transformationen sollen wir einbinden? polynom raw=TRUE (grad der polyn. zum eingeben), log, standardisieren. 
  # logistische Regression?
  # pr??fungsstoff? als gruppe pr??sentieren. 
  # anderer cooks plot  mfrow
  # ausrei??er nur f??r cooks distance mit groupcheckboxtool
  # auswahl variablen (nur unkorrelierte variablen) / modellselektion (welche variablen hab ich nach modellselektion:  
  
  #Lineares Modell Ende --------------------------------------------------------------------------------
}



shinyApp(ui = ui, server = server)






