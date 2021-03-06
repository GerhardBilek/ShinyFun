library(shiny)
library(utils)
library(gridExtra)
library("MASS")
library(ggplot2) 
library(popbio)
library(broom)
library(GGally)
library(corrplot)

pima <- rbind(MASS::Pima.te, MASS::Pima.tr)
pima1 <- pima [,-8] #type  wurde f??r correlation/scatternplot entfernt


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

ui <- fluidPage(
  navbarPage(
    tags$h2(p(code('Pima'))),
    
    
    
    #1st tab Panel--------------------------------------------------------------------------------------  
    tabPanel(tags$h3(p(em('All data'))), tags$h2("Overview of data"), hr(), 
             mainPanel( tableOutput("rawdata_pima"))),
    
    #2nd tab Panel------------------------------------------------------------------------------------------
    tabPanel(tags$h3(p(em('Exploration'))),
             tags$h3("Data Exploration: Distribution of PIMA datasets "), hr(), br(),
             
             sidebarLayout
             (
               sidebarPanel
               (radioButtons(
                 "dataset",
                 "Select a Dataset",
                 choices = c(
                   "Nr of pregn",
                   "glucose",
                   "blood pressure",
                   "skin",
                   "bmi",
                   "ped",
                   "age", 
                   "type [no values]"
                 )
               )),
               
               mainPanel(tags$h4("Data and Visualizations:"),
                         tabsetPanel(
                           tabPanel("Summary", verbatimTextOutput("summary"),
                                    fluidRow(column(width = 3, h5("Mean")), column(width = 3, verbatimTextOutput("the_mean")),
                                             column(width = 3, h5("Standard deviation")), column(width = 3, verbatimTextOutput("the_sd")))
                                    ),
                           tabPanel("Histogram & Boxplot", sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 5), plotOutput("hist"), h4(textOutput("caption")),plotOutput("boxplot")),
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
                 selectInput("regressand", "Dependent Variable", 
                                    choices = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age", "type" ), 
                                    selected = "type"),
                 checkboxGroupInput("checkbox", "Check independent variables", 
                                    choiceNames = c("Nr of pregnancies", "plasma glucose conc", "blood pressure", "skin fold thickness", "BMI", "ped", "age", "type"), 
                                    choiceValues = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age", "type"), 
                                    selected = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age")),
                 radioButtons("transformation", "Apply this transformation", choices = c("No Transformation", "Log(X)", "Log(Y)", "Log/Log", "Standardisation", "Polynom")),
                 checkboxGroupInput("checkGroup", label = h4("Remove Outlier: "), choices = c(rownames(pima)),  selected = c(rownames(pima)))
               ),
               mainPanel(tags$h4("Possible linear Models:"), actionButton("analysis","I have chosen all parameters wisely and want to ANALYSE"), hr(),
                         tabsetPanel(
                           tabPanel("Step (AIC)", verbatimTextOutput("stepmodel")
                                    
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

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "Nr of pregn" = pima$npreg, 
           "glucose" = pima$glu,
           "blood pressure" = pima$bp,
           "skin" = pima$skin,
           "bmi" = pima$bmi,
           "ped" = pima$ped,
           "age" = pima$age,
           "type" = pima$type
    )
  })
  
  # Lagesch??tzer:
  output$the_mean <- renderPrint({round(mean(datasetInput()),digits=2)})
  
  # Streuungsma??e:
  output$the_sd <- renderPrint({round(sd(datasetInput()),digits=2)})
  
  output$rawdata_pima <- renderTable({
    dataset <- pima
    dataset})
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    ggplot(pima, aes(x=dataset)) + geom_histogram(binwidth = input$bins, aes(y= ..density.., fill = ..count..))+geom_density(fill="red", alpha = 0.4)   + labs(x="")
    #hist(dataset)
  })
  
  output$boxplot <- renderPlot({
    dataset <- datasetInput()
    
    #boxplot(dataset)
    ggplot(pima, aes(y = dataset)) + geom_boxplot(outlier.colour = "red")+ coord_flip() +guides(color=guide_legend(),size=guide_legend())  + labs(y="")
  })
  
  #-----------------QQPLot---------------------------------------------------------------
  
  output$qqplot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset); qqline(dataset, col=2)})
  
  output$scatter <- renderPlot({
    dataset <- datasetInput()
    pairs(pima1, lower.panel = panel.smooth, upper.panel = panel.cor,
          gap=0, row1attop=FALSE, main = "Scatterplot")})
  
# Lineares Modell ----------------------------------------------------------  
  myformula <- reactive({
    if (input$transformation == "No Transformation") {
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
      
    } else if (input$transformation == "Log(X)") {
      expln <- paste("log(", input$checkbox, "+1)", collapse = "+") # +1 löst das Problem, das log(0) -inf ist. (anerkannte lösung von data scientists laut internet)
      as.formula(paste(input$regressand, "~", expln))
      
    } else if (input$transformation == "Log(Y)"){
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste("log(",input$regressand, "+1)", "~", expln)) 
      
    } else if (input$transformation == "Log/Log") {
      expln <- paste("log(", input$checkbox, "+1)", collapse = "+")
      as.formula(paste("log(",input$regressand, "+1)", "~", expln))
      
    } else if (input$transformation == "Polynom"){
      expln <- paste("poly(",input$checkbox,",2)", collapse = "+")
      as.formula(paste(input$regressand, "~" , expln))
      
    } else if (input$standardize == "regular data") {
      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
      
    } else if (input$standardize == "standardized data"){
      npreg <- scale(pima$npreg, center = TRUE, scale = TRUE)
      glu <- scale(pima$glu, center = TRUE, scale = TRUE)
      bp <- scale(pima$bp, center = TRUE, scale = TRUE)
      skin <- scale(pima$skin, center = TRUE, scale = TRUE)
      bmi <- scale(pima$bmi, center = TRUE, scale = TRUE)
      ped <- scale(pima$ped, center = TRUE, scale = TRUE)
      age <- scale(pima$age, center = TRUE, scale = TRUE)
      #type <- scale(pima$type, center = TRUE, scale = TRUE)
      pima_scale <- cbind(npreg, glu, bp, skin, bmi, ped, age)
      colnames(pima_scale) <- c("npreg", "glu", "bp", "skin", "bmi", "ped", "age")
      
      pima_scale <- as.data.frame(pima_scale)

      expln <- paste(input$checkbox, collapse = "+")
      as.formula(paste(input$regressand, "~", expln))
    }  
    
    })
  
  mod <- eventReactive(input$analysis, {
   # if (input$standardize == "standardized data") { 
    #  glm(myformula(), family = binomial(link=("logit")), data = pima_scale)
   # } else {
      glm(myformula(), family = binomial(link=("logit")), data = pima[c(input$checkGroup),])#}
  })
  
  output$modelFormula <- renderPrint({
    myformula()
  })
  
  
  output$stepmodel <- renderPrint({
    fit = glm(myformula(), family = binomial(link=("logit")), data= pima[c(input$checkGroup),]) #family = gaussion funktioniert
    step(fit)
  })
  
  output$modelSummary <- renderPrint({
    summary(mod())
  })
  
  output$model_plot <- renderPlot ({
    fit = glm(myformula(), family = binomial(link=("logit")), data=pima[c(input$checkGroup),])  # Remove Outlier via Checkbox#family = gaussian funktioniert, binomial nicht
    par(mfrow=c(2,2))
    plot(fit)
  })
}

shinyApp(ui = ui, server = server)