library(shiny)
library(utils)
library(gridExtra)
library("MASS")

pima <- rbind(MASS::Pima.te, MASS::Pima.tr)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("regressand", "Dependent Variable", choices = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age", "type" )),
      checkboxGroupInput("checkbox", "Check independent variables", 
                         choiceNames = c("Nr of pregnancies", "plasma glucose conc", "blood pressure", "skin fold thickness", "BMI", "ped", "age", "type"), 
                         choiceValues = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age", "type"), 
                         selected = c("Nr of pregnancies", "plasma glucose conc", "blood pressure", "skin fold thickness", "BMI", "ped", "age", "type")),
      checkboxGroupInput("checkGroup", label = h4("Remove Outlier: "), choices = c(rownames(swiss)),  selected = c(rownames(swiss))),
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

server <- function(input, output) {
  myformula <- reactive({
    expln <- paste(input$checkbox, collapse = "+")
    as.formula(paste(input$regressand, "~", expln))
    #temp <- input$regressand
    #temp <- log(temp)
    #as.formula(paste(temp, "~", expln)) Error in log: non-numeric argument to mathematical function
    
    # einen haufen buttons für diverse transf. iwo muss sich formel ändern
    # if? wenn input$transformation == "LOGX" dann as.formula(paste(input$regressand), "~", log(expln)) etc
    #if (input$transformation == "Log(Y)") {
    #expln <- paste(input$checkbox, collapse = "+") # Error in log: non-numeric argument to mathematical function
    #as.formula(paste(log(input$regressand), "~", expln))
    #}
  })
  
  mod <- eventReactive(input$analysis, {
    lm(myformula(), data = pima[c(input$checkGroup),])
  })
  
  output$modelFormula <- renderPrint({
    myformula()
  })
  
  output$stepmodel <- renderPrint({
    fit = lm(myformula(), data=pima)
    #fit = lm(myformula(), data=swiss[c(input$checkGroup),]) # ändert nix
    step(fit)
  })
  
  output$modelSummary <- renderPrint({
    summary(mod())
  })
  
  output$model_plot <- renderPlot ({
    fit = lm(myformula(), data=pima[c(input$checkGroup),])  # Remove Outlier via Checkbox
    #fit = lm(myformula(), data=swiss)
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  
}

shinyApp(ui = ui, server = server)