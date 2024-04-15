library(MASS)
library(pROC)
library(ggpubr)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  
    dashboardHeader(),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Prediction Tools", tabName = "predictVis", icon = icon("magnifying-glass")),
        menuItem("Impressum", tabName = "impressum", icon = icon("book"))
      )
      
    ),
    dashboardBody(
    tabItems(
      
      tabItem(tabName = "predictVis",
  titlePanel("Simple Interactive tool to visualize prediction performance"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "nobs",
                  label = "Number of Participants:",
                  min = 10,
                  max = 2000,
                  value = 1000),
      
      sliderInput(inputId = "correlation",
                  label = "Strength of Association:",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput(inputId = "skew",
                  label = "Percentage of class to be predicted:",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput(inputId = "cutoff",
                  label = "Cut-off value:",
                  min = 0,
                  max = 1,
                  value = 0.5),
    ),
    
    mainPanel(
      
      plotOutput(outputId = "rocPlot"),
      tableOutput(outputId = "predTable")
      
    )
  )
),
tabItem(  
  tabName = "impressum",
  h2("Impressum"),
  h3("This is a Shiny app developed by Andreas Baumer, intended to be used for 
    educational purposes.", tags$br(),
    "The author can be reached under andreas.baumer@uzh.ch")
  
)
)
)
)

server <- function(input, output) {
  

  output$rocPlot <- renderPlot({
    
    set.seed(1)
    covmat <- rbind(c(1,input$correlation), c(input$correlation,1))
    m <- c(10,0)
    dat <- as.data.frame(mvrnorm(n = input$nobs, mu = m, Sigma = covmat))
    
    colnames(dat) <- c("Predictor", "Outcome")
    

    dat$cutof <- dat$Outcome >= quantile(dat$Outcome, probs = 1-input$skew)
    
    corp <- ggplot(data = dat, aes(x = Outcome, y = Predictor, col = cutof)) + geom_point() +
      theme_bw() + 
      geom_hline(yintercept = as.numeric(quantile(dat$Predictor, probs = input$cutoff)))
    dat$Outcome <- ifelse(dat$Outcome >= quantile(dat$Outcome, probs = input$skew), 1, 0)
    rocp <- ggroc(roc(Outcome ~ Predictor, data = dat)) + 
      geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0)) + theme_bw() +
    ggtitle( paste(
      "The Area under the Curve is: ", 
      round(auc(roc(Outcome ~ Predictor, data = dat)), digits = 2)))
    
    ggarrange(corp,rocp)
  })

  
  output$predTable <- renderTable({
    
    set.seed(1)
    covmat <- rbind(c(1,input$correlation), c(input$correlation,1))
    m <- c(10,0)
    dat <- as.data.frame(mvrnorm(n = input$nobs, mu = m, Sigma = covmat))
    
    colnames(dat) <- c("Predictor", "Outcome")
    
    dat$Outcome <- ifelse(dat$Outcome <= quantile(dat$Outcome, probs = input$skew), 1, 0)
    
    dat <- as.data.frame(dat)
    
    
    logmod <- glm(Outcome ~ Predictor, data =  dat, family = "binomial")
    preds <- predict(logmod, newdata = dat, type = "response")
    
    PredictedClass <- ifelse(preds >= quantile(preds, probs = input$cutoff), 1, 0)
    TrueResult<- dat$Outcome
    #CrossTable(TrueResult, PredictedClass, prop.r = F, prop.c = F ,prop.t = F, prop.chisq = F)
    
    
    df <- data.frame(
      Metrics = c("Accuracy: ", "Sensitvity: ", "Specificity: ", "False Positive Rate:", "False Negative Rate:"),
      Percentage = c(
        round(sum(PredictedClass == TrueResult)/length(PredictedClass), digits = 3),
        round(sum((PredictedClass + TrueResult) == 2)/sum(TrueResult), digits = 3),
        round(sum((PredictedClass + TrueResult) == 0)/sum(TrueResult == 0), digits = 3),
        round(sum(PredictedClass == 1 &  TrueResult == 0)/sum(PredictedClass), digits = 3),
        round(sum(PredictedClass == 0 &  TrueResult == 1)/sum(1-PredictedClass), digits = 3)
      )
    )
    df$Percentage <- round(df$Percentage*100, digits = 1)
    df
    })

  
}

shinyApp(ui = ui, server = server)


