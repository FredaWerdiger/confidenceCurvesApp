# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(devtools)) install.packages("devtools")
if (!require(confidenceCurves)) devtools::install_github("FredaWerdiger/confidenceCurves")

library(shiny)
library(confidenceCurves)

# Define UI
ui <- fluidPage(
  titlePanel("Confidence Curves"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data"),
      radioButtons("type", "Data Type:",
                   choices = list("Binary" = 1, 
                                  "Continuous" = 2,
                                  "Ordinal" = 3),
                   selected = 3,
                   inline=FALSE),
      h3("Point Estimate"),
      selectInput("thetaType", "Select method of effect estimation", choice=list(
        "Odds Ratio"=1, "Risk Difference"=2, "Mean Difference"=3, "Risk Ratio"=4
      ), selected = 1),
      conditionalPanel(
        condition = "input.thetaType == 1",
        numericInput("oddsr", "Odds Ratio Value:", value=0.81)
      ),
      conditionalPanel(
        condition = "input.thetaType == 2",
        numericInput("riskd", "Risk Difference Value:", value=NULL)
      ),
      conditionalPanel(
        condition = "input.thetaType == 3",
        numericInput("meand", "Mean Difference Value:", value=NULL)
      ),
      conditionalPanel(
        condition = "input.thetaType == 4",
        numericInput("riskr", "Risk Ratio Value:", value=NULL)
      ),
      uiOutput("theta_error"),
      numericInput("neutral.effect", "Define a neutral treatment effect:", value=1),

      uiOutput("neutral_error"),
      radioButtons("dir.benefit", "Describe a positive treatment effect:",
                   choices=list("Lower is better"=0, "Higher is better"=1),
                   selected = 0, inline=FALSE),
      numericInput("lmb", "Define the lowest meaningful benefit (%)", value=5, min=0, max=100),
      strong("SUMMARY OF DATA"),
      uiOutput("summary"),
      h3("Error estimate"),
      selectInput("errorType", "Select method of error estimation:",
                  choice=list("95% Confidence Interval"=1,
                              "Standard Deviation (σ)"=2,
                              "Standard error"=3),
                  selected=2),
      conditionalPanel(
        condition = "input.errorType == 2",  # Show when "Standard Deviation" is selected
        numericInput("sd", "Standard Deviation (σ):", value = 10),
        numericInput("n", "Sample Size:", value = 1035)
      ),
      conditionalPanel(
        condition = "input.errorType == 1",
        numericInput("ci_lower", "Lower Bound of 95% CI:", value = NULL),
        numericInput("ci_upper", "Upper Bound of 95% CI:", value = NULL)
      ),
      conditionalPanel(
        condition = "input.errorType == 3",
        numericInput("se", "Standard Error:", value=NULL),
      ),
      h3("Confidence curve settings"),
      selectInput("show", "Select what to display on the confidence density plot:", 
                  choices=list("Benefit"="BENEFIT", "Lack of Meaningful Benefit"="LMB"),
                  selected="BENEFIT"),

      actionButton("generate", "Generate Confidence Curve")
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Confidence Analysis",
              textOutput("benefit_text"),
              textOutput("lmb_text"),
              tableOutput("results_table"),
              tableOutput("lmb_table"),
              tags$head(tags$style("#benefit_text{
                           font-size:20px;
                           color:blue;}",
                           "#lmb_text{
                           font-size:20px;
                           color:red;}",))
              ),
        tabPanel(
        "Confidence distribution plot",
        plotOutput("confPlot"),
        strong('INTERPRETATION'),
        textOutput("confPlotText")  
        ),
        tabPanel(
        "Confidence density plot",
        plotOutput("curvePlot")
        ),
        tabPanel(
        "Confidence Curve",
        plotOutput("confc")
        ),
        tabPanel(
        "Null distribution plot",
        plotOutput("nullPlot")
      ),
      

    )
  )
)
)

# Define Server
server <- function(input, output) {
  
  output$theta_error <- renderUI({
    theta.type = list("Odds Ratio",
                      "Risk Difference",
                      "Mean Difference",
                      "Risk Ratio")
    
    if( (input$thetaType == 1 & !is.numeric(input$oddsr)) ||
        (input$thetaType == 2 & !is.numeric(input$riskd)) ||
        (input$thetaType == 3 & !is.numeric(input$meand)) ||
        (input$thetaType == 4 & !is.numeric(input$riskr))) {
      span(paste0("Please enter a value for ", 
                  theta.type[[as.numeric(input$thetaType)]], "."), style = "color:red;")
    } else {
      return(NULL)
    }
  })
  
  output$neutral_error <- renderUI({
    if( (input$thetaType == 1 & input$neutral.effect == 0) ||
        (input$thetaType == 2 & input$neutral.effect == 1) ||
        (input$thetaType == 3 & input$neutral.effect == 1) ||
        (input$thetaType == 4 & input$neutral.effect == 0)){
      span("Warning: Values are not on the log scale. There may be an error with your value for neutral effect.",
           style = "color:orange;")
    }
  })
  
  
  output$summary <- renderUI({
    req((input$thetaType == 1 && is.numeric(input$oddsr)) ||
          (input$thetaType == 2 && is.numeric(input$riskd)) ||
          (input$thetaType == 3 && is.numeric(input$meand)) || 
          (input$thetaType == 4 && is.numeric(input$riskr)))
    
    data.type = list("binary", "continuous", "ordinal")
    benefit = list("less", "more")
    theta.type = list("Odds Ratio", "Risk Difference", "Difference of Means", "Risk Ratio")
    theta = list(input$oddsr, input$riskd, input$meand, input$riskr)
    
    span(paste0("Your response data is ", 
                data.type[[as.numeric(input$type)]], " and group difference is expressed via a ",
                theta.type[[as.numeric(input$thetaType)]],
                ", here ",theta[[as.numeric(input$thetaType)]], 
               ". On the linear (not log) scale, ",
               input$neutral.effect, " means no group difference, and a value ", 
               benefit[[as.numeric(input$dir.benefit) + 1]],
               " than ", input$neutral.effect,
               " indicates benefit to treatment over control.")
         , style = "color:blue")
    
  })
  
  observeEvent(input$generate, {
    
    # Check if Odds Ratio is missing when thetaType is 1
    if (input$thetaType == 1 && !is.numeric(input$oddsr)) {
      showNotification("Please enter Odds Ratio value.", type = "error")
      return()  # Exit without proceeding
    }
    
    # Generate sample data
    set.seed(123)
    
    # find inputs
    if (!is.numeric(input$n)){
      sample.size = NULL
    } else {
      sample.size=input$n
    }

    
    if (!is.numeric(input$se)){
      standard.error = NULL
    } else {
      standard.error = input$se
    }
    
    if (!is.numeric(input$ci_lower)){
      ci_lower = NULL
    } else {
      ci_lower=input$ci_lower
    }
    
    if (!is.numeric(input$ci_upper)){
      ci_upper = NULL
    } else {
      ci_upper=input$ci_upper
    }
    
    # TODO: make this more efficient
    # TODO: Fix error estimations
    
    if (input$thetaType ==1){
      theta = log(input$oddsr)
      min.effect = log(input$neutral.effect +  input$lmb/100)
      min.effects =  log(input$neutral.effect + seq(1,10)/100)
      neutral.effect = log(input$neutral.effect)
    } else if (input$thetaType == 2){
      theta = input$riskd
      neutral.effect = input$neutral.effect
      min.effect = neutral.effect + input$lmb/100
      min.effects = neutral.effect + seq(1,10)/100
    } else if (input$thetaType == 3){
      theta = input$meand
      neutral.effect = input$neutral.effect
      min.effect = neutral.effect + input$lmb/100
      min.effects = neutral.effect + seq(1,10)/100
    } else {
      theta = log(input$riskr)
      min.effect = log(input$neutral.effect +  input$lmb/100)
      min.effects =  log(input$neutral.effect + seq(1,10)/100)
      neutral.effect = log(input$neutral.effect)}
    
    # Compute confidence curves

    df <- data.frame()
    
    cc <- confidenceCurves::makeConfidenceCurves(theta.estimator = theta,
                                                 sample.size = sample.size,
                                                 treat.var = log(input$sd) ** 2,
                                                 confidence.upper = ci_upper,
                                                 confidence.lower = ci_lower,
                                                 standard.error = standard.error,
                                                 dir.benefit = input$dir.benefit,
                                                 show=input$show,
                                                 return.plot=TRUE,
                                                 neutral.effect = neutral.effect,
                                                 min.effect=min.effect
                                                 )
    
    # get dataframe by varying lmb
    
    for (i in 1:10){
      list.out <- confidenceCurves::makeConfidenceCurves(theta.estimator = theta,
                                             sample.size = sample.size,
                                             treat.var = log(input$sd) ** 2,
                                             confidence.upper = ci_upper,
                                             confidence.lower = ci_lower,
                                             standard.error = standard.error,
                                             dir.benefit = input$dir.benefit,
                                             show=input$show,
                                             return.plot=FALSE,
                                             neutral.effect = neutral.effect,
                                             min.effect=min.effects[[i]]
      )
      
      df <- rbind(df, data.frame(min.meaningful.effect=i/100, conf.lack.meaningful.benefit=list.out$conf.lack.meaningful.benefit))
    }
    
    output$benefit_text <- renderText({
      paste("Confidence in benefit: ", round(cc$text$conf.benefit * 100, 3), "%")
      })
    
    output$lmb_text <- renderText({
      paste("Confidence in lack of meaningful benefit: ", round(cc$text$conf.lack.meaningful.benefit * 100, 3), "%")
    })
    
    output$curvePlot <- renderPlot({
      plot(cc$pdf, main = "Confidence Curves")
    })
    
    output$confPlot <- renderPlot({
      plot(cc$cdf, main = "Confidence Curves")
    })
    
    output$confPlotText <- renderText({
      paste0("The Confidence distribution is constructed from all one-sided
              confidence intervals (0 - 100). The point estimate, here ",
             round(theta, 2), ", always sits at the edge of the 50% Confidence Interval.",
             " The one-sided p-value is derived by subtracting Conf(BENEFIT), here ",
             round(cc$text$conf.benefit, 4), 
             ", from 1 and is displayed on the graph.")
    })
    
    output$confc <- renderPlot({
      plot(cc$cc, main = "Confidence Curves")
    })
    
    output$nullPlot <- renderPlot({
      plot(cc$null, main = "Confidence Curves")
    })
    
    output$lmb_table <- renderTable(df)
    output$results_table <- renderTable(cc$text)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)