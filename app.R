# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(devtools)) install.packages("devtools")
if (!require(confidenceCurves)) devtools::install_github("FredaWerdiger/confidenceCurves")

library(shiny)
library(shinydashboard)
library(confidenceCurves)

# Define UI
ui <- fluidPage(
  titlePanel("Frequentist Confidence Analysis"),
  sidebarLayout(
     sidebarPanel(
      h3("Data"),
      radioButtons("type", "Outcome Type:",
                   choices = list("Binary" = 1, 
                                  "Continuous" = 2,
                                  "Ordinal" = 3),
                   selected = 3,
                   inline=FALSE),
      h3("Point Estimate"),
      
      # if data is binary, user has option of directly putting in binary outcome data
      conditionalPanel(
        condition = "input.type == 1",
        radioButtons("binaryOpt", "Calculate point estimate?",
                    choices = list("Yes" = 1,
                                   "No" = 2),
                    selected = 2),
        conditionalPanel(
          condition = "input.binaryOpt == 1",
          fluidRow(
            box(
              title="Binary outcome data",
              width=12,
              fluidRow(
                column( width = 6,
                        numericInput("numgroup1", "Subjects in Group 1:", value=NULL),
                        numericInput("numgroup2", "Subjects in Group 2:", value=NULL)
                        ),
                column(width=6,
                       numericInput("outcomesgroup1", "Outcomes in Group 1:", value=NULL),
                       numericInput("outcomesgroup2", "Outcomes in Group 2:", value=NULL)
                       )
                )
              )
            )
        )
      ),
      # TODO: calculate point estimate from binary data
      # select nature of point estimate
      selectInput("thetaType", "Select method of effect estimation", choice=list(
        "Odds Ratio"=1, "Risk Difference"=2), selected = 1),
      # put in value of point estimate - unless specified through binary data
      conditionalPanel(
        condition = "input.binaryOpt==2",  
        conditionalPanel(
          condition = "input.thetaType == 1",
          numericInput("oddsratio", "Odds Ratio Value:", value=0.81)
          ),
        conditionalPanel(
          condition = "input.thetaType == 2",
          numericInput("riskdiff", "Risk Difference Value:", value=NULL)
          ),
        ),
      # below functionality have not yet been added
      # conditionalPanel(
      #   condition = "input.thetaType == 3",
      #   numericInput("meandiff", "Mean Difference Value:", value=NULL)
      # ),
      # conditionalPanel(
      #   condition = "input.thetaType == 4",
      #   numericInput("riskratio", "Risk Ratio Value:", value=NULL)
      # ),
      
      # error if point estimate was not specified
      uiOutput("theta_error"),
      
      # define a neutral effect
      numericInput("neutral.effect", "Define a neutral treatment effect:", value=1),
      # warning will display if seems wrong
      uiOutput("neutral_error"),
      
      # below is not in use
      # radioButtons("dir.benefit", "Describe a positive treatment effect:",
      #              choices=list("Lower is better"=0, "Higher is better"=1),
      #               selected = 0, inline=FALSE),
      # numericInput("lmb", "Define the smallest clinically  meaningful effect", 
      #              value=0.1, min=-10, max=10),
      strong("DESCRIPTION OF OUTCOME MEASUREMENT SCALE"),
      uiOutput("summary_scale"),
      conditionalPanel(
        condition = "input.binaryOpt == 2",
        h3("Error estimate"),
        selectInput("errorType", "Select method of error estimation:",
                    choice=list("95% Confidence Interval"=1,
                                "Standard Deviation (σ)"=2,
                                "Standard error"=3),
                    selected=1),
        conditionalPanel(
          condition = "input.errorType == 2",  # Show when "Standard Deviation" is selected
          numericInput("sd", "Standard Deviation (σ):", value = 13.5),
          numericInput("n", "Sample Size:", value = 1035)
        ),
        conditionalPanel(
          condition = "input.errorType == 1",
          numericInput("ci_lower", "Lower Bound of 95% CI:", value = 0.70),
          numericInput("ci_upper", "Upper Bound of 95% CI:", value = 0.93)
        ),
        conditionalPanel(
          condition = "input.errorType == 3",
          numericInput("se", "Standard Error:", value=1.073),
        )
      ),
      uiOutput("error_error"),
      strong("DESCRIPTION OF POINT ESTIMATE"),
      uiOutput("summary_value"),
      h3("Flexible Thresholds for any Treatment Eeffect"),
      h5("Here you can enter two thresholds of interest and select whether you want to express confidence
                   in the treatment effect being above or below these thresholds."),
      fluidRow(
        box(
          width = 12,
          fluidRow(
            column(width=6,
                   numericInput(
                     "neutral.effect", "Threshold 1",
                     value=1)
                   ),
            column(width=6,
                   radioButtons(
                     "dir.benefit", "Confidence in Above or Below this?",
                     choices = list("Above"= 1, "Below"=0),
                     selected = 0)
            )
          ),
          fluidRow(
            column(width=6,
                   numericInput(
                     "min.effect", "Threshold 2",
                     value = 0.95)
            ),
            column(width=6,
                   radioButtons(
                     "min.effect.dir", "Confidence in Above or Below this?",
                     choices = list("Above" = 1, "Below" = 0),
                     selected = 1
                   )
            )
          )
        )
      ),
      h3("Confidence curve settings"),
      selectInput("show", "Select which threshold to display under the Confidence Density curve:", 
                  choices=list("Threshold 1" = "BENEFIT", "Threshold 2" = "LMB"),
                  selected="BENEFIT"),

      actionButton("generate", "Generate Confidence Curve"),
      width=4
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Confidence Analysis",
                 HTML("<br><br><u>") ,
                 strong("CONFIDENCE IN SPECIFIED THRESHOLDS"),
                 HTML("</u>"),
                 HTML("<br><br>"),
              textOutput("benefit_text"),
              textOutput("lmb_text"),
              HTML("<br><br>") ,
              HTML("<u>"),
              strong("FULL OUTPUT OF CONFIDENCE CURVE FUNCTION"),
              HTML("<br><br></u>"),
              HTML("The function assumes that the first threshold is for benefit, and the second for meaningful benefit."),
              tableOutput("results_table_1"),
              tableOutput("results_table_2"),
              tableOutput("results_table_3"),
              # tableOutput("lmb_table"),
              tags$head(tags$style("#benefit_text{
                           font-size:20px;
                           color:blue;}",
                           "#lmb_text{
                           font-size:20px;
                           color:forestgreen;}",))
              ),
        tabPanel(
        "Confidence distribution plot",
        textOutput("benefit_text1"),
        textOutput("lmb_text1"),
        plotOutput("confPlot"),
        strong('INTERPRETATION'),
        textOutput("confPlotText"),
        tags$head(tags$style("#benefit_text1{
                           font-size:20px;
                           color:blue;}",
                             "#lmb_text1{
                           font-size:20px;
                           color:forestgreen;}",))
        ),
        tabPanel(
        "Confidence density plot",
        plotOutput("curvePlot"),
        strong('INTERPRETATION'),
        textOutput("curvePlotText")
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
  
  # error function for failing to enter point estimate
  output$theta_error <- renderUI({
    theta.type = list("Odds Ratio",
                      "Risk Difference",
                      "Mean Difference",
                      "Risk Ratio")
    
    if( (input$thetaType == 1 & !is.numeric(input$oddsratio)) ||
        (input$thetaType == 2 & !is.numeric(input$riskdiff)) ||
        (input$thetaType == 3 & !is.numeric(input$meandiff)) ||
        (input$thetaType == 4 & !is.numeric(input$riskratio))) {
      if (input$binaryOpt==2){
        span(paste0("Please enter a value for ", 
                    theta.type[[as.numeric(input$thetaType)]], "."), style = "color:red;")
      }
    } else {
      return(NULL)
    }
  })
  
  # error function for failing to enter an error estimate
  output$error_error <- renderUI({
    error.type = list("95% Confidence Interval",
                      "Standard Deviation",
                      "Standard error")
    
    if( (input$errorType == 1 & (!is.numeric(input$ci_lower) | !is.numeric(input$ci_upper))) ||
        (input$errorType == 2 & (!is.numeric(input$sd) | !is.numeric(input$n))) ||
        (input$errorType == 3 & !is.numeric(input$se))
        ) {
      if (input$binaryOpt==2){
        span(paste0("Please enter a value/s for ", 
                    error.type[[as.numeric(input$errorType)]], "."), style = "color:red;")
        
      }
    } else {
      return(NULL)
    }
  })
  
  
  # warning function for entering suspicious value for neutral value
  output$neutral_error <- renderUI({
    if( (input$thetaType == 1 & input$neutral.effect == 0) ||
        (input$thetaType == 2 & input$neutral.effect == 1) ||
        (input$thetaType == 3 & input$neutral.effect == 1) ||
        (input$thetaType == 4 & input$neutral.effect == 0)){
      span("Warning: Values are not on the log scale.
           There may be an error with your value for neutral effect.",
           style = "color:orange;")
    }
  })
  
  # text output for lay summary of data scale
  output$summary_scale <- renderUI({
    
    data.type = list("binary", "continuous", "ordinal")
    benefit = list("less", "more")
    theta.type = list("Odds Ratio", "Risk Difference",
                      "Difference of Means", "Risk Ratio") 

    span(paste0("Your response outcome is ", 
                data.type[[as.numeric(input$type)]], ". Group difference is expressed via the ",
                theta.type[[as.numeric(input$thetaType)]],
               ", expressed on the linear (not log) scale so that a value of ",
               input$neutral.effect, " indicates a neutral treatment effect.")
               # You indicated that a value ", 
               # benefit[[as.numeric(input$dir.benefit) + 1]],
               # " than this indicates benefit to treatment over control.")
         , style = "color:blue")


  })
  
  # text output for lay summary of entered value
  output$summary_value <- renderUI({
    
    theta = list(input$oddsratio, input$riskdiff, input$meandiff, input$riskratio)
    errorType = list("95% confidence interval","standard deviation", "standard error")
    errorValue = list(paste0("[", input$ci_lower, ",", input$ci_upper, "]"), 
                 input$sd,
                 input$se)
    if (input$binaryOpt==2){
      span(paste0("Your point estimate is ", theta[[as.numeric(input$thetaType)]], ".",
                  " Error is measured using a ", errorType[[as.numeric(input$errorType)]],
                  ", here ", errorValue[[as.numeric(input$errorType)]], "."), style = "color:blue")
      
    } else{
      span(paste0("Your point estimate and resulting standard error will be calculated from the binary outcome data you specified.")
           , style = "color:blue")
    }
  })
  
  observeEvent(input$generate, {
    
    # Check if Odds Ratio is missing when thetaType is 1
    if (input$thetaType == 1 && !is.numeric(input$oddsratio)) {
      showNotification("Please enter Odds Ratio value.", type = "error")
      return()  # Exit without proceeding
    }
    
    # Generate sample data
    set.seed(123)
    
    # find inputs and match up with needs of Confidence Curve function
    
    # sample size
    if (!is.numeric(input$n)){
      sample.size = NULL
    } else {
      sample.size = input$n
    }

    
    # standard error
    if (!is.numeric(input$se)){
      standard.error = NULL
    } else {
      standard.error = input$se
      if (input$thetaType %in% c(1,4)){
        standard.error = log(standard.error)
      }
    }
    
    # standard deviation
    if (!is.numeric(input$sd)){
      var = NULL
    } else {
      var = input$sd ** 2
      if (input$thetaType %in% c(1,4)){
        var = log(var)
      }
    }
    
    # confidence interval, lower and upper
    if (!is.numeric(input$ci_lower)){
      ci_lower = NULL
    } else {
      ci_lower = input$ci_lower
      if (input$thetaType %in% c(1,4)){
        ci_lower = log(ci_lower)
      }
    }
    
    if (!is.numeric(input$ci_upper)){
      ci_upper = NULL
    } else {
      ci_upper = input$ci_upper
      if (input$thetaType %in% c(1,4)){
        ci_upper = log(ci_upper)
      }
    }
    
    # min effect and neutral effect
    if (input$thetaType %in% c(1,4)){
      min.effect = signif(log(input$min.effect), 2)
      if (input$dir.benefit == 1){
        min.effects =  log(input$neutral.effect + seq(1,10)/100)
      } else {
        min.effects =  log(input$neutral.effect - seq(1,10)/100)
      }
      neutral.effect = log(input$neutral.effect)
    } else {
      neutral.effect = input$neutral.effect
      min.effect = input$min.effect
      if (input$dir.benefit == 1){
        min.effects = neutral.effect + seq(1,10)/100
      } else {
        min.effects = neutral.effect - seq(1,10)/100
      }
    }
    
    # thetas = c(input$oddsr, input$riskd, input$meand, input$riskr)
    
    if (input$thetaType ==1){
      theta = log(input$oddsratio)
    } else if (input$thetaType == 2){
      theta = input$riskdiff
    } 
    
    # else if (input$thetaType == 3){
    #   theta = input$meand
    # } else {
    #   theta = log(input$riskr)}
    # 
    
    # specify theta type
    if (input$thetaType == 1){
      estimator.type = 'oddsratio'
    } else if (input$thetaType == 2){
      estimator.type = 'riskdiff'
    }
    
    # for binary outcomes with specified group numbers
    if (input$binaryOpt == 1){
      num.ctrl = input$numgroup1
      num.trmt = input$numgroup2
      num.resp.ctrl = input$outcomesgroup1
      num.resp.trmt = input$outcomesgroup2
      # if these are all non-zero set other values to zero
      if (is.numeric(num.ctrl) &
          is.numeric(num.trmt) &
          is.numeric(num.resp.ctrl) &
          is.numeric(num.resp.trmt)){
        theta = NULL
        sample.size = NULL
        var = NULL
        standard.error = NULL
        ci_upper = NULL
        ci_lower = NULL
      }
    }
    
    if (
      (input$dir.benefit == input$min.effect.dir) &
      input$show == "LMB"){
      show = "MB"
    } else {
      show = input$show
    }
      
    # what to show on the graph
    # threshold one is constructed as benefit 
    # if threshold two is in the same direction, it is meaninful benefit
    # if it is in the opposite direction, it is lack of meaninful benefit
    

    # Compute confidence curves

    df <- data.frame()
    
    cc <- confidenceCurves::makeConfidenceCurves(theta.estimator = theta,
                                                 num.ctrl = num.ctrl,
                                                 num.trmt = num.trmt,
                                                 num.resp.ctrl = num.resp.ctrl,
                                                 num.resp.trmt = num.resp.trmt,
                                                 estimator.type = estimator.type,
                                                 sample.size = sample.size,
                                                 treat.var = var,
                                                 confidence.upper = ci_upper,
                                                 confidence.lower = ci_lower,
                                                 standard.error = standard.error,
                                                 dir.benefit = input$dir.benefit,
                                                 show=show,
                                                 return.plot=TRUE,
                                                 neutral.effect = neutral.effect,
                                                 min.effect=min.effect
                                                 )
    
    # Below is to make a table with varying LMB
    # this isn't part of the generalised version of the app
    
    # get dataframe by varying lmb
    # for (i in 1:10){
    #   list.out <- confidenceCurves::makeConfidenceCurves(theta.estimator = theta,
    #                                                      num.ctrl = num.ctrl,
    #                                                      num.trmt = num.trmt,
    #                                                      num.resp.ctrl = num.resp.ctrl,
    #                                                      num.resp.trmt = num.resp.trmt,
    #                                                      sample.size = sample.size,
    #                                                      estimator.type = estimator.type,
    #                                                      treat.var = var,
    #                                                      confidence.upper = ci_upper,
    #                                                      confidence.lower = ci_lower,
    #                                                      standard.error = standard.error,
    #                                                      dir.benefit = input$dir.benefit,
    #                                                      show=input$show,
    #                                                      return.plot=FALSE,
    #                                                      neutral.effect = neutral.effect,
    #                                                      min.effect=min.effects[[i]]
    #               )
    #   
    #   df <- rbind(df, data.frame("meaningful benefit"= signif(min.effects[[i]],3), conf.lack.meaningful.benefit=list.out$conf.lack.meaningful.benefit*100))
    # }
    
    if (as.numeric(input$dir.benefit) == 0){
        phr_1 <- paste("Conf(","\u03b8", " < Threshold 1): ", round(cc$text$conf.benefit * 100, 3), "%")
      } else {
        phr_1 <- paste("Conf(", "\u03b8", " > Threshold 1): ", round(cc$text$conf.benefit * 100, 3), "%")
      }
    
    output$benefit_text <- renderText({phr_1})
    output$benefit_text1 <- renderText({phr_1})
    
    if (as.numeric(input$dir.benefit) == 0){
      if (input$min.effect.dir == 1){
        phr_2 <- paste("Conf(","\u03b8", " > Threshold 2): ", round(cc$text$conf.lack.meaningful.benefit * 100, 3), "%")
      } else {
        phr_2 <- paste("Conf(","\u03b8", " < Threshold 2): ", round(cc$text$conf.meaningful.benefit * 100, 3), "%")
      }
    } else {
      if (input$min.effect.dir == 1){
        phr_2 <- paste("Conf(","\u03b8", " > Threshold 2): ", round(cc$text$conf.meaningful.benefit * 100, 3), "%")
      } else {
        phr_2 <- paste("Conf(","\u03b8", " < Threshold 2): ", round(cc$text$conf.lack.meaningful.benefit * 100, 3), "%")
      }
    }
    
    
    output$lmb_text <- renderText({ phr_2 })
    output$lmb_text1 <- renderText({ phr_2 })
    
    output$confPlot <- renderPlot({
      plot(cc$cdf, main = "Confidence Curves")
    })
    
    output$confPlotText <- renderText({
      paste0("The Confidence distribution is constructed from all one-sided
              confidence intervals (0 - 100). The point estimate, here ",
             round(theta, 2), ", always sits at the edge of the 50% Confidence Interval.",
             " The one-sided p-value is derived by subtracting Conf(","\u03b8", " < 0), here ",
             round(cc$text$conf.benefit, 4), 
             ", from 1 and is displayed on the graph. The thresholds you have chosen are displayed in blue (Threshold 1)
              and green (Threshold 2) and the confidence levels concerning these thresholds are shown in colour-coded text.
             
             ")
    })
    
    output$curvePlot <- renderPlot({
      plot(cc$pdf, main = "Confidence Curves")
    })
   
    output$curvePlotText <- renderText({
      if (input$show == 'BENEFIT'){
        thresh = 1
        thresh.num = input$neutral.effect
        if (neutral.effect != input$neutral.effect){
          log.text = paste0("(", neutral.effect, " on the log scale)")
        } else (log.text = "")
        if (input$dir.benefit == 0){
          dir.text = "less than "
        } else (dir.text = "greater than ")
      } else if (input$show == "LMB"){
        thresh = 2
        thresh.num = input$min.effect
        if (min.effect != input$min.effect){
          log.text = paste0("(", min.effect, " on the log scale)")
        } else (log.text = "")
        if (input$min.effect.dir == 0){
          dir.text = "less than "
        } else (dir.text = "greater than ")
      }
      paste0("The Confidence Density Function shows the frequentist Confidence that the treatment effect falls within a particular range of values. The area
             under the curve contained in a given interval (blue shading) gives the level of Confidence that the true effect lies within it. You have chosen to display
             the Confidence that the effect is ", dir.text, thresh.num, log.text, ", specified by Threshold ", thresh, ".")
    })
    
    output$confc <- renderPlot({
      plot(cc$cc, main = "Confidence Curves")
    })
    
    output$nullPlot <- renderPlot({
      plot(cc$null, main = "Confidence Curves")
    })
    
    output$lmb_table <- renderTable(df, digits=3, striped=TRUE, bordered=TRUE)
    output$results_table_1 <- renderTable(cc$text[2:3], digits=3)
    output$results_table_2 <- renderTable(cc$text[4:6], digits=3)
    output$results_table_3 <- renderTable(cc$text[7:10], digits=3)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)