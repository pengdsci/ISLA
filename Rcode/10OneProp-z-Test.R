library(shiny)
library(ggplot2)
##########################################################
#    Define UI for application that draws a histogram
###########################################################
ui <- fluidPage(
  #includeCSS('styles.css'), 
  withMathJax(),
  ### Important: The following tag allows inline equation in MathJax
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
  br(),
  ### Important: The following tag allows inline equation in MathJax
  wellPanel(style = "background-color:#5e2442;",
            titlePanel( h3("ISLA: One Sample Normal Test for Population Proportion", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),
  #################

    #################
    sidebarLayout(
        sidebarPanel(style = "background-color: #5e2442; color:gold; ",
                #########################
                tags$b("Data Source"),
                radioButtons(
                    inputId = "datasourcez",
                    label = NULL,
                    choices = c("summarized statistics" = "statistic")),
                #### input information
                conditionalPanel(
                    condition = "input.datasourcez == 'statistic'",
                    ### sample mean
                    numericInput("phat", "sample proportion $(\\hat{p})$",
                                 value = 0.55, min = 0, step = 1),
                    ### sample size
                    numericInput("nnz", "sample size $(n)$",
                                 value = 100, min = 0, step = 1)
                ),

            
            #########################################################
            ####     Unconditional Panels (common to all tests)
            #########################################################

                numericInput("h0",
                             label = "Claimed Value $(p_0)$",
                             value = 0.43, step = 0.1),
                selectInput(
                    inputId = "alternative",
                    label = "Claim Type",
                    choices = c("equal to", "not equal to", 
                                "greater than", "less than or equal to",
                                "less than", "greater than or equal to")),
                sliderInput("alpha",
                        "Significance level $\\alpha$",
                        min = 0.01,
                        max = 0.20,
                        value = 0.05),
            ###
            HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
            HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
            
        ),  ###
        
        ############################################
        ######          Main Panel
        ############################################
        mainPanel(
            uiOutput("z_oneprop"),
            plotOutput("z_plot"),
            br(),
            br()
        )
    )
)
######
######

server <- function(input, output){
################################################################################
##         Some R functions to be used to calculate test results
################################################################################
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    
 ###############################################################################
 ######                  Outputs of test results
 ###############################################################################   
    output$z_oneprop <- renderUI({
        ###################      define test type   ##################
        if(input$alternative %in% c("equal to", "not equal to") ) {
            inputalternative = "two.sided"
        }else if (input$alternative %in% c("greater than", "less than or equal to")){
            inputalternative = "greater"
        } else if (input$alternative %in% c("less than", "greater than or equal to")){
            inputalternative = "less"
        }
      
      
      
        ######################################
            if (input$datasourcez == 'statistic') {
            ##--- p-value based on normal distribution``````````
            pL <- round(pnorm((input$phat-input$h0)/(sqrt(input$h0*(1-input$h0)/input$nnz))), 3)
            pR <- 1 - pL
            pT <- 2*min(pL, pR)
            ##############
            if(inputalternative == "two.sided"){
                p.value <- pT
            } else if(inputalternative == "greater"){
                p.value <- pR
            } else if (inputalternative == "less"){
                p.value <- pL
            }
            ####  output of sample and test statistics
            withMathJax(
                tags$b("Solution: "),
                paste0("$n =$ ", input$nnz, ", "),
                paste0("$\\hat{p} =$ ", input$phat, ". "),
                br(),
                paste0("Since $n\\hat{p} =  $", input$nnz*input$phat , "   and $n (1 -\\hat{p}) =  $", input$nnz*(1-input$phat), ","),
                paste0(ifelse(((input$nnz*input$phat < 5) | (input$nnz*(1-input$phat) < 5)), 
                              "at least one condition of the central limit is not satisfied. The following results may not be reliable!", 
                              "the conditions of the central limit are satisfied.")),
                br(),br(),
                tags$b("Step 1: Identify the claim of the population mean $(p_0)$." ),
                br(),
                paste0("The given information indicates that the claim is:  $p_0$ is ", input$alternative, " ", input$h0, "."),
                br(),br(),
                tags$b("Step 2: Set up the null and alternative hypotheses."),
                br(),
                paste0("Based on the claim, the null and alternative hypotheses are given by"),
                paste0("$H_0 : p = $ ", input$h0, " and $H_1 : p$ ", 
                                              ifelse(inputalternative == "two.sided", "$ \\neq $", 
                                              ifelse(inputalternative == "greater", "$ > $ ", "$ < $ ")), input$h0, "."),
                br(),br(),
                tags$b("Step 3: Evaluate the test statistic."),
                br(),
                paste0("The test statistic is defined to be: $TS = \\dfrac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}} = ",
                       "\\dfrac{", input$phat, ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), "}{ 
                    \\sqrt{(",input$h0,"(1-", input$h0,")/",input$nnz, "}}  =", round((input$phat-input$h0)/sqrt(input$h0*(1-input$h0)/input$nnz), 3), ".$"),
                ###
                br(),br(),
                tags$b("Step 4: Find the critical value and calculate the p-value."),
                br(),
                paste0("Based on the significance level, we found the critical value(s) to be :", 
                    ifelse(inputalternative == "two.sided", " $ \\pm z_{\\alpha/2} = \\pm z_{", 
                           ifelse(inputalternative == "greater", " $ z_{\\alpha} = z_{", " $ -z_{\\alpha} = -z_{")),
                                  ifelse(inputalternative == "two.sided", input$alpha / 2, input$alpha), "}$", " $ = $ ",
                                         ifelse(inputalternative == "two.sided", "$ \\pm $", ifelse(inputalternative == "greater", "", " -")),
                                             ifelse(inputalternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3)), '.'),
                br(),
                paste0("The p-value is can be found as p-value $\\approx$ ", p.value, "."),
                br(),br(),
                tags$b("Step 5: Make a statistical decision on $H_0$."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(p.value < input$alpha, "we reject the null hypothesis. ", 
                              "we do not reject the null hypothesis. "), 
                       ifelse(p.value < 0.001, "$(p$-value< 0.001", paste0("$(p$-value $=$ ", round(p.value, 3))), ")", "."),
                br(),
                br(),
                tags$b("Step 6: Draw conclusion [justify the claim in step 1]."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(p.value < input$alpha, "we conclude the alternative hypothesis", 
                              "we reject the alternative hypothesis "),  "."),
                br(),
                br()
            )   
        } 
    })
  ############################ END of sample z-test ##############################    
  ################################################################################

    
    ################################################################################
    output$z_plot <- renderPlot({
        #********************************************************************************
        if(input$alternative %in% c("equal to", "not equal to") ) {
            inputalternative = "two.sided"
        }else if (input$alternative %in% c("greater than", "less than or equal to")){
            inputalternative = "greater"
        } else if (input$alternative %in% c("less than", "greater than or equal to")){
            inputalternative = "less"
        }
        #******************************************************************************** 
        
        #############################################################################################
        #if (input$datasourcez == 'statistic') {
                ## functions for setting up the axis of the normal density density
            if (inputalternative == "two.sided") {
                funcShaded <- function(x) {
                    y <- dnorm(x)
                    y[x < qnorm(input$alpha / 2, lower.tail = FALSE) & x > qnorm(input$alpha / 2) ] <- NA
                    return(y)
                }
            } else if (inputalternative == "greater") {
                funcShaded <- function(x) {
                    y <- dnorm(x)
                    y[x < qnorm(input$alpha, lower.tail = FALSE) ] <- NA
                    return(y)
                }
            } else if (inputalternative == "less") {
                funcShaded <- function(x) {
                    y <- dnorm(x)
                    y[x > qnorm(input$alpha,  lower.tail = TRUE) ] <- NA
                    return(y)
                }
            }
                ######   Evaluate test statistic
                TSz <- (input$phat - input$h0)/sqrt(input$h0*(1-input$h0)/input$nnz)
                #### ggplot
                p <- ggplot(data.frame(x = c(qnorm(0.999, lower.tail = FALSE), qnorm(0.999, lower.tail = TRUE))), aes(x = x)) +
                    stat_function(fun = dnorm) +
                    stat_function(fun = funcShaded, geom = "area", alpha = 0.8, colour = "blue") +
                    theme_minimal() +
                    geom_vline(xintercept = TSz, color = "steelblue") +
                    geom_text(aes(x = TSz, label = paste0("TS = ", round(TSz, 3)), y = 0.5*dnorm(qnorm(0.5))), 
                              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
                    ggtitle(paste0("Standard Normal Distribution N(0,1)")) +
                    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
                    ylab("Density") +
                    xlab("z-score")
                p
                ###################
            #}  
    }, height = 200, width = 300 )
    
}     ####close server function

#####################
#####################
shinyApp(ui, server)
    