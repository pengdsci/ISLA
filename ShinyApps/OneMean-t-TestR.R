library(shiny)
library(ggplot2)
##########################################################
#    Define UI for application that draws a histogram
###########################################################
ui <- fluidPage(
  withMathJax(),
  ### Important: The following tag allows inline equation in MathJax
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
    # Application title
    titlePanel(h3("IntroStatsApps: One Sample t Test for Population Mean $\\mu$", 
                  align = "center", style = "color:navy", br(), br())),
    #################
    sidebarLayout(
        sidebarPanel(
                #########################
                tags$b("Data Source"),
                radioButtons(
                    inputId = "datasourcez",
                    label = NULL,
                    choices = c("summarized statistics" = "statistic",
                              "raw data" = "raw") ),
                #### input information
                conditionalPanel(
                    condition = "input.datasourcez == 'statistic'",
                    ### sample mean
                    numericInput("xbarz", "sample mean $(\\bar{x})$",
                                 value = 10, min = 0, step = 1),
                    ### sample variance
                    numericInput("varz", "sample variance $(s^2)$",
                                 value = 10, min = 0, step = 1),
                    ### sample size
                    numericInput("nnz", "sample size $(n)$",
                                 value = 10, min = 0, step = 1)
                ),
                ##########
                conditionalPanel(
                    condition = "input.datasourcez == 'raw'",
                    textInput("sample_z", "comma separated raw data", value = "2.9, -1.8, 2.3, -1.3, 3.7, 3,4,5,6,7,8,3,4,5,6,7,8,9,", 
                              placeholder = "Enter values separated by a comma with decimals as points, 
                                        e.g. 4.2, 2.4, 5.4, 5.7, etc.")
            ),
            
            #########################################################
            ####     Unconditional Panels (common to all tests)
            #########################################################

                numericInput("h0",
                             label = "Claimed Value $(\\mu_0)$",
                             value = 5, step = 0.1),
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
        HTML('<p><center> <img src="https://github.com/pengdsci/sta553/blob/main/image/goldenRamLogo.png?raw=true"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center><font size =2> <a href="mailto:cpeng@wcupa.edu">Report bugs to C. Peng</a> </font></center></p>')
    ),  ###
        
        ############################################
        ######          Main Panel
        ############################################
        mainPanel(
            uiOutput("t_onemean"),
            plotOutput("t_plot"),
            br(),
            br()
        )
    )
)
######

server <- function(input, output){
################################################################################
##         Some R function to be used to calculate test results
################################################################################
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    #####
    t.test2 <- function(x, V, m0 = 0, alpha = 0.05, alternative = "two.sided") {
        M <- mean(x)
        n <- length(x)
        DF < n-1     # degrees of freedom
        sigma <- sqrt(V)
        S <- sqrt(V / n)
        statistic <- (M - m0) / S
        p <- if (alternative == "two.sided") {
            2 * pt(abs(statistic), DF, lower.tail = FALSE)
        } else if (alternative == "less") {
            pt(statistic, DF, lower.tail = TRUE)
        } else {
            pt(statistic, DF, lower.tail = FALSE)
        }
        # p <- (1 - pnorm((M-m0)/S))
        LCL <- (M - S * qt(1 - alpha / 2, DF))
        UCL <- (M + S * qt(1 - alpha / 2, DF))
        value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
        return(value)
    }

 ###############################################################################
 ######                  Outputs of test results
 ###############################################################################   
    output$t_onemean <- renderUI({
        nn = input$nnz
        DF = nn -1   # degrees of freedom
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
            ##--- p-value based on normal distribution
            
            pL <- round(pt((input$xbarz-input$h0)/sqrt(input$varz/input$nnz),DF), 3)
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
                tags$b("Solution:"),
                paste("This t test is based on the assumption that the population is 
                      normal and the population variance is unknown."),
                br(),
                br(),
                tags$b("Given sample information: "),
                paste0("$n =$ ", input$nnz, ", "),
                paste0("$\\bar{x} =$ ", input$xbarz, ", "),
                paste0("$s^2 =$ ", input$varz, "."),
                br(),
                br(),
                tags$b("Step 1: Identify the claim of the population mean $(\\mu_0)$." ),
                br(),
                paste0("The given information indicates that the claim is:  $\\mu_0$ is ", input$alternative, " ", input$h0, "."),
                br(),
                br(),
                tags$b("Step 2: Set up the null and alternative hypotheses."),
                br(),
                paste0("Based on the claim, the null and alternative hypotheses are given by"),
                paste0("$H_0 : \\mu = $ ", input$h0, " and $H_1 : \\mu$ ", 
                                              ifelse(inputalternative == "two.sided", "$ \\neq $", 
                                              ifelse(inputalternative == "greater", "$ > $ ", "$ < $ ")), input$h0, "."),
                br(),br(),
                tags$b("Step 3: Evaluate the test statistic."),
                br(),
                paste0("The test statistic is defined to be: $TS = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = ",
                    "\\dfrac{", input$xbarz, ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), "}{ 
                    \\sqrt{",input$varz,"/",input$nnz, "}}  =", round((input$xbarz-input$h0)/sqrt(input$varz/input$nnz), 3), "$"),
                br(),
                br(),
                tags$b("Step 4: Find the critical value and calculate the p-value."),
                br(),
                paste0("Based on the significance level, we found the critical value(s) to be :", 
                    ifelse(inputalternative == "two.sided", " $CV = \\pm t_{\\alpha/2, df} = \\pm t_{", 
                           ifelse(inputalternative == "greater", " $ t_{\\alpha, df} = t_{", "$ -t_{\\alpha, df} = -t_{")),
                                  ifelse(inputalternative == "two.sided", input$alpha / 2, input$alpha), ",",DF, "}$", " $ = $ ",
                                         ifelse(inputalternative == "two.sided", "$ \\pm $", ifelse(inputalternative == "greater", "", " -")),
                                             ifelse(inputalternative == "two.sided", round(qt(input$alpha/2, DF, lower.tail = FALSE), 3), round(qt(input$alpha, DF, lower.tail = FALSE), 3))),
                br(),
                paste0("The p-value is can be found as p-value $\\approx$ ", p.value, "."),
                br(),
                br(),
                tags$b("Step 5: Make a statistical decision on $H_0$."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(p.value < input$alpha, "we reject the null hypothesis that the true mean is ", 
                              "we do not reject the null hypothesis that the true mean is "), input$h0, " $(p$-value ", 
                       ifelse(p.value < 0.001, "< 0.001", paste0("$=$ ", round(p.value, 3))), ")", "."),
                br(),
                br(),
                tags$b("Step 6: Draw conclusion [justify the claim in step 1]."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(p.value < input$alpha, "we reject the null hypothesis that the true mean is ", 
                              "we do not reject the null hypothesis that the true mean is "), input$h0, " $(p$-value ", 
                       ifelse(p.value < 0.001, "< 0.001", paste0("$=$ ", round(p.value, 3))), ")", "."),
                br(),
                br()
            )   
            
           } else if (input$datasourcez == 'raw') {
               #######################  extracting input raw data  ###########
               dat <- extract(input$sample_z)
               nnz <- length(dat)
               DF <- nnz-1
               xbarz <- round(mean(dat), 3)
               varz <- round(var(dat), 3)
               #############
                   pL <- round(pt((mean(dat)-input$h0)/sqrt(var(dat)/length(dat)),DF), 3)
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
                   ####
              test <- t.test(x = dat, mu = input$h0, conf.level = input$alpha, alternative = inputalternative)
              #######
              withMathJax(
                tags$b("Solution:"),
                paste("This t test is based on the assumption that the population is normal and the population variance is known."),
                br(),
                br(),
                tags$b("Given sample information: "),
                paste0("$n =$ ", length(dat), ", "),
                paste0("$\\bar{x} =$ ", round(mean(dat), 3), ", "),
                paste0("$s^2 =$ ", round(var(dat), 3), "."),
                br(),br(),
                tags$b("Step 1: Identify the claim of the population mean $(\\mu_0)$." ),
                br(),
                paste0("The given information indicates that the claim is:  $\\mu_0$ is ", input$alternative, " ", input$h0, "."),
                br(),
                br(),
                tags$b("Step 2: Set up the null and alternative hypotheses."),
                br(),
                paste0("Based on the claim, the null and alternative hypotheses are given by"),
                paste0("$H_0 : \\mu = $ ", input$h0, " and $H_1 : \\mu $ ", 
                       ifelse(inputalternative == "two.sided", "$ \\neq $ ", 
                              ifelse(inputalternative == "greater", "$ >$ ", "$ < $ ")), input$h0),
                br(),br(),
                tags$b("Step 3: Evaluate the test statistic."),
                br(),
                paste0("The test statistic is defined to be: $TS = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = ",
                       "\\dfrac{", xbarz, ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), "}{ 
                    \\sqrt{",varz,"/",nnz, "}}  =", round((xbarz-input$h0)/sqrt(varz/nnz), 3), "$"),
                br(),br(),
                tags$b("Step 4: Find the critical value and calculate the p-value."),
                br(),
                paste0("Based on the significance level, we found the critical value(s) to be :", 
                    ifelse(inputalternative == "two.sided", " $ \\pm t_{\\alpha/2, df} = \\pm t_{", 
                           ifelse(inputalternative == "greater", " $ t_{\\alpha, df} = t_{", "$ -t_{\\alpha, df} = -t_{")),
                                 ifelse(inputalternative == "two.sided", input$alpha / 2, input$alpha), ",", DF, "}$", " $ = $ ",
                                       ifelse(inputalternative == "two.sided", "$ \\pm $", ifelse(inputalternative == "greater", "", " -")),
                                             ifelse(inputalternative == "two.sided", round(qt(input$alpha / 2, DF, lower.tail = FALSE), 3), round(qt(input$alpha, DF, lower.tail = FALSE), 3))
                ),
                br(),br(),
                tags$b("Step 5: Make a statistical decision on $H_0$."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", 
                              "we do not reject the null hypothesis that the true mean is "), input$h0, " $(p$-value ", 
                       ifelse(test$p.value < 0.001, "< 0.001", paste0("$=$ ", round(test$p.value, 3))), ")", "."),
                br(),
                br(),
                tags$b("Step 6: Draw conclusion [justify the claim in step 1]."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", 
                              "we do not reject the null hypothesis that the true mean is "), input$h0, " $(p$-value ", 
                       ifelse(test$p.value < 0.001, "< 0.001", paste0("$=$ ", round(test$p.value, 3))), ")", "."),
                br(),
                br()
             )
         } 
    })
  ############################ END of sample z-test ##############################    
  ################################################################################

    ################################################################################
    output$t_plot <- renderPlot({
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
        if (input$datasourcez == 'statistic') {
            nnz <- input$nnz
            xbarz <-input$xbarz
            varz <- input$varz
            DF <- nnz -1
                ## functions for setting up the axis of the normal density density
            if (inputalternative == "two.sided") {
                funcShaded <- function(x) {
                    y <- dt(x, DF)
                    y[x < qt(input$alpha / 2, DF, lower.tail = FALSE) & x > qt(input$alpha / 2, DF) ] <- NA
                    return(y)
                }
            } else if (inputalternative == "greater") {
                funcShaded <- function(x) {
                    y <- dt(x, DF)
                    y[x < qt(input$alpha, DF, lower.tail = FALSE) ] <- NA
                    return(y)
                }
            } else if (inputalternative == "less") {
                funcShaded <- function(x) {
                    y <- dt(x, DF)
                    y[x > qt(input$alpha, DF, lower.tail = TRUE) ] <- NA
                    return(y)
                }
            }
                ######   Evaluate test statistic
                TSz <- (input$xbarz - input$h0)/sqrt(input$varz/input$nnz)
                #### ggplot
                p <- ggplot(data.frame(x = c(qt(0.999, DF, lower.tail = FALSE), qt(0.999, DF, lower.tail = TRUE))), aes(x = x)) +
                    stat_function(fun = dt, args = list(df = DF)) +
                    stat_function(fun = funcShaded, geom = "area", alpha = 0.8, colour = "blue") +
                    theme_minimal() +
                    geom_vline(xintercept = TSz, color = "steelblue") +
                    geom_text(aes(x = TSz, label = paste0("TS = ", round(TSz, 3)), y = 0.5*dt(qt(0.5, DF), DF)), 
                              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
                    ggtitle(paste0("t Distribution: t[n-1]")) +
                    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
                    ylab("Density") +
                    xlab("t-score")
                p
                ###################
            } else if (input$datasourcez == 'raw'){
                ##### extracting data
                dat <- extract(input$sample_z)
                test <- t.test(x = dat, mu = input$h0, alternative = inputalternative, conf.level = 1 - input$alpha)
                DF <- length(dat)-1
                #####
                teststatistic <- (mean(dat) - input$h0)/sqrt(var(dat)/length(dat))
                #######
                if (inputalternative == "two.sided") {
                    funcShaded <- function(x) {
                        y <- dt(x, DF)
                        y[x < qt(input$alpha / 2, DF, lower.tail = FALSE) & x > qt(input$alpha / 2, DF )] <- NA
                        return(y)
                    }
                } else if (inputalternative == "greater") {
                    funcShaded <- function(x) {
                        y <- dt(x, DF)
                        y[x < qt(input$alpha, DF, lower.tail = FALSE) ] <- NA
                        return(y)
                    }
                } else if (inputalternative == "less") {
                    funcShaded <- function(x) {
                        y <- dt(x, DF)
                        y[x > qt(input$alpha, DF, lower.tail = TRUE) ] <- NA
                        return(y)
                    }
                }
                p <- ggplot(data.frame(x = c(qt(0.999, DF, lower.tail = FALSE), qt(0.999, DF, lower.tail = TRUE))), aes(x = x)) +
                    stat_function(fun = dt, args = list(df = DF)) +
                    stat_function(fun = funcShaded, geom = "area", alpha = 0.8, colour = "blue") +
                    theme_minimal() +
                    geom_vline(xintercept = teststatistic, color = "steelblue") +
                    geom_text(aes(x = teststatistic, label = paste0("TS = ", round(teststatistic, 3)), y = 0.5*dt(qt(0.5, DF), DF)), 
                              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
                    ggtitle(paste0("t Distribution: t[n-1]")) +
                    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
                    ylab("Density") +
                    xlab("t-score")
                p
              } 
    }, height = 200, width = 300 )
    
}     ####close server function

#####################
#####################
shinyApp(ui, server)
    