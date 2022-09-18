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
                    tex2jax: {inlin eMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
    # Application title
    titlePanel(h3("IntroStatsApps: Two-sample Test About $\\mu_1 - \\mu_2$", 
                  align = "center", style = "color:navy", br(), br())),
    #################
    sidebarLayout(
        sidebarPanel(
                #########################
                tags$b("Sample #1"),
                hr(),
                #### input information
                    ### sample mean
                    numericInput("xbar1", "sample mean $(\\bar{x}_1)$",
                                 value = 10, min = 0, step = .1),
                    ### sample variance
                    numericInput("var1", "sample variance $(s_1^2)$",
                                 value = 16, min = 0, step = .1),
                    ### sample size
                    numericInput("nn1", "sample size $(n)$",
                                 value = 16, min = 0, step = 1),

                
                br(),
                tags$b("Sample #2"),
                hr(),
                #### input information

                  ### sample mean
                  numericInput("xbar2", "sample mean $(\\bar{x}_2)$",
                               value = 19, min = 0, step = .1),
                  ### sample variance
                  numericInput("var2", "sample variance $(s_2^2)$",
                               value = 25, min = 0, step = .1),
                  ### sample size
                  numericInput("nn2", "sample size $(n_2)$",
                               value = 49, min = 0, step = 1),

            #########################################################
            ####     Unconditional Panels (common to all tests)
            #########################################################

                numericInput("h0",
                             label = "Claimed Value $(\\mu_1 - \\mu_2)$",
                             value = 0, step = 0.1),
                selectInput(
                    inputId = "alternative",
                    label = "Claim Type",
                    choices = c("equal to", 
                                "not equal to", 
                                "greater than", 
                                "less than or equal to",
                                "less than", 
                                "greater than or equal to")),
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
            uiOutput("two_mean"),
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
##         Some R function to be used to calculate test results
################################################################################
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
   #####
   
 ###############################################################################
 ######                  Outputs of test results
 ###############################################################################   
    output$two_mean <- renderUI({
        ###################      define test type   ##################
        if(input$alternative %in% c("equal to", "not equal to") ) {
            inputalternative = "two.sided"
        }else if (input$alternative %in% c("greater than", "less than or equal to")){
            inputalternative = "greater"
        } else if (input$alternative %in% c("less than", "greater than or equal to")){
            inputalternative = "less"
        }
        ######################################
        ###
            if (min(input$nn1, input$nn2) > 30) {   ## two-sample normal tests
            x1bar = input$xbar1
            s1sq = input$var1
            nn1 = input$nn1
            x2bar = input$xbar2
            s2sq = input$var2
            nn2 = input$nn2
            ###
            TS = ((x1bar - x2bar)-input$h0)/sqrt(s1sq/nn1 + s2sq/nn2)
            ##--- p-value based on normal distribution``````````
            pL <- round(pnorm(TS), 3)
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
                paste("Since both sample sizes are greater than 30, this normal test is based on the Central Limit Theorem ((CLT))."),
                br(),
                br(),
                tags$b("Given sample information: "),
                paste0("$n_1 =$ ", nn1, ",  $\\bar{x}_1 = $ ", x1bar, ",  $s_1^2 =$ ", s1sq, ";"),
                paste0("$n_2 =$ ", nn2, ",  $\\bar{x}_2 = $ ", x2bar, ",  $s_2^2 =$ ", s2sq, "."),
                br(),br(),
                tags$b("Step 1: Identify the claim of the population mean $(\\mu_1 - \\mu_2)$." ),
                br(),
                paste0("The given information indicates that the claim is:  
                       $\\mu_1-\\mu_2$ is ", input$alternative, " ", input$h0, "."),
                br(),br(),
                tags$b("Step 2: Set up the null and alternative hypotheses."),
                br(),
                paste0("Based on the claim, the null and alternative hypotheses are given by"),
                paste0("$H_0 : \\mu_1-\\mu_2 = $ ", input$h0, " and $H_1 : \\mu_1-\\mu_2$ ", 
                                              ifelse(inputalternative == "two.sided", "$ \\neq $", 
                                              ifelse(inputalternative == "greater", "$ > $ ", "$ < $ ")), input$h0, "."),
                br(),br(),
                tags$b("Step 3: Evaluate the test statistic."),
                br(),
                paste0("The test statistic is defined to be: "),
                br(),br(),
                paste0("$TS = \\dfrac{(\\bar{x}_1 -\\bar{x}_2)- (\\mu_1-\\mu_2)}{ \\sqrt{s_1^2/n_1+s_2^2/n_2}} = ",
                       "\\dfrac{ (", x1bar, "-", x2bar, ")", ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), "}{ 
                    \\sqrt{",s1sq,"/",nn1, " + ",s2sq,"/",nn2, "}}  =", round(TS, 3), "$"),
                ###
                br(),br(),
                tags$b("Step 4: Find the critical value and calculate the p-value."),
                br(),
                paste0("Based on the significance level, we found the critical value(s) to be :", 
                    ifelse(inputalternative == "two.sided", " $ \\pm z_{\\alpha/2} = \\pm z_{", 
                           ifelse(inputalternative == "greater", " $ z_{\\alpha} = z_{", " $ -z_{\\alpha} = -z_{")),
                                  ifelse(inputalternative == "two.sided", input$alpha / 2, input$alpha), "}$", " $ = $ ",
                                         ifelse(inputalternative == "two.sided", "$ \\pm $", ifelse(inputalternative == "greater", "", " -")),
                                             ifelse(inputalternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))),
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
                              "we reject the alternative hypothesis "),  ". The claim is addressed 
                       using relationship between the alternative hypothesis and the claim."),
                br(),
                br()
            )   
            
           } else {  ## two-sample t-test
               #######################  extracting input raw data  ###########
             x1bar = input$xbar1
              s1sq = input$var1
               nn1 = input$nn1
             x2bar = input$xbar2
              s2sq = input$var2
               nn2 = input$nn2
                DF = nn1 + nn2 -2
             ##
             ssqPool = ((nn1-1)*s1sq + (nn2-1)*s2sq)/(nn1 +nn2 -2)
             ##
             TS = ((x1bar - x2bar)-input$h0)/sqrt(ssqPool/nn1 + ssqPool/nn2)
               #############
                   pL <- round(pt(TS, (nn1+nn2-2)), 3)
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
              #######
              withMathJax(
                tags$b("Solution:"),
                paste("Since one of the sample sizes is less than 31. The following normal test assumes both populations are normal and the two unknown population variances are equal."),
                br(),
                br(),
                tags$b("Given sample information: "),
                paste0("$n_1 =$ ", nn1, ",  $\\bar{x}_1 = $ ", x1bar, ",  $s_1^2 =$ ", s1sq, "."),
                br(),
                paste0("$n_2 =$ ", nn2, ",  $\\bar{x}_2 = $ ", x2bar, ",  $s_2^2 =$ ", s2sq, "."),
                br(),br(),
                tags$b("Step 1: Identify the claim of the population mean $(\\mu_1-\\mu_2)$." ),
                br(),
                paste0("The given information indicates that the claim is:  $\\mu_1-\\mu_2$ is ", input$alternative, " ", input$h0, "."),
                br(),
                br(),
                tags$b("Step 2: Set up the null and alternative hypotheses."),
                br(),
                paste0("Based on the claim, the null and alternative hypotheses are given by"),
                paste0("$H_0 : \\mu_1 - \\mu_2 = $ ", input$h0, " and $H_1 : \\mu_1 - \\mu_2 $ ", 
                       ifelse(inputalternative == "two.sided", "$ \\neq $ ", 
                              ifelse(inputalternative == "greater", "$ >$ ", "$ < $ ")), input$h0),
                br(),br(),
                tags$b("Step 3: Evaluate the test statistic."),
                br(),
                paste0("We first find the pooled sample variance in the following"),
                br(),
                paste0("$s_{pool}^2 = \\dfrac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 -2}=",
                       "\\dfrac{(",nn1,"- 1)",s1sq, "+ (", nn2, "-1)", s2sq,"}{",nn1, "+", nn2, "-2} =", round(ssqPool,3), "$"),
                br(),
                paste0("The test statistic is defined to be: "),
                br(),br(),
                paste0("$TS = \\dfrac{(\\bar{x}_1 -\\bar{x}_2)- (\\mu_1-\\mu_2)}{ \\sqrt{s_1^2/n_1+s_2^2/n_2}} = ",
                       "\\dfrac{ (", x1bar, "-", x2bar, ")", ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), "}{ 
                    \\sqrt{",round(ssqPool,3),"/",nn1, " + ",round(ssqPool,3),"/",nn2, "}}  =", round(TS, 3), "$"),
                #####
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
                       ifelse(p.value < input$alpha, "we reject the null hypothesis that the true mean is ", 
                              "we do not reject the null hypothesis that the true mean is "), input$h0, " $(p$-value ", 
                       ifelse(p.value < 0.001, "< 0.001", paste0("$=$ ", round(p.value, 3))), ")", "."),
                br(),
                br(),
                tags$b("Step 6: Draw conclusion [justify the claim in step 1]."),
                br(),
                paste0("At the ", input$alpha * 100, "% significance level, ", 
                       ifelse(p.value < input$alpha, "we conclude the alternative hypothesis", 
                              "we reject the alternative hypothesis "),  ". The claim is addressed 
                       using relationship between the alternative hypothesis and the claim."),
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
           ##################################################################
           ##################################################################
              x1bar = input$xbar1
               s1sq = input$var1
                nn1 = input$nn1
              x2bar = input$xbar2
               s2sq = input$var2
                nn2 = input$nn2
              DF = nn1 + nn2 -2
           if (min(input$nn1, input$nn2) > 30) {   ## two-sample normal tests
                  ###
                TSz = ((x1bar - x2bar)-input$h0)/sqrt(s1sq/nn1 + s2sq/nn2)
              } else {
                ssqPool = ((nn1-1)*s1sq + (nn2-1)*s2sq)/(nn1 +nn2 -2)
                ##
                TSz = ((x1bar - x2bar)-input$h0)/sqrt(ssqPool/nn1 + ssqPool/nn2)
              }
               
           ##################################################################
           ##################################################################     
            if(min(input$nn1, input$nn2) > 30){
                #### ggplot
                p <- ggplot(data.frame(x = c(qnorm(0.999, lower.tail = FALSE), qnorm(0.999, lower.tail = TRUE))), aes(x = x)) +
                    stat_function(fun = dnorm) +
                    stat_function(fun = funcShaded, geom = "area", alpha = 0.8, colour = "blue") +
                    theme_minimal() +
                    geom_vline(xintercept = TSz, color = "steelblue") +
                    geom_text(aes(x = TSz, label = paste0("TS = ", round(TSz, 3)), y = 0.5*dnorm(qnorm(0.5))), 
                              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
                    #ggtitle(paste0("t Distribution: t[",DF,"]")) +
                    ggtitle(paste0("Standard Normal Distribution N(0,1)")) +
                    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
                    ylab("Density") +
                    xlab("z-score")
                p
                ###################
            } else{
              p <- ggplot(data.frame(x = c(qnorm(0.999, lower.tail = FALSE), qnorm(0.999, lower.tail = TRUE))), aes(x = x)) +
                stat_function(fun = dnorm) +
                stat_function(fun = funcShaded, geom = "area", alpha = 0.8, colour = "blue") +
                theme_minimal() +
                geom_vline(xintercept = TSz, color = "steelblue") +
                geom_text(aes(x = TSz, label = paste0("TS = ", round(TSz, 3)), y = 0.5*dnorm(qnorm(0.5))), 
                          colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
                ggtitle(paste0("t Distribution: t[",DF,"]")) +
                # ggtitle(paste0("Standard Normal Distribution N(0,1)")) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
                ylab("Density") +
                xlab("z-score")
              p  
            }
                
                
                
    }, height = 200, width = 300 )
    
}     ####close server function

#####################
#####################
shinyApp(ui, server)
    