#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#  

library(shiny)
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
            titlePanel( h3("ISLA: Central Limit Theorem Demonstrations", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),
  
    ##
    withMathJax(),
    sidebarLayout(
        sidebarPanel( style = "background-color: #5e2442; color:gold; ", 
          selectInput(
                inputId = "PopDist",
                label = "Population Distribution",
                choices = c("Uniform [a, b]",
                            "Normal N(mu, sigma)",
                            "Binomial Binom(n, p)",
                            "Poisson Pois(lambda)",
                            "Exponential exp(rate)"),
                multiple = FALSE,
                selected = "Uniform [a, b]"),
            hr(),
            ###############################################################################
            ##                         Uniform Distribution
            ###############################################################################
            conditionalPanel(
                condition = "input.PopDist == 'Uniform [a, b]'",
                #########################
                textInput("interval", "[a, b]", 
                          value = "0, 1", 
                          placeholder = "Enter two numeric values separated by a comma."),
                br(),
               ), 
 
          ###############################################################################
          ##                         Normal Distribution
          ###############################################################################
          conditionalPanel(
            condition = "input.PopDist == 'Normal N(mu, sigma)'",
            #########################
            textInput("mu", "Population Mean", 
                      value = "0", 
                      placeholder = "Enter two numeric values separated by a comma."),
            ###
            textInput("sigma", "Population Standard Deviation", 
                        value = "1", 
                        placeholder = "Enter two numeric values separated by a comma."),
            ###
            br(),
          ),   
            
          ###############################################################################
          ##                         Binomial Distribution
          ###############################################################################            
           conditionalPanel(
             condition = "input.PopDist == 'Binomial Binom(n, p)'",
             ###########
             sliderInput("trialNum", "Number of Trials",
                         value = 15,
                         min = 1,
                         max = 100,
                         step = 1),
             ############
             textInput("prob", "Success Probability",
                       value = "0.5",
                       placeholder = "Enter character values separated by a comma."
                 ),
             br(),
             ),
 
          #################################################
          #######          Poisson Distribution
          #################################################
          conditionalPanel(
            condition = "input.PopDist == 'Poisson Pois(lambda)'",
            #########################
            textInput("lambda", "Poisson Mean", 
                      value = 1, 
                      placeholder = "Enter character values separated by a comma."),
            ),
          
          
          #################################################
          #######          Exponential Distribution
          #################################################
          conditionalPanel(
            condition = "input.PopDist == 'Exponential exp(rate)'",
            #########################
            textInput("gamma", "Exponent Rate = 1/Mean", 
                      value = 5, 
                      placeholder = "Enter character values separated by a comma."),
          ),
          
          #################################################
          #######          Sample size
          #################################################
          sliderInput(
            inputId = "size",
            label = "Sample Size",
            value = 2,
            min = 1,
            max = 500,
            step = 5,
            animate=list(interval=500, loop=T)),
        
          #################################################
          #######          Number of samples
          #################################################
          sliderInput(
            inputId = "sampleNum",
            label = "Number of Samples",
            value = 500,
             min = 500,
             max = 5000,
             step = 100,
            animate=list(interval=1000, loop=T)),
           #####
            hr(),
            ###
          HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
          HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
          
          #########################################################
            ####   End of unconditional Panels (common to all tests)
            #########################################################
        ),
        ###
        
        ############################################
        ######          Main Panel
        ############################################
        mainPanel(  style = "background-color:lightgray;", 
           conditionalPanel(
              condition = "input.PopDist == 'Uniform [a, b]'",
                 uiOutput("uniform")
                 ),
            
           conditionalPanel(
              condition = "input.PopDist == 'Normal N(mu, sigma)'",
                uiOutput("normal")
                ),
            
           conditionalPanel(
               condition = "input.PopDist == 'Binomial Binom(n, p)'",
               uiOutput("binomial")
              ),
           
           conditionalPanel(
             condition = "input.PopDist == 'Poisson Pois(lambda)'",
             uiOutput("Poisson")
           ),
           
           conditionalPanel(
             condition = "input.PopDist == 'Exponential exp(rate)'",
             uiOutput("exponential")
           ),
         )  #mainPanel
     )
)
######
######

server <- function(input, output){
################################################################################
##         Some R function to be used to calculate test results
################################################################################
   extract_num <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
        }
   ####
   extract_cat <- function(text) {
      text <- gsub(" ", "", text)
      split <- strsplit(text, ",", fixed = FALSE)[[1]]
      split
      }
   ### finding decimal places for each data value in a data set
   decimalplaces <- function(x) {
           ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
            nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
            0)
     }
   
 ###############################################################################
 ######                  Uniform Population
 ###############################################################################  
   output$uniform <- renderUI({
     ############
     NumInput <- extract_num(input$interval)
     a <- min(NumInput)
     b <- max(NumInput)
     ### boundary
     size <- input$size
     NumSample <- input$sampleNum 
     ############
     rand.vec <- runif(size*NumSample, a, b)
     rand.mtx <- matrix(rand.vec, ncol=NumSample)
     sample.avg <- apply(rand.mtx, 2, mean)
     ymax=max(1.1*density(sample.avg)$y)
     ############
     withMathJax(
       br(),
       br(),
       tags$b("The input information:"),
       br(),
       br(),
       paste("The random samples is taken from uniform population, U[", a, ",  ", b, "]."),
       br(),
       paste("The sample size n =", input$size, " and the number of samples is ", input$sampleNum, "."),
       br(),
       br(),
       tags$b("The distribution of the population and the sampling distribution of sample means are depicted in the following fitgure."),
       br(),
       br(),
       output$unif <- renderPlot({
         # Normal curve
         x2 <- runif(1000, a, b)
         fun <- dunif(x2, a,b)
         par(bg="lightgray")
         # Histogram
         hist(sample.avg, prob = TRUE, col = "cyan4", border = "orange",
              xlim=c(0.9*a,1.1*b),
              ylim = c(0, ymax),
              main = "Sampling Distribution Means: Uniform Population ")
         lines(density(sample.avg, adjust=2),  col = "blue", lty = 1, lwd =2)
         lines(x2, fun, col = "darkred", lwd = 1, lty=1)
         segments(c(a,b), c(0,0), c(a,b), c(1/(b-a), 1/(b-a)), col="darkred")
         legend("topright", c("Sampling Distribution", "Population Distribution"), 
                lty=rep(1,2), lwd=rep(2,2), col=c("blue", "darkred"), bty = "n", cex = 0.8)
          },height = 400, width = 550),
       br(),
       br()
      ) # mathjax
   })
   ###############################################################################
   ######                  Normal Population
   ############################################################################### 
   
   output$normal <- renderUI({
     ############
     mu <- extract_num(input$mu)
     sigma <- extract_num(input$sigma)
     ### boundary
     size <- input$size
     NumSample <- input$sampleNum 
     ############
     rand.vec <- rnorm(size*NumSample, mu, sigma)
     rand.mtx <- matrix(rand.vec, ncol=NumSample)
     sample.avg <- apply(rand.mtx, 2, mean)
     ymax=max(1.1*density(sample.avg)$y)
     ############
     withMathJax(
       br(),
       br(),
       tags$b("The input information:"),
       br(),
       br(),
       paste("The random samples is taken from normal population, N(", mu, ",  ", sigma, ")."),
       br(),
       paste("The sample size n =", input$size, " and the number of samples is ", input$sampleNum, "."),
       br(),
       br(),
       tags$b("The distribution of the population and the sampling distribution of sample means are depicted in the following fitgure."),
       br(),
       br(),
       output$unif <- renderPlot({
         # Normal curve
         x2 <- seq(mu-3*sigma, mu+3*sigma, length = 200)
         fun <- dnorm(x2, mu, sigma)
         # Histogram
         par(bg="lightgray")
         hist(sample.avg, prob = TRUE, col = "cyan4", border = "orange",
              xlim=c(mu-3*sigma, mu+3*sigma),
              ylim = c(0, ymax),
              main = "Sampling Distribution Means: Normal Population ")
         lines(density(sample.avg),  col = "blue", lty=1, lwd =2)
         lines(x2, fun, col = "darkred", lwd = 2, lty=1)
         legend("topright", c("Sampling Distribution", "Population Distribution"), 
                lty=rep(1,2), lwd=rep(2,2), col=c("blue", "darkred"), bty = "n", cex = 0.8)
       },height = 400, width = 550),
       br(),
       br()
     ) # mathjax
   })  
   
   ###############################################################################
   ######                  Binomial Population
   ############################################################################### 
   
   output$binomial <- renderUI({
     ############
     trialNum <- input$trialNum
     prob <- extract_num(input$prob)
     ### boundary
     size <- input$size
     NumSample <- input$sampleNum 
     ############
     rand.vec <- rbinom(size*NumSample, trialNum, prob)
     rand.mtx <- matrix(rand.vec, ncol=NumSample)
     sample.avg <- apply(rand.mtx, 2, mean)
     ymax=max(1.1*density(sample.avg)$y)
     ############
     withMathJax(
       br(),
       br(),
       tags$b("The input information:"),
       br(),
       br(),
       paste("The random samples is taken from binomial population, Biom(", trialNum, ",  ", prob, ")."),
       br(),
       paste("The sample size n =", input$size, " and the number of samples is ", input$sampleNum, "."),
       br(),
       br(),
       tags$b("The distribution of the population and the sampling distribution of sample means are depicted in the following fitgure."),
       br(),
       br(),
       output$binom<- renderPlot({
         ## Histogram
         x0 <- 0:trialNum
         y0 <- rep(0, trialNum+1)
         x1 <- x0
         y1 <- dbinom(x0, trialNum, prob)
         ##
         par(bg="lightgray")
         hist(sample.avg, prob = TRUE, col = "cyan4", border = "orange",
              xlim=c(0,trialNum),
              ylim = c(0, ymax),
              main = "Sampling Distribution Means: Binomial Population ")
         lines(density(sample.avg),  col = "blue", lty=1, lwd =2)
         ####
         segments(x0, y0, x1, y1, lty=1, lwd = 5, col = "darkred")
         ####
         legend("topright", c("Sampling Distribution", "Population Distribution"), 
                lty=rep(1,2), lwd=c(2,5), col=c("blue", "darkred"), bty = "n", cex = 0.8)
       },height = 400, width = 550),
       br(),
       br()
     ) # mathjax
   })  
   
   
   ###############################################################################
   ######                  Poisson Population
   ############################################################################### 
   
   output$Poisson <- renderUI({
     ############
     lambda <- extract_num(input$lambda)
     ### boundary
     size <- input$size
     NumSample <- input$sampleNum 
     ############
     rand.vec <- rpois(size*NumSample, lambda)
     rand.mtx <- matrix(rand.vec, ncol=NumSample)
     sample.avg <- apply(rand.mtx, 2, mean)
     ymax=max(1.1*density(sample.avg)$y)
     ############
     withMathJax(
       br(),
       br(),
       tags$b("The input information:"),
       br(),
       br(),
       paste("The random samples is taken from binomial population, Pois(", lambda, ")."),
       br(),
       paste("The sample size n =", input$size, " and the number of samples is ", input$sampleNum, "."),
       br(),
       br(),
       tags$b("The distribution of the population and the sampling distribution of sample means are depicted in the following fitgure."),
       br(),
       br(),
       output$pois<- renderPlot({
         ## Histogram
         x0 <- 0:qpois(0.999, lambda)
         y0 <- rep(0, length(x0))
         x1 <- x0
         y1 <- dpois(x0, lambda)
         ##
         par(bg="lightgray")
         hist(sample.avg, prob = TRUE, col = "cyan4", border = "orange",
              xlim=c(0,qpois(0.999, lambda)),
              ylim = c(0, ymax),
              main = "Sampling Distribution Means: Poisson Population ")
         lines(density(sample.avg),  col = "blue", lty=1, lwd =2)
         ####
         segments(x0, y0, x1, y1, lty=1, lwd = 5, col = "darkred")
         ####
         legend("topright", c("Sampling Distribution", "Population Distribution"), 
                lty=rep(1,2), lwd=c(2,5), col=c("blue", "darkred"), bty = "n", cex = 0.8)
       },height = 400, width = 550),
       br(),
       br()
     ) # mathjax
   })  
   
   
   ###############################################################################
   ######                  Exponential Population
   ############################################################################### 
   
   output$exponential <- renderUI({
     ############
     gamma <- extract_num(input$gamma)
     ### boundary
     size <- input$size
     NumSample <- input$sampleNum 
     ############
     rand.vec <- rexp(size*NumSample, gamma)
     rand.mtx <- matrix(rand.vec, ncol=NumSample)
     sample.avg <- apply(rand.mtx, 2, mean)
     ymax=max(1.1*density(sample.avg)$y)
     ############
     withMathJax(
       br(),
       br(),
       tags$b("The input information:"),
       br(),
       br(),
       paste("The random samples is taken from binomial population, Exp$($", gamma, "$)$.", sep=""),
       br(),
       paste("The sample size n =", input$size, " and the number of samples is ", input$sampleNum, "."),
       br(),
       br(),
       tags$b("The distribution of the population and the sampling distribution of sample means are depicted in the following fitgure."),
       br(),
       br(),
       output$exp <- renderPlot({
         ## Histogram
         x.seq <- seq(0,qexp(0.99, gamma), length = 200)
         ##
         par(bg="lightgray")
         hist(sample.avg, prob = TRUE, col = "cyan4", border = "orange",
              xlim=c(0,qexp(0.99, gamma)),
              ylim = c(0, ymax),
              main = "Sampling Distribution Means: Exponential Population ")
         lines(density(sample.avg),  col = "blue", lty=1, lwd =2)
         ####
         lines(x.seq, dexp(x.seq, gamma), lty=1, lwd = 5, col = "darkred")
         ####
         legend("topright", c("Sampling Distribution", "Population Distribution"), 
                lty=rep(1,2), lwd=c(2,5), col=c("blue", "darkred"), bty = "n", cex = 0.8)
       },height = 400, width = 550),
       br(),
       br()
     ) # mathjax
   })  
   
   
#####   
} 
####

######################  

#####################
#####################
shinyApp(ui, server)
    




