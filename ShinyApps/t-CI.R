#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
       tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
    fluidRow(
       column(10,
        wellPanel(#style = "background: white",
        h3(strong('IntroStatsApps: t Confidence Intervals for Population Mean $(\\mu)$')), 
        ))),
    fluidRow(
        ######################
        column(4, wellPanel(
            strong('1. CI for $\\mu$ or $p$?'),
            ###
            radioButtons('CItype', label='',
                         choices=list('Population Mean $(\\mu)$' = 1,
                                      'Population Proportion $(p)$' = 2), 
                         selected=1), 
            
            strong('2. Sample Statistics'),
            br(),
            br(),
            ###########
            conditionalPanel(condition="input.CItype == '1'",
                 numericInput('xbar', label='Sample Mean: $\\bar{X}$', 
                        min=10, max=90, value=47, step=.1), 
                 numericInput('s', label='Standard Deviation: $s$ or $\\sigma_0$', min=0, max=NA, value=9), 
            ),
            #####
            conditionalPanel(condition="input.CItype == '2'",
                   numericInput('phat', label='Sample Proportion [ in decimal form ]: $\\hat{p}$', 
                                          min=0, max=1, value=0.6, step=1), 
            ),

            strong('3. Sample Size and Confidence Level'),
            br(),
            br(),
            numericInput('n', label='Sample Size: $n$', 
                         min=2, max=100, value=10, step=1),
            sliderInput('C', label='Confidence Level: $\\alpha$', min=80, max=99, value=95, step=1, post='%'),
            #########
            hr(),
            HTML('<p><center> <img src="https://github.com/pengdsci/sta553/blob/main/image/goldenRamLogo.png?raw=true"  width="100" height="100"></center></p>'),
            HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center><font size =2> <a href="mailto:cpeng@wcupa.edu">Report bugs to C. Peng</a> </font></center></p>')
            
        )),

        ######################
        ######################
        column(6, wellPanel( 
            h4(strong('Steps for Constructing A C.I.')), 
            hr(),
            strong("Step 1. The given confidence level."),
            br(),
            'conf.level $= 1 -\\alpha$ = ', textOutput('confLevel', inline=T),
            br(),
            br(),
            strong("Step 2. CV on the standard normal density curve."),
            br(),
            '$CV = Z_{\\alpha/2}$ = ', textOutput('tstar', inline=T),
            br(),
            br(),
            plotOutput('stdevPlot', height="125px"),
            uiOutput("Steps"),
#            textOutput('ciSteps'),
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ##########
    withMathJax()   
    #####################
    results <- reactive({
        tstar <- qnorm((1-input$C/100)/2, lower.tail=F)
        confLevel = input$C/100
        list(tstar=tstar, conf = confLevel)
    })
    #########
    output$tstar <- renderText({
        sprintf('%.3f', results()$tstar)
    })
    #########
    output$confLevel <- renderText({
        paste0(input$C, '%')
   })
    
    #########################################
    #########################################
    output$stdevPlot <- renderPlot({
        par(mar=c(2,.1,.1,.1), bg = "#f7f7f7")
        z <- seq(from=-4, to=4, by=.01)
        ###########
        plot(z, dnorm(z), type='l', xaxt='n', yaxt='n', lwd=2, 
             ylim=c(-0.1*dnorm(0), dnorm(0)), bty = 'n', axes = F, col = "navy")
        abline(h=0, lwd = 2, col = "navy")
        ############
        tstar3 <- round(results()$tstar, 3)
        tstar2 <- floor(100*tstar3)/100  # round down to keep 2 decimal places for creating a sequence
        segments(x0=tstar3*c(-1,1), y0=0, y1=dnorm(tstar3))
        polygon(x=c(-tstar3, -tstar3, seq(from=-tstar2, to=tstar2, by=.01), tstar3, tstar3), 
                y=c(0, dnorm(-tstar3), dnorm(seq(from=-tstar2, to=tstar2, by=.01)),dnorm(tstar3), 0), 
                col='skyblue')
        ###### label confidence level
        text(0, .5*dnorm(0), paste0(input$C,'%'), col = "blue")
        ######  label CV
        text(tstar3, -0.095*dnorm(0),  bquote(paste('CV'[R],'=',.(tstar3))), col = "red", cex = 0.9)
        text(-tstar3, -0.095*dnorm(0), bquote(paste('CV'[L],'= ',.(-tstar3))), col = "red", cex = 0.9)
    }, height = 150, width = 250 )
    
    ###############################
    #####################################
    output$Steps <- renderUI({  
    ##########
    nn <- input$n
    conf <- input$C/100
    CV <- round(qnorm((1-conf)/2, lower.tail=F),3)
    ##########
    #output$ciSteps <- renderText({
       # withMathJax()
        if(input$CItype == '1'){
            xbar <- input$xbar
            s <- input$s
            E <- round(CV*s/sqrt(nn), 3)
            withMathJax(
                br(),
                tags$b("Step 3: Margin of Error"),
                paste('$$E = CV\\times \\frac{s}{\\sqrt{n}} =',CV,'\\times \\frac{',s,'}{\\sqrt{',nn,'}} =', E,'$$'),
                tags$b("Step 4: Expression of Confidence Interval"),
                paste('$$(\\bar{X} - E, \\bar{X} + E) = (',xbar,'-',E,', ',xbar,'+',E,') = (',xbar-E,',',xbar+E,')$$'),
                tags$b("Step 5: Interpretation of Confidence Interval"),
                br(),
                br(),
                paste('There is a ',input$C,'% chance that the confidence interval $($',xbar-E, ', ',xbar+E,'$)$ contains the true population mean.'),
                br()
            )
        } else {
            phat <- input$phat
            s.phat <- sqrt(phat*(1-phat)/nn)
            E = round(CV*s.phat,3)
            withMathJax(
                br(),
                tags$b("Step 3: Margin of Error"),
                paste('$$E = CV\\times \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} =',CV,'\\times \\sqrt{\\frac{',phat,'( 1- ',phat,')}{',nn,'}} =', E,'$$'),
                tags$b("Step 4: Expression of Confidence Interval"),
                paste('$$(\\hat{p} - E, \\hat{p} + E) = (',phat,'-',E,', ',phat,'+',E,') = (',phat-E,',',phat+E,')$$'),
                tags$b("Step 5: Interpretation of Confidence Interval"),
                br(),
                br(),
                paste('There is a ',input$C,'% chance that the confidence interval $($',phat-E, ', ',phat+E,'$)$ contains the true population proportion.'),
                br()
            )
        } 
    })
#  }) 
}
# Run the application 
shinyApp(ui, server)
