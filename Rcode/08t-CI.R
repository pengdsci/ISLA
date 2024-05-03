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
  wellPanel(style = "background-color:#5e2442; ",
            titlePanel( h3("ISLA: t Confidence Intervals for Population Mean", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),  
    
    fluidRow(
        ######################
        column(4, wellPanel(style = "background-color: #5e2442; color:gold; ", 

            br(),
            strong('1. Sample Statistics', style="color:gold;"),
            br(),
            br(),
            ###########
           
                 numericInput('xbar', label='Sample Mean: $\\bar{X}$', 
                        min=10, max=90, value=47, step=.1), 
                 numericInput('s', label='Standard Deviation: $s$ or $\\sigma_0$', min=0, max=NA, value=9), 
           
            #####
            # conditionalPanel(condition="input.CItype == '2'",
            #       numericInput('phat', label='Sample Proportion [ in decimal form ]: $\\hat{p}$', 
            #                              min=0, max=1, value=0.6, step=1), 
            #),

            strong('2. Sample Size and Confidence Level', style="color:gold"),
            br(),
            br(),
            numericInput('n', label='Sample Size: $n$', 
                         min=2, max=100, value=10, step=1),
            sliderInput('C', label='Confidence Level: $1-\\alpha$', min=80, max=99, value=95, step=1, post='%'),
            #########
            
            HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
            HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
            
        )),

        ######################
        ######################
        column(8, wellPanel( style = "background-color:lightgray", 
            strong('Solution:', style="color:darkred; font-weight:bold;"), 
            paste("The desired confidence interval is constructed in the following steps."),
            hr(),
            strong("Step 1. The given confidence level."),
            br(),
            'conf.level $= 1 -\\alpha$ = 1 -', textOutput('alpha', inline=T),' = ', textOutput('confLevel', inline=T),'.',
            br(),
            br(),
            strong("Step 2. CV on the t density curve."),
            br(),
            '$CV = t(\\alpha/2, df) = t($',textOutput('halfalpha', inline=T),',', textOutput('df', inline=T),'$)$ = ', textOutput('tstar', inline=T), '.',
            br(),
            wellPanel(style = "background-color:lightgray; text-align:center;",
                plotOutput('stdevPlot', height="125px"),
              ),
            uiOutput("Steps"),
#            textOutput('ciSteps'),
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##########
    withMathJax()   
    ############
    ### Any input information to be displayed in UI must be
    ### processed using reactive function and then return
    ### as an output object in an appropriate format.
    results <- reactive({ 
        df=input$n -1
        tstar <- qt((1-input$C/100)/2, df, lower.tail=F)
        confLevel = input$C/100
        halfalpha = (1-confLevel)/2
        alpha = 2*halfalpha 
        list(tstar=tstar, conf = confLevel, df = df, halfalpha = halfalpha, alpha=alpha)
    })
    #########
    output$tstar <- renderText({
        sprintf('%.3f', results()$tstar)
    })
    #########
    output$confLevel <- renderText({
        paste0(input$C, '%')
   })
    #########
    output$df <- renderText({results()$df
      #sprintf('%.3f', results()$df)
    })
    #########
    output$alpha <- renderText({
      sprintf('%.2f', results()$alpha)
    })
    
    #########
    output$halfalpha <- renderText({
      sprintf('%.2f', results()$halfalpha)
    })
    
    #########################################
    #########################################
    output$stdevPlot <- renderPlot({
        par(mar=c(2,.1,.1,.1), bg = "#f7f7f7")
        df = results()$df
        z <- seq(from=-4, to=4, by=.01)
        ###########
        par(bg="lightgray")
        plot(z, dt(z, df), type='l', xaxt='n', yaxt='n', lwd=2, 
             ylim=c(-0.1*dt(0, df), dt(0,df)), bty = 'n', axes = F, col = "navy")
        abline(h=0, lwd = 2, col = "navy")
        ############
        tstar3 <- round(results()$tstar, 3)
        tstar2 <- floor(100*tstar3)/100  # round down to keep 2 decimal places for creating a sequence
        segments(x0=tstar3*c(-1,1), y0=0, y1=dt(tstar3,df))
        polygon(x=c(-tstar3, -tstar3, seq(from=-tstar2, to=tstar2, by=.01), tstar3, tstar3), 
                y=c(0, dt(-tstar3,df), dt(seq(from=-tstar2, to=tstar2, by=.01),df),dt(tstar3, df), 0), 
                col='skyblue')
        ###### label confidence level
        text(0, .5*dt(0,df), paste0(input$C,'%'), col = "blue")
        ######  label CV
        text(tstar3, -0.095*dt(0,df),  bquote(paste('CV'[R],'=',.(tstar3))), col = "red", cex = 0.9)
        text(-tstar3, -0.095*dt(0,df), bquote(paste('CV'[L],'= ',.(-tstar3))), col = "red", cex = 0.9)
    }, height = 150, width = 250 )
    
    ###############################
    #####################################
    output$Steps <- renderUI({  
    ##########
    nn <- input$n
    df=nn - 1
    conf <- input$C/100
    CV <- round(qt((1-conf)/2, df, lower.tail=F),3)
    ##########
    #output$ciSteps <- renderText({
       # withMathJax()
            xbar <- input$xbar
            s <- input$s
            E <- round(CV*s/sqrt(nn), 3)
            withMathJax(
                tags$b("Step 3: Margin of Error"),
                paste('$$E = CV\\times \\frac{s}{\\sqrt{n}} =',CV,'\\times \\frac{',s,'}{\\sqrt{',nn,'}} =', E,'.$$'),
                tags$b("Step 4: Expression of Confidence Interval"),
                paste('$$(\\bar{X} - E, \\bar{X} + E) = (',xbar,'-',E,', ',xbar,'+',E,') = (',xbar-E,',',xbar+E,').$$'),
                tags$b("Step 5: Interpretation of Confidence Interval"),
                br(),
                br(),
                paste('There is a ',input$C,'% chance that the confidence interval $($',xbar-E, ', ',xbar+E,'$)$ contains the true population mean.', sep=""),
                br()
            )
    })
}
# Run the application 
shinyApp(ui, server)
