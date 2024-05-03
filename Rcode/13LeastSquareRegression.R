#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(graphics)
###
###
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
            titlePanel( h3("ISLA: Understanding Least Square Regression", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),
  #################

      fluidRow(
        ###########################  Panel 1  ##################################
           column(3, wellPanel(style = "background-color: #5e2442; color:gold; ",
            
            h4(strong('Select Slope and Intercept:')),
            br(),
            sliderInput(inputId="m", label='Slope (m)', value=1.3, min=-3, max=3,
                        step=.1, animate=list(interval=550, loop=T)), 
            br(),
            sliderInput(inputId="b", label='Y-Intercept (b)', value=0.7, min=-2, max=1, 
                        step=.1, animate=list(interval=550, loop=T)),
            
           ###
           HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
           HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
           )
           ), 
        
       ##########################  Panel 2   ###################################
       column(2, wellPanel(
           h4(strong('Data Set:')),
           br(),
           tags$head(tags$style("#mtxt{color: blue;} #btxt{color: red;}" )),
           tableOutput('datatable')
          )), 
       #########################  Pane 3     ###################################
        column(4, wellPanel(
            h4(strong('Least Square Regression:'), 'y = ', textOutput('mtxt', inline=T), 'x', textOutput('btxt', inline=T)),
            br(),
            tags$head(tags$style("#mtxt{color: blue;} #btxt{color: red;}")),
            plotOutput("plot", height='auto'),
           )), 
       ########################  Panel 4    ####################################
        column(3, wellPanel(
          tags$h4(strong('Correlation Coefficient (r):')),
          textOutput('pearsontxt'),
          br(),
          ###
          tags$h4(strong('Least Sqaure Regression:')),
          textOutput('lse.reg'),
          br(),
          ###
          tags$h4(strong('Hypothesis Test Ho: m = 0 ')),
          textOutput('signif4m'),
          br(),
          ###
          tags$h4(strong('C.I. of Slope (m):')),
          textOutput('ci4m'),
          br(),
          
          tags$h4(strong('Coeeficient of Determination:')),
          textOutput('Rsq'),
         #   br()
            #tags$p(strong('Black line:'), 'The Plot of the line \\(y = mx+b\\)'), 
            #tags$p(strong(HTML('<span style="color:blue;">Blue annotations:</span>')),
            #       'The', strong('slope (m)'), 
            #       'is the change in \\(y\\) value for a 1-unit increase in \\(x\\) value.'), 
            # p(strong(HTML('<span style="color:red;">Red annotations:</span>')), 
            #  'Show that the value of the', 
            #  strong('y-intercept \\((b)\\)'), 
            #  'is the \\(y\\) value at which the line cross the \\(y\\)-axis (where \\(x=0\\)).'),
            #uiOutput("reg"),
        ))
    )
)
##########
##########

##########
##########
server <- function(input, output, session) {
    xpts = round(runif(12, min = -3, max = 3),1)
    noice = rnorm(12)
    ############
    x <- c(-3,0,3)
    #xpts = runif(15, min = -3, max = 3)
    #noice =  rnorm(15, 0, 0.5)
    output$plot <- renderPlot({
        ypts = input$m*xpts+input$b + noice
        ####
        par(cex.main=2, cex.lab=1.5, mar=c(4.1, 4.1, .5, .5))
        plot(x, input$m*x+input$b, type='l', ylim=c(-3, 3), 
             ylab="", xlab="",lwd=3, bty="n", axes = F)
        points(xpts, ypts, pch=16, col = "purple")
        axis.lab = as.character(seq(-3, 3, by = 0.5))
        axis.lab[7] = " "
        axis(1, at = axis.lab, pos=0)
        axis(2, at = axis.lab, pos=0, las = 2)
        grid()
        abline(v=0); abline(h=0)
        points(0, input$b, col=2, pch=16, cex=2)
        ###
        arrows(0, input$b, 1, input$b, col=4, lty=1, lwd=2, length=.1)
        arrows(1, input$b, 1, input$b+input$m, col=4, lty=1, lwd=2, length=.1)
        ###
        if(input$m < 0) text(0.5, input$b+.3, '1')
        if(input$m > 0) text(0.5, input$b-.3, '1')
        if(input$m != 0) text(1.5, input$b + input$m/2, input$m, col=4, cex=1.5)
        ###
        #arrows(0, input$b, -3.1, input$b, col=2, length=.1, lwd=2)
        #text(-2.8, input$b+.3, input$b, col=2, cex=1.5)
    }, height = function() {
        session$clientData$output_plot_width
    })
    ############
    ############
    output$datatable <- renderTable({
        ypts = input$m * xpts + input$b + noice
        dat = round(data.frame(x = xpts, y = ypts),1)
        #colnames(dat) = NULL
        dat
        })
    ############
    ############
    output$mtxt <- renderText({input$m})
    output$btxt <- renderText({
        ifelse(input$b >= 0, paste('+', input$b), paste('-', abs(input$b)))
    })
    
    output$pearsontxt <- renderText({
         ypts = input$m * xpts + input$b + noice
         pearson.r = round(cor(xpts, ypts),3)
         paste0("Pearson correlation coefficient: r = ", pearson.r)
      })
    
    output$signif4m = renderText({
      ypts = input$m * xpts + input$b + noice
      mod = lm(ypts ~ xpts)
      lm.stats = coef(summary(mod))
      ts.m = round(lm.stats[2,3] ,3)
      p.val = round(lm.stats[2,4],3)
      paste('The test statistic TS =', ts.m, 'that yields a p-value =', p.val, 'based on the t-distribution with 10 degrees of freedom.')
    })
    
    output$lse.reg = renderText({
      paste('y = ', input$m, 'x', ifelse(input$b >= 0, paste('+', input$b), paste('-', abs(input$b))))
    })
    
    output$ci4m = renderText({
      ypts = input$m * xpts + input$b + noice
      mod = lm(ypts ~ xpts)
      lm.stats = coef(summary(mod))
      lcl = round(lm.stats[2,1] - lm.stats[2,2]*qt(0.975, 10),3)
      ucl = round(lm.stats[2,1] + lm.stats[2,2]*qt(0.975, 10),3)
      paste('95% confidence interval of the slope: (', lcl, ',  ', ucl, ').')
    })
    
    output$Rsq = renderText({
      ypts = input$m * xpts + input$b + noice
      mod = lm(ypts ~ xpts)
      rr=round(summary(mod)$r.squared,4)
      paste('R.sq = ', rr, ', this implies that ', 100*rr, '% variation in response is explained by the linear regression.')
    })
    
}

shinyApp(ui, server)