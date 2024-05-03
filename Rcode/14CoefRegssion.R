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
        titlePanel( h2("ISLA: Correlation Coefficient and Inference of Regression", 
                      align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
      ),
  
      fluidRow(
        ###########################  Panel 1  ##################################
           column(3, wellPanel(style = "background-color: #5e2442; color:gold;", 
            h4(strong('User Input Data')),
            textInput("X", "Comma Separated Numerical Data (X)", 
                      value = "150, 159, 172, 153, 166, 161, 156, 150, 167", 
                      placeholder = "Enter values separated by a comma."), 
            textInput("Y", "Comma Separated Numerical Data (Y)", 
                      value = "49.44, 62.60, 75.75, 48.99, 53.09, 52.62, 47.97, 45.59, 57.85", 
                      placeholder = "Enter values separated by a comma."),
            hr(),
            h4(strong('Meaningful Variable Names')),
            textInput("Xname", "Explanatory Variable (X)", 
                      value = "Height(cm)", 
                      placeholder = "Enter character values separated by a comma."),
            textInput("Yname", "Response Variable (Y)", 
                      value = "Weight(kg)", 
                      placeholder = "Enter character values separated by a comma."),
           hr(),
           h4(strong('New X Value for Prediction')),
           textInput("newX", "New Value [in the range of X]", 
                     value = 163, 
                     placeholder = "Enter character values separated by a comma."),
           hr(),
           ###
           HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
           HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
           
            ), 
      ),
       ##########################  Panel 2   ###################################
       column(3, wellPanel( style = "background-color:lightgray;",
         withMathJax(),
           h4(strong('Validate Input Data:')),
           tags$head(tags$style("#mtxt{color: blue;} #btxt{color: red;}" )),
           tableOutput('datatable'),
           ###
           h4(strong('Descriptive Statistics of Data:')),
           tableOutput('summaryStats'),
           ###
           h4(strong('Sum of Squares:')),
           uiOutput('SSsq'),
          )), 
       #########################  Pane 3     ###################################
        column(3, wellPanel(style = "background-color:lightgray;",
            h4(strong('Least Square Regression:')),
              textOutput('YnameTxt', inline=T), ' = ', textOutput('mtxt', inline=T), textOutput('XnameTxt', inline=T), textOutput('btxt', inline=T),
            br(),br(),
            tags$head(tags$style("#mtxt{color: blue;} #btxt{color: red;}")),
            plotOutput("plot", height='auto'),
            br(),br(),
            ###
            h4(strong('Estimate Slope and Intercept:')),
            br(),
            uiOutput('coef'),
            ###
            h4(strong('Coefficient of Determination:')),
            br(),
            uiOutput('RSQD'),
           )), 
      
       ########################  Panel 4    ####################################
        column(3, wellPanel(style = "background-color:lightgray;",
          tags$h4(strong('Interpretaions and Inferences:')),
          tags$h5(strong('Correlation Coefficient (r)')),
          uiOutput('pearsonCorr'),
          br(), 
          ###
          
          tags$h5(strong('Interpretation of Coeeficient of Determination')),
          uiOutput('Rsq'),
          br(),
          tags$h5(strong('Interpretation of the slope')),
          textOutput('explainslope'),
          br(),
          ###
          tags$h5(strong('Hypothesis Test of the Slope ')),
          uiOutput('test4m'),
          br(),
          ###
          tags$h5(strong('Confidence Interval of the Slope')),
          uiOutput('ci4m'),
          br(),
          ###
          tags$h5(strong('Prediction')),
          uiOutput('pred'),
          br(),
        ))
    )
)

##########
##########
server <- function(input, output, session) {
  ######
  extract_num <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  #####
  extract_cat <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    split
  }
  
    output$plot <- renderPlot({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      newX = extract_num(input$newX)
      ############
      reg.coef = coef(lm(ypts~xpts))
      m = round(reg.coef[2],2)
      b = round(reg.coef[1],2)
      predY = b + m*newX
        ####
        par(cex.main=2, cex.lab=1.5, mar=c(4.1, 4.1, .5, .5), bg = "lightgray")
        plot(xpts, ypts, type='p', pch=16,  ylab=input$Yname, xlab=input$Xname,lwd=3, col = "red", cex = 1.5)
        points(newX, predY, pch=19, col = "purple", cex = 2)
        points(newX, predY, pch=19, col = "yellow", cex = 1)
        grid()
        abline(lm(ypts~xpts), col = "blue")

        }, height = 300, width = 300 
    )
    ############
    ############
    output$datatable <- renderTable({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
        dat = round(data.frame(x = xpts, y = ypts),2)
        #colnames(dat) = NULL
        names(dat)[1] = input$Xname
        names(dat)[2] = input$Yname
        dat
        })
    
    output$summaryStats <- renderTable({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      xx=c(mean(xpts), var(xpts), length(xpts))
      yy=c(mean(ypts), var(ypts), length(ypts))
      rID = c("mean", "variance", "size")
      dat = data.frame(id =rID, x = xx, y = yy)
      ##
      names(dat)[1] = "StatsType"
      names(dat)[2] = input$Xname
      names(dat)[3] = input$Yname
      ##
      dat
    })

     ##########  Regression coefficients
     
     output$SSsq = renderUI({
       xpts = extract_num(input$X)
       ypts = extract_num(input$Y)
       ssx = round(sum((xpts-mean(xpts))^2),4)
       ssy = round(sum((ypts-mean(ypts))^2),4)
       ssxy = round(sum((xpts-mean(xpts))*(ypts-mean(ypts))),4)
       slope =round(ssxy/ssx,2)
       #####
       withMathJax(
       paste0("$S_{xx} = \\sum_{i=1}^n(x_i-\\bar{x})^2 =$", ssx, "."),
       br(),
       paste0("$S_{yy} = \\sum_{i=1}^n(y_i-\\bar{y})^2 =$", ssy, "."),
       br(),
       paste0("$S_{yy} = \\sum_{i=1}^n(x_i-\\bar{x})(y_i-\\bar{y}) =$", ssxy, "."),
       br(),
       br()
       )
     })
     
     
     output$coef = renderUI({
       xpts = extract_num(input$X)
       ypts = extract_num(input$Y)
       ssx = round(sum((xpts-mean(xpts))^2),4)
       ssy = round(sum((ypts-mean(ypts))^2),4)
       ssxy = round(sum((xpts-mean(xpts))*(ypts-mean(ypts))),4)
       slope =round(ssxy/ssx,2)
       intercept = round(mean(ypts)-slope*mean(xpts),2)
       #####
       withMathJax(
       paste0("$m = S_{xy} / S_{xx} = $", ssxy, "/", ssx, " = ", slope, "."),
       br(),
       paste0("$b = \\bar{y} - m \\times \\bar{x}$ = ",round(mean(ypts),4), "-" , slope, "$\\times$", round(mean(xpts),4), " = ", intercept, "."),
       br(),
       br()
       )
     })
     
     
     output$RSQD = renderUI({
       xpts = extract_num(input$X)
       ypts = extract_num(input$Y)
       ssx = round(sum((xpts-mean(xpts))^2),4)
       ssy = round(sum((ypts-mean(ypts))^2),4)
       ssxy = round(sum((xpts-mean(xpts))*(ypts-mean(ypts))),4)
       slope =round(ssxy/ssx,2)
       intercept = round(mean(ypts)-slope*mean(xpts),2)
       mod = lm(ypts ~ xpts)
       rr=round(summary(mod)$r.squared,4)
       #####
       withMathJax(
         paste0("$R^2 = \\dfrac{\\text{Explained Variation}}{\\text{Total Variation}} = $", rr, "."),
         br(),
         br()
       )
     })
     
     
    ############
    ############
    output$mtxt <- renderText({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      reg.coef = coef(lm(ypts~xpts))
      m = round(reg.coef[2],2)
      m
      })
    output$btxt <- renderText({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      reg.coef = coef(lm(ypts~xpts))
      b = round(reg.coef[1],2)
        ifelse(b >= 0, paste('+', b), paste('-', abs(b)))
    })
    
    
    output$XnameTxt <- renderText({
      input$Xname
    })
    
    output$YnameTxt <- renderText({
      input$Yname
    })
    
    
    
    output$pearsonCorr <- renderUI({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      ssx = round(sum((xpts-mean(xpts))^2),4)
      ssy = round(sum((ypts-mean(ypts))^2),4)
      ssxy = round(sum((xpts-mean(xpts))*(ypts-mean(ypts))),4)
      ########
         pearson.r = round(cor(xpts, ypts),3)
      #####
        withMathJax(
         paste0("Pearson correlation coefficient is calculated by:"),
         br(),
         paste0("$r = \\dfrac{S_{xy}}{\\sqrt{S_{xx}}\\sqrt{S_{yy}}}  = \\dfrac{", ssxy, "}{\\sqrt{",ssx,"} \\sqrt{",ssy,"}}$ = ", pearson.r),
         br()
      )
      })
    
    output$test4m = renderUI({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      df = length(xpts) -2
      mod = lm(ypts ~ xpts)
      lm.stats = coef(summary(mod))
      ts.m = round(lm.stats[2,3] ,3)
      p.val = round(lm.stats[2,4],3)
      #####
      withMathJax(
      paste0("The null and alternative hypotheses are given by:"),
      br(),
      paste0("$H_0: m = 0$ \  vs  \  $H_a: m \\ne 0$."),
      br(),
      paste0("The test statistic TS = ", ts.m, " that yields a p-value = ", p.val, " based on the t-distribution with ",df, " degrees of freedom."),
      br()
      )
    })
    

    
    output$lse.reg = renderText({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      reg.coef = coef(lm(ypts~xpts))
      m = round(reg.coef[2],2)
      b = round(reg.coef[1],2)
      paste('y = ', m, 'x', ifelse(b >= 0, paste('+', b), paste('-', abs(b))))
    })
    
    
    output$explainslope = renderText({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      reg.coef = coef(lm(ypts~xpts))
      m = round(reg.coef[2],2)
      paste('when ', input$Xname, 'increases by one unit', 'the corresponding change of ', input$Yname, ' is ', m, '.')
    }) 
    
    
    output$ci4m = renderUI({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      mod = lm(ypts ~ xpts)
      lm.stats = coef(summary(mod))
      lcl = round(lm.stats[2,1] - lm.stats[2,2]*qt(0.975, 10),3)
      ucl = round(lm.stats[2,1] + lm.stats[2,2]*qt(0.975, 10),3)
      ###
      withMathJax(
       paste('95% confidence interval of the slope is given by:'),
       br(),
       paste0("[", lcl, ",  ", ucl, "]."),
       br()
      )
    })
    
    output$Rsq = renderUI({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      ############
      mod = lm(ypts ~ xpts)
      rr=round(summary(mod)$r.squared,4)
      withMathJax(
      paste('$R^2 =$ ', rr, ' = ', 100*rr, '%.  This implies that ', 100*rr, '% variation in the response is explained by the linear regression.'),
      br()
      )
    })
    
    output$pred = renderUI({
      xpts = extract_num(input$X)
      ypts = extract_num(input$Y)
      newX = extract_num(input$newX)
      ###
      reg.coef = coef(lm(ypts~xpts))
      m = round(reg.coef[2],2)
      b = round(reg.coef[1],2)
      predY = b + m*newX
      newY = input$Yname
      withMathJax(
        paste('$\\widehat{', input$Yname, '} =',b , ' + (', m,')\\times (', newX, ') = ', predY, '$.'),
        br()
      )
    })
}

shinyApp(ui, server)