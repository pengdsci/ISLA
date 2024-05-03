#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#
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
              titlePanel( h3("ISLA: t-Distribution Table", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
       ),  
  
    fluidRow(
         ######################
        column(3, wellPanel( style = "background-color: #5e2442; color:gold; ", 
            tags$div('This app creates a t-table that is commonly used in introductory statistics textbooks. 
                 It is primarily used for finding critical values.', 
                     style='font-size: small; color: skyblue'),
            br(),     
            
            radioButtons('prob_type', label='1. What Tail Probability Is Given?', 
                         choices=list('Right' = 1, 
                                      'Left' = 2), 
                         selected=1), 
            
            hr(),
            ################
            conditionalPanel(
                             condition = "input.prob_type=='1'",
                             numericInput('Rprob', label='2. Given Right Tail Probability', min=0, 
                                         max=1, value=0.025, step=.00001)), 
            ################
            conditionalPanel(
                             condition = "input.prob_type=='2'",
                             numericInput('Lprob', label='2. Given Left Tail Probability', min=0, max=1, 
                                         value=.025, step=.00001)), 
            ################
            hr(),
            numericInput("df", label = '3. Degrees of Freedom $(n-1)$',
                         min = 1, max = 100, value = 15),
            #strong('Summary of Problem'), br(),
            uiOutput('summary_prob'),
        #),
        
        ###
        HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
        )
        ),
        
        column(9, wellPanel(
            #strong('Picture of problem'),
            plotOutput('plot', height='180px'),
            strong('The numbers in the top row are tail probabilties.'),
            hr(),
            tableOutput('table'),
            textOutput('info')
            ############
        ))
   )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #######################################################
    qseq = function(df){
        df0 = 1:100
        right.prob = c(0.40,0.25, 0.10, 0.05, 0.025, 0.01, 0.005, 0.0025, 0.001, 0.0005)
        options(digits=4)
        t.matrix = matrix(0, ncol=length(right.prob), nrow=100)
        for (i in 1:length(right.prob)) t.matrix[, i] = qt(right.prob[i], df0, lower.tail = FALSE)
        t.matrix = as.data.frame(t.matrix)
        rownames(t.matrix)=as.character(sprintf("%.0f",round(df0,0)))
        colnames(t.matrix) = as.character(sprintf("%.4f",round(right.prob,4)))   
        t.matrix$d.f = df0
        t.matrix = as.data.frame(t.matrix[, c(11,1:10)])
        ## normal table
        ttable = t.matrix
        cname = names(t.matrix)
        ###
        colnames(ttable) = cname
        ######################
        if (df > 95){
            qseq0 = 90:100
        } else if (df < 5){
            qseq0 = 1:10
        } else {
            qseq0 = (df-5):(df+5)
        }
        ttable[qseq0,]
    } 
    
    
    ###############
    output$plot <- renderPlot({
        left = qt(0.001, input$df)
        right = qt(0.999, input$df)
        x <- seq(left, right, by = 0.1)
        par(mar=c(2.1, .1, .1, .1), cex.main=1.7, cex.lab=1.5, cex.axis=1.3, bg = "#f7f7f7")
        ####
        plot(x, dt(x, input$df), type='l', ylab='', yaxt='n', xlab='', col = "navy",
             xlim=c(3*left, 3*right),
             ylim = c(-0.1*dt(0, input$df), 1.1*dt(0, input$df)),
             main="", xaxt='n', bty = "n", lwd = 2)
        ###
        abline(h=0, lwd = 2, col = "navy")
        segments(c(3.5*left, 1.5*right), c(0,0), c(1.5*left, 3.5*right), c(0,0), col="#f7f7f7", lwd = 4)
        
        if(input$prob_type==1){ #less than z
            p0 = input$Rprob
            cv.qt = qt(1-p0, input$df)
            tseq <- seq(from = cv.qt, to=1.5*right, length=100)
            polygon(c(tseq[1], tseq, tseq[100]), y=c(0,dt(tseq, input$df), 0),
                    col='skyblue')
            text(cv.qt, -0.1*dt(0.5, input$df), paste("CV = ",round(cv.qt,4)), col = "blue", cex = 1)
            text(1.85*right, 0, paste("d.f =", input$df), col = "red", cex = 1.2)
            ####
            arrows(1.1*right, 0.5*dt(0.5, input$df), qt(1-0.5*p0, input$df), 0.3*dt(cv.qt, input$df), 
                   col = "navy", length = 0.1, angle = 15   )
            text(1.2*right, 0.6*dt(0.5, input$df), paste('Given Tail Probability =', input$Rprob), col = "red", cex = 0.9)
        }
        if(input$prob_type==2){ #less than z
            p0 = input$Lprob
            cv.qt = qt(p0, input$df)
            tseq <- seq(from = 1.5*left, to=cv.qt, length=100)
            polygon(c(tseq[1], tseq, tseq[100]), y=c(0,dt(tseq, input$df), 0),  col='skyblue')
            ###############
            text(cv.qt, -0.1*dt(0.5, input$df), paste("CV = ",round(cv.qt,4)), col = "blue", cex = 1)
            text(1.85*right, 0, paste("d.f =", input$df), col = "red", cex = 1.2)
            ####
            arrows(1.1*left, 0.5*dt(0.5, input$df), qt(0.5*p0, input$df), 0.3*dt(cv.qt, input$df), 
                   col = "navy", length = 0.1, angle = 15   )
            text(1.2*left, 0.6*dt(0.5, input$df), paste('Given Tail Probability =', input$Rprob), col = "red", cex = 0.9)
        }
    }, height = 150, width = 600 )
    

    ##################################################################
    ##   Adding standard normal table related to the input value 
    ##################################################################
    output$table <- renderTable({
    ####################
      qseq(input$df)
    #############    
    }, digits = 4, rownames = FALSE)
}

# Run the application 
shinyApp(ui, server)
