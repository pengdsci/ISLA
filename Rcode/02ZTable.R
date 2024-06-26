#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(magrittr)
library(DT)
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
  wellPanel(style = "background-color:#5e2442;",
            titlePanel( h3("ISLA: Standard Normal Distribution Tables", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),    
    #############
    #############
    withMathJax(), 
    fluidRow(
        column(3, wellPanel( style = "background-color: #5e2442; color:gold; ", 
                             
            br(),
            h4('Input Values', style="color:skyblue; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold; text-align: center;"),
            br(),
            radioButtons('problem_type', label='1. What to Find?', 
                         choices=list('Probability' = 1, 
                                      'Percentile' = 2), 
                         selected=1), 
            
            hr(),
            ################
            conditionalPanel(condition = "input.problem_type=='1'", 
                             radioButtons('range_type', label='2. Left-tail Probability:',
                                          choices=list('$P[Z < z_0] = ?$'=3), 
                                          selected=3)), ##  '\\(P[Z > z_0] = ?\\)'=2, 
            ################
            conditionalPanel(condition = "input.problem_type=='2'", 
                             radioButtons('prob_type', label='2. Percentile Zo: ',
                                          choices=list('$Z_0 = ?$'=3), 
                                          selected=3)), 
            ################
            hr(),
            ################
            conditionalPanel(condition = "input.problem_type=='1'",
                             numericInput('z', label='3. Given Zo:', min=-4, 
                                         max=4, value=0.25, step=.001)), 
            ################
            conditionalPanel(condition = "input.problem_type=='2'",
                             numericInput('prob', label='3. Given Probability:', min=.0001, max=.9999, 
                                         value=.5764, step=.0091)), 
            ################
            
            #strong('Summary of Problem'), br(),
            #uiOutput('summary_prob'),
        hr(),
        ###
        HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
        
        )
        ),
        
        column(9, wellPanel(  style = "background-color:lightgray; font-size:14px;", 
            wellPanel(style = "background-color:lightgray; text-align:center;",                  
                plotOutput('plot', height='180px'),
                tableOutput('table')
                #DTOutput('table')
            ############
            ),
        ), height = "300px")

    )
)

#################################################################################
# Define server logic required to draw a histogram
server <- function(input, output){
    #######################################################
    qseq = function(qval){
        ######################### positive z-scores
        u = seq(0,3.09,by=0.01)
        p0 = pnorm(u)
        options(digits=4)
        m = round(matrix(p0,ncol=10,byrow=TRUE),4)
        ########################  Negative z-scores
        u1 = seq(-3.09,0,by=0.01)
        p1 = pnorm(u1)
        options(digits=4)
        m1 = round(matrix(p1,ncol=10,byrow=TRUE),4)
        m0=t(apply(m1, 1, rev))
        ## normal table
        ztable = rbind(m0, m)
        ########################  column and row names
        cname = sprintf("%.2f",round(seq(0.0,0.09, by=0.01),2))
        cname = as.character(cname)
        rname = sprintf("%.1f",c(round(seq(-3,0, by=0.1),1),round(seq(0,3, by=0.1),1)))
        rname = as.character(rname)
        rname[31] = "-0.0"
        ###
        row.names(ztable) = rname
        colnames(ztable) = cname
        ######################
        q.seq = round(qval, 1) + seq(-0.4, 0.5, 0.1)
        upper = sum(q.seq > 3)
        lower = sum(q.seq < -3)
        if (upper > 10){
            qseq0 = 2.5 + seq(-0.5, 0.5, 0.1)
        } else if (lower > 10){
            qseq0 = -2.5 + seq(-0.5, 0.5, 0.1)
        } else if (upper > 0 && upper <= 10){
            qseq0 = q.seq - 0.1*upper
        } else {
            qseq0 = q.seq + 0.1*lower
        }
        rowID = which(round(seq(-3, 3, 0.1),1) %in% round(qseq0,1))
        ztable[rowID,]
    } 
    ########################################################
    output$prob.text <- renderText({
        if(input$problem_type==1){
            if(input$range_type==3) #less than z
                prob <- pnorm(input$z)
            return(ifelse(prob >= .00005, sprintf("%.4f", prob), '< 0.0001'))
        }
    })
    ###############
    output$z.txt <- renderText({
        if(input$problem_type==2 & input$prob_type==3) #less than z
            return(round(qnorm(input$prob), 2))
    })
    ###############
    output$plot <- renderPlot({
        x <- seq(-4, 4, by=.01)
        #par(bg = "#f7f7f7")
        par(mar=c(2.1, .1, .1, .1), cex.main=1.7, cex.lab=1.5, cex.axis=1.3, bg = "lightgray")
        plot(x, dnorm(x), type='l', ylab='', yaxt='n', xlab='', 
             xlim=c(-8, 12),
             main="", xaxt='n', bty = "n")
        axis(side=1, at=-4:4, labels=F, pos = 0)
        abline(h=0)
        segments(c(-9, 5), c(0,0), c(-5, 14), c(0,0), col="lightgray", lwd = 4)
        
        if(input$problem_type==1 & input$range_type==3){ #less than z
            axis(side=1, at=input$z)
            segments(x0=input$z, y0=0, y1=dnorm(input$z), col=4)
            zseq <- seq(from=-4, to=input$z, by=.01)
            polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
                    col='steelblue')
            text(8, 0.75*dnorm(0.5), paste("Since given Zo = ",input$z), col = "blue", cex = 1.2)
            ####
            text(8,0.5*dnorm(0.5), paste("Answer: P(Z < ",input$z, ") = ", 
                                   round(pnorm(input$z),4)), col = "red", cex = 1.2)
        }
        if(input$problem_type==2 & input$prob_type==3){ #less than z
            
            z0 <- round(qnorm(input$prob), 3)
            z = (floor(100*z0) + ceiling(z0*100))/200
            axis(side=1, at=z, label=paste('z =', z), col = "red")
            segments(x0=z, y0=0, y1=dnorm(z), col=4)
            zseq <- seq(from=-4, to=z, by=.01)
            polygon(c(zseq, rev(zseq)), y=c(dnorm(zseq), rep(0, length(zseq))),
                    col='steelblue')
            text(8, 0.75*dnorm(0.5), paste("Since P( Z < Zo) = ",input$prob), col = "blue", cex = 1.2)
            text(8,0.5*dnorm(0.5), paste("Answer: Zo =[(", 0.01*floor(100*z0), ") + (", 0.01*ceiling(100*z0), ")]/2 = ", z), col = "red", cex = 1.2)
        }
    }, height = 130, width = 600 )
    ##################################################################
    ##   Adding standard normal table related to the input value 
    ##################################################################
    output$table <- renderTable({
    # output$table <-  DT::renderDataTable({
    ####################
    if(input$problem_type==1){ 
        z00 <- input$z
     }        
    #########
    if(input$problem_type==2){ 
       z00 <- round(qnorm(input$prob), 4)
    }      
      qseq(z00)
     # datatable(qseq(z00),
     #           options = list(#paging = TRUE,    ## paginate the output
     #                          #pageLength = 10,  ## number of rows to output for each page
     #                          #scrollX = TRUE,   ## enable scrolling on X axis
     #                          #scrollY = TRUE,   ## enable scrolling on Y axis
     #                          #autoWidth = TRUE, ## use smart column width handling
     #                          #server = FALSE,   ## use client-side processing
     #                          #dom = 'Bfrtip',
     #                          #uttons = c('csv', 'excel'),
     #                          #columnDefs = list(list(targets = '_all', className = 'dt-center'),
     #                          #                  list(targets = c(0, 8, 9), visible = FALSE))
     #           ),
     #           #extensions = 'Buttons',
     #           #selection = 'single',  ## enable selection of a single row
     #           #filter = 'bottom',     ## include column filters at the bottom
     #           #rownames = TRUE       ## don't show row numbers/names
     #           ) %>% formatStyle(
     #             columns = 1,
     #             valueColumns = 0,
     #             target = 'cell',
     #             backgroundColor = styleEqual(3, 'red')
     # )
    #############    
    }, digits = 4, rownames = TRUE)
    #},  rownames = TRUE)
    
}
#######################################
# Run the application 
shinyApp(ui, server)
