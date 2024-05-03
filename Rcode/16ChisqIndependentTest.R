#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
# Define UI for application that draws a histogram
ui <- fluidPage(
    #includeCSS('styles.css'),
  #includeCSS('styles.css'), 
  withMathJax(),
  ### Important: The following tag allows inline equation in MathJax
  #tags$div(HTML("<script type='text/x-mathjax-config' >
  #          MathJax.Hub.Config({
  #          tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
  #          });
  #          </script >
  #          ")),
  br(),
  ### Important: The following tag allows inline equation in MathJax
  wellPanel(style = "background-color:#5e2442;",
            titlePanel( h3("ISLA: Chi-squared Independent Test", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),
  #################
         fluidRow(
                      column(4, wellPanel(style = "background-color: #5e2442; color:steelblue; ",
                      tags$div(id="myScrollBox", 
                               style = "background-color: #5e2442;",
                               radioButtons('nrow', label='How many rows?', inline=T, choices=list(2,3,4,5,6,7,8,9, 10, 11, 12)),
                               radioButtons('ncol', label='How many columns?', inline=T, choices=list(2,3,4,5)), 
                               strong('Enter Counts Here'),
                               br(),
                               br(),
                               rHandsontableOutput('table')),

                      br(),
                      br(),
                      HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
                      HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
                      )
                      ),
                      
         
                      
                      column(4,  
                             wellPanel( tags$div(id="myScrollBox",
                             br(),
                             strong('HYPOTHESES'),
                             br(),
                             br(),
                             strong("Ho:"),
                             paste0('Row and Column are independent.'),
                             br(),
                             strong('Ha:'), 
                             paste0('Row and Column are dependent.'),
                             br(),
                             br(),
                             strong('OBSERVED & EXPECTED COUNTS'),
                             br(),
                             br(),
                             uiOutput('twoway')))
                            ),
                      
                      column(4,  
                             wellPanel( br(),
                                        strong('TEST STATISTIC'), 
                                        br(),
                                        br(),
                                        '\\(\\chi^2 = \\)', textOutput('x2', inline=T), 
                                        br(), br(),
                                        strong('CALCULATION'), 
                                        br(),
                                        br(),
                                        tags$table(tags$tr(
                                        tags$td('\\(\\chi^2 = \\sum \\frac{(O - E)^2}{E} = \\)', style='border-style: none'), 
                                        tags$td(uiOutput('test_stat_calc', inline=T), style='border-style: none')
                                        ), style='border-style: none'),
                                        ###################################################################
                                        ###################################################################
                                        br(),
                                        strong('NULL DISTRIUBTION OF TEST STATISTIC'), 
                                        br(),
                                        br(),
                                        '\\(\\chi^2\\) distribution with df = ', textOutput('df', inline=T), 
                                        br(), br(),
                                        strong('P-VALUE'), 
                                        br(), 
                                        br(),
                                        textOutput('pval', inline=T)
                                  )
                            ),
                      )
                  )
# Define server logic required to draw a histogram
server <- function(input, output, session){
    ###
    init <- 10
    ###
    rv <- reactiveValues(
        cur.mat=matrix(as.integer(init), nrow=2, ncol=2, dimnames=list(1:2, 1:2)), 
        last.mat=matrix(as.integer(init), nrow=2, ncol=2, dimnames=list(1:2, 1:2)),
        cur.nrow=2, last.nrow=2, cur.ncol=2, last.ncol=2, 
        exp=matrix(init, nrow=2, ncol=2)
    )
    
    ####
    output$table <- renderRHandsontable({
        rhandsontable(rv$cur.mat)
    })
    ###
    observeEvent(input$nrow, {
        req(input$table)
        ncol <- as.numeric(input$ncol)
        rv$last.nrow <- rv$cur.nrow
        rv$cur.nrow <- as.numeric(input$nrow)
        rv$last.mat <- as.matrix(hot_to_r(input$table))
        if(rv$cur.nrow < rv$last.nrow){
            rv$cur.mat <- rv$last.mat[1:rv$cur.nrow,]
            rownames(rv$cur.mat) <- 1:rv$cur.nrow
        }
        if(rv$cur.nrow > rv$last.nrow){
            add0 <- matrix(init, nrow=rv$cur.nrow - rv$last.nrow, ncol=ncol)
            rv$cur.mat <- rbind(rv$last.mat, add0)
            rownames(rv$cur.mat) <- 1:rv$cur.nrow
        }
        rv$cur.mat <- apply (rv$cur.mat, c (1, 2), function (x) {(as.integer(x))})
    })
    ####
    observeEvent(input$ncol, {
        req(input$table)
        nrow <- as.numeric(input$nrow)
        rv$last.ncol <- rv$cur.ncol
        rv$cur.ncol <- as.numeric(input$ncol)
        rv$last.mat <- as.matrix(hot_to_r(input$table))
        if(rv$cur.ncol < rv$last.ncol){
            rv$cur.mat <- rv$last.mat[,1:rv$cur.ncol]
            colnames(rv$cur.mat) <- 1:rv$cur.ncol
        }
        if(rv$cur.ncol > rv$last.ncol){
            add0 <- matrix(init, nrow=nrow, ncol=rv$cur.ncol - rv$last.ncol)
            rv$cur.mat <- cbind(rv$last.mat, add0)
            colnames(rv$cur.mat) <- 1:rv$cur.ncol
        }
        rownames(rv$cur.mat) <- 1:nrow
        rv$cur.mat <- apply (rv$cur.mat, c (1, 2), function (x) {(as.integer(x))})
    })
    ####
    observeEvent({input$table; rv$cur.mat}, {
        req(input$table)
        obs.cts <- rv$exp <- as.matrix(hot_to_r(input$table))
        obs.cts <- apply (obs.cts, c (1, 2), function (x) {(as.double(x))})
        rowtot <- apply(obs.cts, 1, sum)
        coltot <- apply(obs.cts, 2, sum)
        for(i in 1:nrow(obs.cts))
            for(j in 1:ncol(obs.cts))
                rv$exp[i,j] <- rowtot[i]*coltot[j]/sum(coltot)
    })
    ###
    output$twoway <- renderUI({
        req(input$table)
        req(rv$exp)
        obs <- as.matrix(hot_to_r(input$table))
        nrow <- nrow(obs); ncol <- ncol(obs)
        rowtot <- apply(obs, 1, sum)
        coltot <- apply(obs, 2, sum)
        colheads <- tagList(tags$td('Observed', br(), 
                                    HTML('<font color="red">Expected</font>')))
        for(j in 1:ncol) colheads[[j+1]] <- tags$th(paste0('Col ',j))
        colheads[[ncol+2]] <- tags$th('Total')
        bodytable <- tagList()
        for(i in 1:nrow){
            onerow <- tagList(tags$th(paste0('Row ',i)))
            for(j in 1:ncol) 
                onerow[[j+1]] <- tags$td(obs[i,j], br(), 
                                         HTML(paste0('<font color="red">',
                                                     round(rv$exp[i,j], 1),' </font>')), 
                                         style='font-size: x-small;')
            onerow[[ncol+2]] <- tags$th(rowtot[i])
            bodytable[[i]] <- tags$tr(onerow)
        }
        lastrow <- tagList(tags$th('Total'))
        for(j in 1:ncol) lastrow[[j+1]] <- tags$th(coltot[j])
        lastrow[[ncol+2]] <- tags$th(sum(coltot))
        tags$table(
            tags$tr(colheads),
            bodytable,
            tags$tr(lastrow)
        )
    })
    
    ####   conditions panel
    ####   Test statistic tab

    output$x2 <- renderText({
        req(input$table)
        obs <- as.matrix(hot_to_r(input$table))
        x2 <- sum((obs - rv$exp)^2 / rv$exp)
        round(x2, 2)
    })
    
    output$test_stat_calc <- renderUI({
        req(input$table)
        obs <- as.matrix(hot_to_r(input$table))
        exp <- round(rv$exp, 1)
        nrow <- nrow(obs); ncol <- ncol(obs)
        calc <- '\\('
        for(i in 1:nrow){
            for(j in 1:ncol){
                txt <- paste0('\\frac{(', obs[i,j], '-', exp[i,j], ')^2}{', exp[i,j], '}')
                if(!(j==ncol & i==nrow)) 
                    calc <- paste0(calc, txt, '+')
                else
                    calc <- paste0(calc, txt)
                if(j==ncol & i!=nrow)
                    calc <- paste0(calc, '\\)<br>\\(')
                
            }
        }
        calc <- paste0(calc, '\\)')
        withMathJax(HTML(calc))
    })
    
    output$df <- renderText({
        res()$df
    })
    output$pval <- renderText({
        ifelse(res()$pval < .0001, 'p-value < 0.0001', paste("p-value =",sprintf("%.4f", res()$pval)))
    })

    
    res <- reactive({
        req(input$table)
        obs <- as.matrix(hot_to_r(input$table))
        x2 <- sum((obs - rv$exp)^2 / rv$exp)
        df <- (as.numeric(input$nrow)-1) * (as.numeric(input$ncol)-1)
        pval <- pchisq(x2, df=df, lower.tail=F)
        list(x2=x2, df=df, pval=pval)
    })
    output$x2 <- renderText({
        round(res()$x2, 2)
    })


    
    observe({
        updateSliderInput(session, 'stat', value=input$stat, min=0, max=input$max_stat, step=.1)
    })
}

# Run the application 
shinyApp(ui, server)
