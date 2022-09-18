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
    withMathJax(),
    ### Important: The following tag allows inline equation in MathJax

      wellPanel( h3(strong('IntroStatsApps:  Chi-squared \\((\\chi^2)\\) Independent Test',align = "center", style = "color:navy", br()))),
               fluidRow(
                      column(4, wellPanel(
                      tags$div(id="myScrollBox",
                      radioButtons('nrow', label='How many rows?', inline=T, choices=list(2,3,4,5,6,7,8,9, 10, 11, 12)),
                      radioButtons('ncol', label='How many columns?', inline=T, choices=list(2,3,4,5)), 
                      strong('Enter Counts Here'),
                      rHandsontableOutput('table'))),
                      #wellPanel( tags$div(id="myScrollBox",
                      #strong('Observed and Expected Counts'), 
                      #uiOutput('twoway')))
                      HTML('<p><center> <img src="https://github.com/pengdsci/sta553/blob/main/image/goldenRamLogo.png?raw=true"  width="100" height="100"></center></p>'),
                      HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center><font size =2> <a href="mailto:cpeng@wcupa.edu">Report bugs to C. Peng</a> </font></center></p>')
                      
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
    
    #output$pval_tab <- renderUI({fluidPage(
     #   br(),
     #   withMathJax(),
     #   h4(strong('P-value for Chi-squared Test')),
     #   br(),
     #   conditionalPanel('output.cond_tf == false', 
     #                    'It\'s not appropriate to calculate the p-value because the conditions are 
     #                not satisfied.', strong('(See conditions tab)')
     #   ),
    #    conditionalPanel('output.cond_tf == true', 
    #                     strong('Null distribution'), br(),
    #                     '\\(\\chi^2\\) distribution with df = ', textOutput('df', inline=T), 
    #                     hr(), 
     #                    strong('P-value'), br(), 
    #                     textOutput('pval', inline=T)
    #    )
    #)})
    output$df <- renderText({
        res()$df
    })
    output$pval <- renderText({
        ifelse(res()$pval < .0001, 'p-value < 0.0001', paste("p-value =",sprintf("%.4f", res()$pval)))
    })
    #output$graph_tab <- renderUI({fluidPage(
    #    br(),
    #    radioButtons('which_rect', label='Show rectangles representing which counts?', 
    #                 choiceNames=list('Observed Counts (black)', 
    #                                  HTML('Expected Counts <span style=color:red>(red)</span>'), 
    #                                  'Both Observed and Expected Counts'), 
    #                 choiceValues=list('obs', 'exp', 'both')
     #   ),
     #   plotOutput('mosaic')
    #)})
    #output$mosaic <- renderPlot({
    #    req(input$table)
     #   obs <- as.matrix(hot_to_r(input$table))
     #   par(mar=c(0, 0, 0, 0), plt=c(0,1,0,1))
    #    plot(.5, .5, xlim=c(0,1), ylim=c(0,1), type='n', frame.plot=F, 
    #         axes=F, ann=F, xaxs='i', yaxs='i')
    #    if(input$which_rect == 'obs'){
    #        draw.obs.rect(obs, cts=T)
    #    }
    #    if(input$which_rect == 'exp'){
    #        draw.exp.rect(obs, rv, cts=T)
    #    }
    #    if(input$which_rect == 'both'){
    #        draw.obs.rect(obs, cts=F)
    #        draw.exp.rect(obs, rv, cts=F)
    #    }
   # })
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


    
    #output$plot <- renderPlot({
    #    x <- seq(0, input$max_stat, by=.01)
    #    par(cex.main=1.5, cex.lab=1.3, cex.axis=1, mar=c(4.1, .1, 2.5, .1))
    #    plot(x, dchisq(x, df=input$df), type='l', ylab='', yaxt='n', 
     #        xlab='Chi-squared statistic')
     #   axis(side=1, at=input$stat, col.axis=2)
     #   segments(x0=input$stat, y0=0, y1=dchisq(input$stat, df=input$df))
     #   x.seq <- seq(input$stat, input$max_stat, by=.01)
    #    x.poly <- c(input$stat, x.seq, input$max_stat, input$stat)
     #   y.poly <- c(0, dchisq(x.seq, df=input$df),0,0)
     #   polygon(x.poly, y.poly, col=grey(.8))
     #   pval <- pchisq(input$stat, df=input$df, lower.tail=F)
     #   pval <- ifelse(pval < .0001, '<.0001', sprintf('%.4f', pval))
     #   title(paste0('P-value = ', pval))
    #})
    observe({
        updateSliderInput(session, 'stat', value=input$stat, min=0, max=input$max_stat, step=.1)
    })
}

# Run the application 
shinyApp(ui, server)
