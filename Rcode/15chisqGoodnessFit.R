#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(
    includeCSS('styles.css'), 
    withMathJax(),
    ### Important: The following tag allows inline equation in MathJax
    #tags$div(HTML("<script type='text/x-mathjax-config' >
    #        MathJax.Hub.Config({
    #        tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
    #        });
    #        </script >
    #        ")),
    br(),
    ### Important: The following tag allows inline equation in MathJax
    wellPanel(style = "background-color:#5e2442;",
              titlePanel( h3("ISLA: Chi-squared Goodness-of-fit Test", 
                             align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
    ),
    #################

    fluidRow(
        ###########################  Panel 1  ##################################
        column(3, wellPanel(style = "background-color: #5e2442; color:gold; ",
           # numericInput("m", "Numer of Categories:", 10, min = 1),
            ##
            h4(strong('Instructions:')),
            br(),
            br(),
            paste0('The table in the panel 2 is editable. You double click the cell to modify the default value.'),
            br(),
            br(),
            strong('1. '),
            paste0('Type the cell counts in the first column. All values must be'),
            strong('non-negative integers.'),
            br(),
            br(),
            strong('2. '),
            paste0('Type the cell probabilities in the null hypothesis (Ho).'),
            paste0('Please make sure that:'),
            br(),
            br(),
            paste0('A). the cell probabilities in the right column must add up to 1, and '),
            br(),
            br(),
            strong('B). each individual cell value is between 0 and 1.'),
            br(),
        ###
        HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
        ),
        ), 
        
        ##########################  Panel 2   ###################################
        column(3, wellPanel(
            h4(strong('Input Shelf Table:')),
            br(),
            DTOutput("my_datatable"),
        )), 
        #########################  panel 3     ##################################
        column(3, wellPanel(
            h4(strong('Summarized Input:')),
            br(),
            tableOutput("obs_exp"),
            hr(),
            h4(strong('Observed vs Expected Counts')),
            br(),
            span(tableOutput("exp_table"), style="color:blue")
            #tableOutput("exp_table"),
        )), 

        #########################  Panel 4     ###################################
        column(3, wellPanel(
            h4(strong('\\(\\chi^2\\) Test Results')),
            span(tableOutput("check"), style="color:red"),
            br(),
            br(),
            h5(strong('Null Hypothesis')),
            strong('Ho:'),
            paste0('The data follows the designated distribution.'),
            br(),
            br(),
            h5(strong('Test Statistic (TS):')),
            '\\(\\chi^2 = \\)', textOutput('x2', inline=T),'.',
            br(),
            br(),
            h5(strong('Calculation of TS:')),
            #textOutput("check"),
            tags$table(tags$tr(
                tags$td('\\(\\chi^2 = \\sum \\frac{(O - E)^2}{E} = \\)', style='border-style: none'), 
                tags$td(uiOutput('test_stat_calc', inline=T), '.', style='border-style: none')
            ), style='border-style: none'),
            ###############
            br(),
            h5(strong('P-value:')),
            'The above \\(\\chi^2\\) test statistic with ', textOutput('df', inline=T), 'degrees of freedom yiels a ',textOutput('pval', inline=T),'.'
        )), 

    )
)
##########    
server <- function(input, output, session) {
    #mm = input$m
    #initialize a blank dataframe
    v <- reactiveValues(
        data = { 
        data.frame(Observed.value = numeric(0),Prob.in.Ho = numeric(0)) %>% 
            #add_row(Obs = c(5,10,rep(0, 8)), Prob = c(0.5, 0.5, rep(0, 8)))
           add_row(Observed.value = c(2,rep(0, 9)), Prob.in.Ho = c(1,rep(0, 9)))
    })
    
    #output the datatable based on the dataframe (and make it editable)
    output$my_datatable <- renderDT({
        DT::datatable(v$data, editable = TRUE, options = list(dom = 't'))
    })

    ###################
    # when there is any edit to a cell, write that edit to the initial dataframe
    # check to make sure it's positive, if not convert
    observeEvent(input$my_datatable_cell_edit, {
        #get values
        info = input$my_datatable_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        k = as.numeric(info$value)
        if(k < 0){      #convert to positive if negative
            k <- k * -1
        }
        ##########################
        # write values to reactive
        v$data[i,j] <- k
    })
    ##########################
    ##################
    ##################
    output$obs_exp <- renderTable({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho > 0)
        if(sum(datset$Prob.in.Ho) ==1)  actual = datset[valid.id,]
    })
    #######
    output$exp_table <- renderTable({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho > 0 )
        actual = datset[valid.id,]
        Expected = round(sum(actual$Observed.value) *actual$Prob.in.Ho,1)
        Observed = actual$Observed.value
        if(round(sum(datset$Prob.in.Ho),4) ==1 ) data.frame(Observed.Value = Observed, Expected.Value = Expected)
    })
    #### validity check
    output$check = renderText({
        datset = v$data
        if(round(sum(datset$Prob.in.Ho),4) !=1 ) {
          paste0("ERROR: The sum of input probabilities is NOT equal to 1.")
        }
    })
    
    #############################################
    output$x2 <- renderText({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho >0 )
        actual = datset[valid.id,]
        #####
        obs <- as.matrix(actual$Observed.value)
        exp <- as.matrix(round(sum(obs)*actual$Prob.in.Ho, 1))
        x2 <- sum((obs - exp)^2 / exp)
        if(round(sum(datset$Prob.in.Ho),4) ==1)round(x2, 2)
    })
    
    #######################################
    output$test_stat_calc <- renderUI({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho >0 )
        actual = datset[valid.id,]
        #####
        obs <- as.matrix(actual$Observed.value)
        exp <- as.matrix(round(sum(obs)*actual$Prob.in.Ho, 1))
        #####
        obs.vec = as.vector(obs)
        exp.vec = as.vector(exp)
        TS = sum((obs.vec-exp.vec)^2/exp.vec)
        #####
        nrow <- nrow(obs) 
        ncol <- ncol(obs)
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
        calc <- paste0(calc, '\\) =', round(TS,2))
        if(round(sum(datset$Prob.in.Ho),4) ==1) withMathJax(HTML(calc))
    })
    ##############
    
    output$df <- renderText({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho > 0 )
        actual = datset[valid.id,]
        df=dim(actual)[1]-1
        if(round(sum(datset$Prob.in.Ho),4) ==1)df
    })
    
    ##############
    output$pval <- renderText({
        datset = v$data
        valid.id = which(datset$Prob.in.Ho >0 )
        actual = datset[valid.id,]
        #####
        obs <- as.matrix(actual$Observed.value)
        exp <- as.matrix(round(sum(obs)*actual$Prob.in.Ho, 1))
        #####
        obs.vec = as.vector(obs)
        exp.vec = as.vector(exp)
        TS = sum((obs.vec-exp.vec)^2/exp.vec)
        ####
        pval = round(1-pchisq(TS, length(exp)-1),4)
        ####
        if(round(sum(datset$Prob.in.Ho),4) ==1) ifelse(pval < .0001, 'p-value < 0.0001', paste("p-value =",sprintf("%.4f", pval)))
    })
    

    #render plot
    output$my_plot <- renderPlot({
        #req(input$go)        #  require the input button to be non-0 (ie: don't load the plot when the app first loads)
        isolate(v$data) %>%  #  don't react to any changes in the data
            ggplot(aes(Obs,Prob)) +
            geom_point() +
            geom_smooth(method = "lm")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)