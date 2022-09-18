library(shiny)
##########################################################
#    Define UI for application that draws a histogram
###########################################################
ui <- fluidPage(
    # Application title
    #h4("StatsKey: One Sample Hypothesis Testing", style = "font-family: Jura; color: red; font-size: 44px;"),

    titlePanel(h2("IntroStatsApps: Descriptive Statistics", 
                  align = "center", style = "font-family: sans-serif; color:navy", br(), br())),
    ##
    withMathJax(),
    sidebarLayout(
        sidebarPanel(
          selectInput(
                inputId = "DescriptiveStats",
                label = "Types of Descriptive Statistics",
                choices = c("Table and Chart: Categorical Data",
                            "Table and Chart: Numerical Data",
                            "Numerical Measures"),
                multiple = FALSE,
                selected = "Table and Chart: Categorical Data"
                ),
          
            hr(),
            ###############################################################################
            ##                         one mean large sample (Z)
            ###############################################################################
            conditionalPanel(
                condition = "input.DescriptiveStats == 'Table and Chart: Categorical Data'",
                #########################
                textInput("cate_dat", "comma separated categorical raw data", 
                          value = "A, C, B, F, D, B, C, B, C, C, A, D, C, B, C, B, A, C, B, D", 
                          placeholder = "Enter character values separated by a comma."),
                br(),
                br(),
                radioButtons(
                       inputId = "cate_stats",
                       label = "Summary Types",
                       choices = c("Frequency Tables",
                                   "Bar Chart",
                                   "Pie Chart")
                       )
               ), 
 
                ### ============================================
          conditionalPanel(
            condition = "input.DescriptiveStats == 'Table and Chart: Numerical Data'",
            #########################
            textInput("num_dat", "comma separated numeric raw data", 
                      value = "20.5, 29, 32, 32, 32, 33, 36, 37, 38, 39, 39, 43, 47, 48, 49, 49, 49, 50, 50, 51, 51, 52, 52, 52, 53, 54, 54, 54, 56, 56, 57, 58, 60, 61, 62, 62, 69, 73, 74, 74.5", 
                      placeholder = "Enter character values separated by a comma."),
            selectInput(
              inputId = "num_stats",
              label = "Summary Types",
              choices = c("Frequency Tables",
                          "histogram")
              ),
             conditionalPanel(
               condition = "input.num_stats == 'Frequency Tables'",
               textInput("bound.freq", "Boundary",
                         value = "20, 31, 42, 53, 64, 75",
                         placeholder = "Enter character values separated by a comma."
                 )
             ),
            conditionalPanel(
              condition = "input.num_stats == 'histogram'",
              textInput("bound.hist", "Boundary",
                        value = "20, 31, 42, 53, 64, 75",
                        placeholder = "Enter character values separated by a comma."
              )
            )
          ), 
          #################################################
          #######           Numerical measures
          #################################################
          conditionalPanel(
            condition = "input.DescriptiveStats == 'Numerical Measures'",
            #########################
            textInput("num_val", "comma separated numerical data", 
                      value = "20.5, 29, 32, 32, 32, 33, 36, 37, 38, 39, 39, 43, 47, 48, 49, 49, 49, 50, 50, 51, 51, 52, 52, 52, 53, 54, 54, 54, 56, 56, 57, 58, 60, 61, 62, 62, 69, 73, 74, 74.5", 
                      placeholder = "Enter character values separated by a comma."),
            
            selectInput(
              inputId = "num_measure",
              label = "Measure Types",
              choices = c("measures of center",
                          "measures of variation",
                          "Percentile",
                          "5-number summary and boxplot")
              
            ),
            conditionalPanel(
              condition = "input.num_measure == 'Percentile'",
              sliderInput("percentile", "k-th Percentile",
                          min = 0,
                          max = 100,
                         value = 75)
            )
          ), 
          
            ###
            HTML('<p><center> <img src="https://github.com/pengdsci/sta553/blob/main/image/goldenRamLogo.png?raw=true"  width="100" height="100"></center></p>'),
            HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center><font size =2> <a href="mailto:cpeng@wcupa.edu">Report bugs to C. Peng</a> </font></center></p>')
            #########################################################
            ####   End of unconditional Panels (common to all tests)
            #########################################################
        ),
        ###
        
        ############################################
        ######          Main Panel
        ############################################
        mainPanel(
            conditionalPanel(
                 condition = "input.DescriptiveStats == 'Table and Chart: Categorical Data'",
                 uiOutput("cate_summary"),
                 ),
            
            conditionalPanel(
                condition = "input.DescriptiveStats == 'Table and Chart: Numerical Data'",
                uiOutput("freqboundary"),
                br(),
                uiOutput("num_summary"),
                ),
            
           conditionalPanel(
               condition = "input.DescriptiveStats == 'Numerical Measures'",
               uiOutput("center"),
               plotOutput("boxplot")
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
    ###
   extract_cat <- function(text) {
      text <- gsub(" ", "", text)
      split <- strsplit(text, ",", fixed = FALSE)[[1]]
      split
   }
   
   getmode <- function(dat){
     tbl <- table(dat)
     tbl.name <- names(tbl)
     tbl.vec <- as.vector(tbl)
     mode.id <- which(tbl.vec==max(tbl.vec))
     dat.mode <- tbl.name[mode.id]
     as.numeric(dat.mode)
   }
   ### finding decimal places for each data value in a data set
   decimalplaces <- function(x) {
     ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
            nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
            0)
   }
   
 ###############################################################################
 ######                  Outputs of test results
 ###############################################################################  
   output$cate_summary <- renderUI({
      ############
      textdat <- extract_cat(input$cate_dat)
      textdat.srt <- sort(textdat)
      ############  
      freq.table <- data.frame(table(textdat))
      freq.table$rel.freq <- round(freq.table$Freq/sum(freq.table$Freq),3)
      freq.table$cum.freq <- cumsum(freq.table$Freq)
      freq.table$rel.cum.freq <- round(cumsum(freq.table$Freq)/sum(freq.table$Freq),3)
      ####
      colcnt <- length(freq.table$Freq)   
      ############
      class.name <- names(table(textdat))
      ############
        #########
        if(input$cate_stats == "Frequency Tables"){
       withMathJax(
          tags$b("The input data values:"),
          br(),
          br(),
          paste0(c("", paste(textdat, collapse = ", ")), collapse = " "),
          br(),
          br(),
          tags$b("The sorted input data values:"),
          br(),
          br(),
          paste0(c("", paste(textdat.srt, collapse = ", ")), collapse = " "),
          br(),
          br(),
          paste("\\( \\bf{The \\ frequency \\ table}\\)"),
          br(),
          br(),
          paste("The frequency table with the given boundary values and the four types of frequencies are given by:"),
          br(),
          br(),
           ##### The four frequency tables are given below
           output$cat_raw <- renderTable(freq.table)
           )
          } else if(input$cate_stats == "Bar Chart"){
            withMathJax(
              tags$b("The input data values:"),
              br(),
              br(),
              paste0(c("", paste(textdat, collapse = ", ")), collapse = " "),
              br(),
              br(),
              tags$b("The sorted input data values:"),
              br(),
              br(),
              paste0(c("", paste(textdat.srt, collapse = ", ")), collapse = " "),
              br(),
              br(),
              paste("\\( \\bf{ Bar \\ chart}\\)"),
              br(),
              br(),
              paste(" The bar chart of the data set is given by:"),
              br(),
              br(),
             ####
             output$barchart <- renderPlot({
                 xx <- barplot(table(textdat), xlab=" ", width = 0.85, ylim=c(0, 1.3*max(as.vector(table(textdat)))))
                 text(xx, freq.table$Freq+0.1*max(freq.table$Freq), paste(100*freq.table$rel.freq, "%", sep = ""), cex =1, col="darkred")
                }, width = 400, height = 300)
              )
             } else if ( input$cate_stats == "Pie Chart"){
               withMathJax(
                 tags$b("The input data values:"),
                 br(),
                 br(),
                 paste0(c("", paste(textdat, collapse = ", ")), collapse = " "),
                 br(),
                 br(),
                 tags$b("The sorted input data values:"),
                 br(),
                 br(),
                 paste0(c("", paste(textdat.srt, collapse = ", ")), collapse = " "),
                 br(),
                 br(),
                 paste("\\( \\bf{ Pie \\ chart}\\)"),
                 br(),
                 br(),
                 paste(" The pie chart of the data set is given by:"),
                 br(),
                 br(),
               
             output$piechart <- renderPlot(
                  pie(table(textdat), 
                      label =paste(class.name, ": ", sep="", 100*freq.table$rel.freq, "%")
                      )
                )
               )
             }
     ################################  
     }
   )

###################################### numerical results
   output$num_summary <- renderUI({
     ############
     NumDat <- extract_num(input$num_dat)
     NumDat.srt <- sort(NumDat)
     ### Number of decimal places to keep in the calculation
     ### We keep one decimal place more than that in the original data values
     d0 <- max(decimalplaces(NumDat))+1
     ### boundary
     boundary.freq <- extract_num(input$bound.freq)
     boundary.hist <- extract_num(input$bound.hist)
     ###
     cut.data <- cut(x=NumDat, breaks = boundary.freq, labels = NULL,
         include.lowest = TRUE, right = TRUE, dig.lab = 3)
     ############
     midpt <- (boundary.freq[-1] + boundary.freq[-length(boundary.freq)])/2
     ############  For relative frequencies, we keep three decimal places
     num.freq.table <- data.frame(table(cut.data))
     num.freq.table$midpts <- midpt
     num.freq.table$rel.freq <- round(num.freq.table$Freq/sum(num.freq.table$Freq),3)
     num.freq.table$cum.freq <- cumsum(num.freq.table$Freq)
     num.freq.table$rel.cum.freq <- round(cumsum(num.freq.table$Freq)/sum(num.freq.table$Freq),3)
     ####
     colcnt <- length(num.freq.table$Freq)   
     ############
     freqvec <- as.vector(table(cut.data))
     ############

       #)   # mathjax
       if(input$num_stats == "Frequency Tables"){
         withMathJax(
           tags$b("The input data values:"),
           br(),
           br(),
           paste0(c("", paste(NumDat, collapse = ", ")), collapse = " "),
           br(),
           br(),
           tags$b("The sorted input data values:"),
           br(),
           br(),
           paste0(c("", paste(NumDat.srt, collapse = ", ")), collapse = " "),
           br(),
           br(),
           ######
           helpText(paste("The class boundary is: ",  input$bound.freq)),
           ##### The four frequency tables are given below
           output$num_raw <- renderTable({num.freq.table})
         )
         ############
       } else if(input$num_stats == "histogram"){
          midpt <- 
           withMathJax(
           tags$b("The input data values:"),
           br(),
           br(),
           paste0(c("", paste(NumDat, collapse = ", ")), collapse = " "),
           br(),
           br(),
           tags$b("The sorted input data values:"),
           br(),
           br(),
           paste0(c("", paste(NumDat.srt, collapse = ", ")), collapse = " "),
           br(),
           br(),
         ####
         output$histogram <- renderPlot({
           h<- hist(NumDat, breaks =boundary.hist, main = "", xlab="", ylab="", ylim=c(0,1.2*max(freqvec)))
           text(h$mids,h$counts,labels=paste(100*round(h$counts/sum(h$counts),3),"%",sep=""), adj=c(0.5, -0.5))
           title("Probability Distribution Histogram")
         }, height = 350, width = 600)
       )
     } 
     ################################  
     }
   )
   ###########################
   ##    Centers
   ###########################
   output$center <- renderUI({
     ######
     NumDatMeasure <- extract_num(input$num_val)
     NumDat.srt <- sort(NumDatMeasure)
     nn <- length(NumDat.srt)
     mymode <- getmode(NumDatMeasure)
     ### Number of decimal places to keep in the calculation
     ### We keep one decimal place more than that in the original data values
     d0 <- max(decimalplaces(NumDatMeasure))+1
     ######
     if(input$num_measure=="measures of center"){
     ######
     withMathJax(
       paste("\\( \\bf{Measures \\ of \\ Center}\\)"),
       br(),
       br(),
       paste("The data values are: "),
       br(),
       paste(c("", paste(NumDatMeasure, collapse = ", ")), collapse = " "),
       br(),
       br(),
       paste("The sorted data values are:"),
       br(),
       paste(c("", paste(NumDat.srt, collapse = ", ")), collapse = " "),
       br(),
       br(),
       paste("\\( \\bf{1. \\ Sample \\ (population) \\ mean}\\)"),
       br(),
       br(),
       paste("        \\( \\bar{x} = \\sum_{i=1}^{n}  \\frac{x_{i}}{n} = \\) ", round(mean(NumDatMeasure),d0), 
             ", and \\( \\mu =\\sum_{i=1}^n \\frac{ x_i}{n}\\) = ", round(mean(NumDatMeasure),d0),"(if this data set is a population)."),
       br(),
       br(),
       paste("\\( \\bf{2. \\ Median}\\)"),
       br(),
       #br(),
       paste("The meadian of a given data set is the middle number of \\(\\bf{sorted} \\) data set. Based on this definition, the median of the given data set is: ", quantile(NumDatMeasure, 0.5, type = 2)),
       br(),
       br(),
       paste("\\( \\bf{3. \\ Mode}\\)"),
       br(),
       #br(),
       paste("A data value that appears most frquently (frequency > 1) is called the mode of the data. Based on this definition, a set of data may have one mode, more than one mode, or no mode at all."),
       #br(),
       paste("Using the above definition, this data set has ", length(mymode), ifelse(length(mymode) > 1," modes: ", "mode: "), paste(mymode, collapse = ", "), "."),
       br()
      )
     } else if(input$num_measure=="measures of variation"){
       ###
       withMathJax(
         paste("\\( \\bf{Measures \\ of \\ Variation}\\)"),
         br(),
         br(),
         paste("The data values are: "),
         br(),
         paste(c("", paste(NumDatMeasure, collapse = ", ")), collapse = " "),
         br(),
         br(),
         paste("The sorted data values are:"),
         br(),
         paste(c("", paste(NumDat.srt, collapse = ", ")), collapse = " "),
         br(),
         br(),
         paste("\\( \\bf{1. \\ Sample \\ ( population ) \\ variance} \\)"),
         br(),
         br(),
         paste("        \\( s^2 = \\sum_{i=1}^{n}  \\frac{(x_{i} - \\bar{x})^2}{n-1} = \\) ", round(var(NumDatMeasure),d0), 
               ", and \\( \\sigma^2 =\\sum_{i=1}^n \\frac{( x_{i} - \\mu )^2 }{n}\\) = ", round((length(NumDatMeasure)-1)/length(NumDatMeasure)*var(NumDatMeasure),d0), "(if this data set is a population"),
         br(),
         br(),
         paste("\\( \\bf{2. \\ Sample \\ (population) \\ standard \\ deviation}\\)"),
         br(),
         #br(),
         paste("The standard deviation is the square root of variance. Therefore, the both standard deviations are: ", quantile(NumDatMeasure, 0.5, type = 2)),
         br(),
         br(),
         paste("        \\( s = \\sqrt{s^2} = \\sqrt{\\sum_{i=1}^{n}  \\frac{(x_{i} - \\bar{x})^2}{n-1}} = \\) ", round(sd(NumDatMeasure),d0), 
               ", and \\( \\sigma = \\sqrt{\\sigma^2} =\\sqrt{\\sum_{i=1}^n \\frac{( x_{i} - \\mu )^2 }{n}}\\) = ", round(sqrt((length(NumDatMeasure)-1)/length(NumDatMeasure)*var(NumDatMeasure)),d0), "(if this data set is a population"),
         br(),
         br(),
         paste("\\( \\bf{3. \\ Inter-quartile \\ range \\ (IQR)}\\)"),
         br(),
         #br(),
         paste("The inter-quartile range is defined to the difference between the first and third quartiles. By the definition,  \\(IQR = P_{75} - P_{25} =\\) ", quantile(NumDatMeasure, 0.75, type = 2), " - ",  quantile(NumDatMeasure, 0.25, type = 2), " = ", quantile(NumDatMeasure, 0.75, type = 2)-quantile(NumDatMeasure, 0.25, type = 2),"."),
         #br(),
         #paste("Using the above definition, this data set has ", length(mymode), ifelse(length(mymode) > 1," modes: ", "mode: "), paste(mymode, collapse = ", "), "."),
         br()
       )
     } else if (input$num_measure=="Percentile"){
       locator = (input$percentile/100) * length(NumDatMeasure)
       withMathJax(
         paste("\\( \\bf{Finding \\ Percentiles}\\)"),
         br(),
         br(),
         paste("The data values are: "),
         br(),
         paste(c("", paste(NumDatMeasure, collapse = ", ")), collapse = " "),
         br(),
         br(),
         paste("The sorted data values are:"),
         br(),
         paste(c("", paste(NumDat.srt, collapse = ", ")), collapse = " "),
         br(),
         br(),
         paste("\\( \\bf{\\ The \\ General \\ Steps \\ for \\ finding \\ k^{th} \\ percentile} \\)"),
         br(),
         br(),
         paste("\\(  \\  {\\bf{ Step \\ 1}}: \\ Find \\ the \\ rough \\ location \\ of \\ k^{th} \\ percentile: \\)"),
         br(),
         br(),
         paste("\\(  \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\  L =  \\frac{k}{100} \\times n  \\)"),
          br(),
         br(),
         paste(" \\( \\ {\\bf{ Step \\ 2: }} \\ \\ if \\ L \\ is \\ an \\ integer,\\ the \\ k^{th} \\ percentile \\ is \\ the \\ average \\ of \\ L^{th} \\ and \\ (L+1)^{th} \\ values \\ in \\ the \\  SORTED \\ data.\\)"),
         paste(" \\( \\ \\ \\ if \\ L \\ is \\ a \\ decimal,\\ the \\ k^{th} \\ percentile \\ of \\ the \\ data \\ is \\ (L +1)^{th} \\ value \\ in \\ the \\ SORTED \\ data \\ set\\)"),
         br(),
         br(),
         paste("\\( \\bf{\\ The \\ solution \\ to \\ the \\ input \\ data:}\\)"),
         br(),
         br(),
         paste("Let \\( SD[j] \\ be \\ j^{th}\\) data value in the sorted data. Based on the above algorithm, \\(L_{", input$percentile,"} =  \\frac{",input$percentile,"}{100} \\times n = ", (input$percentile/100)*length(NumDatMeasure), "\\)"),
         paste(".   ", ifelse(round(locator)==locator ," Since  L is an integer, " , "Since L is a decimal, ")),
         paste("\\( the \\", input$percentile, "^{th} \\ percentile: \\ P_{", input$percentile,"} = ", 
               ifelse(round(locator)==locator ,paste("( SD[",floor(locator), "] + SD[",floor(locator)+1, "] / 2"),paste(" SD[",floor(locator) + 1, "]")), "\\ = ",
         ifelse(round(locator)==locator , paste("(",NumDat.srt[locator], " + ", NumDat.srt[locator + 1],") / 2 = ", mean(c(NumDat.srt[locator], NumDat.srt[locator+1])), "." ), NumDat.srt[locator + 1]), "\\)"),
         br(),
         br()
       )  
     } else if (input$num_measure=="5-number summary and boxplot"){
       stats <- names(summary(NumDatMeasure)[-4])
       value <- as.vector(summary(NumDatMeasure))[-4]
       fivenum <- data.frame(stats=stats, value = value)
       withMathJax(
         paste("\\( \\bf{Five \\ Number \\ Summary \\ and \\ Boxplot}\\)"),
         br(),
         br(),
         paste("The data values are: "),
         br(),
         paste(c("", paste(NumDatMeasure, collapse = ", ")), collapse = " "),
         br(),
         br(),
         paste("\\(\\bf{1. \\ Five \\ Number \\ Summary:} \\)"),
         br(),
         paste("The five-number summary is use used to describe the shape of the distribution of a 
               given numerical data. It consists of five numbers: minimum data value, first quartile, median, 
               the third quartile, and the maximum data value."),
         br(),
         br(),
         paste("The five-number summary of this given data set is:"),
         br(),
         renderTable({fivenum}),
         paste("\\(\\bf{2. \\ Boxplot:} \\)"),
         br(),
         paste("The boxplot is a geometric representation of the five-number summary. 
               The boxplot of the given data set is given below."),
         renderPlot({
           boxplot(NumDatMeasure, horizontal = TRUE, bty="n")
           text(as.vector(summary(NumDatMeasure))[-4], rep(1.25,5), as.character(as.vector(summary(NumDatMeasure))[-4]), cex = 0.8)
           },height = 250, width = 600),
         br()
       )
     }
       
       
   })
   
#####   
} 
####

######################  

#####################
#####################
shinyApp(ui, server)
    




