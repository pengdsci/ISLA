#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##
##   Author:  Cheng Peng
##     Date:  2/14/2022
##  revised:  4/15/2024
##  West Chester University
##
#
library(shiny)
library(scales)
#####
ui <- fluidPage(
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
            titlePanel( h3("ISLA: Applications of CLT for Sample Means", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
  ),

    ############################################################################
    fluidRow(
        column(4, wellPanel( style = "background-color: #5e2442; color:gold;", 
            radioButtons('problem_type', label='1. What to Find?', 
                         choices=list('Probability $(P_0)$' = 1, 
                                      'Percentile $(X_0)$' = 2), 
                         selected =1), 
            
            hr(),
            ###################################################################################
            conditionalPanel(condition = "input.problem_type=='1'", 
                             radioButtons('range_type', label='2. Which Probability?',
                                          choices=list('$P[ V_0 <\\bar{X} < V_1] = ?$' = 1 ,
                                              '$P[ \\bar{X} > V_0] = ?$' = 2,
                                              '$P[ \\bar{X} < V_0] = ?$' = 3), 
                                          selected=1)),  
               ###########
               conditionalPanel(condition="input.problem_type=='1' & input.range_type == '1'",
                            numericInput('v00', label='Given Value #1: $V_0$', 
                                         min = NA, max = NA, value=3.5, step=.000001), 
                            ########################################################
                            numericInput('v01', label='Given Value #2: $V_1$', 
                                         min = NA, max = NA, value=4.3, step=.000001), 
              ),
              ######
              conditionalPanel(condition="input.problem_type=='1' & input.range_type == '2'",
                             numericInput('GV', label='Given Value: $V_0$', 
                                          min = NA, max = NA, value=4.25), 
              ),
              #######
              conditionalPanel(condition="input.problem_type=='1' &  input.range_type == '3'",
                             numericInput('LV', label='Given Value: $V_0$', 
                                          min = NA, max = NA, value=3.79), 
              ),
            ##############################################################################################
            ##############################################################################################
            conditionalPanel(condition = "input.problem_type=='2'", 
                             radioButtons('prob_type', label='2. $X_0$ in Which Probability?',
                                          choices=list('$P[ X_0 <\\bar{X} < V] = P_0$' = 1,
                                                       '$P[ V <\\bar{X} < X_0] = P_0$' =2,
                                                       '$P[ \\bar{X} > X_0] = P_0$' = 3,
                                                       '$P[ \\bar{X} < X_0] = P_0$' = 4), 
                                          selected=1),
                             numericInput('prob', label='Given Probability: $P_0$', 
                                          min = NA, max = NA, value=0.25),
                             ),   
            
            ###########
            conditionalPanel(condition="input.problem_type=='2' & input.prob_type == '1'",
                             numericInput('rV', label='Given Value: $V$', 
                                          min = NA, max = NA, value=4.25), 
            ),
            ##########
            conditionalPanel(condition="input.problem_type=='2' & input.prob_type == '2'",
                             numericInput('lV', label='Given Value: $V$', 
                                          min = NA, max = NA, value=3.8), 
            ),
            ####################################################################################
            
            hr(),
            h5(strong('3. Input Information')),
            numericInput('mu', label='Population Mean: $\\mu$', 
                        min = NA, max = NA, value=4), 
            numericInput('sigma', label='Population Standard Deviation: $\\sigma$', 
                        min=0.000001, max = NA, value=2),
            numericInput('nc', label='Sample Size: $n$', min=1, max = NA, 
                        value=20, step=1),
 
        hr(),
        HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
        
        )),
        ###########################################################################   
        #column(1, wellPanel( style = "background-color:lightgray", 
                             #####
                             #plotOutput('stdevCurve', height='220px'),
                             ##### 
                             #uiOutput('panel3'),
        #                     hr(),
        #                      hr(),
        #)),
      ###########################################################################
        column(8, wellPanel( style = "background-color:lightgray", 
            br(),
            plotOutput('genNormCurve', width="620px", height="220px"),
            ######
            uiOutput('panel2'), 
            br(),
            #hr(),
        )) 

     ###########################################################################
    )
)

#############################################################
#############################################################
#############################################################
server <- function(input, output){
    #################################################
    ###      Sampling distribution of sample means
    #################################################
    output$genNormCurve <- renderPlot({
        mu <- input$mu
        sigma <- input$sigma
        nn <- input$nc
        s.xbar <- sigma/sqrt(nn)
        
        #################################
        ##    Two Density Curves
        #################################
        nf <- layout(
          matrix(c(1,2,3), ncol=3, byrow=TRUE), 
          widths=c(2,1,2), 
          #heights=c(2,2)
        )
        par(oma = c(1,1,0,1), mar = c(1,1,1,1), bg = "#f7f7f7")
        
        #####
        xseq = seq(mu-4*s.xbar, mu+4*s.xbar, length = 200)
        ymax = dnorm(mu, mu, s.xbar)
        par(bg = "lightgray")
        plot(xseq, dnorm(xseq, mean = mu, sd = s.xbar), type = "l", lty = 1, 
             main =paste('Sampling Distribution of Sample Means'),
             ylim=c(-0.1*ymax, ymax),
             xlab= '', 
             ylab='',
             axes = FALSE,
             cex.main = 1.5)
        #axis(1, pos = 0)
        segments(mu-4*s.xbar, 0, mu+4*s.xbar, 0 )
        ###############################################################
        #####    Conditional statements for shade the regions
        ###############################################################
        ###
        if(input$problem_type=='1' & input$range_type == '1'){ # between
            V0 <- min(input$v00, input$v01) 
            V1 <- max(input$v00, input$v01)
            if(V0 < (mu-4*s.xbar) & V1 < (mu+4*s.xbar) ){
                v.seq = seq((mu-4*s.xbar), V1, length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c((mu-4*s.xbar),v.seq, V1),c(0,y.seq,0),col="lightblue") 
                text(c((mu-4*s.xbar),V1), c(-0.08*ymax,-0.08*ymax), c(expression(V[0]), expression(V[1])), col = "red", cex = 1.5)    
            } else if(V0<(mu -4*s.xbar) & V1 > (mu+4*s.xbar)){
                v.seq = seq((mu -4*s.xbar), (mu + 4*s.xbar), length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c((mu -4*s.xbar),v.seq, (mu+4*s.xbar)),c(0,y.seq,0),col="lightblue") 
                text(c((mu -4*s.xbar),(mu+4*s.xbar)), c(-0.08*ymax,-0.08*ymax), c(expression('V'[0]), expression('V'[1])), col = "red", cex = 1.5)    
            } else{
                v.seq = seq(V0, V1, length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c(V0,v.seq, V1),c(0,y.seq,0),col="lightblue")
                text(c(V0,V1), c(-0.08*ymax,-0.08*ymax), c(expression('V'[0]), expression('V'[1])), col = "red", cex = 1.5)
            }
            #####
            text(mu-2.5*s.xbar, 0.9*ymax, "Given Information", col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.78*ymax, paste('V0 = ',V0), col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.65*ymax, paste('V1 = ',V1), col = "red", cex = 1.5)
        }
        ######
        if(input$problem_type=='1' & input$range_type == '2'){ # between
            GV <- input$GV
            if(GV < (mu+4*s.xbar) & GV > (mu -4*s.xbar)){
               v.seq = seq(GV, mu+4*s.xbar, length = 200)
               y.seq = dnorm(v.seq, mu, s.xbar)
               polygon(c(GV,v.seq, mu+4*s.xbar),c(0,y.seq,0),col="lightblue")
               text(c(GV), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)
            } else if (GV < (mu-4*s.xbar) ){
                v.seq = seq((mu-4*s.xbar), mu+4*s.xbar, length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c((mu-4*s.xbar),v.seq, mu+4*s.xbar),c(0,y.seq,0),col="lightblue")
                text(c((mu-4*s.xbar)), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)
                text(c(mu-4*s.xbar), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)                
            } else{
                text(c(mu+4*s.xbar), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)  
            }
            ####
              text(mu-2.5*s.xbar, 0.9*ymax, "Given Information", col = "red", cex = 1.5)
              text(mu-2.5*s.xbar, 0.69*ymax, paste('V0 = ',GV), col = "red", cex = 1.5)
         }
        ######
        if(input$problem_type=='1' & input$range_type == '3'){ # between
            LV <- input$LV
            
            if(LV > mu-4*s.xbar & LV < mu + 4*s.xbar){
               v.seq = seq (mu-4*s.xbar, LV, length = 200)
               y.seq = dnorm(v.seq, mu, s.xbar)
               polygon(c(mu-4*s.xbar,v.seq, LV),c(0,y.seq,0),col="lightblue") 
               text(c(LV), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)
            } else if(LV < mu-4*s.xbar){
                text(c(mu-4*s.xbar), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5) 
            } else{
                v.seq = seq (mu-4*s.xbar, mu+4*s.xbar, length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c(mu-4*s.xbar,v.seq, mu+4*s.xbar),c(0,y.seq,0),col="lightblue")
                text(c(mu + 4*s.xbar), c(-0.08*ymax), c(expression('V'[0])), col = "red", cex = 1.5)
            }
            ###            
            text(mu-2.5*s.xbar, 0.9*ymax, "Given Information", col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.69*ymax, paste('V0 = ',LV), col = "red", cex = 1.5)
        }
        ############################
        ######  Quantile  ##########
        ############################
        ###  P[Xo < bar(X) < V]
        if(input$problem_type=='2' & input$prob_type == '1'){
         p0 = input$prob
         if(p0 < 0 | p0 >1) stop("A probability must between 0 and 1.")
         rV = input$rV
         lQ = qnorm((pnorm(rV, mu, s.xbar)-p0), mu, s.xbar)  ## X0
         if (pnorm(rV, mu, s.xbar) < p0) stop("The input value is too small!")
         V0 = lQ
         V1 = rV
         #######
         if(V1 < (mu + 4*s.xbar) ){
             v.seq = seq(V0, V1, length = 200)
             y.seq = dnorm(v.seq, mu, s.xbar)
             polygon(c(V0,v.seq, V1),c(0,y.seq,0),col="lightblue") 
             text(c(V0,V1), c(-0.08*ymax,-0.08*ymax), c(expression('X'[0]), expression('V')), col = "red", cex = 1.5)
         } else {
             v.seq = seq(V0,(mu +4*s.xbar), length = 200)
             y.seq = dnorm(v.seq, mu, s.xbar)
             polygon(c(V0,v.seq, (mu + 4*s.xbar)),c(0,y.seq,0),col="lightblue") 
             text(c(V0,(mu +4*s.xbar)), c(-0.08*ymax,-0.08*ymax), c(expression('X'[0]), expression('V')), col = "red", cex = 1.5)    
         } 
         ###
         text(mu-2.5*s.xbar, 0.92*ymax, "Given Information", col = "red", cex = 1.5)
         text(mu-2.5*s.xbar, 0.8*ymax, paste('P0 = ',p0), col = "red", cex = 1.5)
         text(mu-2.5*s.xbar, 0.69*ymax, paste('V = ',rV), col = "red", cex = 1.5)
         }
         ##### P[V < bar(X) < Xo]
        if(input$problem_type=='2' & input$prob_type == '2'){
            p0 = input$prob
            if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
            lV = input$lV
            rQ = qnorm((pnorm(lV, mu, s.xbar) + p0), mu, s.xbar)
            if(pnorm(lV, mu, s.xbar) > 0.75) stop("The input value is too big!")
            
            V0 = lV
            V1 = rQ
            #######
            if(lV < (mu - 4*s.xbar)){
                v.seq = seq((mu - 4*s.xbar), V1, length = 200)
                y.seq = dnorm(v.seq, mu, s.xbar)
                polygon(c((mu - 4*s.xbar),v.seq, V1),c(0,y.seq,0),col="lightblue") 
                text(c((mu - 4*s.xbar),V1), c(-0.08*ymax,-0.08*ymax), c(expression('V'), expression('X'[0])), col = "red", cex = 1.5) 
            } else {
               v.seq = seq(V0, V1, length = 200)
               y.seq = dnorm(v.seq, mu, s.xbar)
               polygon(c(V0,v.seq, V1),c(0,y.seq,0),col="lightblue") 
               text(c(V0,V1), c(-0.08*ymax,-0.08*ymax), c(expression('V'), expression('X'[0])), col = "red", cex = 1.5)
            }
            ##
            text(mu-2.5*s.xbar, 0.92*ymax, "Given Information", col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.8*ymax, paste('P0 = ',p0), col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.69*ymax, paste('V = ',lV), col = "red", cex = 1.5)
        }
        ##### P[ bar(X) > Xo]
        if(input$problem_type=='2' & input$prob_type == '3'){
            p0 = input$prob
            rQ = qnorm((1-p0), mu, s.xbar)
            ##
            uplim = mu + 4*s.xbar
            #######
            v.seq = seq(rQ, uplim, length = 200)
            y.seq = dnorm(v.seq, mu, s.xbar)
            polygon(c(rQ,v.seq, uplim),c(0,y.seq,0),col="lightblue") 
            text(c(rQ), c(-0.08*ymax), c(expression('X'[0])), col = "red", cex = 1.5)
            ###
            text(mu-2.5*s.xbar, 0.92*ymax, "Given Information", col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.8*ymax, paste('P0 = ',p0), col = "red", cex = 1.5)
        }
        ##### P[ bar(X) < Xo]
        if(input$problem_type=='2' & input$prob_type == '4'){
            p0 = input$prob
            lQ = qnorm(p0, mu, s.xbar)
            ##
            lowlim = mu - 4*s.xbar
            #######
            v.seq = seq(lowlim, lQ, length = 200)
            y.seq = dnorm(v.seq, mu, s.xbar)
            polygon(c(lowlim,v.seq, lQ),c(0,y.seq,0),col="lightblue") 
            text(c(lQ), c(-0.08*ymax), c(expression('X'[0])), col = "red", cex =1.5)
            ###
            text(mu-2.5*s.xbar, 0.92*ymax, "Given Information", col = "red", cex = 1.5)
            text(mu-2.5*s.xbar, 0.8*ymax, paste('P0 = ',p0), col = "red", cex =1.5)
        }
        
        ##############################################################################
        #######################   Z-score Transformation #############################
        ##############################################################################
        
        
        
        plot(1,1, type = "n", axes=FALSE)
        text(1,1, expression(Z == frac(X-mu, sigma/sqrt(n))), cex = 2, col = "darkred")
        
        
        
        ##############################################################################
        ###                   Standard Normal Density
        ##############################################################################
        
        ##############################
        x <- seq(-4, 4, by=.01)
        #par(oma = c(1,1,0,1), mar = c(1,1,1,1), bg = "#f7f7f7")
        #par(bg = "lightgray")
        plot(x, dnorm(x), type='l', main='Standard Normal Distribution N(0,1)', 
             ylab='', xlab='Z Score', axes = FALSE,
             ylim=c(-0.15*dnorm(0),dnorm(0)),
             cex.main = 1.5
        )
        #axis(1, pos = 0)
        segments(-4, 0, 4, 0)
        ###################################################### 
        ##  Same conditions used in the sampling distribution
        ####################################################### 
        ###
        if(input$problem_type=='1' & input$range_type == '1'){ 
          # between: P(v0 <xbar <v1)
          V0 <- min(input$v00, input$v01) 
          V1 <- max(input$v00, input$v01)
          Z0 <- (V0 - mu)/s.xbar
          Z1 <- (V1 - mu)/s.xbar
          zmax = dnorm(0)
          ##### end point handling
          if(Z0 < -4 & Z1 < 4){ 
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            text(c(-4,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('z'[0]), expression('z'[1])), col = "blue", cex = 1.5)  
          } else if (Z0 < -4 & Z1 > 4){
            z.seq02 = seq(-4, 4, length = 200)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, 4),c(0,yz.seq02,0), col="lightblue")
            text(c(-4,4), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)    
          } else if (Z0 > -4 & Z1 > 4){
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, 4, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, 4),c(0,yz.seq02,0), col="lightblue")
            segments(z.seq01, rep(0,round(8*(4+4))), z.seq01, dnorm(z.seq01), col="darkred")
            text(c(Z0,4), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)  
          } else {
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            segments(z.seq01, rep(0,round(8*(Z1+4))), z.seq01, dnorm(z.seq01), col="darkred")
            text(c(Z0,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)
          }
        }
        ######
        if(input$problem_type=='1' & input$range_type == '2'){ 
          ### P(xbar > Xo)
          GV <- input$GV
          GZ <- (GV-mu)/s.xbar
          zmax = dnorm(0)
          if(GZ > 4) {
            text(4, c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)   
          } else if(GZ < -4){
            z.seq = seq(-4, 4, length = 200)
            yz.seq = dnorm(z.seq, 0, 1)
            polygon(c(-4,z.seq, 4),c(0,yz.seq,0),col="lightblue") 
            text(-4, c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)                 
            text(-4, c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)   
          }else {
            z.seq = seq(GZ, 4, length = 200)
            yz.seq = dnorm(z.seq, 0, 1)
            polygon(c(GZ,z.seq, 4),c(0,yz.seq,0),col="lightblue") 
            text(c(GZ), c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)
          }
        }
        ######
        if(input$problem_type=='1' & input$range_type == '3'){ 
          ### P(xbar < Vo)
          LV <- input$LV
          LZ <-(LV-mu)/s.xbar
          zmax = dnorm(0)
          #####
          if(LZ < - 4) {
            text(-4, c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)  
          } else if (LZ > 4){
            z.seq = seq (-4, 4, length = 200)
            yz.seq = dnorm(z.seq, 0, 1)
            polygon(c(-4,z.seq, 4),c(0,yz.seq,0),col="lightblue") 
            text(4, c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)  
          }else{
            z.seq = seq (-4, LZ, length = 200)
            yz.seq = dnorm(z.seq, 0, 1)
            polygon(c(-4,z.seq, LZ),c(0,yz.seq,0),col="lightblue") 
            text(c(LZ), c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.59)
          }
        }
        ############################
        ######  Quantile  ##########
        ############################
        ###  P[Xo < bar(X) < V]
        if(input$problem_type=='2' & input$prob_type == '1'){ # between
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          rV = input$rV
          lQ = qnorm((pnorm(rV, mu, s.xbar) - p0), mu, s.xbar)
          if(pnorm(rV, mu, s.xbar) < p0) stop("The input value is too small!")
          V0 = lQ
          V1 = rV
          ##########
          Z0 <- (V0 - mu)/s.xbar
          Z1 <- (V1 - mu)/s.xbar
          zmax = dnorm(0)
          #####
          if(Z1 > 4){
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, 4, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            ###
            polygon(c(-4,z.seq02, 4),c(0,yz.seq02,0), col="lightblue")
            segments(z.seq01, rep(0,round(8*(4+4))), z.seq01, dnorm(z.seq01), col="darkred")
            text(c(Z0,4), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[V])), col = "blue", cex = 1.5)
          }
          if(Z1 < 4){
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            ###
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            segments(z.seq01, rep(0,round(8*(Z1+4))), z.seq01, dnorm(z.seq01), col="darkred")
            text(c(Z0,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[V])), col = "blue", cex = 1.5)
          }
          
        }
        #####
        ###  P[V < bar(X) < Xo]
        if(input$problem_type=='2' & input$prob_type == '2'){ # between
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          lV = input$lV
          if(pnorm(lV, mu, s.xbar) > (1- p0)) stop("The input value is too big!")
          rQ = qnorm((pnorm(lV, mu, s.xbar) + p0), mu, s.xbar)
          ##########
          V0 = lV
          V1 = rQ
          ##########
          Z0 <- (V0 - mu)/s.xbar
          Z1 <- (V1 - mu)/s.xbar
          ###
          zmax = dnorm(0)
          #####
          if (Z0 < -4){
            #z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, Z1, length = 200)
            #yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            #segments(z.seq01, rep(0,round(8*(Z1+4))), z.seq01, dnorm(z.seq01), col=alpha("darkred", 0.6))
            ##
            text(c(-4,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[V]), expression('Z'[0])), col = "blue", cex = 1.5)                
          } else{
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            segments(z.seq01, rep(0,round(8*(Z1+4))), z.seq01, dnorm(z.seq01), col="darkred")
            ##
            text(c(Z0,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[V]), expression('Z'[0])), col = "blue", cex = 1.5)
          }
        }
        #####
        ###  P[ bar(X) > Xo]
        if(input$problem_type=='2' & input$prob_type == '3'){ # between
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          V0 = qnorm((1- p0), mu, s.xbar)
          ##########
          Z0 <- (V0 - mu)/s.xbar
          #####
          z.seq02 = seq(Z0, 4, length = 200)
          yz.seq02 = dnorm(z.seq02, 0, 1)
          ###
          zmax = dnorm(0)
          polygon(c(Z0,z.seq02, 4),c(0,yz.seq02,0), col="lightblue")
          ##
          text(c(Z0), c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.59)
        }
        #####
        ###  P[ bar(X) < Xo]
        if(input$problem_type=='2' & input$prob_type == '4'){ # between
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          V1 = qnorm(p0, mu, s.xbar)
          ##########
          Z1 <- (V1 - mu)/s.xbar
          #####
          z.seq01 = seq(-4, Z1, length = 200)
          yz.seq01 = dnorm(z.seq01, 0, 1)
          ###
          zmax = dnorm(0)
          polygon(c(-4,z.seq01, Z1),c(0,yz.seq01,0), col="lightblue")
          ##
          text(c(Z1), c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)
        }        
        ###############################################################
    }, height = 200, width = 700 )
    
    ######
    ####################################################################
    #########    Output of calculations: Sampling distribution
    ####################################################################
    output$panel2 <- renderUI({
    ####################
        mu <- input$mu
        sigma <- input$sigma
        nn <- input$nc
        s.xbar <- sigma/sqrt(nn)
   fluidPage(
      if(input$problem_type=='1' & input$range_type == '1'){ 
          
        ########
        V0 <- min(input$v00, input$v01)
        V1 <- max(input$v00, input$v01)
        Z0 <- round((V0 - mu)/s.xbar,2)
        Z1 <- round((V1 - mu)/s.xbar,2)
        #####################
          withMathJax(
                strong('Question:', style="color:darkred; font-weight:bold;"),
                paste('$ P(', V0, '< \\bar{X} <', V1,') = ? $'),
                br(),
                br(),
                strong('Solution:', style="color:darkred; font-weight:bold;"),
                paste('The answer is given in the following steps.'),
                br(),
                br(),
                paste('Step 1. Z-score Transformation'),
                #br(),
                paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} .$$'),
                #br(),
                paste('Step 2. Z-scores for $V_0$=',V0, 'and $V_1$=', V1, 'are given by'),
                br(),
                paste0('$$Z_0 = \\frac{',V0, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z0, ',$$'),
                paste0('$$ Z_1 = \\frac{',V1, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z1, '.$$'),
                br(),
                paste0('Step 3. Note that'),
                paste0('$$P(', V0, '< \\bar{X} <', V1,') = P \\left(', Z0, '< Z <', Z1,  '\\right)$$ '),
                paste0('$$= P \\left( Z < ',Z1,  '\\right) - P \\left(Z < ',Z0 , '\\right)$$'),
                paste0('$$= ',round(pnorm(Z1),4) , ' - ',round(pnorm(Z0),4) , ' = ',round(pnorm(Z1),4)-round(pnorm(Z0),4), '.$$'),
                paste0('Step 4. That is, $$ P(', V0, '< \\bar{X} <', V1,') = ', round(pnorm(Z1),4)-round(pnorm(Z0),4) ,'.$$')
                )
            },
        #####
        if(input$problem_type=='1' & input$range_type == '2'){ 
            #####################
            GV <- input$GV
            GZ <- round((GV-mu)/s.xbar,4) 
            #####    
                withMathJax(
                    strong('Question:', style="color:darkred; font-weight:bold;"),
                    paste('$ P( \\bar{X} >', GV,') = ? $'),
                    br(),
                    br(),
                    strong('Solution:', style="color:darkred; font-weight:bold;"),
                    paste('The answer is given in the following steps.'),
                    br(),
                    br(),
                    paste('Step 1. Z-score Transformation'),
                    #br(),
                    paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                    paste('Step 2. The Z-score for $V_0$ = ', GV, 'is'),
                    br(),
                    paste0('$$Z_0 = \\frac{',GV, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', GZ, '$$'),
                    paste('Step 3. Note that'),
                    paste0('$$P( Z < ', GZ, ') = ',round(pnorm(GZ),4), '$$'),
                    paste0('Step 4. Therefore, '),
                    paste0('$$P(\\bar{X} >', GV,') = P \\left(  Z >', GZ,  '\\right)$$'),
                    paste0('$$= 1- P \\left(Z < ',GZ , '\\right)$$'),
                    paste0('$$= 1 - ',round(pnorm(GZ),4) , ' = ', round(1-pnorm(GZ),4), '$$')
                )
        },    
      if(input$problem_type=='1' & input$range_type == '3'){ 
          #####################
          LV <- input$LV
          LZ <- round((LV-mu)/s.xbar,4) 
          #####    
              withMathJax(
              strong('Question:', style="color:darkred; font-weight:bold;"),
              paste('$ P( \\bar{X} <', LV,') = ?$'),
              br(),
              br(),
              strong('Solution:', style="color:darkred; font-weight:bold;"),
              paste('The answer is given in the following steps.'),
              br(),
              br(),
              paste('Step 1. Recall the Z-score transformation'),
              paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} .$$'),
              paste('Step 2. Z-scores for $V_0$ =', LV, 'is given by'),
              br(),
              paste0('$$Z_0 = \\frac{',LV, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', LZ, '.$$'),
              #paste('Step 3. Finding the left-tail Probabilities'),
              #paste0('$$P( Z <', LZ, ') = ',round(pnorm(LZ),4), '$$'),
              paste0('Step 3. Note that'),
              paste0('$$P( \\bar{X} <', LV,')$$'),
              paste0('$$= P \\left(Z < ',LZ , '\\right) =' ,round(pnorm(LZ),4) , '.$$'),
              paste0('Step 4. Therefore, $$ P( \\bar{X} <', LV,') = ', round(pnorm(LZ),4) ,' .$$')
          )
      },
      #####
      ############################
      ######  Quantile  ##########
      ############################
      if(input$problem_type=='2' & input$prob_type == '1'){
          ####
          ###### Xo < Xbar < V
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability MUST be between 0 and 1.")
          rV = input$rV
          xlV = qnorm((pnorm(rV, mu, s.xbar) - p0), mu, s.xbar)     # X_0
          if (pnorm(rV, mu, s.xbar) < p0) stop("The input value is too small!")
          #########
          V0 <- min(rV, xlV)
          V1 <- max(rV, xlV)
          ####
          Z0 <- round((V0 - mu)/s.xbar,2)
          Z1 <- round((V1 - mu)/s.xbar,2)
          #####################
          withMathJax(
            strong('Question:', style="color:darkred; font-weight:bold;"),
            paste('Given $P({X}_0 < \\bar{X} <', rV, ') =', p0, '$, what is ${X}_0$?'),
            br(),
            br(),
            strong('Solution:', style="color:darkred; font-weight:bold;"),
            paste('The answer is given in the following steps.'),
            br(),
            br(),            
            paste('Step 1. Z-score Transformation'),
            #br(),
            paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} .$$'),
            #br(),
            paste('Step 2. Find the Z-score for the given $V$.'),
            br(),
            paste0('$$Z_V = \\frac{',V1, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z1, '.$$'),
            paste('Step 3. Find the Z-score corresponding to $X_0$'),
            paste0('$$P( Z_0 <  Z <', Z1, ') = ',p0, ',$$'),
            ###
            paste0('$$P( Z <', Z1,') - P(Z <Z_0 )= ', p0, ',$$'),
            paste0('$$P( Z < Z_0 ) =  P(Z <',Z0, ') -', p0,' = ',round(pnorm(Z1) -p0,4) ,',$$'),
            ###
            paste('   \ \ \ \  which gives $$Z_0  = ',round(qnorm(pnorm(Z1)-p0),2), '.$$'),
            paste0('Step 4. Note that $$\\frac{X_0 -',mu, '}{',sigma, '/ \\sqrt{',nn,'}} = Z_0 =', round(qnorm(pnorm(Z1)-p0),2), '.$$'),
            paste0('Step 5. Therefore, ${X}_0 =',mu,'+ (', round(qnorm(pnorm(Z1)-p0),2),')\\times',sigma,'/\\sqrt{',nn,'} =', round(mu+round(qnorm(pnorm(Z1)-p0),2)*s.xbar,2),'.$')
          )
      },
      #####
      if(input$problem_type=='2' & input$prob_type == '2'){
          ####
          ###### V < Xbar < Xo
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          lV = input$lV
          if(pnorm(lV, mu, s.xbar) > (1- p0)) stop("The input value is too big!")
          ####
          xrV = qnorm(pnorm(lV, mu, s.xbar) + p0, mu, s.xbar)
          #########
          V0 <- min(lV, xrV)
          V1 <- max(lV, xrV)
          ####
          Z0 <- round((V0 - mu)/s.xbar,2)
          Z1 <- round((V1 - mu)/s.xbar,2)
          #####################
          withMathJax(
              strong('Question:', style="color:darkred; font-weight:bold;"),
              paste('Given $P(',lV,'< \\bar{X} < X_0) =', p0, '$, what is $X_0$?'),
              br(),
              br(),
              strong('Solution:', style="color:darkred; font-weight:bold;"),
              paste('The answer is given in the following steps.'),
              br(),
              br(), 
              paste('Step 1. Z-score Transformation'),
              #br(),
              paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} .$$'),
              #br(),
              paste('Step 2. The Z-scores for the given $V$ =',lV ,' and $X_0$ are given below.'),
              br(),
              paste0('$$Z_V = \\frac{',V0, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z0, '\\text{  and  } Z_0 = \\frac{ X_0 -', mu, '}{',sigma, '/ \\sqrt{',nn,'}}. $$'),
              paste('Step 3. The given $P(',lV,'< \\bar{X} < X_0) =', p0, '$ is equivalent to'),
              paste0('$$P(', Z0,' <  Z < Z_0 ) = ', p0, '$$'),
              paste0('$$P( Z < Z_0 ) - P(Z <',Z0, ')= ', p0, '$$'),
              paste0('$$P( Z < Z_0 ) = P(Z <',Z0, ') +', p0, ' = ',round(pnorm(Z0) +p0,4) ,',$$'),
              paste('  \  \  \     which gives $$Z_0  = ',round(qnorm(pnorm(Z0)+p0),2), '.$$'),
              paste0('Step 4. Note that $$\\frac{X_0 -',mu, '}{',sigma, '/ \\sqrt{',nn,'}} = Z_0 =', round(qnorm(pnorm(Z0)+p0),2), '.$$'), 
              paste0('Step 5. Therefore $X_0 =',mu,'+ (', round(qnorm(pnorm(Z0)+p0),2),')\\times',sigma,'/\\sqrt{',nn,'} =', round(mu+round(qnorm(pnorm(Z0)+p0),2)*s.xbar,2),'.$'),
              br()
          )
      },        
      #####
      if(input$problem_type=='2' & input$prob_type == '3'){
          ####
          ###### Xbar > Xo
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          xlV = qnorm(1- p0, mu, s.xbar)
          ####
          Z0 <- round((xlV - mu)/s.xbar,2)
          #####################
          withMathJax(
            strong('Question:', style="color:darkred; font-weight:bold;"),
            paste('Given $P(\\bar{X} > X_0) =', p0, '$, what is $X_0$?'),
            br(),
            br(),
            strong('Solution:', style="color:darkred; font-weight:bold;"),
            paste('The answer is given in the following steps.'),
            br(),
            br(),            
            paste('Step 1. Z-score Transformation'),
            #br(),
            paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} \\text{  and  } Z_0 = \\frac{ \\bar{X}_0 -', mu, '}{',sigma, '/ \\sqrt{',nn,'}}. $$'),
            #br(),
            paste('Step 2. Find the Z-score corresponding to $X_0$'),
            paste0('$$P( Z  >  Z_0) = ', p0, ',$$'),
            paste('  \  \  \     or equivalent, $$P( Z  <  Z_0) = 1 -', p0, ' = ', 1 - p0, '.$$'),
            br(),
            paste(' Therefore, $$Z_0 = ', round(qnorm(1-p0),2), '.$$'),
            paste0('Step 3. Note that $$\\frac{X_0 -',mu, '}{',sigma, '/ \\sqrt{',nn,'}} = Z_0 =', round(qnorm(1-p0),2), '.$$'), 
            paste0('Step 4. Solve for $X_0$, we have  $X_0 =',mu,'+ (', round(qnorm(1-p0),2),')\\times',sigma,'/\\sqrt{',nn,'} =', round(mu+round(qnorm(1-p0),2)*s.xbar,2),'$')
          )
      }, 
      #####
      if(input$problem_type=='2' & input$prob_type == '4'){
          ####
          ###### Xbar < Xo
          p0 = input$prob
          if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
          xlV = qnorm(p0, mu, s.xbar)
          ####
          Z0 <- round((xlV - mu)/s.xbar,2)
          #####################
          withMathJax(
            strong('Question:', style="color:darkred; font-weight:bold;"),
            paste('Given $P(\\bar{X} < X_0) =', p0, '$, what is $X_0$?'),
            br(),
            br(),
            strong('Solution:', style="color:darkred; font-weight:bold;"),
            paste('The answer is given in the following steps.'),
            br(),
            br(),            
            paste('Step 1. Z-score Transformation'),
            br(),
            paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} \\text{ and  } Z_0 = \\frac{ \\bar{X}_0 -', mu, '}{',sigma, '/ \\sqrt{',nn,'}}   . $$'),
            br(),
            paste('Step 2. Find the Z-score corresponding to $X_0$'),
            paste0('$$P( Z  <  Z_0) = ', p0, '.$$'),
            paste(' Therefore,   $$Z_0 = ', round(qnorm(p0),2), '.$$'),
            paste0('Step 3. Therefore, $$\\frac{X_0 -',mu, '}{',sigma, '/ \\sqrt{',nn,'}} = Z_0 =', round(qnorm(p0),2), '.$$'), 
            paste0('Step 4. Solve for $X_0$, we have $$X_0 =',mu,'+ (', round(qnorm(p0),2),')\\times',sigma,'/\\sqrt{',nn,'} =', round(mu+round(qnorm(p0),2)*s.xbar,2),'.$$')
          )
      },      
    ) # close fluidPage  
    ####
    })
    
    
 
    
 #################
    output$panel3 <- renderUI({
        mu <- input$mu
        sigma <- input$sigma
        nn <- input$nc
        s.xbar <- sigma/sqrt(nn)
     fluidPage(
        ##############################
        if(input$problem_type=='1' & input$range_type == '1'){
        ##############################
         V0 <- min(input$v00, input$v01)
         V1 <- max(input$v00, input$v01)
         Z0 <- round((V0 - mu)/s.xbar,2)
         Z1 <- round((V1 - mu)/s.xbar,2)
        ##############################
                withMathJax(
                    br(),
                    paste('Step 1. Z-score Transformation'),
                    #br(),
                    paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                    #br(),
                    paste('Step 2. Z-scores for $V_0$ and $V_1$'),
                    br(),
                    paste0('$$Z_0 = \\frac{',V0, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z0, '$$'),
                    paste0('$$Z_1 = \\frac{',V1, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z1, '$$'),
                    paste('Step 3. Finding the left-tail Probabilities'),
                    paste0('$$P( Z <', Z1, ') = ',round(pnorm(Z1),4), '$$'),
                    paste0('$$P( Z <', Z0, ') = ',round(pnorm(Z0),4), '$$')
                )
            },
        if(input$problem_type=='1' & input$range_type == '2'){ 
            GV <- input$GV
            GZ <- round((GV-mu)/s.xbar,4) 
            #####    
                withMathJax(
                    br(),
                    paste('Step 1. Z-score Transformation'),
                    #br(),
                    paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                    paste('Step 2. Z-scores for $V$'),
                    br(),
                    paste0('$$Z_0 = \\frac{',GV, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', GZ, '$$'),
                    paste('Step 3. Finding the left-tail Probabilities'),
                    paste0('$$P( Z < ', GZ, ') = ',round(pnorm(GZ),4), '$$')
                )
        },
        if(input$problem_type=='1' & input$range_type == '3'){ 
            LV <- input$LV
            LZ <- round((LV-mu)/s.xbar,4)    
            #####    
                withMathJax(
                    br(),
                    paste('Step 1. Z-score Transformation'),
                    #br(),
                    paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                    paste('Step 2. Z-scores for $V$'),
                    br(),
                    paste0('$$Z_0 = \\frac{',LV, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', LZ, '$$'),
                    paste('Step 3. Finding the left-tail Probabilities'),
                    paste0('$$P( Z <', LZ, ') = ',round(pnorm(LZ),4), '$$')
                )
        }, 
        
        ############################
        ######  Quantile  ##########
        ############################ 
        ##############################
        if(input$problem_type=='2' & input$prob_type == '1'){
            ###### Xo < Xbar < V
            p0 = input$prob
            if(p0 < 0 | p0 > 1) stop("A probability MUST be between 0 and 1.")
            rV = input$rV
            xlV = qnorm((pnorm(rV, mu, s.xbar) - p0), mu, s.xbar)     # X_0
            if (pnorm(rV, mu, s.xbar) < p0) stop("The input value is too small!")
            #########
            V0 <- xlV
            V1 <- rV
            ####
            Z0 <- round((V0 - mu)/s.xbar,2)
            Z1 <- round((V1 - mu)/s.xbar,2)
            ##############################
            withMathJax(
                br(),
                paste('Step 1. Z-score Transformation'),
                #br(),
                paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                #br(),
                paste('Step 2. Find the Z-score for the given $V$.'),
                br(),
                paste0('$$Z_V = \\frac{',V1, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z1, '$$'),
                paste('Step 3. Find the Z-score corresponding to $X_0$'),
                paste0('$$P( Z_0 <  Z <', Z1, ') = ',p0, '$$'),
                ###
                paste0('$$P( Z <', Z1,') - P(Z <Z_0 )= ', p0, '$$'),
                paste0('$$P( Z < Z_0 ) =  P(Z <',Z0, ') -', p0,' = ',round(pnorm(Z1) -p0,4) ,'$$'),
                ###
                paste('  \  \  \     which gives $Z_0  = ',round(qnorm(pnorm(Z1)-p0),2), '$')
            )
        },
        #######
        if(input$problem_type=='2' & input$prob_type == '2'){
            ###### V < Xbar < Xo
            p0 = input$prob
            if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
            lV = input$lV
            if(pnorm(lV, mu, s.xbar) > (1- p0)) stop("The input value is too big!")
            xrV = qnorm((pnorm(lV, mu, s.xbar) + p0), mu, s.xbar)
            #########
            V0 <- min(lV, xrV)
            V1 <- max(lV, xrV)
            ####
            Z0 <- round((V0 - mu)/s.xbar,2)
            Z1 <- round((V1 - mu)/s.xbar,2)
            ##############################
            withMathJax(
                br(),
                paste('Step 1. Z-score Transformation'),
                #br(),
                paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}} $$'),
                #br(),
                paste('Step 2. Find the Z-score for the given $V$.'),
                br(),
                paste0('$$Z_V = \\frac{',V0, ' - ', mu, '}{',sigma, '/ \\sqrt{',nn,'}} = ', Z0, '$$'),
                paste('Step 3. Find the Z-score corresponding to $X_0$'),
                paste0('$$P(', Z0,' <  Z < Z_0 ) = ', p0, '$$'),
                paste0('$$P( Z < Z_0 ) - P(Z <',Z0, ')= ', p0, '$$'),
                paste0('$$P( Z < Z_0 ) = P(Z <',Z0, ') +', p0, ' = ',round(pnorm(Z0) +p0,4) ,'$$'),
                paste('  \  \  \     which gives $Z_0  = ',round(qnorm(pnorm(Z0)+p0),2), '$')
            )
        },
        if(input$problem_type=='2' & input$prob_type == '3'){
            ######  P[ bar(X) > Xo]
            p0 = input$prob
            if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
            xV = qnorm((1-p0), mu, s.xbar)
            #########
            ####
            Z0 <- round((xV - mu)/s.xbar,2)
            ##############################
            withMathJax(
                br(),
                paste('Step 1. Z-score Transformation'),
                #br(),
                paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}}. $$'),
                #br(),
                paste('Step 2. Find the Z-score corresponding to $X_0$'),
                paste0('$$P( Z  >  Z_0) = ', p0, ',$$'),
                paste('  \  \  \     or equivalent, $1 -P( Z  <  Z_0) = ', p0, '.$'),
                br(),
                br(),
                paste(' Therefore,   $Z_0 = ', round(qnorm(1-p0),2), '.$')
            )
        },
        #########
        if(input$problem_type=='2' & input$prob_type == '4'){
            ######  P[ bar(X) < Xo]
            p0 = input$prob
            if(p0 < 0 | p0 > 1) stop("A probability must be between 0 and 1!")
            xV = qnorm((p0), mu, s.xbar)
            #########
            ####
            Z0 <- round((xV - mu)/s.xbar,2)
            ##############################
            withMathJax(
                br(),
                paste('Step 1. Z-score Transformation'),
                #br(),
                paste0('$$ Z = \\frac{ \\bar{X} -', mu, '}{',sigma, '/ \\sqrt{',nn,'}}. $$'),
                #br(),
                paste('Step 2. Find the Z-score corresponding to $X_0$'),
                paste0('$$P( Z  <  Z_0) = ', p0, '.$$'),
                br(),
                br(),
                paste(' Therefore,   $Z_0 = ', round(qnorm(p0),2), '.$')
            )
        }
        )  # close fluidPage  
   ####    
    })
}
######
shinyApp(ui, server)






