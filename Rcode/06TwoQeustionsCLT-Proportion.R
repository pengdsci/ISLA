#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##
##  Author:  Cheng Peng
##    Date:  2/14/2022
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
            titlePanel( 
                     h3("ISLA: Application of CLT: Sample Proportions", 
                           align = "center", style = "color:gold; font-family:verdana; 
                      font-variant: small-caps; font-weight: bold;") )
     ),
  
    ############################################################################
    fluidRow(
        column(4, wellPanel(style = "background-color: #5e2442; color:gold; ", 
          tags$div('The primary interest of applying the CLT to sample proportion 
                   is to find the probability of an event defined by the sampling 
                   distribution of sample proportions.', 
                   style='font-size: small; color: skyblue;'),
            hr(),
             radioButtons('range_type', label='1. Which Probability to Find?',
                                          choices=list('$P[ p_0 <\\hat{p} < p_1] = ?$' = 1 ,
                                              '$P[ \\hat{p} > p_0] = ?$' = 2,
                                              '$P[ \\hat{p} < p_0] = ?$' = 3), 
                                          selected=1),  
               ###########
            conditionalPanel(condition="input.range_type == '1'",
                            numericInput('p00', label='Given Value #1: $p_0$', 
                                         min = 0, max = 1, value=0.45, step=.000001), 
                            ########################################################
                            numericInput('p01', label='Given Value #2: $p_1$', 
                                         min = 0, max = 1, value=0.52, step=.000001), 
              ),
              ######
              conditionalPanel(condition="input.range_type == '2'",
                             numericInput('Gp', label='Given Value: $p_0$', 
                                          min = 0, max = 1, value=0.535, step=.000001), 
              ),
              #######
              conditionalPanel(condition="input.range_type == '3'",
                             numericInput('Lp', label='Given Value: $p_0$', 
                                          min = 0, max = 1, value=0.465, step=.000001), 
              ),
            ##############################################################################################
            ####################################################################################
            
            hr(),
            h5(strong('2. Input Information')),
            numericInput('p', label='Population Proportion: $p$', 
                        min = 0, max = 1, value=0.5), 

            numericInput('nc', label='Sample Size: $n$', min=1, max = NA, 
                        value=50, step=1),
 
        hr(),
        HTML('<p><center> <img src="https://raw.githubusercontent.com/pengdsci/ISLA/main/image/ISLAlogo.png"  width="100" height="100"></center></p>'),
        HTML('<p style="font-family:Courier; color:Red; font-size: 20px;"><center> <a href="mailto:cpeng@wcupa.edu"><font size =2 color = "skyblue">Report bugs to C. Peng</font></a> </center></p>')
        
        )),
     ###########################################################################
        column(8, wellPanel(style = "background-color:lightgray", 
            plotOutput('genNormCurve', height="200px"),
            ######
            uiOutput('panel2'), 
        )), 
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
        p <- input$p
        sigma <- sqrt(p*(1-p))
        nn <- input$nc
        s.phat <- sigma/sqrt(nn)
        #####################
        #################################
        ##    Two Density Curves
        #################################
        nf <- layout(
          matrix(c(1,2,3), ncol=3, byrow=TRUE), 
          widths=c(2,1,2), 
          #heights=c(2,2)
        )
        
        par(oma = c(1,1,0,1), mar = c(1,1,1,1), bg = "lightgray")
        #####################################################
        ##############  Asymptotic Normal Curve #############
        #####################################################
        xseq = seq(p-4*s.phat, p+4*s.phat, length = 200)
        
        ymax = dnorm(p, p, s.phat)
        plot(xseq, dnorm(xseq, mean = p, sd = s.phat), type = "l", lty = 1, 
             main = "Sampling Distribution of Sample Proportions",
             ylim=c(-0.1*ymax, ymax),
             xlab= '', 
             ylab='',
             cex.main = 1.5,
             axes = FALSE)
        #axis(1, pos = 0)
        segments(p-4*s.phat, 0, p+4*s.phat, 0 )
        ###############################################################
        #####    Conditional statements for shade the regions
        ###############################################################
        ###
        if(input$range_type == '1'){ # between
            p0 <- min(input$p00, input$p01) 
            p1 <- max(input$p00, input$p01)
            if(p0 < (p-4*s.phat) & p1 < (p+4*s.phat) ){
                v.seq = seq((p-4*s.phat), p1, length = 200)
                y.seq = dnorm(v.seq, p, s.phat)
                polygon(c((p-4*p),v.seq, p1),c(0,y.seq,0),col="lightblue") 
                text(c((p-4*s.phat),p1), c(-0.08*ymax,-0.08*ymax), c(expression('p'[0]), expression('p'[1])), col = "red", cex = 1.5)    
            } else if(p0<(p -4*s.phat) & p1 > (p+4*s.phat)){
                v.seq = seq((p -4*s.phat), (p + 4*s.phat), length = 200)
                y.seq = dnorm(v.seq, p, s.phat)
                polygon(c((p -4*s.phat),v.seq, (p+4*s.phat)),c(0,y.seq,0),col="lightblue") 
                text(c((p -4*s.phat),(p+4*s.phat)), c(-0.08*ymax,-0.08*ymax), c(expression('p'[0]), expression('p'[1])), col = "red", cex = 1.5)    
            } else{
                v.seq = seq(p0, p1, length = 200)
                y.seq = dnorm(v.seq, p, s.phat)
                polygon(c(p0,v.seq, p1),c(0,y.seq,0),col="lightblue")
                text(c(p0,p1), c(-0.08*ymax,-0.08*ymax), c(expression('p'[0]), expression('p'[1])), col = "red", cex = 1.5)
            }
            #####
            text(p-2.5*s.phat, 0.92*ymax, "Given Information", col = "red", cex = 1.5)
            text(p-2.5*s.phat, 0.8*ymax, paste('p0 = ',p0), col = "red", cex = 1.5)
            text(p-2.5*s.phat, 0.69*ymax, paste('p1 = ',p1), col = "red", cex = 1.58)
        }
        ######
        if( input$range_type == '2'){ # bigger than
            Gp <- input$Gp
            if(Gp < (p+4*s.phat) & Gp > (p -4*s.phat)){
               v.seq = seq(Gp, p+4*s.phat, length = 200)
               y.seq = dnorm(v.seq, p, s.phat)
               polygon(c(Gp,v.seq, p+4*s.phat),c(0,y.seq,0),col="lightblue")
               text(c(Gp), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)
            } else if (Gp < (p-4*s.phat) ){
                v.seq = seq((p-4*p), p+4*p, length = 200)
                y.seq = dnorm(v.seq, p, s.phat)
                polygon(c((p-4*s.phat),v.seq, p+4*s.phat),c(0,y.seq,0),col="lightblue")
                text(c((p-4*s.phat)), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)
                text(c(p-4*s.phat), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)                
            } else{
                text(c(p+4*s.phat), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)  
            }
            ####
              text(p-2.5*s.phat, 0.9*ymax, "Given Information", col = "red", cex = 1.5)
              text(p-2.5*s.phat, 0.72*ymax, paste('p0 = ',Gp), col = "red", cex = 1.5)
         }
        ######
        if(input$range_type == '3'){ # less than
            Lp <- input$Lp
            
            if(Lp > p-4*s.phat & Lp < p + 4*s.phat){
               v.seq = seq (p-4*s.phat, Lp, length = 200)
               y.seq = dnorm(v.seq, p, s.phat)
               polygon(c(p-4*p,v.seq, Lp),c(0,y.seq,0),col="lightblue") 
               text(c(Lp), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)
            } else if(Lp < p-4*s.phat){
                text(c(p-4*s.phat), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5) 
            } else{
                v.seq = seq (p-4*s.phat, p+4*s.phat, length = 200)
                y.seq = dnorm(v.seq, p, s.phat)
                polygon(c(p-4*s.phat,v.seq, p+4*s.phat),c(0,y.seq,0),col="lightblue")
                text(c(p + 4*s.phat), c(-0.08*ymax), c(expression('p'[0])), col = "red", cex = 1.5)
            }
            ###            
            text(p-2.5*s.phat, 0.9*ymax, "Given Information", col = "red", cex = 1.5)
            text(p-2.5*s.phat, 0.72*ymax, paste('p0 = ',Lp), col = "red", cex = 1.5)
           }
        
        ##############################################################################
        #######################   Z-score Transformation #############################
        ##############################################################################
        
        #par(bg = "lightgray")
        plot(1,1, type = "n", axes=FALSE)
        text(1,1, expression(Z == frac(hat(p)-p, sqrt(frac(p(1-p),n)))), cex = 2, col = "darkred")
        
        
        ##############################################################################
        ###                   Standard Normal Density
        ##############################################################################
        p <- input$p
        sigma <- sqrt(p*(1-p))
        nn <- input$nc
        s.phat <- sigma/sqrt(nn)
        ##############################
        x <- seq(-4, 4, by=.01)
        #par(oma = c(1,1,0,1), mar = c(1,1,1,1))
        #par(bg = "lightgray")
        plot(x, dnorm(x), type='l', main='Standard Normal Distribution N(0,1)', 
             ylab='', xlab='Z Score', axes = FALSE, cex.main = 1.5,
             ylim=c(-0.1*dnorm(0),dnorm(0))
        )
        segments(-4, 0, 4, 0)
        ###################################################### 
        ##  Same conditions used in the sampling distribution
        ####################################################### 
        ###
        if(input$range_type == '1'){ 
          # between: P(v0 <xbar <v1)
          p0 <- min(input$p00, input$p01) 
          p1 <- max(input$p00, input$p01)
          Z0 <- (p0 - p)/s.phat
          Z1 <- (p1 - p)/s.phat
          zmax = dnorm(0)
          ##### end point handling
          if(Z0 < -4 & Z1 < 4){ 
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col="lightblue")
            text(c(-4,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)  
          } else if (Z0 < -4 & Z1 > 4){
            z.seq02 = seq(-4, 4, length = 200)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, 4),c(0,yz.seq02,0), col= "lightblue" )
            text(c(-4,4), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)    
          } else if (Z0 > -4 & Z1 > 4){
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, 4, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, 4),c(0,yz.seq02,0), col= "lightblue" )
            segments(z.seq01, rep(0,round(8*(4+4))), z.seq01, dnorm(z.seq01), col= "darkred" )
            text(c(Z0,4), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)  
          } else {
            z.seq01 = seq(-4, Z0, length = round(8*(Z0+4)))
            z.seq02 = seq(-4, Z1, length = 200)
            yz.seq01 = dnorm(z.seq01, 0, 1)
            yz.seq02 = dnorm(z.seq02, 0, 1)
            polygon(c(-4,z.seq02, Z1),c(0,yz.seq02,0), col= "lightblue" )
            segments(z.seq01, rep(0,round(8*(Z1+4))), z.seq01, dnorm(z.seq01), col= "darkred" )
            text(c(Z0,Z1), c(-0.08*zmax,-0.08*zmax), c(expression('Z'[0]), expression('Z'[1])), col = "blue", cex = 1.5)
          }
        }
        ######
        if(input$range_type == '2'){ 
          ### P(xbar > Xo
          Gp <- input$Gp
          GZ <- (Gp-p)/s.phat
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
        if(input$range_type == '3'){ 
          ### P(xbar < Vo)
          Lp <- input$Lp
          LZ <-(Lp-p)/s.phat
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
            text(c(LZ), c(-0.08*zmax), c(expression('Z'[0])), col = "blue", cex = 1.5)
          }
        }
        ###############################################################
    }, height = 200, width = 700 )
    
    
    
    
    ######
    ####################################################################
    #########    Output of the calculations: Sampling distribution
    ####################################################################
    output$panel2 <- renderUI({
    ####################
      p <- input$p
      sigma <- sqrt(p*(1-p))
      nn <- input$nc
      s.phat <- sigma/sqrt(nn)
   fluidPage(
     ########
      if(input$range_type == '1'){ 
        ######
        p0 <- min(input$p00, input$p01)
        p1 <- max(input$p00, input$p01)
        Z0 <- round((p0 - p)/s.phat,2)
        Z1 <- round((p1 - p)/s.phat,2)
        #####################
          withMathJax(
            strong('Question:', style="color:darkred; font-weight:bold;"),
            paste('$ P(', p0, '< \\hat{p} <', p1,') = ?  $'),
            br(),
            br(),
            strong('Solution:', style="color:darkred; font-weight:bold;"),
            paste('The answer is given in the following steps.'),
            br(),
            br(),  
            paste('Step 1. Recall that Z-score Transformation has the following'),
            #br(),
            paste0('$$ Z = \\frac{ \\hat{p} -', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}} .$$'),
            #br(),
            paste('Step 2. Z-scores for $p_0 =$',p0 , 'and $p_1=$',p1, 'are given respectively by' ),
            br(),
            paste0('$$Z_0 = \\frac{',p0, ' - ', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}} = ', round(Z0,2), '\\text{  and  } Z_1 = \\frac{',p1, ' - ', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}} = ', round(Z1,2), '.$$'),
            paste('Step 3. The the left-tail probabilities corresponding to the above two z-scores are'),
            paste0('$$P( Z <', round(Z1,2), ') = ',round(pnorm(Z1),4), '\\text{  and  }  P( Z <', round(Z0,2), ') = ',round(pnorm(Z0),4), '.$$'),

                paste0('Step 4. Note that $$ P(', p0, '< \\hat{p} <', p1,') =  P \\left( Z_0 < Z < Z_1  \\right)$$'),
                paste0('$$= P \\left( Z < Z_1  \\right) - P \\left(Z < Z_0  \\right) = P \\left( Z < ',Z1,  '\\right) - P \\left(Z < ',Z0 , '\\right)$$'),
                paste0('$$= ',round(pnorm(Z1),4) , ' - ',round(pnorm(Z0),4) , ' = ',round(pnorm(Z1),4)-round(pnorm(Z0),4), '.$$'),
                paste0('Step 5. That is, $ P(', p0, '< \\hat{p} <', p1,') = ', round(pnorm(Z1),4)-round(pnorm(Z0),4) ,' .$')
                )
            },
        #####
        if(input$range_type == '2'){ 
            #####################
            Gp <- input$Gp
            GZ <- round((Gp-p)/s.phat,4) 
            #####    
                withMathJax(
                  strong('Question:', style="color:darkred; font-weight:bold;"),
                  paste('$ P( \\hat{p} >', Gp,') = ?  $'),
                  br(),
                  br(),
                  strong('Solution:', style="color:darkred; font-weight:bold;"),
                  paste('The answer is given in the following steps.'),
                  br(),
                  br(),                  
                  paste('Step 1. Recall the Z-score Transformation'),
                  paste0('$$ Z = \\frac{ \\hat{p} -', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}}  .$$'),
                  paste('Step 2. Z-score for $p_0$ =', Gp, ' is given by'),
                  br(),
                  paste0('$$Z_0 = \\frac{',Gp, ' - ', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}} = ', GZ, '.$$'),
                  paste('Step 3. Finding the left-tail probabilities'),
                  paste0('$$P( Z < ', GZ, ') = ',round(pnorm(GZ),4), '.$$'),                  
                  paste0('Step 4. Note that $$ P( \\hat{p} >', Gp,') = P \\left(  Z > Z_1  \\right)$$'),
                  paste0('$$= 1- P \\left(Z < ',GZ , '\\right)= 1 - ',round(pnorm(GZ),4) , ' = ', round(1-pnorm(GZ),4), '.$$'),
                  paste0('Step 5. That is, $ P( \\hat{p} >', Gp,') = ', round(1-pnorm(GZ),4) ,' .$')
                )
        },    
      if(input$range_type == '3'){ 
          #####################
          Lp <- input$Lp
          LZ <- round((Lp-p)/s.phat,4) 
          #####    
          withMathJax(
            strong('Question:', style="color:darkred; font-weight:bold;"),
            paste('$ P( \\hat{p} <', Lp,') = ?  $'),
            br(),
            br(),
            strong('Solution:', style="color:darkred; font-weight:bold;"),
            paste('The answer is given in the following steps.'),
            br(),
            br(),             
            paste('Step 1. Recall that Z-score Transformation has the following form'),
            #br(),
            paste0('$$ Z = \\frac{ \\hat{p} -', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}}  .$$'),
            paste('Step 2. The Z-score of $ p_0 =',Lp ,'$'),
            br(),
            paste0('$$Z_0 = \\frac{',Lp, ' - ', p, '}{ \\sqrt{ \\frac{',p,'\\times (1 -',p ,')}{',nn,'}}}  = ', LZ, '.$$'),
            paste('Step 3. The left-tail probability of the above z-score is given by'),
            paste0('$$P( Z <', LZ, ') = ',round(pnorm(LZ),4), '.$$'),
            paste0('Step 4. Note that $$P( \\hat{p} <', Lp,') = P \\left(  Z < Z_0  \\right)$$'),
            paste0('$$= P \\left(Z < ',LZ , '\\right) =' ,round(pnorm(LZ),4) , '.$$'),
            paste0('Step 5. That is, $ P( \\hat{p} <', LZ,') = ', round(pnorm(LZ),4) ,' .$')
          )
      },
      #####
    ) # close fluidPage  
    ####
    })
    

}
######
shinyApp(ui, server)






