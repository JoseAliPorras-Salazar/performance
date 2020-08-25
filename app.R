rm(list=ls())

# Load packages ###################################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

#shinythemes

#Color coding
#Viridis 440154, 482878, 3E4A89, 31688E, 26828E, 1F9E89, 35B779, 6DCD59, B4DE2C, FDE725

#VAMOS A COLOCAR ESTO POR AQUI

#Theme
Thm <-   theme(
    panel.grid.major = element_line (colour = "white", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color  =  NA),
    axis.ticks = element_line(color = "black", size  =  0.5),
    axis.ticks.length = unit(0.2, "lines"),   
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.title = element_text(size = 10),
    text = element_text(size= 10),
    legend.position="top",legend.key=element_blank()
)

#Load data ########################################################################################
#Temperature
df.T <- read.csv("C:/Users/sbbjap/Dropbox/02 Jose/20 SBB/02 Literature review/TOWP_DF01.csv")
df.T <- df.T[-c(1)]

#Thermal sensation
df.TS <- read.csv("C:/Users/sbbjap/Dropbox/02 Jose/20 SBB/02 Literature review/TSOWP_DF01.csv")
df.TS <- df.TS[-c(1)]

#UI ###############################################################################################
ui <- navbarPage("Effects of the Indoor Thermal Environment on Office Work Performance",
                 
#Tab 1 #####
    tabPanel("Temperature",
                          
    sidebarLayout(
        sidebarPanel(
                                  
            sliderInput(inputId ="range", 
                        label = "Select the temperature range:",
                        min = round(min(df.T$TLow),0), max = round(max(df.T$THigh),0),
                        value = c(min(df.T$AvgT), max(df.T$AvgT)), round = 0, step = 1),
                                  
            strong("Uncheck those factors you want to remove:"),
                                  
            checkboxGroupInput(inputId = "T.climate",
                               label = "Climate:",
                               choices = levels(df.T$Climate),
                               selected = levels(df.T$Climate)),
                                  
            checkboxGroupInput(inputId = "T.metric",
                               label = "Task metric:",
                               choices = levels(df.T$SA),
                               selected = levels(df.T$SA)),
                                  
                                  
            checkboxGroupInput(inputId = "T.complex",
                               label = "Task complexity:",
                               choices = levels(df.T$Hancock),
                               selected = levels(df.T$Hancock)),
                                  
            selectInput(inputId = "model",
                        label = "Choose the regression model to be fitted:",
                        choices = c("Linear", "Quadratic", "Cubic", "Quartic")),
                              
            checkboxInput(inputId = "pv",
                          label = "p-value:",
                          value = FALSE),
                              
            checkboxInput(inputId = "ar2",
                          label = "Adjusted R²:",
                          value = FALSE)),
                              
            mainPanel(plotOutput('T.plot1'),
                      plotOutput('T.plot2'),
                      textOutput('T.obs'),
                      textOutput('T.pv'),
                      textOutput('T.ar2'))
                          )),
                 
#Tab 2 #####
        tabPanel("Thermal Sensation",
             
        sidebarLayout(
            sidebarPanel(
                     
                sliderInput(inputId ="TS.range", 
                            label = "Select the thermal sensation range:",
                            min = round(min(df.TS$TSLow),2), max = round(max(df.TS$TSHigh),2),
                            value = c(min(df.TS$AvgTS), max(df.TS$AvgTS)), round = 2, step = 0.1),
                     
                strong("Uncheck those factors you want to remove:"),
                     
                checkboxGroupInput(inputId = "TS.climate",
                                   label = "Climate:",
                                   choices = levels(df.TS$Climate),
                                   selected = levels(df.TS$Climate)),
                     
                checkboxGroupInput(inputId = "TS.metric",
                                   label = "Task metric:",
                                   choices = levels(df.TS$SA),
                                   selected = levels(df.TS$SA)),
                     
                checkboxGroupInput(inputId = "TS.complex",
                                   label = "Task complexity:",
                                   choices = levels(df.TS$Hancock),
                                   selected = levels(df.TS$Hancock)),
                     
                selectInput(inputId = "TS.model",
                            label = "Choose the regression model to be fitted:",
                            choices = c("Linear", "Quadratic", "Cubic", "Quartic")),
                     
                checkboxInput(inputId = "TS.pv",
                              label = "p-value:",
                              value = FALSE),
                     
                checkboxInput(inputId = "TS.ar2",
                              label = "Adjusted R²:",
                              value = FALSE)),
                 
                mainPanel(plotOutput('TS.plot1'), 
                          plotOutput('TS.plot2'), 
                          textOutput('TS.obs'), 
                          textOutput('TS.pv'), 
                          textOutput('TS.ar2')) 
             ))
)

#SERVER ###########################################################################################
server <- function(input, output, session) {
    
#Reactive- Temperature #####    
    datab <- reactive ({df.T %>%
            subset(TLow >= input$range[1] & THigh <= input$range[2]) %>%
            dplyr::filter(Climate %in% input$T.climate & SA %in% input$T.metric & Hancock %in% input$T.complex)
    })
    
    Ta <- reactive ({c(seq(round(min(datab()$AvgT),0),round(max(datab()$AvgT),0),0.5))
    })
    
    b <- reactive ({if (input$model == "Linear") {b <- 1
    } else if (input$model == "Quadratic") {b <- 2
    } else if (input$model == "Cubic") {b <- 3
    } else {b <- 4
    }
    })
    
###  
#     piece.formula <- function(var.name, knots) {
#         formula.sign <- rep(" - ", length(knots))
#         formula.sign[knots < 0] <- " + "
#         paste(var.name, "+",
#               paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
#                     collapse = " + ", sep=""))
#     }}
# 
# #K <- 3  # number of knots/inflection points
# #knots <- seq(min(AvgT), max(AvgT), len = K + 2)[-c(1, K + 2)]
# #create a vector of k+2, equally spaced points
# #and remove the first and last points
# #the knots vector has k points now
# 
# knots <- c(23,25,27)
# 
# model.temp.5 <- lm(formula(paste("y ~", piece.formula("AvgT", knots))))
###    
    
    #Function to capture the coefficients according to the model
    T.fct <- reactive({function(database, x, y){
        
        fit <- lm(y~poly(x,b(), raw = T), database)
        
        if (b() == 1)        {c <- cbind(signif(summary(fit)$adj.r.squared, 5),
                                         signif(summary(fit)$coef[2,4], 5),
                                         signif(fit$coef[[1]],5),
                                         signif(fit$coef[[2]],5))
        } else if (b() == 2) {c <- cbind(signif(summary(fit)$adj.r.squared, 5),
                                         signif(summary(fit)$coef[2,4], 5),
                                         signif(fit$coef[[1]],5),
                                         signif(fit$coef[[2]],5),
                                         signif(fit$coef[[3]],5))
        } else if (b() == 3) {c <- cbind(signif(summary(fit)$adj.r.squared, 5),
                                         signif(summary(fit)$coef[2,4], 5),
                                         signif(fit$coef[[1]],5),
                                         signif(fit$coef[[2]],5),
                                         signif(fit$coef[[3]],5),
                                         signif(fit$coef[[4]],5))
        } else               {c <- cbind(signif(summary(fit)$adj.r.squared, 5),
                                         signif(summary(fit)$coef[2,4], 5),
                                         signif(fit$coef[[1]],5),
                                         signif(fit$coef[[2]],5),
                                         signif(fit$coef[[3]],5),
                                         signif(fit$coef[[4]],5),
                                         signif(fit$coef[[5]],5))
        }
        {T.COE <<-  c}}
    })
        
    ABC <- reactive ({T.fct()(datab(),datab()$AvgT, datab()$Lamb)})
    
    #Functions to transform the data according to the model
    T.fMOD.1 <- reactive ({function(equis) {
        MOD.1 <-matrix(NA,length (equis),1)
        MOD.1 <- data.frame(cbind(equis,MOD.1)); colnames(MOD.1) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.1$V[i] = 1}
            if (i > 1){MOD.1$V[i] = exp(1*(1/100) * (ABC()[1,3]*(MOD.1$equis[i]-MOD.1$equis[1]) + 
                                                     ABC()[1,4]/2*(MOD.1$equis[i]^2-MOD.1$equis[1]^2)))
            }}
        {T.MOD <<- MOD.1}}
    })
    
    T.fMOD.2 <- reactive ({function(equis) {
        MOD.2 <-matrix(NA,length (equis),1)
        MOD.2 <- data.frame(cbind(equis,MOD.2)); colnames(MOD.2) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.2$V[i] = 1}
            if (i > 1){MOD.2$V[i] = exp(1*(1/100) * (ABC()[1,3]*(MOD.2$equis[i]-MOD.2$equis[1]) + 
                                                     ABC()[1,4]/2*(MOD.2$equis[i]^2-MOD.2$equis[1]^2) +
                                                     ABC()[1,5]/3*(MOD.2$equis[i]^3-MOD.2$equis[1]^3)))
            }}
        {T.MOD <<- MOD.2}}
    })
        
    T.fMOD.3 <- reactive ({function(equis) {
        MOD.3 <-matrix(NA,length (equis),1)
        MOD.3 <- data.frame(cbind(equis,MOD.3)); colnames(MOD.3) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.3$V[i] = 1}
            if (i > 1){MOD.3$V[i] = exp(1*(1/100) * (ABC()[1,3]*(MOD.3$equis[i]-MOD.3$equis[1]) + 
                                                     ABC()[1,4]/2*(MOD.3$equis[i]^2-MOD.3$equis[1]^2) +
                                                     ABC()[1,5]/3*(MOD.3$equis[i]^3-MOD.3$equis[1]^3) +
                                                     ABC()[1,6]/4*(MOD.3$equis[i]^4-MOD.3$equis[1]^4)))
            }}
        {MOD <<- MOD.3}}
    })
        
    T.fMOD.4 <- reactive ({function(equis) {
        MOD.4 <-matrix(NA,length (equis),1)
        MOD.4 <- data.frame(cbind(equis,MOD.4)); colnames(MOD.4) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.4$V[i] = 1}
            if (i > 1){MOD.4$V[i] = exp(1*(1/100) * (ABC()[1,3]*(MOD.4$equis[i]-MOD.4$equis[1]) + 
                                                     ABC()[1,4]/2*(MOD.4$equis[i]^2-MOD.4$equis[1]^2) +
                                                     ABC()[1,5]/3*(MOD.4$equis[i]^3-MOD.4$equis[1]^3) +
                                                     ABC()[1,6]/4*(MOD.4$equis[i]^4-MOD.4$equis[1]^4) +
                                                     ABC()[1,7]/5*(MOD.4$equis[i]^5-MOD.4$equis[1]^5)))
            }}
        {T.MOD <<- cbind(MOD.4)}}
    })
    
    T.DEF.1 <- reactive ({T.fMOD.1()(Ta())})
    T.DEF.2 <- reactive ({T.fMOD.2()(Ta())})
    T.DEF.3 <- reactive ({T.fMOD.3()(Ta())})
    T.DEF.4 <- reactive ({T.fMOD.4()(Ta())})
    
    T.M <- reactive({if (b() == 1) { T.m <- T.DEF.1()
    } else if (b() == 2) { T.m <- T.DEF.2()  
    } else if (b() == 3) { T.m <- T.DEF.3() 
    } else { T.m <- T.DEF.4()  
    }   
    })
    
#Reactive- Thermal sensation #####
    datad <- reactive ({ df.TS  %>%
            subset(TSLow >= input$TS.range[1] & TSHigh <= input$TS.range[2]) %>%
            dplyr::filter(Climate %in% input$TS.climate & SA %in% input$TS.metric & Hancock %in% input$TS.complex)
    })
    
    Ts <- reactive ({ c(seq(round(min(datad()$AvgTS),2),round(max(datad()$AvgTS),2),0.1))
    })
    
    d <- reactive ({if (input$TS.model == "Linear") {d <- 1
    } else if (input$TS.model == "Quadratic") {d <- 2
    } else if (input$TS.model == "Cubic") {d <- 3
    } else {d <- 4
    }
    })
    
    #Function to capture the coefficients according to the model
    TS.fct <- reactive({function(database, x, y){
        
        fit.TS <- lm(y~poly(x,d(), raw = T), database)
        
        if (d() == 1)        {e <- cbind(signif(summary(fit.TS)$adj.r.squared, 5),
                                         signif(summary(fit.TS)$coef[2,4], 5),
                                         signif(fit.TS$coef[[1]],5),
                                         signif(fit.TS$coef[[2]],5))
        } else if (d() == 2) {e <- cbind(signif(summary(fit.TS)$adj.r.squared, 5),
                                         signif(summary(fit.TS)$coef[2,4], 5),
                                         signif(fit.TS$coef[[1]],5),
                                         signif(fit.TS$coef[[2]],5),
                                         signif(fit.TS$coef[[3]],5))
        } else if (d() == 3) {e <- cbind(signif(summary(fit.TS)$adj.r.squared, 5),
                                         signif(summary(fit.TS)$coef[2,4], 5),
                                         signif(fit.TS$coef[[1]],5),
                                         signif(fit.TS$coef[[2]],5),
                                         signif(fit.TS$coef[[3]],5),
                                         signif(fit.TS$coef[[4]],5))
        } else               {e <- cbind(signif(summary(fit.TS)$adj.r.squared, 5),
                                         signif(summary(fit.TS)$coef[2,4], 5),
                                         signif(fit.TS$coef[[1]],5),
                                         signif(fit.TS$coef[[2]],5),
                                         signif(fit.TS$coef[[3]],5),
                                         signif(fit.TS$coef[[4]],5),
                                         signif(fit.TS$coef[[5]],5))
        }
        {TS.COE <<-  e}}
    })
    
    XYZ <- reactive ({TS.fct()(datad(),datad()$AvgTS, datad()$Lamb)})
    
    #Functions to transform the data according to the model
    TS.fMOD.1 <- reactive ({function(equis) {
        MOD.1 <-matrix(NA,length (equis),1)
        MOD.1 <- data.frame(cbind(equis,MOD.1)); colnames(MOD.1) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.1$V[i] = 1}
            if (i > 1){MOD.1$V[i] = exp(10*(1/100) * (XYZ()[1,3]*(MOD.1$equis[i]-MOD.1$equis[1]) + 
                                                      XYZ()[1,4]/2*(MOD.1$equis[i]^2-MOD.1$equis[1]^2)))
            }}
        {TS.MOD <<- MOD.1}}
    })
    
    TS.fMOD.2 <- reactive ({function(equis) {
        MOD.2 <-matrix(NA,length (equis),1)
        MOD.2 <- data.frame(cbind(equis,MOD.2)); colnames(MOD.2) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.2$V[i] = 1}
            if (i > 1){MOD.2$V[i] = exp(10*(1/100) * (XYZ()[1,3]*(MOD.2$equis[i]-MOD.2$equis[1]) + 
                                                      XYZ()[1,4]/2*(MOD.2$equis[i]^2-MOD.2$equis[1]^2) +
                                                      XYZ()[1,5]/3*(MOD.2$equis[i]^3-MOD.2$equis[1]^3)))
            }}
        {TS.MOD <<- MOD.2}}
    })
    
    TS.fMOD.3 <- reactive ({function(equis) {
        MOD.3 <-matrix(NA,length (equis),1)
        MOD.3 <- data.frame(cbind(equis,MOD.3)); colnames(MOD.3) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.3$V[i] = 1}
            if (i > 1){MOD.3$V[i] = exp(10*(1/100) * (XYZ()[1,3]*(MOD.3$equis[i]-MOD.3$equis[1]) + 
                                                      XYZ()[1,4]/2*(MOD.3$equis[i]^2-MOD.3$equis[1]^2) +
                                                      XYZ()[1,5]/3*(MOD.3$equis[i]^3-MOD.3$equis[1]^3) +
                                                      XYZ()[1,6]/4*(MOD.3$equis[i]^4-MOD.3$equis[1]^4)))
            }}
        {TS.MOD <<- MOD.3}}
    })
    
    TS.fMOD.4 <- reactive ({function(equis) {
        MOD.4 <-matrix(NA,length (equis),1)
        MOD.4 <- data.frame(cbind(equis,MOD.4)); colnames(MOD.4) <- c("equis","V")
        
        for (i in 1:length (equis)) {if (i == 1){MOD.4$V[i] = 1}
            if (i > 1){MOD.4$V[i] = exp(1*(10/100) * (XYZ()[1,3]*(MOD.4$equis[i]-MOD.4$equis[1]) + 
                                                      XYZ()[1,4]/2*(MOD.4$equis[i]^2-MOD.4$equis[1]^2) +
                                                      XYZ()[1,5]/3*(MOD.4$equis[i]^3-MOD.4$equis[1]^3) +
                                                      XYZ()[1,6]/4*(MOD.4$equis[i]^4-MOD.4$equis[1]^4) +
                                                      XYZ()[1,7]/5*(MOD.4$equis[i]^5-MOD.4$equis[1]^5)))
            }}
        {TS.MOD <<- cbind(MOD.4)}}
    })
    
    TS.DEF.1 <- reactive ({TS.fMOD.1()(Ts())})
    TS.DEF.2 <- reactive ({TS.fMOD.2()(Ts())})
    TS.DEF.3 <- reactive ({TS.fMOD.3()(Ts())})
    TS.DEF.4 <- reactive ({TS.fMOD.4()(Ts())})
    
    TS.M <- reactive({if (d() == 1) { TS.m <- TS.DEF.1()
    } else if (d() == 2) { TS.m <- TS.DEF.2()  
    } else if (d() == 3) { TS.m <- TS.DEF.3() 
    } else { TS.m <- TS.DEF.4()  
    }   
    })

#Outputs- Temperature #####    
    output$T.obs    <- renderText({paste ("No. of data points:", nrow(datab()))
    })
    output$T.pv   <- renderText({ paste ("p-value:", if (input$pv == TRUE) {round(ABC()[1,2],3)})
    })
    output$T.ar2    <- renderText({ paste ("Adjusted R²:", if (input$ar2 == TRUE) {round(ABC()[1,1],3)})
    })
    
    output$T.plot1 <- renderPlot({
    
    ggplot (datab(), aes(x = AvgT, y = Lamb, color = Climate, shape = SA)) +
        geom_point(cex = 2) +
        scale_shape_manual(values=c(16,4), "Metric:") +
        scale_colour_viridis_d("Climate:") +
        geom_smooth(aes(group = 1),method = "lm", formula = y ~ poly(x, b()), size = 1, color = "black") +
        geom_hline(yintercept = 0, color = "gray50", lwd = 0.3) +
        scale_y_continuous(limits = c(-12,14.4), breaks = c(seq(-12,14,2))) +
        scale_x_continuous(limits = c(16,36), breaks = c(seq(16,36,4)))+
        labs(x = "Temperature (°C)", y = "% Change in Performance per 1°C") +
        Thm
    })

    output$T.plot2 <- renderPlot({

        ggplot (data = T.M(), aes(x = equis, y = V)) +
        geom_line() +
        geom_hline(yintercept = 1, color = "gray50", lwd = 0.3) +
        labs(x = "Temperature (°C)", y = "Relative Performance") +
        scale_x_continuous(limits = c(16,36), breaks = c(seq(16,36,4)))+
        Thm
           # scale_y_continuous(limits = c(-12,14.4), breaks = c(seq(-12,14,2))) +
           # scale_x_continuous(limits = c(16,36), breaks = c(seq(16,36,4)))
    })
    
#Outputs- Thermal sensation #####
    output$TS.obs    <- renderText({paste ("No. of data points:", nrow(datad()))
    })
    output$TS.pv   <- renderText({paste ("p-value:", if (input$TS.pv == TRUE) {round(XYZ()[1,2],3)})
    })
    output$TS.ar2    <- renderText({paste ("Adjusted R²:", if (input$TS.ar2 == TRUE) {round(XYZ()[1,1],3)})
    })
    output$TS.plot1 <- renderPlot({
        
        ggplot (datad(), aes(x = AvgTS, y = Lamb, color = Climate, shape = SA)) +
            geom_point(cex = 2) +
            scale_shape_manual(values=c(16,4), "Metric:") +
            scale_colour_viridis_d("Climate:") +
            geom_smooth(aes(group = 1),method = "lm", formula = y ~ poly(x, d()), size = 1, color = "black") +
            geom_hline(yintercept = 0, color = "gray50", lwd = 0.3) +
            scale_y_continuous(limits = c(-2,2), breaks = c(seq(-2,2,1))) +
            scale_x_continuous(limits = c(-2.1,2), breaks = c(seq(-2,2,0.5)))+
            labs(x = "Thermal Sensation (TSu)", y = "% Change in Performance per 0.1 TSu") +
            Thm
    })
    
    output$TS.plot2 <- renderPlot({
        
        ggplot (data = TS.M(), aes(x = equis, y = V)) +
            geom_line() +
            geom_hline(yintercept = 1, color = "gray50", lwd = 0.3) +
            labs(x = "Temperature (°C)", y = "Relative Performance") +
            scale_x_continuous(limits = c(-2.1,2), breaks = c(seq(-2,2,0.5)))+
            Thm
        # scale_y_continuous(limits = c(-12,14.4), breaks = c(seq(-12,14,2))) +
        # scale_x_continuous(limits = c(16,36), breaks = c(seq(16,36,4)))
    })
}
#SHINYAPP #########################################################################################
shinyApp(ui, server)
