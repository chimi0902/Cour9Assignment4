Visualization of Population and GPD Variation
========================================================
author: Olivier Chimi C
date: 2017-09-17
autosize: true

Introduction
========================================================

This aplication alowed to load population or GDP variation from 1960 to 2016. 


This aplication alowed to visualize Data on one panel and on other panel: 

- The population variation of selected country from 1960 to 2016
- The GDP variation of selected country from 1960 to 2016


Input
========================================================

- X axis is the date which has default values (Date)
- X axis is the population variation or GDP according to the file load

Follow the exemplde of the data format for this application 

- Population of 10 country from 196 to 2016

```r
str(df)
```

```
'data.frame':	57 obs. of  10 variables:
 $ Afghanistan         : int  8996351 9166764 9345868 9533954 9731361 9938414 10152331 10372630 10604346 10854428 ...
 $ Angola              : int  5643182 5753024 5866061 5980417 6093321 6203299 6309770 6414995 6523791 6642632 ...
 $ Albanie             : int  1608800 1659800 1711319 1762621 1814135 1864791 1914573 1965598 2022272 2081695 ...
 $ Andorre             : int  13411 14375 15370 16412 17469 18549 19647 20758 21890 23058 ...
 $ Le.monde.arabe      : int  92490932 95044497 97682294 100411076 103239902 106174988 109230593 112406932 115680165 119016542 ...
 $ Ã.mirats.arabes.unis: int  92634 101078 112472 125566 138529 150362 160481 170283 183194 203820 ...
 $ Argentine           : int  20619075 20953077 21287682 21621840 21953929 22283390 22608748 22932203 23261278 23605987 ...
 $ ArmÃ.nie            : int  1874120 1941491 2009526 2077575 2144998 2211316 2276031 2339124 2401140 2462925 ...
 $ Samoa.amÃ.ricaines  : int  20013 20486 21117 21882 22698 23520 24321 25116 25885 26614 ...
 $ Date                : Factor w/ 57 levels "1960-01-01","1961-01-01",..: 1 2 3 4 5 6 7 8 9 10 ...
```

ui.R
========================================================


```r
library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Population and GDP over the word"),
    sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        
        fluidRow(
            column(6,selectInput("xaxisGrp","X-Axis:", 
                                 c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6"))),
            column(6,checkboxGroupInput("yaxisGrp","Y-axis:", 
                                        c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6")))
        ),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = "light", label = strong("Change light line"), value = FALSE),
        
        # Display only if the light is checked
        conditionalPanel(condition = "input.light == true",
                         sliderInput(inputId = "f", label = "Curve light:",
                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                     animate = animationOptions(interval = 100)),
                         HTML("Higher values give more smoothness.")
        ),        
        radioButtons('sep', 'Separator',
                     c(Comma=',', Semicolon=';',Tab='\t'), ','),
        uiOutput("choose_columns")
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Plot",plotOutput("plot")),
            tabPanel("Data", tableOutput('contents'))
        )
    )
)
)
```


Server.R
========================================================


```r
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)
library(scales)
shinyServer(
    function(input, output,session) {
        dsnames <- c()
        
        data_set <- reactive({
            inFile <- input$file1
            
            if (is.null(inFile))
                return(read.csv("PopPays.csv"))
            
            data_set<-read.csv(inFile$datapath, header=input$header, 
                               sep=input$sep, quote=input$quote)
        })
        
        output$contents <- renderTable({data_set()[2:(ncol(data_set())-1)]})
        
        observe({
            dsnames <- names(data_set())[2:(ncol(data_set())-1)]
            cb_options <- list()
            cb_options[ dsnames] <- dsnames
            
            dsnames1 <-c()
            dsnames1 <- "Date" #names(data_set())[11]
            cb_options1 <- list()
            cb_options1[ dsnames1] <- dsnames1
            
            updateSelectInput(session, "xaxisGrp",
                              label = "X-Axis",
                              choices = cb_options1,
                              selected = "")
            updateCheckboxGroupInput(session, "yaxisGrp",
                                     label = "Y-Axis",
                                     choices = cb_options,
                                     selected = "")
        })
        output$choose_dataset <- renderUI({
            selectInput("dataset", "Data set", as.list(data_sets))
        })
        output$plot = renderPlot(
            {
                df <- data_set()[2:11]
                gp <- NULL
                if (!is.null(df)){
                    xv <- input$xaxisGrp
                    yv <- input$yaxisGrp
                    
                    if (!is.null(xv) & !is.null(yv)){
                        if (sum(xv %in% names(df))>0){ # supress error when changing files

                                mdf <- melt(df,id.vars=xv,measure.vars=yv)
                                if(input$light){
                                gp <- ggplot(data=mdf) + 
                                    geom_point(aes_string(x=xv,y="value",color="variable"),alpha = input$f)+
                                    labs(x = input$xaxisGrp, y ="value")
                                }else{
                                    gp <- ggplot(data=mdf) + 
                                        geom_point(aes_string(x=xv,y="value",color="variable"))+
                                        labs(x = input$xaxisGrp, y ="value")
                                }
                        }
                    }
                }
                return(gp)
            }
        )
        output$choose_columns <- renderUI({
            
            if(is.null(input$dataset))
                return()
            colnames <- names(contents)
            checkboxGroupInput("columns", "Choose columns", 
                               choices  = colnames,
                               selected = colnames)
        }) 
    }
)
```

Application scream
========================================================

![plot of chunk unnamed-chunk-5](ApplPresentation.R-figure/unnamed-chunk-5-1.png)
