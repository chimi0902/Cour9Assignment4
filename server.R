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