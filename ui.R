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
