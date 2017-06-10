library(shinydashboard)
library(DT)

source("R/selection.R")
source("R/plot.R")
source("R/outlier.R")


header <- dashboardHeader(
    title = "Test"
)


sidebar <- dashboardSidebar(collapsed = T,
                            sidebarMenu(
                                # Setting id makes input$tabs give the tabName of currently-selected tab
                                id = "tabs",
                                
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                                         fileInput("fileas", "Choose File")
                                         
                                ),
                                menuItem("Filter", icon = icon("th"), tabName = "widgets",
                                         selectizeInput("asd", "Column", choices = c("preco", "qts", "area"),
                                                        options = list(
                                                            placeholder = 'Select an option',
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                        )
                                         ),
                                         
                                         sliderInput("integer", "Value:", 
                                                     min=0, max=1000, value=500),
                                         
                                         actionButton("Apply", "Apply"),
                                         br()
                                ),
                                menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = T,
                                         menuSubItem("Sub-item 1", tabName = "subitem1"),
                                         menuSubItem("Sub-item 2", tabName = "subitem2")
                                         
                                )
                                
                                
                                
                                
                                
                            ),
                            textOutput("res")
)


body <- dashboardBody(
    fluidRow(
        column(width = 3,
               # File
               box(width = NULL, status = "primary", title = "File", collapsible = T,
                   fileInput("file", "Choose File"),
                   downloadButton('downloadData', 'Download')
               ),
               
               # Filter
               box(width = NULL, status = "primary", title ="Filter", solidHeader = T, collapsible = T,
                   
                   uiOutput("filterList"),
                   
                   sliderInput("interval", "Value:", 
                               min=0, max=10, value=c(0,10)),
                   
                   actionButton("applyFilter", "Apply")
               ),
               
               
               # Edit
               box(width = NULL, status = "primary", title ="Edit", solidHeader = T, collapsible = T,
                   radioButtons("editType", NULL, inline = TRUE,
                                c("Row", "Column")),
                   
                   conditionalPanel( condition = "input.editType == 'Row'",
                                     actionButton("rmDup", "Remove Duplicates"),
                                     br(),
                                     br(),
                                     actionButton("rmNA", "Remove NA's")),
                   
                   conditionalPanel( condition = "input.editType == 'Column'",
                                     uiOutput("delColSelect"),
                                     
                                     actionButton("delCol", "Delete Column")
                   )
                   
                   
               ),
               
               # Plot
               box(width = NULL, status = "primary", title ="Plot", solidHeader = T, collapsible = T,
                   
                   tabsetPanel(
                       id = "plotTabset",
                       
                       tabPanel("Univariate", 
                                br(),
                                uiOutput("plotUni"),
                                
                                radioButtons("plotTypeUni", "Plot type",
                                             c("Scatter"="p", "Bloxplot"="bp", "Histogram" = "h")
                                )
                                
                                
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("plotBi1"),
                                uiOutput("plotBi2"),
                                
                                radioButtons("plotTypeBi", "Plot type",
                                             c("Bivariate Boxplot" = "bvboxplot",
                                               "Bagplot" = "bagplot")
                                )
                                
                                
                                
                                
                       )
                       
                       
                   ),
                   actionButton("plotButton", "Plot")
                   
                   
               ),
               
               
               
               # Outlier
               box(width = NULL, status = "primary", title ="Oulier", solidHeader = T, collapsible = T,
                   
                   tabsetPanel(
                       id = "outlierTabset",
                       
                       tabPanel("Univariate", 
                                br(),
                                uiOutput("outlierUni"),
                                
                                radioButtons("outlierType", "Method",
                                             c("Bloxplot"="bp")
                                ),
                                
                                actionButton("outlierButton", "Detect")
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("outlierBi1"),
                                uiOutput("outlierBi2"),
                                
                                radioButtons("outlierTypeBi", "Method",
                                             c("Bivariate Boxplot" = "bvboxplot",
                                               "Bagplot" = "bagplot")
                                ),
                                
                                actionButton("outlierButton", "Detect")
                                
                       )
                       
                   )
                   
                   
               )
               
               
               
               
               
        ),
        
        column(width = 9,
               
               box(width = NULL, title = "Plot", collapsible = T,
                   plotOutput('plotOutput')
               ),
               
               box(width = NULL, solidHeader = TRUE,  collapsible = T,
                   verbatimTextOutput('summary')
               ),
               
               
               
               
               box(width = NULL, solidHeader = TRUE,  collapsible = T,
                   div(style="display:inline-block",
                       textOutput('nr', inline = T),
                       dataTableOutput("table"), inline = T)
                   
                   
               ),
               
               box(width = NULL, solidHeader = TRUE, collapsible = T,
                   tabsetPanel(
                       
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1",
                       tabPanel("Tab1", "Tab content 1"
                       ),
                       tabPanel("Tab2", "Tab content 2")
                   )
               )
               
               
        )
        
        
        
    )
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)



server <- function(input, output) {
    values <- reactiveValues()
    
    dataFile = reactive({
        
        data <- input$file
        
        if (is.null(data))
            return(NULL)
        
        file = read.csv(data$datapath)
        values$table = file
        values$select = values$table
    })
    
    
    info = reactive({
        getInfo(values$select)
    })
    
    observeEvent(input$applyFilter, {
        if (!is.null(values$select) & input$filterInput != ""){
            val = input$interval
            values$select = selectNum(values$select, input$filterInput, val[1], val[2])
            print(values$select)
        }
        
    })
    
    
    numElements = reactive({
        if (is.null(values$table)) {
            return("")
        }
        
        paste(nrow(values$select), "rows selected")
        
    })
    
    output$table <- DT::renderDataTable({
        if (is.null(values$table)) {
            values$table = dataFile()
        }
        
        datatable(values$select, options = list(scrollX = TRUE))
        
    })
    
    output$nr = renderText({
        if (is.null(values$table)) {
            values$table = dataFile()
        }
        
        numElements()
        
    })
    
    output$summary = renderPrint({
        getSummary(values$select)
    })
    
    
    # Ui Elements
    output$filterList = renderUI({
        selectizeInput("filterInput", "Column:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$filterValue = renderUI({
        sliderInput("filterValue", "Value:", min=0, max=10, value=c(0,10))
    })
    
    output$delColSelect = renderUI({
        selectizeInput("delColSelect", "Column:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$plotUni = renderUI({
        selectizeInput("plotUni", "Column:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$plotBi1 = renderUI({
        selectizeInput("plotBi1", "Column 1:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    
    output$plotBi2 = renderUI({
        selectizeInput("plotBi2", "Column 2:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    
    output$outlierUni = renderUI({
        selectizeInput("outlierUni", "Column:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$outlierBi1 = renderUI({
        selectizeInput("outlierBi1", "Column 1:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    
    output$outlierBi2 = renderUI({
        selectizeInput("outlierBi2", "Column 2:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    
    
    observeEvent(input$delCol, {
        values$select = deleteCol(values$select, input$delColSelect)
    })
    
    
    
    plotVal = eventReactive(input$plotButton, {
        print(input$plotTabset)
        plotUnivar(values$select, input$plotUni, type = input$plotTypeUni)

    })


    
    output$plotOutput = renderPlot({
        plotVal()
    })
    
    
    
}

shinyApp(ui, server)