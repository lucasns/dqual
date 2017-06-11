library(shinydashboard)
library(DT)

source("R/selection.R")
source("R/plot.R")
source("R/outlier.R")
source("R/imputation.R")


options(encoding = 'UTF-8')


header <- dashboardHeader(
    title = "Test"
)


sidebar <- dashboardSidebar(collapsed = T, disable = T,
                            sidebarMenu(
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
                                         
                                         
                                ),
                                menuItem("Filter", icon = icon("th"), tabName = "widgets"
                                ),
                                menuItem("Charts", icon = icon("bar-chart-o")
                                )
                                
                                
                                
                                
                                
                            ),
                            
                            textOutput("res")
)


body <- dashboardBody(
    fluidRow(
        column(width = 3,
               # File
               box(width = NULL, status = "primary", title = "File", solidHeader = T, collapsible = T,
                   fileInput("file", "Choose File"),
                   downloadButton('downloadData', 'Download')
               ),
               
               # Filter
               box(width = NULL, status = "primary", title ="Filter", solidHeader = T, collapsible = T, collapsed = T,
                   
                   uiOutput("filterList"),
                   
                   uiOutput("filterValue"),
                   
                   actionButton("applyFilter", "Apply")
               ),
               
               
               # Edit
               box(width = NULL, status = "primary", title ="Edit", solidHeader = T, collapsible = T, collapsed = T,
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
               box(width = NULL, status = "primary", title ="Plot", solidHeader = T, collapsible = T, collapsed = T,
                   
                   tabsetPanel(
                       id = "plotTabset",
                       
                       tabPanel("Univariate", 
                                br(),
                                uiOutput("plotUni"),
                                
                                radioButtons("plotTypeUni", "Plot type",
                                             c("Plot"="plot", "Bloxplot"="boxplot", "Histogram" = "histogram")
                                )
                                
                                
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("plotBi1"),
                                uiOutput("plotBi2"),
                                
                                radioButtons("plotTypeBi", "Plot type",
                                             c("Bivariate Boxplot" = "bvboxplot",
                                               "Plot"="plot")
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
                                
                                radioButtons("outlierTypeUni", "Method",
                                             c("Bloxplot"="boxplot")
                                )
                                
                                
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("outlierBi1"),
                                uiOutput("outlierBi2"),
                                
                                radioButtons("outlierTypeBi", "Method",
                                             c("Bivariate Boxplot" = "bvboxplot",
                                               "Bagplot" = "bagplot")
                                )
                                
                       )
                       
                   ),
                   
                   fluidRow(
                       column(4,
                              actionButton("outlierDetect", "Detect")),
                       column(4,
                              actionButton("outlierRemove", "Remove"))
                   )
                   
                   
                   
               )
               
               
               
               
               
        ),
        
        column(width = 9,
               
               box(width = NULL, title = "Plot", collapsible = T,
                   plotOutput('plotOutput')
               ),
               
               
               
               
               box(width = NULL, title = "Info", solidHeader = TRUE, collapsible = T,
                   tabsetPanel(
                       
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "infoTabset",
                       tabPanel("Output",
                                verbatimTextOutput('infoOutput')
                       ),
                       tabPanel("Summary", 
                                verbatimTextOutput('summary')
                       ),
                       tabPanel("Table", 
                                textOutput('nr'),
                                dataTableOutput("table")
                                
                       )
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
    values <- reactiveValues(table = NULL,
                             select = NULL,
                             outliers = NULL,
                             plot = NULL)
    
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
        if (!is.null(values$select) & !is.null(input$filterList) && input$filterList != ""){
            val = input$filterValue
            colInfo = info()[[input$filterList]]
            
            if (colInfo$type == 'numeric') {
                values$select = selectNum(values$select, input$filterList, val[1], val[2])
            } else {
                if (!is.null(input$filterValue)) {
                    values$select = selectFactor(values$select, input$filterList, val)
                } else {
                    print("Value not determined")
                }
            }
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
        selectizeInput("filterList", "Column:", choices = names(info()),
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$filterValue = renderUI({
        if (input$filterList == "") {
            return()
        }
        
        colInfo = info()[[input$filterList]]
        if (colInfo$type == "factor") {
            selectizeInput("filterValue", "Column:", choices = colInfo$values, multiple = T,
                           options = list(
                               placeholder = 'Select an option'
                           )
            )
        } else {
            sliderInput("filterValue", "Value:", colInfo$values[1], colInfo$values[2], value=c(0,10))
        }
        
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
    
    
    observeEvent(input$rmDup, {
        values$select = getUnique(values$select)
    })
    
    observeEvent(input$rmNA, {
        values$select = getCompleteRows(values$select)
    })
    
    
    observeEvent(input$delCol, {
        if (input$delColSelect != "") {
            values$select = deleteCol(values$select, input$delColSelect)
        } else {
            print("Column not selected")
        }
    })
    
    
    test = eventReactive(list(input$plotButton, input$outlierButton), {
        print(paste(input$plotButton, input$outlierButton))
    })
    
    
    observeEvent(input$plotButton, {
        if (input$plotTabset == "Univariate" && input$plotUni != "") {
            output$plotOutput = renderPlot({
                isolate(plotUnivar(values$select, input$plotUni, type = input$plotTypeUni))
            })
            
        } else if (input$plotTabset == "Bivariate" && input$plotBi1 != "" && input$plotBi2 != "") {
            output$plotOutput = renderPlot({
                isolate(plotBivar(values$select, input$plotBi1, input$plotBi2, type = input$plotTypeBi))
            })
        }
    })
    

    output$plotOutput = renderPlot({
        test()
        if (is.null(values$table)) {
            values$table = dataFile()
        }
        
    })
    
    
    observeEvent(input$outlierDetect, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "" ) {
            output$plotOutput = renderPlot({
                isolate(plotUnivar(values$select, input$outlierUni, type = input$outlierTypeUni))
            })
            
            values$outliers = univarOutliers(values$select, input$outlierUni, input$outlierTypeUni)$outliers
        } else if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            output$plotOutput = renderPlot({
                isolate(plotBivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi))
            })
            
            values$outliers = bivarOutliers(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi)$outliers
        } else {
            
        }
        
        print(nrow(values$outliers))
        
    })
    
    observeEvent(input$outlierRemove, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "") {
            outliers = univarOutliers(values$select, input$outlierUni, input$outlierTypeUni)$outliers
            print(head(outliers,100))
            values$select = rmOutliers(values$select, outliers)
            
            output$plotOutput = renderPlot({
                isolate(plotUnivar(values$select, input$outlierUni, type = input$outlierTypeUni))
            })
        } else if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            out = bivarOutliers(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi)
            values$select = rmOutliers(values$select, out$outliers)
            
            output$plotOutput = renderPlot({
                isolate(plotBivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi))
            })
            
        } else {
            
        }
        
        print(length(values$outliers))
        
    })
    
    
    
    
    
    
}

shinyApp(ui, server)