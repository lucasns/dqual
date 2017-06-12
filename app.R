library(shinydashboard)
library(DT)

source("R/selection.R")
source("R/plot.R")
source("R/outlier.R")
source("R/imputation.R")


options(encoding = 'UTF-8')
options(shiny.maxRequestSize=1000*1024^2)


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
                   
                   actionButton("applyFilter", "Apply"),
                   
                   br(),
                   br(),
                   
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
                                checkboxInput("plotLogUni", 'log'),
                                
                                selectInput("plotTypeUni", "Plot type",
                                             c("Histogram" = "histogram", "Plot"="plot", "Bloxplot"="boxplot")
                                )
                                
                                
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("plotBi1"),
                                uiOutput("plotBi2"),
                                checkboxGroupInput("plotLogBi", NULL, c(log = "log"), FALSE),
                                
                                selectInput("plotTypeBi", "Plot type",
                                             c("Plot"="plot",
                                               "Bivariate Boxplot" = "bvboxplot"
                                               )
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
                                checkboxInput("outLogUni", 'log'),
                                
                                selectInput("outlierTypeUni", "Method",
                                             c("Bloxplot"="boxplot")
                                )
                                
                                
                       ),
                       
                       
                       tabPanel("Bivariate",
                                br(),
                                uiOutput("outlierBi1"),
                                uiOutput("outlierBi2"),
                                checkboxGroupInput("outLogBi", NULL, c(log = "log")),
                                
                                selectInput("outlierTypeBi", "Method",
                                             c("Bivariate Boxplot" = "bvboxplot",
                                               "Bagplot" = "bagplot")
                                )
                                
                       )
                       
                   ),
                   
                   actionButton("outlierDetect", "Detect"),
                   actionButton("outlierRemove", "Remove")

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
            sliderInput("filterValue", "Value:", colInfo$values[1], colInfo$values[2], value=c(colInfo$values[1], colInfo$values[2]))
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
        selectizeInput("outlierUni", "Column:", choices = names(info()), width = "90%",
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
  

    observeEvent(input$plotButton, {
        if (input$plotTabset == "Univariate" && input$plotUni != "") {
            ct = info()[[input$plotUni]]$type
            if (ct != "factor") {
                output$plotOutput = renderPlot({
                    isolate(plotUnivar(values$select, input$plotUni, log = input$plotLogUni, type = input$plotTypeUni))
                })
            } else {
                print("Plot var must be numeric")
            }
            
        } else if (input$plotTabset == "Bivariate" && input$plotBi1 != "" && input$plotBi2 != "") {
            ct1 = info()[[input$plotBi1]]$type
            ct2 = info()[[input$plotBi2]]$type
            if (ct1 != "factor" && ct2 != "factor") {
                output$plotOutput = renderPlot({
                    isolate(plotBivar(values$select, input$plotBi1, input$plotBi2, input$outLogUni, type = input$plotTypeBi))
                })
            } else {
                print("Plot var must be numeric")
            }
            
        }
    })
    
    
    output$plotOutput = renderPlot({
        if (is.null(values$table)) {
            values$table = dataFile()
        }
        
    })
    
    
    observeEvent(input$outlierDetect, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "" ) {
            ct = info()[[input$outlierUni]]$type
            if (ct != "factor") {
                output$plotOutput = renderPlot({
                    isolate(plotUnivar(values$select, input$outlierUni, log = input$outLogUni, type = input$outlierTypeUni))
                })
                
                values$outliers = univarOutliers(values$select, input$outlierUni, input$outLogUni, input$outlierTypeUni)$outliers
                
            } else {
                print("Outlier var must be numeric")
            }
            
        } else if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            ct1 = info()[[input$outlierBi1]]$type
            ct2 = info()[[input$outlierBi2]]$type
            
            if (ct1 != "factor" && ct2 != "factor") {
                
                output$plotOutput = renderPlot({
                    isolate(plotBivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi))
                })
                
                values$outliers = bivarOutliers(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi)$outliers
                
            } else {
                print("Outlier var must be numeric")
            }
        } else {
            
        }
        
    })
    
    observeEvent(input$outlierRemove, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "") {
            ct = info()[[input$outlierUni]]$type
            if (ct != "factor") {
                
                outliers = univarOutliers(values$select, input$outlierUni, log = input$outLogUni, input$outlierTypeUni)$outliers
                values$select = rmOutliers(values$select, outliers)
                
                output$plotOutput = renderPlot({
                    isolate(plotUnivar(values$select, input$outlierUni, type = input$outlierTypeUni))
                })
                
                
            } else {
                print("Outlier var must be numeric")
            }
            
        } else if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            ct1 = info()[[input$outlierBi1]]$type
            ct2 = info()[[input$outlierBi2]]$type
            
            if (ct1 != "factor" && ct2 != "factor") {
                out = bivarOutliers(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi)
                values$select = rmOutliers(values$select, out$outliers)
                
                output$plotOutput = renderPlot({
                    isolate(plotBivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi))
                })
                
            } else {
                print("Outlier var must be numeric")
            }
            
        } else {
            
        }
        
        
    })
    
    
    
    
    
    
}

shinyApp(ui, server)