library(shinydashboard)
library(DT)

source("R/selection.R")


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
               box(width = NULL, status = "warning",
                   fileInput("file", "Choose File"),
                   downloadButton('downloadData', 'Download')
               ),
               
               # Filter
               box(width = NULL, status = "primary", title ="Filter", solidHeader = T,
                   
                   uiOutput("filterList"),
                   
                   sliderInput("interval", "Value:", 
                               min=0, max=10, value=c(0,10)),
                   
                   actionButton("applyFilter", "Apply")
               ),
               
               box(width = NULL, status = "primary", title ="Edit", solidHeader = T,
                   radioButtons("editType", NULL,
                                c("Row", "Column")),
                   
                   conditionalPanel( condition = "input.editType == 'Row'",
                                     actionButton("impute", "Remove Duplicates"),
                                     br(),
                                     br(),
                                     actionButton("impute", "Remove NA's")),
                   
                   conditionalPanel( condition = "input.editType == 'Column'",
                                     selectizeInput("asd", NULL, choices = c("preco", "qts", "area"),
                                                    options = list(
                                                        placeholder = 'Select an option',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                    )
                                     ),
                                     
                                     actionButton("impute", "Delete Column")
                   )
                   
                   
               ),
               
               box(width = NULL, status = "primary", title ="Plot", solidHeader = T,
                   selectizeInput("asd", "Column", choices = c("preco", "qts", "area"),
                                  options = list(
                                      placeholder = 'Select an option',
                                      onInitialize = I('function() { this.setValue(""); }')
                                  )
                   ),
                   
                   radioButtons("plotType", "Plot type",
                                c("Scatter"="p", "Bloxplot"="bp", "Histogram" = "h")
                   ),
                   
                   actionButton("impute", "Plot")
               ),
               
               
               box(width = NULL, status = "primary", title ="Oulier", solidHeader = T,
                   selectizeInput("asd", "Column", choices = c("preco", "qts", "area"),
                                  options = list(
                                      placeholder = 'Select an option',
                                      onInitialize = I('function() { this.setValue(""); }')
                                  )
                   ),
                   
                   radioButtons("plotType", "Method",
                                c("Bloxplot"="bp")
                   ),
                   
                   actionButton("impute", "Detect")
               )
               
               
               
               
               
        ),
        
        column(width = 9,
               fluidRow(
                   column(width = 6,
                          box(width = NULL, solidHeader = TRUE,
                              div(style = 'overflow-x: scroll', verbatimTextOutput('summary'))
                          )
                   ),
                   column(width = 6,
                          box(width = NULL, solidHeader = TRUE,
                              plotOutput('plot')
                          )
                   )
               ),
               
               
               
               box(width = NULL, solidHeader = TRUE,
                   div(style="display:inline-block",
                       textOutput('nr', inline = T),
                       dataTableOutput("table"), inline = T)
                   
                   
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
        if (is.null(values$select)) {
            return()
        }
        
        summary(values$select)
    })
    
    
    output$filterList = renderUI({
        selectizeInput("filterInput", "Column:", choices = names(values$select), 
                       options = list(
                           placeholder = 'Select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
        )
    })
    
    output$filterValue = renderUI({
        sliderInput("filterValue", "Value:", min=0, max=10, value=c(0,10))
    })
    
    output$plot = renderPlot({
        hist(c(1,1,1,2,2,3,3,5,5,5,5,4,4), main = "Histogram", xlab = "Data")
        
    })
    
    
    
}

shinyApp(ui, server)