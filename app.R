library(shinydashboard)
library(DT)

source("R/selection.R")
source("R/plot.R")
source("R/outlier.R")
source("R/dataset_functions.R")


options(encoding = 'UTF-8')
options(shiny.maxRequestSize=1000*1024^2)


header <- dashboardHeader(
    title = "Test"
)


sidebar <- dashboardSidebar(collapsed = T, disable = F,
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
                   actionButton("rmFilter", "Remove Filters"),
                   br(),
                   br(),
                   actionButton("rmUnselected", "Remove Unselected"),

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
               box(width = NULL, status = "primary", title ="Plot", solidHeader = T, collapsible = T, collapsed = T,

                   tabsetPanel(
                       id = "plotTabset",

                       tabPanel("Univariate",

                                br(),
                                uiOutput("plotUni"),
                                checkboxGroupInput("plotLogUni", NULL, c(log = "log"), FALSE),

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
                                checkboxGroupInput("outLogUni", NULL, c(log = "log"), FALSE),

                                selectInput("outlierTypeUni", "Method",
                                            c("Bloxplot"="boxplot",
                                              "2SD" = "sd2",
                                              "2MADe" = "made2")
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
                                div(id = 'text', style = 'overflow: auto; max-height: 200px', verbatimTextOutput('infoOutput'))

                       ),
                       tabPanel("Summary",
                                div(style = 'overflow-y: scroll', tableOutput('summary'))
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
    values <- reactiveValues(dataset = NULL,
                             select = NULL,
                             outliers = NULL,
                             plot = NULL,
                             info_log = "")

    dataFile = reactive({

        data <- input$file

        if (is.null(data))
            return(NULL)

        values$info_log = paste0(values$info_log, "Data loaded", sep = '\n')
        file = read_dataset(data$datapath)
        values$dataset = file
        values$select = values$dataset

    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste('test', '.csv', sep='')
        },
        content = function(file) {
            write.csv(values$select, file, na = "", row.names = FALSE, quote = FALSE)
        }
    )


    info = reactive({
        get_info(values$select)
    })

    camps = reactive({
        c = get_info(values$select)
        if (length(c) > 0 && !is.null(values$camps) && c == values$camps) {
            print("same")
        } else {
            values$camps = c
        }

        values$camps
    })

    numElements = reactive({
        if (is.null(values$dataset)) {
            return("")
        }

        paste(nrow(values$select), "rows selected")

    })

    output$table <- DT::renderDataTable({
        if (is.null(values$dataset)) {
            values$dataset = dataFile()
        }

        datatable(values$select,
                  extensions = c("Scroller"),
                  options = list(
                      # dom = 't',
                      # deferRender = TRUE,
                      searching = TRUE,
                      autoWidth = TRUE,
                      # scrollCollapse = TRUE,
                      rownames = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                          leftColumns = 3,
                          heightMatch = 'none'
                      )
                  )
        )

    })

    output$nr = renderText({
        if (is.null(values$dataset)) {
            values$dataset = dataFile()
        }

        numElements()

    })

    output$summary = renderTable(rownames =  TRUE, {
        # identifying numeric columns



        # applying the function to numeric columns only
        get_summary(values$select)
    })


    output$infoOutput = renderText({
        values$info_log
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

            n = nrow(values$select)
            msg = paste(n, "rows seleceted")
            values$info_log = paste0(values$info_log, msg, sep = '\n')
        }

    })


    observeEvent(input$rmFilter, {
        values$select = values$dataset
        n = nrow(values$dataset)
        msg = paste(n, "rows seleceted")
        values$info_log = paste0(values$info_log, msg, sep = '\n')
    })

    observeEvent(input$rmUnselected, {
        n = nrow(values$dataset) - nrow(values$select)
        values$dataset = values$select
        msg = paste(n, "rows deleted")
        values$info_log = paste0(values$info_log, msg, sep = '\n')
    })

    observeEvent(input$rmDup, {
        old_nrow = nrow(values$select)
        old_select = values$select

        values$select = getUnique(values$select)
        dif_select = sub_dataset(old_select, values$select)

        values$dataset = sub_dataset(values$dataset, dif_select)
        n = old_nrow - nrow(values$select)
        msg = paste(n, "rows deleted")
        values$info_log = paste0(values$info_log, msg, sep = '\n')
    })

    observeEvent(input$rmNA, {
        old_nrow = nrow(values$select)
        old_select = values$select

        values$select = getCompleteRows(values$select)
        dif_select = sub_dataset(old_select, values$select)

        values$dataset = sub_dataset(values$dataset, dif_select)
        n = old_nrow - nrow(values$select)
        msg = paste(n, "rows deleted")
        values$info_log = paste0(values$info_log, msg, sep = '\n')
    })


    observeEvent(input$delCol, {
        if (input$delColSelect != "") {
            values$select = deleteCol(values$select, input$delColSelect)
        } else {
            print("Column not selected")
        }
    })


    observeEvent(input$plotButton, {
        if (input$plotTabset == "Univariate") {
            ct = info()[[input$plotUni]]$type
            output$plotOutput = renderPlot({
                isolate({
                    validate(
                        need(input$plotUni != "" && ct != "factor", "Select numeric column"),
                        need(plot_univar(values$select, input$plotUni, type = input$plotTypeUni, modifier = input$plotLogUni), "Error ploting")
                    )


                })

            })

        } else if (input$plotTabset == "Bivariate") {
            ct1 = info()[[input$plotBi1]]$type
            ct2 = info()[[input$plotBi2]]$type
                output$plotOutput = renderPlot({
                    isolate({
                    validate(
                        need(input$plotBi1 != "" && input$plotBi2 != "" && ct1 != "factor" && ct2 != "factor", "Select numeric columns"),
                        need(plot_bivar(values$select, input$plotBi1, input$plotBi2, type = input$plotTypeBi, modifier = input$plotLogBi),
                             "Error ploting")
                    )

                    })
                })


        }
    })


    output$plotOutput = renderPlot({
        if (is.null(values$dataset)) {
            values$dataset = dataFile()
        }

        validate(
            need(!is.null(values$dataset), "Please select a data set")
        )
    })


    observeEvent(input$outlierDetect, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "" ) {
            ct = info()[[input$outlierUni]]$type

            if (ct != "factor") {
                output$plotOutput = renderPlot({
                    isolate({
                        plot_univar(values$select, input$outlierUni, type = "boxplot", input$outLogUni)
                    })
                })

                out = univariate_outliers(values$select, input$outlierUni, input$outlierTypeUni, input$outLogUni)
                values$info_log = paste0(values$info_log, out$info, sep = '\n')

            } else {
                print("Column must be numeric")
            }

        } else if (input$outlierTabset == "Univariate") {
            print("Select a column")
        }

        if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            ct1 = info()[[input$outlierBi1]]$type
            ct2 = info()[[input$outlierBi2]]$type

            if (ct1 != "factor" && ct2 != "factor") {

                output$plotOutput = renderPlot({
                    isolate({
                        plot_bivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi, input$outLogBi)
                    })
                })

                out = bivariate_outliers(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi, input$outLogBi)
                values$info_log = paste0(values$info_log, out$info, sep = '\n')

            } else {
                print("Column must be numeric")
            }
        } else if (input$outlierTabset == "Bivariate") {
            print("Select two columns")
        }

    })

    observeEvent(input$outlierRemove, {
        if (input$outlierTabset == "Univariate" && input$outlierUni != "") {
            ct = info()[[input$outlierUni]]$type
            if (ct != "factor") {
                out = remove_outliers(values$select, input$outlierUni, input$outlierTypeUni, input$outLogUni)
                old_select = values$select
                values$select = out$data
                dif_select = sub_dataset(old_select, values$select)
                values$dataset = sub_dataset(values$dataset, dif_select)
                values$info_log = paste0(values$info_log, out$info, sep = '\n')

                output$plotOutput = renderPlot({
                    isolate(plot_univar(values$select, input$outlierUni, type = input$outlierTypeUni))
                })


            } else {
                print("Outlier var must be numeric")
            }

        } else if (input$outlierTabset == "Univariate") {
            print("Select a column")
        }

        if (input$outlierTabset == "Bivariate" && input$outlierBi1 != "" && input$outlierBi2 != "") {
            ct1 = info()[[input$outlierBi1]]$type
            ct2 = info()[[input$outlierBi2]]$type

            if (ct1 != "factor" && ct2 != "factor") {
                out = remove_outliers(values$select, c(input$outlierBi1, input$outlierBi2), input$outlierTypeBi, input$outLogBi)
                values$select = out$data
                values$info_log = paste0(values$info_log, out$info, sep = '\n')

                output$plotOutput = renderPlot({
                    isolate(plot_bivar(values$select, input$outlierBi1, input$outlierBi2, type = input$outlierTypeBi, input$outLogBi))
                })

            } else {
                print("Columns must be numeric")
            }

        } else if (input$outlierTabset == "Bivariate") {
            print("Select two columns")
        }


    })






}

shinyApp(ui, server)
