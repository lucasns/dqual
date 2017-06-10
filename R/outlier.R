#input <- read.csv("test.csv", colClasses = c(NA, NA, NA, NA, NA, NA, NA, NA, 
#                                                              "NULL", "NULL", "NULL", "NULL", NA))

#input = input[!(input$qts > 20 & !is.na(input$qts)),]
#input = input[!(input$preco > 1000000 & !is.na(input$preco)),]
#input = input[!(is.na(input$area) | is.na(input$qts) | is.na(input$vagas)),]



#univar_camp = "qts"




library(aplpack)
library(asbio)


outlierBoxplot = function(data, col, rm_na = TRUE) {
    bp = boxplot.stats(data[[col]])$out
    ifelse(data[[col]] %in% bp, TRUE, FALSE)
}


outlierBagPlot = function(data, col1, col2) {
    bp = bagplot(data[[col1]], data[[col2]])$pxy.outlier
    bp = data.frame(bp)
    paste(input[[col1]], input[[col2]], sep=":") %in% paste(bp$x, bp$y
                                                            , sep=":")
}


outlierBvBoxplot = function(data, col1, col2) {
    bp = bv.boxplot(data[[col1]], data[[col2]])$outlier
    paste(input[[col1]], input[[col2]], sep=":") %in% paste(bp$X, bp$Y, sep=":")

}


univarOutTypes = list(
    boxplot = outlierBoxplot
)


bivarOutTypes = list(
    bagplot = outlierBvBoxplot,
    bvboxplot = outlierBvBoxplot
    
)


univarOutliers = function(input, var, type = "boxplot") {
    univarOutTypes[type](input, var)
}

bivarOutliers = function(input, var1, var2, type = "bvboxplot") {
    bivarOutTypes[[type]](input, var1, var2)
}


#a = outlierBoxplot(input, 'qts')
#b = outlierBvBoxplot(input, 'qts', 'lat')

