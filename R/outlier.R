#input <- read.csv("test.csv", colClasses = c(NA, NA, NA, NA, NA, NA, NA, NA, 
#                                                              "NULL", "NULL", "NULL", "NULL", NA))

#input = input[!(input$qts > 20 & !is.na(input$qts)),]
#input = input[!(input$preco > 1000000 & !is.na(input$preco)),]
#input = input[!(is.na(input$area) | is.na(input$qts) | is.na(input$vagas)),]



#univar_camp = "qts"




library(aplpack)
library(asbio)
library(dplyr)


outlierBoxplot = function(data, col, rm_na = TRUE) {
    bp = boxplot(data[[col]])
    out = data.frame(bp$out)
    names(out) = col

    list(plot = bp, outliers = out)

    #ifelse(data[[col]] %in% bp, TRUE, FALSE)
}


outlierBagplot = function(data, col1, col2) {
    bp = bagplot(data[[col1]], data[[col2]])
    out = data.frame(bp$pxy.outlier)
    names(out) = c(col1, col2)
    
    list(plot = bp, outliers = out)
    #paste(input[[col1]], input[[col2]], sep=":") %in% paste(bp$x, bp$y, sep=":")
}


outlierBvBoxplot = function(data, col1, col2) {
    bp = bv.boxplot(data[[col1]], data[[col2]])
    out = bp$out
    names(out) = c(col1, col2)
    list(plot = bp, outliers = out)
    #paste(input[[col1]], input[[col2]], sep=":") %in% paste(bp$X, bp$Y, sep=":")

}


univarOutTypes = list(
    boxplot = outlierBoxplot
)


bivarOutTypes = list(
    bagplot = outlierBvBoxplot,
    bvboxplot = outlierBvBoxplot
    
)


univarOutliers = function(input, var, type = "boxplot", rmNA = TRUE) {
    if (is.null(input)) {
        return()
    }
    
    if (rmNA == TRUE) {
        input = input[!(is.na(input[[var]])),]
    }

    univarOutTypes[[type]](input, var)
}

bivarOutliers = function(input, var1, var2, type = "bvboxplot", rmNA = TRUE) {
    if (is.null(input)) {
        return()
    }
    
    if (rmNA == TRUE) {
        input = input[!(is.na(input[[var1]])) & !(is.na(input[[var2]])),]
    }
    
    bivarOutTypes[[type]](input, var1, var2)
}


rmOutliers = function(data, outliers) {
    anti_join(data, outliers, by=names(outliers))
}

#a = outlierBoxplot(input, 'qts')
#b = outlierBvBoxplot(input, 'qts', 'lat')

