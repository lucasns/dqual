
plotUnivar = function(data, col, type="histogram", rmNA = TRUE) {
    
    if (is.null(data)) {
        return()
    }
    
    if (rmNA == TRUE) {
        data = data[!(is.na(data[[col]])),]
    }
    
    x = data[[col]]
    plotFunc = switch(type,
                      plot = plot,
                      histogram = hist,
                      boxplot = boxplot
    )
    
    return(list(plot = plotFunc(x), n = 1))
    
}


plotBivar = function(data, var1, var2, type="bvboxplot", rmNA = TRUE) {
    if (is.null(data)) {
        return()
    }
    
    if (rmNA) {
        data = data[!(is.na(data[[var1]]) | is.na(data[[var2]])),]
    }
    
    x = data[[var1]]
    y = data[[var2]]
    plotFunc = switch(type,
                      bvboxplot = function(x, y) {
                          bv.boxplot(x, y, bg = 'blue', bg.out = 'red')
                      },
                      plot = plot
    )
    
    plotFunc(x, y)
    
}
