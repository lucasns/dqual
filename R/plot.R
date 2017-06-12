
plotUnivar = function(data, col, log = FALSE, type="histogram", rmNA = TRUE) {
    
    if (is.null(data)) {
        return()
    }
    
    if (rmNA == TRUE) {
        data = data[!(is.na(data[[col]])),]
    }
    
    x = data[[col]]
    if (log) x = log(x)

    plotFunc = switch(type,
                      plot = plot,
                      histogram = hist,
                      boxplot = function(x, xlab) {
                          boxplot(x, xlab=xlab, outcol='red')
                      }
    )
    
    
    plotFunc(x, xlab=col)
    
}


plotBivar = function(data, var1, var2, log = FALSE, type="bvboxplot", rmNA = TRUE) {
    if (is.null(data)) {
        return()
    }
    
    if (rmNA) {
        data = data[!(is.na(data[[var1]]) | is.na(data[[var2]])),]
    }
    
    x = data[[var1]]
    y = data[[var2]]
    
    aux = data.frame(x,y)
    print(head(aux, 10))
    
    
    if (log) {
        aux = data.frame(sapply(aux, log))
        aux = aux[complete.cases(aux) && !duplicated(aux$x) && !duplicated(aux$y), ]
        
        x = aux[['x']]
        y = aux[['y']]


    }
    
    plotFunc = switch(type,
                      bvboxplot = function(x, y, xlab, ylab) {
                          bv.boxplot(x, y, bg = 'blue', bg.out = 'red', xlab = xlab, ylab = ylab)
                      },
                      plot = plot
    )
    
    plotFunc(x, y, var1, var2)
    
}
