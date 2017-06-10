
plotUnivar = function(data, col, type="histogram") {
    x = data[[col]]
    ret = switch(type,
                  scatter = plot(x),
                  histogram = hist(x),
                  boxplot = boxplot(x)
    )

}


plotBivar = function(data, var1, var2, type="bvboxplot", rmNa = TRUE) {
    if (rmNA) {
        data = data[!(is.na(data[[var1]]) | is.na(data[[var2]])),]
    }
    
    x = data[[var1]]
    y = data[[var2]]
    plot = switch(type,
                  bvboxplot = bv.boxplot(x, y, bg = 'blue', bg.out = 'red')
    )

}
