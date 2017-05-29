
plotUnivar = function(data, col, type="histogram") {
    plot = switch(type,
                   histogram = hist,
                   boxplot = boxplot
    )
    
    plot(data[[col]])
}


plotBivar = function(data, var1, var2, type="bvboxplot") {
    plot = switch(type,
                  bvboxplot = bv.boxplot
    )
    
    plot(data[[var1]], data[[var2]], bg = 'blue', bg.out = 'red')
}
