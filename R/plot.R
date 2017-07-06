plot_univar = function(dataset, var, type="histogram", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var]])),]
    }

    x = dataset[[var]]

    if (!is.null(modifier)) {
        x = apply_modifier(x, modifier)
        var_name = paste(modifier, paste0("(", var, ")"))
    } else {
        var_name = var
    }

    plot_func = switch(type,
                       plot = function(x, var_name) {
                           plot(x, main = NULL, ylab = var_name)
                       },

                       histogram = function(x, var_name) {
                           hist(x, main = NULL, xlab = var_name)
                       },

                       boxplot = function(x, var_name) {
                           boxplot(x, main = NULL, xlab = var_name, ylab = "Value")
                       }
    )

    tryCatch({
        plot_func(x, var_name)
        return(TRUE)
    }, error = function(cond) {
        print(cond)
        return(FALSE)
    })
}


plot_bivar = function(dataset, var1, var2, type="bvboxplot", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na) {
        dataset = dataset[!(is.na(dataset[[var1]]) | is.na(dataset[[var2]])),]
    }

    x = dataset[[var1]]
    y = dataset[[var2]]

    if (!is.null(modifier)) {
        x = apply_modifier(x, modifier)
        y = apply_modifier(y, modifier)

        aux_df = data.frame(x,y)
        aux_df = aux_df[is.finite(aux_df$x) & is.finite(aux_df$y), ]

        x = aux_df[['x']]
        y = aux_df[['y']]

        xl = paste(modifier, paste0("(", var1, ")"))
        yl = paste(modifier, paste0("(", var2, ")"))

    } else {
        xl = var1
        yl = var2
    }

    plot_func = switch(type,
                       bvboxplot = function(x, y, xlab, ylab) {
                           asbio::bv.boxplot(x, y, bg = 'blue', bg.out = 'red', xlab = xlab, ylab = ylab)
                       },

                       bagplot = function(x, y, xlab, ylab) {
                           aplpack::bagplot(x, y, xlab = xlab, ylab = ylab)
                       },

                       plot = plot
    )

    tryCatch({
        plot_func(x, y, xlab = xl, ylab = yl)
        return(TRUE)
    }, error = function(cond) {
        print(cond)
        return(FALSE)
    })


}
