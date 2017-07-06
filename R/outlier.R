library(aplpack)
library(asbio)
library(dplyr)


outlier_sd = function(x, n = 2) {
    x_mean = mean(x)
    x_sd = sd(x)
    lower = x_mean - n * x_sd
    upper = x_mean + n * x_sd
    out = x[x < lower | x > upper]
    return(out)
}


outlier_mad = function(x, n = 2) {
    x_median = median(x)
    x_made = mad(x)
    lower = x_median - n * x_made
    upper =  x_median + n * x_made
    out = x[x < lower | x > upper]
    return(out)
}


outlier_boxplot = function(x, rm_na = TRUE) {
    bp = boxplot(x)
    out = bp$out
    return(out)
}


outlier_bagplot = function(x, y) {
    bp = aplpack::bagplot(x, y)
    out = data.frame(bp$pxy.outlier)
    return(out)
}


outlier_bv_boxplot = function(x, y) {
    bp = asbio::bv.boxplot(x, y)
    out = bp$out
    return(out)
}


univar_func = list(
    sd2 = outlier_sd,
    made2 = outlier_mad,
    boxplot = outlier_boxplot
)


bivar_func = list(
    bagplot = outlier_bagplot,
    bvboxplot = outlier_bv_boxplot
)


univariate_outliers = function(dataset, var, type = "boxplot", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var]])),]
    }

    x = dataset[[var]]

    if (!is.null(modifier)) {
        x = apply_modifier(x, modifier)
    }

    tryCatch({
        out = univar_func[[type]](x)
        out = data.frame(out)
        names(out) = var
        info = paste(nrow(out), "outliers detected")
    }, error = function(cond) {
        out = NULL
        info = "Error"
        print(cond)
    })

    return(list(outliers = out, info = info))
}


bivariate_outliers = function(dataset, var1, var2, type = "bvboxplot", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var1]])) & !(is.na(dataset[[var2]])),]
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
    }

    out = NULL
    info = "Error"

    tryCatch({
        out = bivar_func[[type]](x, y)
        names(out) = c(var1, var2)
        info = paste(nrow(out), "outliers detected")
    }, error = function(cond) {
        print(cond)
    })


    print(info)

    return(list(outliers = out, info = info))
}


remove_outliers = function(dataset, var, type, modifier = NULL) {
    dataset = cbind("idx_aux" = as.numeric(rownames(dataset)), dataset)

    if (length(var) == 1) { # Univar
        outliers = univariate_outliers(dataset, var, type, modifier)$outliers

    } else if (length(var) == 2) { # Bivar
        outliers = bivariate_outliers(dataset, var[1], var[2], type, modifier)$outliers
    }

    if (!is.null(modifier)) {
        dataset[var] = data.frame(lapply(dataset[var], function(x) apply_modifier(x, modifier)))
    }

    if (!is.null(outliers)) {
        clean_data = dplyr::anti_join(dataset, outliers, by = names(outliers))
        clean_data = dplyr::arrange(clean_data, idx_aux)
        rownames(clean_data) = clean_data$idx_aux

        info = paste(nrow(outliers), "rows with outliers removed")
    } else {
        info = "Error"
        clean_data = NULL
    }

    return(list(data = clean_data[, !(colnames(clean_data) == "idx_aux")], info = info))
}
