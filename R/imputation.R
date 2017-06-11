

imputeVector = function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
}

impute = function(data, col, round = TRUE) {
    value = imputeVector(data[,col])
    print(value)
    if (round) {
        value = round(value, 1)
    }
    
    data[is.na(data[col]), ] = value
    return(data)
    
}


getNaRows = function(data) {
    if (is.null(data)) {
        return()
    }
    
    return(data[!complete.cases(data), ])        
}


getCompleteRows = function(data) {
    if (is.null(data)) {
        return(NULL)
    }
    
    return(data[complete.cases(data), ])        
}


getUnique = function(data) {
    if (is.null(data)) {
        return(NULL)
    }
    
    return(unique(data))        
}


getNaSummary = function(data) {
    lapply(data, function(y) sum(is.na(y)))
}


deleteCol = function(dataTable, col) {
    dataTable[col] = NULL
    return(dataTable)
}
