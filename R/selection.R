#input <- read.csv("test.csv", colClasses = c(NA, NA, NA, NA, NA, NA, NA, NA,  "NULL", "NULL", NA, NA, NA))




selectFactor = function(data, var, val) {
    data[data[[var]] %in% val & !is.na(data[[var]]),]
}


selectNum = function(data, var, begin, end) {
    data[data[[var]] >= begin & data[[var]] <= end & !is.na(data[[var]]),]
}


deleteCol = function(dataTable, col) {
    dataTable[col] = NULL
    print(col)
    print(dataTable)
    return(dataTable)
}


getInfo = function(data) {
    types = colnames(data)
    ret = list()
    for (t in colnames(data)) {
        col = data[[t]]
        if (is.numeric(col)) {
            ret[[t]] = list(type = "numeric", values = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
            
        } else if(is.factor(col)) {
            ret[[t]] = list(type = "factor", values = levels(col))
        }
    }
    
    ret

}


getSummary = function(data) {
    return(paste(""))
}
