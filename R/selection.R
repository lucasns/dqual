#input <- read.csv("test.csv", colClasses = c(NA, NA, NA, NA, NA, NA, NA, NA,  "NULL", "NULL", NA, NA, NA))




selectFactor = function(data, var, val) {
    data[data[[var]] %in% val & !is.na(data[[var]]),]
}

selectNum = function(data, var, begin, end) {
    print(paste(var, begin, end))
    data[data[[var]] >= begin & data[[var]] <= end & !is.na(data[[var]]),]
}

