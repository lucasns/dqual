input <- read.csv("test.csv", colClasses = c(NA, NA, NA, NA, NA, NA, NA, NA,  "NULL", "NULL", NA, NA, NA))
input <- cbind("idx_" = as.numeric(rownames(input)), input)

test = input[input$qts == 3 & !is.na(input$qts), ]

v = boxplot(test$area)$out
outliers = data.frame(v)
names(outliers) = 'area'

rm_test = test[10:nrow(test), ]
new_test = dplyr::anti_join(test, rm_test, by = names(rm_test))
new_test = dplyr::arrange(new_test, idx_)
rownames(new_test) = new_test$idx_


new_input = dplyr::anti_join(input, new_test, by = names(rm_test))
new_input = dplyr::arrange(new_input, idx_)
rownames(new_input) = new_input$idx_

clean = dplyr::anti_join(test, outliers, by = names(outliers))

clean = dplyr::arrange(clean, idx_)
rownames(clean) <- clean$idx_


v_log = boxplot(log(test$banheiros))$out
test_log = test
test_log['banheiros'] = data.frame(lapply(test_log['banheiros'], log))
outliers_log = data.frame(v_log)
names(outliers_log) = 'banheiros'


clean2 = dplyr::anti_join(test_log, outliers_log, by = names(outliers_log))

clean2 = dplyr::arrange(clean2, idx_)
clean3 = test[test$idx_ %in% clean2$idx_,]
rownames(clean2) <- clean2$idx_


x = c(3.2, 3.4, 3.7, 3.7, 3.8, 3.9, 4, 4, 4.1, 4.2, 4.7, 4.8, 14, 15)
