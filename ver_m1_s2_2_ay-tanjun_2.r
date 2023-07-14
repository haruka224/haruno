# 指標別に作成
for (i in 1:length(y_lis1)) {
    sum <- NULL
    sum <- paste("sum_", y_lis1[i], sep = "")
    frm <- paste(sum, " <- data.frame(matrix(nrow = 6, ncol = 8))", sep = "")
    asn <- assign(sum, eval(parse(text = frm)))
    for (j in 1:length(y_lis4)) {
        var <- paste("sum_", y_lis4[j], sep = "")
        var <- paste(var, "[,", i+1, "]", sep = "")
        lis <- NULL
        lis[j] <- var
        bind <- paste(sum, "[", j,",] <- ", lis[j], sep = "")
        eval(parse(text = bind))
    }  
}

