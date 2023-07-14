# 農場・品種ごと、年度ごとに単純平均値を算出
# y_lis4の1,4つ目が農場別
# y_lis4の1,4つ目以外が農場・品種別

y_lis1 <- c("sousan", "sizan", "honyu", "jiko", "rinyu", "sw", "rw", "zw", "hd", "pd", "fd")
y_lis2 <- c("mean")
y_lis3 <- c("分娩数", "死産", "哺乳頭数", "事故", "離乳数", "総分娩重", "総離乳重", "(総離乳重 - 総分娩重)", "哺乳期間", "妊娠期間", "WMI3")
y_lis4 <- c("h", "h_l", "h_w", "t", "t_l", "t_w")
x <- c("年度")

for (i in 1:length(y_lis4)) {
    sum <- NULL
    sum <- paste("sum_", y_lis4[i], sep = "")
    frm <- paste(sum, " <- data.frame(matrix(nrow = 8))", sep = "")
    asn <- assign(sum, eval(parse(text = frm)))
    for (j in 1:length(y_lis1)) {
        for(k in 1:length(y_lis2)) {
            var <- paste(y_lis1[j], "_", y_lis4[i], sep = "")
            pur <- paste("df_", y_lis4[i], "$", y_lis3[j], sep = "")
            agg <- paste("aggregate(", y_lis3[j], "~", x, ", data = df_", y_lis4[i], " ,FUN = ", y_lis2[k], ")", sep = "")
            a_y <- assign(var, eval(parse(text = agg)))
            bind <- paste(sum, " <- cbind(", sum, ", ", var, "[,2])",sep = "")
            eval(parse(text = bind))
        }
    }
    nend <- paste(sum, "[,1] <- ", var, "$年度")
    eval(parse(text = nend))
}
