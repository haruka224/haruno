# 農場ごとに単純平均値を算出
# haruno
# df_h：haruno全体のデータ、分娩腹数0も含む
# df_haru：harunoの分娩腹数1のみのデータ
# ton_h_l, ton_h_w：df_hから品種L、Wを抜粋したデータ

# 母豚数
boton_hL <- length(unique(ton_h_l$メスNo.))
boton_hW <- length(unique(ton_h_w$メスNo.))
boton_h <- length(unique(df_h$メスNo.))
boton_H <- c(boton_hL, boton_hW, boton_h)

# 交配腹数=種付腹数
kouhai_h <- aggregate(メスNo.~品種, data = df_h, FUN = summary)
kouhai_H <- c(kouhai_h$メスNo.[1, 1], kouhai_h$メスNo.[2, 1],length(df_h[, 1]))

# 分娩腹数
bunben_h <- aggregate(交配腹数 == "1"~品種, data = haruno_y, FUN = sum)
names(bunben_h) <- c("品種", "分娩腹数")
bunben_H <- c(bunben_h$分娩腹数, length(df_haru))

# 分娩率
per_bunben_h <- aggregate(分娩腹数 != "0"~品種, data = df_h, FUN = mean)
names(per_bunben_h) <- c("品種", "分娩率")
per_bunben_H <- c(per_bunben_h$分娩率, mean(per_bunben_h$分娩率))

one_bunben_h <- aggregate((交配回数 == "1") & (分娩腹数 != "0")~品種, data = df_h, FUN = sum)
names(one_bunben_h) <- c("品種", "一度で成功")
one_bunben_H <- c(one_bunben_h$一度で成功, sum(one_bunben_h$一度で成功))


# 産次別交配腹数
kouhai_san_hL <- aggregate(交配腹数 == "1"~産次, data = ton_h_l, FUN = sum)
kouhai_san_hW <- aggregate(交配腹数 == "1"~産次, data = ton_h_w, FUN = sum)
kouhai_san_h <- aggregate(交配腹数 == "1"~産次, data = df_h, FUN = sum)
names(kouhai_san_hL) <- c("品種", "交配腹数")
names(kouhai_san_hW) <- c("品種", "交配腹数")
names(kouhai_san_h) <- c("品種", "交配腹数")


# 産次別分娩腹数
bunben_san_hL <- aggregate(分娩腹数 != "0"~産次, data = ton_h_l, FUN = sum)
bunben_san_hW <- aggregate(分娩腹数 != "0"~産次, data = ton_h_w, FUN = sum)
bunben_san_h <- aggregate(交配腹数 == "1"~産次, data = haruno_y, FUN = sum)
names(bunben_san_hL) <- c("品種", "分娩腹数")
names(bunben_san_hW) <- c("品種", "分娩腹数")
names(bunben_san_h) <- c("品種", "分娩腹数")

y_lis1 <- c("sousan", "sizan", "honyu", "jiko", "rinyu", "sw", "rw", "hd", "pd", "fd")
y_lis2 <- c("mean", "sd")
y_lis3 <- c("分娩数", "死産", "哺乳頭数", "事故", "離乳数", "総分娩重", "総離乳重", "哺乳期間", "妊娠期間", "WMI3")
x <- c("df_haru$品種")
var_H_list <- NULL
var_H_list <- data.frame(matrix(nrow = length(y_lis1)+1, ncol = 5))

for (i in 1:length(y_lis1)) {
    for(j in 1:length(y_lis2)) {
        var <- paste(y_lis1[i], "_h_", y_lis2[j], sep = "")
        pur <- paste("df_haru$", y_lis3[i], sep = "")
        agg <- paste("aggregate(",pur,"~",x," ,FUN = ",y_lis2[j],")",sep = "")
        df <- assign(var, eval(parse(text=agg)))
        if (j == 1){
            var_H_list[i, 1:2] <- df[, 2]
            var_H_list[i, 3] <- mean(df[, 2])
        }
        else{
            var_H_list[i, 4:5] <- df[, 2]
        }
    }
}

# 離乳時-哺乳開始時の増加体重
zw_h_ave <- aggregate((総離乳重 - 総分娩重)~品種, data = df_haru, FUN = mean)
zw_h_sd <- aggregate((総離乳重 - 総分娩重)~品種, data = df_haru, FUN = sd)
names(zw_h_ave) <- c("品種", "増加体重")
names(zw_h_sd) <- c("品種", "増加体重")
var_H_list[11, 1:2] <- zw_h_ave[, 2]
var_H_list[11, 3] <- mean(zw_h_ave[, 2])
var_H_list[11, 4:5] <- zw_h_sd[, 2]


# aggregate(発情再起~品種, data = df_haru, FUN = mean)
# aggregate(~品種, data = df_haru, FUN = mean)

# 一度の交配で分娩に成功する分娩腹数
one_H_list <- NULL
one_H_list <- data.frame(matrix(nrow = 8, ncol = 3))
one_san_h <- aggregate((交配回数 == "1") & (分娩腹数 != "0")~産次+品種, data = df_h, FUN = sum)

for (i in 1:length(one_san_h[, 1])) {
    if (i<9){
        one_H_list[i, 1] <- as.numeric(one_san_h[i, 3])
    }
    else{
        one_H_list[i-8, 2] <- one_san_h[i, 3]
    }
    as.numeric(one_H_list[, 1])
}
one_H_list[, 1] <- as.numeric(one_H_list[, 1])
for (i in 1:length(one_H_list[, 1])) {
    one_H_list[i, 3] <- sum(one_H_list[i, 1], one_H_list[i, 2])
}

aggregate((交配回数 == "1") & (分娩腹数 != "0")~産次, data = df_h, FUN = sum)
aggregate(非生産日数~品種, data = df_h, FUN = mean)
aggregate(交配回数>2~品種, data = df_h, FUN = sum)

# tirol
# df_t：tirol全体のデータ、分娩腹数0も含む
# df_tir：tirolの分娩腹数1のみのデータ
# ton_t_l, ton_t_w：df_tから品種L、Wを抜粋したデータ

# 母豚数
boton_tL <- length(unique(ton_t_l$メスNo.))
boton_tW <- length(unique(ton_t_w$メスNo.))
boton_t <- length(unique(df_t$メスNo.))
boton_T <- c(boton_tL, boton_tW, boton_t)

# 交配腹数=種付腹数
kouhai_t <- aggregate(メスNo.~品種, data = df_t, FUN = summary)
kouhai_T <- c(kouhai_t$メスNo.[1, 1], kouhai_t$メスNo.[2, 1],length(df_t[, 1]))

# 分娩腹数
bunben_t <- aggregate(交配腹数 == "1"~品種, data = tirol_y, FUN = sum)
names(bunben_t) <- c("品種", "分娩腹数")
bunben_T <- c(bunben_t$分娩腹数, length(df_tir))

# 分娩率
per_bunben_t <- aggregate(分娩腹数 != "0"~品種, data = df_t, FUN = mean)
names(per_bunben_t) <- c("品種", "分娩率")
per_bunben_T <- c(per_bunben_t$分娩率, mean(per_bunben_t$分娩率))

one_bunben_t <- aggregate((交配回数 == "1") & (分娩腹数 != "0")~品種, data = df_t, FUN = sum)
names(one_bunben_t) <- c("品種", "一度で成功")
one_bunben_T <- c(one_bunben_t$一度で成功, sum(one_bunben_t$一度で成功))


# 産次別交配腹数
kouhai_san_tL <- aggregate(交配腹数 == "1"~産次, data = ton_t_l, FUN = sum)
kouhai_san_tW <- aggregate(交配腹数 == "1"~産次, data = ton_t_w, FUN = sum)
kouhai_san_t <- aggregate(交配腹数 == "1"~産次, data = df_t, FUN = sum)
names(kouhai_san_tL) <- c("品種", "交配腹数")
names(kouhai_san_tW) <- c("品種", "交配腹数")
names(kouhai_san_t) <- c("品種", "交配腹数")


# 産次別分娩腹数
bunben_san_tL <- aggregate(分娩腹数 != "0"~産次, data = ton_t_l, FUN = sum)
bunben_san_tW <- aggregate(分娩腹数 != "0"~産次, data = ton_t_w, FUN = sum)
bunben_san_t <- aggregate(交配腹数 == "1"~産次, data = tirol_y, FUN = sum)
names(bunben_san_tL) <- c("品種", "分娩腹数")
names(bunben_san_tW) <- c("品種", "分娩腹数")
names(bunben_san_t) <- c("品種", "分娩腹数")

y_lis1 <- c("sousan", "sizan", "honyu", "jiko", "rinyu", "sw", "rw", "hd", "pd", "fd")
y_lis2 <- c("mean", "sd")
y_lis3 <- c("分娩数", "死産", "哺乳頭数", "事故", "離乳数", "総分娩重", "総離乳重", "哺乳期間", "妊娠期間", "WMI3")
x <- c("df_tir$品種")
var_T_list <- NULL
var_T_list <- data.frame(matrix(nrow = length(y_lis1)+1, ncol = 5))

for (i in 1:length(y_lis1)) {
    for(j in 1:length(y_lis2)) {
        var <- paste(y_lis1[i], "_t_", y_lis2[j], sep = "")
        pur <- paste("df_tir$", y_lis3[i], sep = "")
        agg <- paste("aggregate(",pur,"~",x," ,FUN = ",y_lis2[j],")",sep = "")
        df <- assign(var, eval(parse(text=agg)))
        if (j == 1) {
            var_T_list[i, 1:2] <- df[, 2]
            var_T_list[i, 3] <- mean(df[, 2])
        }
        else{
            var_T_list[i, 4:5] <- df[, 2]
        }
    }
}

# 離乳時-哺乳開始時の増加体重
zw_t_ave <- aggregate((総離乳重 - 総分娩重)~品種, data = df_tir, FUN = mean)
zw_t_sd <- aggregate((総離乳重 - 総分娩重)~品種, data = df_tir, FUN = sd)
names(zw_t_ave) <- c("品種", "増加体重")
names(zw_t_sd) <- c("品種", "増加体重")
var_T_list[11, 1:2] <- zw_t_ave[, 2]
var_T_list[11, 3] <- mean(zw_t_ave[, 2])
var_T_list[11, 4:5] <- zw_t_sd[, 2]


# aggregate(発情再起~品種, data = df_tir, FUN = mean)
# aggregate(~品種, data = df_tir, FUN = mean)

# 一度の交配で分娩に成功する分娩腹数
one_T_list <- NULL
one_T_list <- data.frame(matrix(nrow = 8, ncol = 3))
one_san_t <- aggregate((交配回数 == "1") & (分娩腹数 != "0")~産次+品種, data = df_t, FUN = sum)

for (i in 1:length(one_san_t[, 1])) {
    if (i < 9){
        one_T_list[i, 1] <- as.numeric(one_san_t[i, 3])
    }
    else{
        one_T_list[i-8, 2] <- one_san_t[i, 3]
    }
    as.numeric(one_T_list[, 1])
}
one_T_list[, 1] <- as.numeric(one_T_list[, 1])
for (i in 1:length(one_T_list[, 1])) {
    one_T_list[i, 3] <- sum(one_T_list[i, 1], one_T_list[i, 2])
}

aggregate((交配回数 == "1") & (分娩腹数 != "0")~産次, data = df_t, FUN = sum)
aggregate(非生産日数~品種, data = df_t, FUN = mean)
aggregate(交配回数 > 2 ~品種, data = df_t, FUN = sum)
