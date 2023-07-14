# データ処理のための再度ファイルup
ton <- read.csv("~/ver_m1_6_corp.csv")
colnames(ton) <- c("ID", "農場", "メスNo.", "生年月日", 
    "交配日", "分娩日", "離乳日", "再発", "流産", "分娩重", 
    "離乳重", "分娩♂", "分娩♀", "離乳♂", "離乳♀", "回転率", 
    "WMI1", "初の交配", "WMI2", "分娩腹数", "交配腹数", 
    "交配年", "交配月", "分娩予定年", "分娩予定月", "年度", 
    "産次", "品種", "腹品種", "初回日齢dami", "WMI_dami", 
    "総分娩重", "総離乳重", "分娩数", "死産", "哺乳頭数", 
    "事故", "離乳数", "哺乳期間", "妊娠期間", "WMI3", 
    "非生産日数", "交配回数", "初回日齢", "発情再起日数")

ton$生年月日 <- as.Date(ton$生年月日)
ton$交配日 <- as.Date(ton$交配日)
ton$分娩日 <- as.Date(ton$分娩日)
ton$離乳日 <- as.Date(ton$離乳日)
sapply(ton, class)

library(lubridate)
library(dplyr)

y8ton = ton[which(ton$交配日 >= as.Date("2014-4-1") & ton$交配日 <= as.Date("2022-3-31")), ]
# haruno/tirolのみ抽出
haruno <- y8ton[y8ton$農場 == "1", ]
tirol <- y8ton[y8ton$農場 == "2", ]

# 気候データの追加
env1 <- read.csv("~/ver2_tenki_haruno.csv")
env2 <- read.csv("~/ver2_tenki_tirol.csv")
colnames(env1) <- c("交配日", "平均気温", "平均湿度", "分娩前二週平均気温")
colnames(env2) <- c("交配日", "平均気温", "平均湿度", "分娩前二週平均気温")
env1$交配日 <- as.Date(env1$交配日)
env2$交配日 <- as.Date(env2$交配日)
haruno <- left_join(haruno, env1, by = "交配日")
tirol <- left_join(tirol, env2, by = "交配日")

# harunoの加工
for(i in 1:length(haruno[,1])){
    if(haruno[i,27]>8){
        haruno[i,27] <- 8
    }
}
for(i in 1:length(unique(haruno$産次))){
    san_name <- paste('s', i, sep = '')
    assign(san_name, haruno[haruno$産次 == i, ])
}
haruno_y <- subset(haruno, 分娩腹数 == 1|分娩腹数 == 2)
df_h <- haruno
df_haru <- haruno_y

ton_h_l <- subset(df_h, 品種 == "L")
ton_h_w <- subset(df_h, 品種 == "W")

# ここからtirol
for(i in 1:length(tirol[,1])){
    if(tirol[i,27]>8){
        tirol[i,27] <- 8
    }
}
for(i in 1:length(unique(tirol$産次))){
    san_name <- paste('s', i, sep = '')
    assign(san_name, tirol[tirol$産次 == i, ])
}
tirol_y <- subset(tirol, 分娩腹数 == 1|分娩腹数 == 2)
df_t <- tirol
df_tir <- tirol_y

ton_t_l <- subset(df_t, 品種 == "L")
ton_t_w <- subset(df_t, 品種 == "W")

df_sc_haru <- scale(df_haru[32:48])
df_sc_haru <- as.data.frame(df_sc_haru)
df_sc_haru <- cbind(df_haru[1:31],df_sc_haru)
df_sc_tir <- scale(df_tir[32:48])
df_sc_tir <- as.data.frame(df_sc_tir)
df_sc_tir <- cbind(df_tir[1:31],df_sc_tir)
