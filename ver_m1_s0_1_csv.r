# データの前処理を行うための
# Excelファイルを出力するためのコード
youton <- read.csv("~/ver1.4_corp.csv")
colnames(youton) <- c("ID", "農場", "産次", "メスNo.", "品種", "交配日", "分娩日", "離乳日", "再発", "流産", "総分娩重", "総離乳重", "分娩数", "死産", "哺乳頭数", "事故", "離乳数", "分娩腹数", "交配年", "交配月", "哺乳期間", "妊娠期間", "備考", "淘汰", "♀", "♂", "再交配", "腹品種", "Litter No.", "分娩♂", "分娩♀", "離乳♂", "離乳♀", "発情再起", "離乳重", "BC前期", "BC中期", "BC後期", "BC分娩", "♂2", "♂No.", "♂2No.", "分娩重", "豚舎", "ライン", "SPI", "回転率", "育種価", "交配数", "分娩予定", "出荷用途", "生年月日", "以前のID", "分娩予定年", "分娩予定月", "♂ライン")

youton$交配日 <- as.Date(youton$交配日)
youton$分娩日 <- as.Date(youton$分娩日)
youton$離乳日 <- as.Date(youton$離乳日)
sapply(youton, class)

# install.packages("lubridate")
library(lubridate)
library(dplyr)
# year(youton$交配日[1] - months(3))

ton <- subset(youton, 品種 == "L" | 品種 == "W")
sapply(ton, class)

# 交配日を起算として2014/4/1から8年分
y8ton = ton[which(ton$交配日 >= as.Date("2013-4-1") & ton$交配日 <= as.Date("2022-3-31")), ]
# haruno/tirolのみ抽出
haruno <- y8ton[y8ton$農場 == "1", ]
tirol <- y8ton[y8ton$農場 == "2", ]

nendo <- NULL
nendo <- data.frame(matrix(nrow = length(y8ton$交配日))[,1])
for(i in 1:length(y8ton$交配日)){
  x <- c(y8ton$交配日[i] - months(3))
  nendo[i,1] <- year(x)
}
names(nendo) <- c("年度")
y8ton <- cbind(y8ton,nendo)

# colnames(y8ton) <- c("ID", "農場", "産次", "メスNo.", "品種", "交配日", "分娩日", "離乳日", "再発", "流産", "総分娩重", "総離乳重", "分娩数", "死産", "哺乳頭数", "事故", "離乳数", "分娩腹数", "交配年", "交配月", "哺乳期間", "妊娠期間", "備考", "淘汰", "♀", "♂", "再交配", "腹品種", "Litter No.", "分娩♂", "分娩♀", "離乳♂", "離乳♀", "発情再起", "離乳重", "BC前期", "BC中期", "BC後期", "BC分娩", "♂2", "♂No.", "♂2No.", "分娩重", "豚舎", "ライン", "SPI", "回転率", "育種価", "交配数", "分娩予定", "品種", "生年月日", "以前のID", "分娩予定年", "分娩予定月", "♂ライン","年度")
# colnames(y8ton) <- c("ID", "農場", "産次", "メスNo.", "品種", "交配日", "分娩日", "離乳日", "再発", "流産", "総分娩重", "総離乳重", "分娩数", "死産", "哺乳頭数", "事故", "離乳数", "分娩腹数", "交配年", "交配月", "哺乳期間", "妊娠期間", "備考", "淘汰", "♀", "♂", "再交配", "腹品種", "Litter No.", "分娩♂", "分娩♀", "離乳♂", "離乳♀", "発情再起", "離乳重", "BC前期", "BC中期", "BC後期", "BC分娩", "♂2", "♂No.", "♂2No.", "分娩重", "豚舎", "ライン", "SPI", "回転率", "育種価", "交配数", "分娩予定", "出荷用途", "生年月日", "以前のID", "分娩予定年", "分娩予定月", "♂ライン", "年度")

oya <-  read.csv("~/ver2.6_oya_corp.csv")
colnames(oya) <- c("ID", "農場", "性別" , "編入日", "出荷日", "生年月日", "メスNo.", "品種", "リッター", "個体番号", "タッグNo.")
oya$編入日 <- as.Date(oya$編入日)
oya$出荷日 <- as.Date(oya$出荷日)
oya$生年月日 <- as.Date(oya$生年月日)
test <- right_join(y8ton, oya, by = "メスNo.")
test2 <- right_join(oya,y8ton,by="メスNo.")

san1 = test[test$産次 == "1", ]
san2 = test[test$産次 != "1", ]

write.csv(x = san1, file = "~/san1_test.csv")
write.csv(x = san2, file = "~/san2_test.csv")
write.csv(x = test, file = "~/test1.csv")
write.csv(x = test2, file = "~/test2.csv")


y8ton = ton[which(ton$交配日 >= as.Date("2014-4-1") & ton$交配日 <= as.Date("2022-3-31")), ]
