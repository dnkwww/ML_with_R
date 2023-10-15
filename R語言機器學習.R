library(rpart) # 建立決策樹
library(rattle) # 美化決策樹
library(RColorBrewer) # 美化決策樹
library(rpart.plot) # 提供畫出使用rpart套件所得出的決策樹函數：plot()、text()

評估資料 <- read.csv("決策樹訓練資料評估新設店面.csv", header= T, sep = ",")
評估資料

評估資料樹 <- rpart(決定 ~ 城市規模 + 平均收入 + 教育程度 + 當地投資者, data = 評估資料,
                  method = "class",
                  control = rpart.control(minsplit = 5))

plot(評估資料樹)
text(評估資料樹)
fancyRpartPlot(評估資料樹)
getwd() # 可以找到文件存放位置

##################################

客戶資料 <- read.csv("客戶行銷表.csv", header= T, sep = ",")
客戶資料

客戶分群 <- kmeans(客戶資料, centers = 3, nstart =10)
客戶分群

plot(formula = 購買總價 ~ 客戶, data = 客戶資料, col = 客戶分群$cluster)
points(客戶分群$center[, c("客戶", "購買總價")], col = 1:3, pch = 8, cex = 2)

##################################

病患紀錄 <- read.csv("病患相關資料記錄.csv", header= T, sep = ",")
病患紀錄

醫療決策樹 <- rpart(使用藥物 ~ 血壓 + 年齡, data = 病患紀錄,
                  method = "class",
                  control = rpart.control(minsplit = 5))

fancyRpartPlot(醫療決策樹)
