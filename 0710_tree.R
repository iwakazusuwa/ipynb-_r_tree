# ライブラリ
library(rpart)
library(rpart.plot)

#----------------------------------------------
# <必要に応じて> 作業フォルダに移動
#----------------------------------------------
#現在の作業フォルダ表示
getwd()
setwd("C:/Users/user/iwaiwa/0707_ki/2_data/")   
#----------------------------------------------
# データ読み込み
#----------------------------------------------
df <- read.csv("11.csv", fileEncoding = "shift-jis")
#----------------------------------------------
# 説明変数・目的変数の準備
#----------------------------------------------
x_df <- subset(df, select = -c(ID,car, cluster))
y_df <- df$car
# データを結合（rpartはformulaで指定するので）
data_all <- data.frame(y_df, x_df)
#----------------------------------------------
# 訓練・テスト分割
#----------------------------------------------
set.seed(0)
train_idx <- sample(seq_len(nrow(data_all)), size = 0.75 * nrow(data_all))
train_data <- data_all[train_idx, ]
test_data <- data_all[-train_idx, ]
#----------------------------------------------
# モデル訓練
#----------------------------------------------
model <- rpart(y_df ~ ., 
               data = train_data, 
               method = "class", 
               parms = list(split = "gini"),
               control = rpart.control(maxdepth = 3))
#----------------------------------------------
# 精度計算
#----------------------------------------------
train_pred <- predict(model, train_data, type = "class")
train_acc <- mean(train_pred == train_data$y_df)

#----------------------------------------------
# # テストデータに対して予測
#----------------------------------------------
test_pred <- predict(model, test_data, type = "class")
#----------------------------------------------
# テストデータの正解率
#----------------------------------------------
test_acc <- mean(test_pred == test_data$y_df)




cat(sprintf("正解率(train): %.3f\n", train_acc))
cat(sprintf("正解率(test): %.3f\n", test_acc))

#----------------------------------------------
# 決定木の設定
#----------------------------------------------
model <- rpart(y_df ~ .,
               data = x_df,
               method = "class",
               parms = list(split = "gini"),
               control = rpart.control(maxdepth = 6)) 
#----------------------------------------------
# 決定木のプロット　比率　＆PNG保存
#----------------------------------------------
rpart.plot(model, 
           type = 2, 
           extra = 104, 
           fallen.leaves = TRUE, 
           main = "決定木")

# PNGに保存
png("decision_tree.png", width = 1000, height = 800)

rpart.plot(model, 
           type = 2, 
           extra = 104, 
           fallen.leaves = TRUE, 
           main = "決定木")
dev.off()

#----------------------------------------------
# 決定木のプロット　Ｎ数　＆PNG保存
#----------------------------------------------
rpart.plot(model,
           type = 2,
           extra = 1,
           fallen.leaves = TRUE,
           main = "決定木（人数のみ）")           

# PNGに保存　　
png("decision_tree_N.png", width = 1000, height = 800)
rpart.plot(model, 
           type = 2, 
           extra = 1, 
           fallen.leaves = TRUE, 
           main = "決定木（人数のみ）")
dev.off()

