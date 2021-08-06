library(dplyr)
library(data.table) #fread dcast 
library(naniar) #chk na
library(randomForest)
library(e1071)
library(xgboost)
library(rminer) 
library(glmnet)
library(MLmetrics)
旭隼 <- fread(file="D:\\2.旭隼.csv",header=TRUE,stringsAsFactors = FALSE)
#na踢掉
any_na(旭隼)
旭隼$進場 <- as.factor(旭隼$進場)
旭隼 <- mutate(旭隼,年=year(年月日))
#照時間切，準確度都只有3~4成
#traindata <- subset(旭隼,年<2018)
#traindata <- traindata[,-c(1:3,20)]
#testdata <- subset(旭隼,年>=2018)
#testdata <- testdata[,-c(1:3,20)]
n <- nrow(旭隼)
set.seed(1117)
random <- sample(seq_len(n), size = round(0.7 * n))
traindata <- 旭隼[random,-c(1:3,20)]
testdata <- 旭隼[-random,-c(1:3,20)]


##rf ####
features <- setdiff(x = names(traindata), y = "進場")
set.seed(123)
tuneRF(x = traindata[features], y = traindata$進場,
       mtryStart = 1,ntreeTry = 500)
rf_model <- randomForest(進場~., data = traindata,
                           ntree = 5000, mtry = 4,
                           do.trace = 100,na.action = na.roughfix)
rf_future <- predict(rf_model,testdata)
rf_future <- as.data.frame(rf_future)
rf_final <- cbind(rf_future,testdata)
confusion <- table(rf_final$進場,rf_final$rf_future, dnn = c("實際", "預測"))
confusion
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy # 84.6%
F1_Score(rf_final$進場, rf_final$rf_future, positive = NULL)
rf <- importance(rf_model)
varImpPlot(rf_model)
#write.table(rf_final, file="D:\\旭隼_2018後.csv", sep = ",", na = "", row.names=FALSE, col.names = TRUE)

## svm ####
tune.model <- tune.svm(進場~.,data=traindata,type="C-classification",kernel="radial",
                       range=list(cost = 2^c(-8,-4,-2,0), epsilon = seq(0,10,0.1),gamma = 2^c(-8,-4,0,4)))
tune.model$best.model #挑 lowest MSE 
svm_model <- svm(進場~.,data=traindata,type="C-classification",kernel="radial",cost=1)
svm_future <- predict(svm_model,testdata)
svm_future <- as.data.frame(svm_future)
svm_final <- cbind(svm_future,testdata)
confusion <- table(svm_final$進場,svm_final$svm_future, dnn = c("實際", "預測"))
confusion
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy # 64%

svm_model <- fit(進場~.,data=traindata,model="svm",
             kpar=list(sigma=0.1),C=1)
svm <- Importance(svm_model,traindata,measure="AAD")
svm <- svm$imp
L <- list(runs=1,sen=t(svm$imp))
mgraph(L,graph="IMP",leg=names(traindata),col="gray",Grid=10)

## xgb ####
trainx <- traindata[,c(-16)] %>% as.matrix()
trainy <- as.numeric(as.factor(traindata$進場))-1
testx <- testdata[,c(-16)] %>% as.matrix()
testy <- as.numeric(as.factor(testdata$進場))-1

set.seed(123)
xgb_model <-  xgboost(data = trainx,
                      label = trainy,
                      nrounds = 105,eta=0.05,num_class=3,
                      min_child_weight=3,max_depth=7,
                      subsample=0.8,colsample_bytree=1,
                      objective = "multi:softmax",verbose = 0) 
xgb_future <- predict(xgb_model,testx)
xgb_future <- as.data.frame(xgb_future)
xgb_final <- cbind(xgb_future,testdata)
confusion <- table(xgb_final$進場,xgb_final$xgb_future, dnn = c("實際", "預測"))
confusion
accuracy <- sum(diag(confusion)) / sum(confusion)
accuracy # 83%

importance <- xgb.importance(model = xgb_model)
par(mfrow=c(1,1))
xgb.plot.importance(importance, top_n = 20, measure = "Gain")

## 其他特徵篩選法 ####
# ridge & lasso 算成0,1間的值 需另外定義xx

trainx <- traindata[,c(-16)] %>% as.matrix()
trainy <- as.factor(traindata$進場) 
testx <- testdata[,c(-16)] %>% as.matrix()
testy <- as.factor(testdata$進場)
ridege_model <- cv.glmnet(x = trainx,y = trainy,alpha = 0,
                          family = "multinomial")
# 多個multinomial 兩個binomial
#交叉驗證 預設k=10，alpha = 0為ridege_model, =1為lasso
ridege_model
#選自變量
coef(ridege_model, ridege_model$lambda.1se)

library(Rdimtools)
旭隼.dat = as.matrix(旭隼[,4:18])
旭隼.lab = as.factor(旭隼[,19])
fsocre = do.fscore(旭隼.dat, 旭隼.lab, ndim = 8)
fsocre$featidx #特徵重要性排序
