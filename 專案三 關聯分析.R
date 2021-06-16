library(dplyr)
library(data.table) #fread dcast 
library(arules) #關聯分析包
library(arulesViz) 
data <- fread(file="D:\\3.關聯用.csv",header=TRUE)
關聯格式 <- data %>%
  group_by(方法) %>% 
  mutate(row = row_number()) %>% 
  dcast(方法 ~ row, value.var=c("指標"))
關聯格式 <- 關聯格式[,-c(1)]
sum(is.na(關聯格式))
關聯格式[is.na(關聯格式)] <- ""
write.table(關聯格式, file="D:\\關聯格式.csv", sep = ",", na = "", row.names=FALSE, col.names = TRUE)
trans <- read.transactions(file="D:\\關聯格式.csv",sep=",", rm.duplicates=TRUE)
rule1 <- apriori(trans,parameter = list(support = 0.01,confidence = 0.1,
                                        minlen=1,maxlen=2))
# support = 兩邊品項同時出現在單筆帳單數/總帳單數
# confidence = 買左邊品項下，購買右邊品項的機率
# lift = 向購買左邊品項的人推薦右邊品項/向所有人推薦右邊品項
summary(rule1)
outcome <- data.frame(lhs = labels(lhs(rule1)),rhs = labels(rhs(rule1)),rule1@quality)
加指標 <- data.frame(interestMeasure(rule1, measure = c("coverage", "chiSquared", "jaccard"), significance=T, data))
eda <- cbind(outcome,加指標)
colnames(eda) <- c("LHS", "RHS","support","confidence","lift","count","coverage","Chi-square","jaccard") 
eda$LHS <- gsub("[{}]","",eda$LHS)
eda$RHS <- gsub("[{}]","",eda$RHS)
eda$LHS <- gsub(",","",eda$LHS)
eda$RHS <- gsub(",","",eda$RHS)
eda <- subset(eda,eda$LHS!=""&eda$RHS!="")
eda <- eda[-c(76:131),]
