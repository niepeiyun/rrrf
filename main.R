# library(rpart)
# library(rpart.plot)
# library(randomForest)
# library(adabag)

setwd('/Users/niepeiyun/Documents/rrrf')
source('predict_rf2.R')
source('rf.R')
source('rf_blb_imb.R')
source('rf_blb_merge.R')
data_num = 1500
data0<- data_imit(data_num,9)
ALLDATA=data0[[1]]
data.train=data0[[2]]
data.test=data0[[3]]
sum(is.na(ALLDATA$y))
fit=boosting(y~.,data=data.train,method = 'class')
ss=table(as.character(data.test$y),predict(fit,data.test,type = "class"))
index12=rep(0,4,1)
index12[2] =ss[2,2]/(ss[1,2]+ss[2,2])
index12[3]=ss[1,1]/(ss[1,1]+ss[2,1])
index12[4]=(ss[1,1]+ss[2,2])/(ss[1,1]+ss[1,2]+ss[2,1]+ss[2,2])
index12[1] =(index12[3]*index12[2])^0.5
names(index12)=c("Gmeans","TPR","TNR","Overall Acurracy")
print(index12)
# write.csv(ALLDATA,'alldata.csv',row.names = F)
# write.csv(data.train,'training.csv',row.names = F)
# write.csv(data.test,'testing.csv',row.names = F)
# 
# M <- 500  #决策树数
# index=rf(data.train,data.test,M)
# print(index)
# index=blb_rf_imb(data.train,data.test,5,M/5) 
# print(index)
# index=blb_rf(data.train,data.test,5,M/5)
# print(index)
