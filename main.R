library(rpart)
library(rpart.plot)
library(randomForest)
library(adabag)

setwd('/Users/niepeiyun/Documents/rrrf')
source('predict_rf2.R')
source('rf.R')
source('rf_blb_imb.R')
source('rf_blb_merge.R')
source('rf_blb_var.R')

time_all=c()
index_all=data.frame()
time_all_blb=c()
index_all_blb=data.frame()


  print(paste('This is the',j,'th cycle'))
data_num = 1500
a=Sys.time()
data0<- data_high(data_num,1,1000)
ALLDATA=data0[[1]]
data.train=data0[[2]]
data.test=data0[[3]]
sum(is.na(ALLDATA$y))
Sys.time()-a

fit=rpart(y~.,data=data.train,method = 'class')
ss=table(as.character(data.test$y),predict(fit,data.test,type = 'class'))
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
M <- 100  #决策树数
bag_num<-5
index_blb=0
time_blb=0
index_rf=0
time_rf=0

index=blb_rf_p(data.train,data.test ,bag_num ,M)
index=rf(data.train,data.test )


for (i in 1:20){
  print(paste('BLB_RF----------------------------------------------',i))
  
index=blb_rf(data.train,data.test ,bag_num ,M)
index_blb=index[[1]]+index_blb
time_blb=index[[2]]+time_blb
print(paste('RF--------------------------------------------------',i))
index=rf(data.train,data.test  ,M)
index_rf=index[[1]]+index_rf
time_rf=index[[2]]+time_rf

time_all_blb=c(time_all_blb,as.numeric(time_blb)/20)
index_all_blb=rbind(index_all_blb,index_blb/20)

time_all=c(time_all,as.numeric(time_rf)/20)
index_all=rbind(index_all,index_rf/20)

# index=blb_rf_imb(data.train,data.test,5,M/5) 
# print(index)
# index=blb_rf(data.train,data.test,5,M/5)
# print(index)
}