library(rpart)
library(rpart.plot)
library(randomForest)
library(adabag)
library(MASS)
library(methods)

setwd('/home/liyang/niepeiyun/rrrf')
source('predict_rf2.R')
source('rf.R')
source('rf_blb_n.R')
source('rf_blb_p.R')
source('rf_blb_np.R')

###########生成数据并测试##############

for (k in 1:10){
print(paste('这是数据量',100000*k,'次的结果'))
data_num = 150000*k
#var_num=1000
a=Sys.time()
# data0<- data_corr(data_num,1,var_num,0.5)
set.seed(1)
data0<- data_imit(data_num,1)
ALLDATA=data0[[1]]
data.train=data0[[2]]
data.test=data0[[3]]
sum(is.na(ALLDATA$y))
Sys.time()-a

time_all=c()
index_all=data.frame()
time_all_blb=c()
index_all_blb=data.frame()
fit=rpart(y~.,data=data.train,method = 'class')
ss=table(as.character(data.test$y),predict(fit,data.test,type = 'class'))
index12=rep(0,4,1)
index12[2] =ss[2,2]/(ss[1,2]+ss[2,2])
index12[3]=ss[1,1]/(ss[1,1]+ss[2,1])
index12[4]=(ss[1,1]+ss[2,2])/(ss[1,1]+ss[1,2]+ss[2,1]+ss[2,2])
index12[1] =(index12[3]*index12[2])^0.5
names(index12)=c("Gmeans","TPR","TNR","Overall Acurracy")
print(index12)

# 
# write.csv(data.train,'/Users/niepeiyun/Documents/毕业论文/模拟数据/training_p.csv')
# write.csv(data.test,'/Users/niepeiyun/Documents/毕业论文/模拟数据/testing_p.csv')
# 


################模拟结果################

M <- 500  #决策树数
bag_num<-5
index_blb=0
time_blb=0
index_rf=0
time_rf=0

#index=rf(data.train,data.test ,M)


# 
# index=blb_rf_np(data.train,data.test ,bag_num ,M)
# index=rf(data.train,data.test,M )
index=c()
time=c()

index_all_blb=c()
time_all_blb=c()

bag_num_vec<-c(10,15,20,25,30,35,40,45,50,60,70,80)

for (i in 1:length(bag_num_vec)){
  index_blb=0
  time_blb=0
  for(j in 1:100){
  
  print(paste('BLB_RF--bag_num---------------------------------------',i))
  bag_num<-bag_num_vec[i]
  index=blb_rf_n(data.train,data.test ,bag_num ,M)
  index_blb=index[[1]]+index_blb
  time_blb=index[[2]]+time_blb
  # print(paste('RF--------------------------------------------------',i))
  # index=rf(data.train,data.test  ,M)
  # index_rf=index[[1]]+index_rf
  # time_rf=index[[2]]+time_rf
  # 
  
  # time_all=c(time_all,as.numeric(time_rf)/20)
  # index_all=rbind(index_all,index_rf/20)
  print(index_blb)
print(time_blb)
  }
  time_all_blb=c(time_all_blb,as.numeric(time_blb)/100)
  index_all_blb=rbind(index_all_blb,index_blb/100)
  
}
index=rbind(index,index_all_blb)
time=c(time,time_all_blb)
write.csv(index,'/home/liyang/niepeiyun/result/bag_index.csv')
write.csv(time,'/home/liyang/niepeiyun/result/bag_time.csv')

# index=blb_rf_imb(data.train,data.test,5,M/5) 
# print(index)
# index=blb_rf(data.train,data.test,5,M/5)
# print(index)
}

