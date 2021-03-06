library(rpart)
library(rpart.plot)
library(randomForest)

setwd('/Users/niepeiyun/Documents/rrrf')
source('predict_rf2.R')

cicle_times=20

time_count=0
index_count=0
gmean=c()

bag_num=50
M <- 10  #决策树数
for(sss in 1:cicle_times){

a=Sys.time()



data_num = 15000
train_num <- data_num*2/3  #学习数据的总数量
test_num <- data_num*1/3  #测试数据的总数
ALLDATA <- data_imit(data_num,1)
CLASS_NAME <- "y"
CLASSES <- unique(ALLDATA[[CLASS_NAME]])
feature_num <- ncol(ALLDATA)-1  #解释变量的数量
use_feature_num <- as.integer(feature_num/2)  #用于学习的解释变量的数量
#随机选择学习和测试数据
findex <- sample(nrow(ALLDATA),train_num)
data.train <- ALLDATA[findex, ] #总学习数据
data.test <- ALLDATA[-findex,] #所有测试数据
# 决策树声明一个具有模型，学习数据和用作成员变量的说明变量的类
setClass("decisionTree", representation( model = "list", data.train = "data.frame", feature = "vector"))
bag_count=train_num/bag_num

index12=rep(0,4,1)

ss=0
for (i in 1:bag_num){
  trees <- list()
  index_unique=c()
  index_bag=sample(train_num,train_num)
  training_bag=data.train[index_bag[((i-1)*bag_count+1):((i)*bag_count)],]
  for(j in 1:M){
    #随机提取用于学习的数据
    #index <- sample(nrow(data.train), nrow(data.train)*rate_of_usedata)
    index <- sample(nrow(training_bag),nrow(training_bag),replace = T)
    index_uni=sort(unique(index))
    index_unique=c(index_unique,index)
    traindata_a_tree <- training_bag[index_uni,]
    #随机选取解释变量
    dec <- sample(feature_num, use_feature_num)
    features <- c(1:ncol(ALLDATA))
    features <- features[-dec]	
    #用选定的说明变量创建训练数据
    tree <- new("decisionTree")
    tree@data.train  <- as.data.frame(traindata_a_tree[features])
    tree@feature  <- features
    weight=table(index)
    #学习选定的解释变量和学习数据
    treeModel <- rpart(paste(CLASS_NAME, "~.", sep=""),weights = weight, data = tree@data.train, method = "class")
    tree@model  <- list(treeModel)  #rpart返回列表，但是因为它不能被设置为decisionTree为什么它被存储在list $
    #decisionTree在列表中存储类
    trees <- c(trees, list(tree))
  }
  #index_unique=unique(index_unique)
  #testing_bag=training_bag[-index_unique,]
  testing_bag=data.test[index_bag[((i-1)*bag_count+1):((i)*bag_count)],]
  rf.res <- rf_predict(trees, testing_bag)
  ss=table(rf.res,as.character(testing_bag[,feature_num+1]))+ss
}


# 预测执行

# Crosstab
# rf.evl = data.frame(rf.res)
# for(i in 1:nrow(as.array(rf.res))){
#   pred_class = rf.res[[i]][2];
#   ins <- data.frame(Species=c(pred_class[[1]]))
#   rf.evl <- rbind(rf.evl, ins)
# }

# print(table(rf.evl[,1],data.test[,5]))

print(ss)
index12[2] =ss[2,2]/(ss[1,2]+ss[2,2])
index12[3]=ss[1,1]/(ss[1,1]+ss[2,1])
index12[4]=(ss[1,1]+ss[2,2])/(ss[1,1]+ss[1,2]+ss[2,1]+ss[2,2])
index12[1] =(index12[3]*index12[2])^0.5
names(index12)=c("Gmeans","TPR","TNR","Overall Acurracy")
print(paste('第',sss,'次模拟'))

print(index12)

print(Sys.time()-a)


time_count=Sys.time()-a+time_count
index_count=index12+index_count
gmean=c(gmean,index12[1])
}

index_count_final=index_count/cicle_times
time_count_final=time_count/cicle_times
print('--------------------------')
print(paste('time:',time_count_final))
print(paste('index:',index_count_final))
print('--------------------------')



