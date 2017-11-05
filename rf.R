rf=function(data.train,data.test,M){
a=Sys.time()


CLASS_NAME <- "y"
CLASSES <- unique(data.train[[CLASS_NAME]])
feature_num <- ncol(data.train)-1  #解释变量的数量
use_feature_num <- as.integer(feature_num/2)  #用于学习的解释变量的数量
# 决策树声明一个具有模型，学习数据和用作成员变量的说明变量的类
setClass("decisionTree", representation( model = "list", data.train = "data.frame", feature = "vector"))

trees <- list()
for (i in 1:M){
  #随机提取用于学习的数据
  #index <- sample(nrow(data.train), nrow(data.train)*rate_of_usedata)
  index <- sample(nrow(data.train),nrow(data.train),replace = T)
  traindata_a_tree <- data.train[index,]
  #随机选取解释变量
  dec <- sample(feature_num, use_feature_num)
  features <- c(1:ncol(ALLDATA))
  features <- features[-dec]	
  #用选定的说明变量创建训练数据
  tree <- new("decisionTree")
  tree@data.train  <- traindata_a_tree[features]
  tree@feature  <- features
  #学习选定的解释变量和学习数据
  treeModel <- rpart(paste(CLASS_NAME, "~.", sep=""), data = tree@data.train, method = "class")
  tree@model  <- list(treeModel)  #rpart返回列表，但是因为它不能被设置为decisionTree为什么它被存储在list $
  #decisionTree在列表中存储类
  trees <- c(trees, list(tree))
  print
}


# 预测执行
rf.res <- rf_predict(trees, data.test,CLASSES);
print(Sys.time()-a)

# Crosstab
# rf.evl = data.frame(rf.res)
# for(i in 1:nrow(as.array(rf.res))){
#   pred_class = rf.res[[i]][2];
#   ins <- data.frame(Species=c(pred_class[[1]]))
#   rf.evl <- rbind(rf.evl, ins)
# }

# print(table(rf.evl[,1],data.test[,5]))
index12=rep(0,4,1)
ss=table(rf.res,as.character(data.test[,feature_num+1]))
print(ss)
index12[2] =ss[2,2]/(ss[1,2]+ss[2,2])
index12[3]=ss[1,1]/(ss[1,1]+ss[2,1])
index12[4]=(ss[1,1]+ss[2,2])/(ss[1,1]+ss[1,2]+ss[2,1]+ss[2,2])
index12[1] =(index12[3]*index12[2])^0.5
names(index12)=c("Gmeans","TPR","TNR","Overall Acurracy")
return(index12)
}



