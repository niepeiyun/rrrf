blb_rf_p=function(data.train,data.test,bag_num,M){
  gmean=c()
  a=Sys.time()
  
  CLASS_NAME <- "y"
  CLASSES <- unique(data.train[[CLASS_NAME]])
  # 决策树声明一个具有模型，学习数据和用作成员变量的说明变量的类
  setClass("decisionTree", representation( model = "list", data.train = "data.frame", feature = "vector"))
  train_num=nrow(data.train)
  bag_count=train_num/bag_num
  
  index12=rep(0,4,1)
  
  clust=5
  cluster=kmeans(t(data.train[,-ncol(data.train)]),clust)
  select_var=c()
  for(i in 1:clust){
    cluster_data=data.frame(data.train[,which(cluster$cluster==i)],y=data.train$y)
    rf_fit=randomForest(y~.,data=cluster_data)
    select_x=sample(which(cluster$cluster==i),(ncol(data.train)-1)/100,prob = importance(rf_fit)/sum(importance(rf_fit)))
    select_var=c(select_var,select_x)
    }
  
  feature_num <- length(select_var) #解释变量的数量
  imp=data.frame( row.names = paste('x',select_var,sep=''),x = paste('x',select_var,sep=''))
  
  trees <- list()
  for (i in 1:M){
    print(paste('tree',i))
    #随机提取用于学习的数据
    #index <- sample(nrow(data.train), nrow(data.train)*rate_of_usedata)
    index <- sample(nrow(data.train),nrow(data.train),replace = T)
    traindata_a_tree <- data.train[index,]
    #随机选取解释变量
    dec <- sample(feature_num, feature_num/2)
    features <- select_var[-dec]	
    #用选定的说明变量创建训练数据
    tree <- new("decisionTree")
    tree@data.train  <- as.data.frame(traindata_a_tree[,features])
    tree@feature  <- features
    tree@data.train=as.data.frame(cbind(traindata_a_tree[,c(features,ncol(data.train))]))
    #学习选定的解释变量和学习数据
    treeModel <- rpart(paste(CLASS_NAME, "~.", sep=""), data = tree@data.train, method = "class")
    tree@model  <- list(treeModel)  #rpart返回列表，但是因为它不能被设置为decisionTree为什么它被存储在list $
    #decisionTree在列表中存储类
    trees <- c(trees, list(tree))
    imp=merge(imp,data.frame(x=names(treeModel$variable.importance),importance=treeModel$variable.importance),by='x',all.x=T)
    
  }
  
  rf.res <- rf_predict(trees, data.test,CLASSES)
  ss=table(rf.res,as.character(data.test[,ncol(data.train)]))
  
  importance=data.frame(data=imp$x,imp=apply(imp[,-1],1,na_mean))
  
  
  index12[2] =ss[2,2]/(ss[1,2]+ss[2,2])
  index12[3]=ss[1,1]/(ss[1,1]+ss[2,1])
  index12[4]=(ss[1,1]+ss[2,2])/(ss[1,1]+ss[1,2]+ss[2,1]+ss[2,2])
  index12[1] =(index12[3]*index12[2])^0.5
  names(index12)=c("Gmeans","TPR","TNR","Overall Acurracy")
  print(Sys.time()-a)
  c=Sys.time()-a
  print(index12)
  
  return(list(index12,c,importance))
  
}




