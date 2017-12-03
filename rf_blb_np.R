blb_rf_np=function(data.train,data.test,bag_num,M){
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
  rf_fit=blb_rf(data.train ,data.test,bag_num,M)
  
  #rpart_fit= rpart(y~.,data = data.train, method = "class")
  #rpart_imp=data.frame(x=names(rpart_fit$variable.importance),imp=rpart_fit$variable.importance)
  rpart_imp=data.frame(x=paste('x',1:(ncol(data.train)-1),sep=''),imp=rf_fit[[3]])
  cluster_x=data.frame(x=names(cluster$cluster),imp=cluster$cluster,f=1:length(cluster$cluster))
  cluster_imp=merge(cluster_x,rpart_imp,by='x',all.x = T)
  cluster_imp$imp.y[is.na(cluster_imp$imp.y)]=0
  select_var=c()
  for(i in 1:clust){
    cluster_i=cluster_imp[cluster_imp$imp.x==i,]
    select_i=sample(cluster_i$f,(ncol(data.train)-1)/100,prob = cluster_i$imp.y/sum(cluster_i$imp.y))
    select_var=c(select_var,select_i)
    # select_i=cluster_i[order(-cluster_i$imp.y),]
    # select_var=c(select_var,select_i$f[1:5])
  }
  feature_num <- length(select_var) #解释变量的数量
  imp=data.frame( row.names = paste('x',1:feature_num,sep=''),x = paste('x',1:feature_num,sep=''))
  
  
  ss=0
  for (i in 1:bag_num){
    trees <- list()
    index_bag=sample(train_num,train_num)
    training_bag=data.train[index_bag[((i-1)*bag_count+1):((i)*bag_count)],]
    for(j in 1:(M/bag_num)){
      #随机提取 用于学习的数据
      #index <- sample(nrow(data.train), nrow(data.train)*rate_of_usedata)
      index <- sample(nrow(training_bag),train_num,replace = T)
      index_uni=sort(unique(index))
      traindata_a_tree <- training_bag[index_uni,]
      #随机选取解释变量
      dec <- sample(feature_num, feature_num/2)
      features <- select_var[-dec]	
      #用选定的说明变量创建训练数据
      tree <- new("decisionTree")
      tree@feature  <- features
      weight=as.integer(table(index))
      tree@data.train  <- as.data.frame(traindata_a_tree[,features])
      tree@data.train=as.data.frame(cbind(traindata_a_tree[,c(features,ncol(data.train))],weight=weight))
      #tree@data.train  <- as.data.frame(cbind(traindata_a_tree[features],weight=weight))
      #学习选定的解释变量和学习数据
      #feat=paste('~','colnames(tree@data.train)')
      treeModel <- rpart(paste(CLASS_NAME, "~", paste(colnames(tree@data.train[1:(ncol(data_a_tree)-2)]),collapse="+"), sep=""),data = tree@data.train,weights = weight, method = "class")
      tree@model  <- list(treeModel)  #rpart返回列表，但是因为它不能被设置为decisionTree为什么它被存储在list $
      imp=merge(imp,data.frame(x=names(treeModel$variable.importance),importance=treeModel$variable.importance),by='x',all.x=T)
      #decisionTree在列表中存储类
      trees <- c(trees, list(tree))
    }
    #index_unique=unique(index_unique)
    #testing_bag=training_bag[-index_unique,]
  }
  rf.res <- rf_predict(trees, data.test,CLASSES)
  ss=table(rf.res,as.character(data.test[,ncol(data.train)]))
  
  importance=apply(imp[,-1],1,na_mean)
  
  
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




