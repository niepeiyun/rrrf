#随机森林的预测函数
rf_predict <- function(trees, data, classes=CLASSES){
  class_num = nrow(as.array(classes));
  tree_num = nrow(as.array(trees));
  predicted_rets = list();
  result_vote=data.frame(row.names=rownames(data))
  #决定多数投票决定M决策树
  for(j in 1:tree_num){
    tree = trees[[j]]@model[[1]];
    feature = trees[[j]]@feature;
    result <- predict(tree, newdata = data[feature], type = "class")
    result_vote=cbind(result_vote,result)
  }
  result_vote=apply(result_vote,2,as.character)
  vote=apply(result_vote,1,vote)
  
  return(vote);
}


vote=function(result)
{
  a=table(result)
  max_a=max(a)
  name_a=names(which.max(a))
  return(name_a)
}

data_imit=function(n,IR)
{
  pos=floor(n/(IR+1))
  neg=n-pos
  a=rnorm(neg,0.5,1)+rnorm(neg,0,0.2)
  b=runif(neg,-1,1)+rnorm(neg,0,0.2)
  c=rnorm(neg,0.6,1)+rnorm(neg,0,0.5)
  d=runif(neg,-1,1)+rnorm(neg,0,0.5)
  e=rnorm(neg,-0.4,0.5)
  f=runif(neg,-0.5,1)
  g=rnorm(neg,-1,1)
  y='no'
  hhh=data.frame(a,b,c,d,e,f,g,y)
  a=rnorm(pos,-0.5,1)+rnorm(pos,0,0.2)
  b=runif(pos,-1,1)+rnorm(pos,0,0.2)
  c=rnorm(pos,-0.6,1)+rnorm(pos,0,0.5)
  d=runif(pos,-1,1)+rnorm(pos,0,0.5)
  e=rnorm(pos,0.4,0.8)
  f=runif(pos,0.5,1)
  g=rnorm(pos,1,1)
  y="yes"
  jjj=data.frame(a,b,c,d,e,f,g,y)
  hhh=rbind(jjj,hhh)  
  hhh
}