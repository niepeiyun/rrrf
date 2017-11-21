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
  m=2.5*n
  pro=1-IR/(IR+1)
  posn=round(pro*n)
  negn=n-posn
  www=0
  jjj=0
  sss=0
  uy=rep(0,m)
  y=rep(0,m)
  a=rnorm(m,0.5,0.4)
  b=rnorm(m,0.7,0.3)-a*0.4
  d=runif(m,0.3,0.9)
  e=rnorm(m,0.1,0.5)
  f=rnorm(m,2,2)
  g=a+b+rnorm(m,0,1)
  c=d+e+f+g-a+rnorm(m,0,1)
  uy=14*a-10*b-8*c+10*d-6*e-2*f+g*6+rnorm(m,0,10)
  uy=1/(1+exp(-uy))
  for(i in 1:m)
  { 
    if(rbinom(1,1,uy[i])>0.5)
    {
      y[i]="yes"
    }
    else
    {y[i]="no"}
  }
  sss=data.frame(a,b,c,d,e,f,g,y)
  jjj=sss[which(sss$y=="yes"),]
  www=sss[which(sss$y=="no"),]
  ALLDATA=rbind(jjj[1:posn,],www[1:negn,])
  data.train=rbind(jjj[1:floor(posn*2/3),],www[1:floor(negn*2/3),])
  data.test=rbind(jjj[1:floor(posn/3),],www[1:floor(negn/3),])
  aaa=list(ALLDATA,data.train,data.test)
  return(aaa)
}