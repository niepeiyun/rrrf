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
  x1=rnorm(m,0.5,0.4)
  x2=rnorm(m,0.7,0.3)-x1*0.4
  x3=runif(m,0.3,0.9)
  x4=rnorm(m,0.1,0.5)
  x5=rnorm(m,2,2)
  x6=x1+x2+rnorm(m,0,1)
  x7=x4+x5+x6-x1+rnorm(m,0,1)
  uy=14*x1-10*x2-8*x3+10*x4-6*x5-2*x6+x7*6+rnorm(m,0,5)
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
  sss=data.frame(x1,x2,x3,x4,x5,x6,x7,y)
  jjj=sss[which(sss$y=="yes"),]
  www=sss[which(sss$y=="no"),]
  ALLDATA=rbind(jjj[1:posn,],www[1:negn,])
  data.train=rbind(jjj[1:floor(posn*2/3),],www[1:floor(negn*2/3),])
  data.test=rbind(jjj[1:floor(posn/3),],www[1:floor(negn/3),])
  aaa=list(ALLDATA,data.train,data.test)
  return(aaa)
}

data_high=function(n,IR,var_num)
{
  m=2.5*n
  pro=1-IR/(IR+1)
  posn=round(pro*n)
  negn=n-posn
  www=0
  jjj=0
  sss=data.frame()
  uy=rnorm(m,0,2000)
  para=rnorm(var_num,0,5)
  vector=rnorm(m*var_num,1:var_num,abs(para))
  mat=matrix(vector,ncol=var_num,byrow=T)
  coef=rnorm(var_num,0,5)
  mat_coef=mat*coef
  for(i in 1:var_num)
  {
    var_i=rnorm(m,i,abs(para[i]))
    uy=uy+var_i*rnorm(1,0,1)
    sss=rbind(sss,var_i)
    print(i)
  }
  uy=1/(1+exp(-uy))
  colnames(sss)=1:m
  y=c()
  for(i in 1:m)
  { 
    if(rbinom(1,1,uy[i])>0.5)
    {
      y[i]="yes"
    }
    else
    {y[i]="no"}
  }
  sy=as.data.frame(y)
  row.names(sy)=1:m
  sss=as.data.frame(t(sss))
  colnames(sss)=paste('x',1:var_num,sep='')
  sss=cbind(sss,y=sy)
  jjj=sss[which(sss$y=="yes"),]
  www=sss[which(sss$y=="no"),]
  ALLDATA=rbind(jjj[1:posn,],www[1:negn,])
  data.train=rbind(jjj[1:floor(posn*2/3),],www[1:floor(negn*2/3),])
  data.test=rbind(jjj[1:floor(posn/3),],www[1:floor(negn/3),])
  aaa=list(ALLDATA,data.train,data.test)
  return(aaa)
}

na_mean=function(x){
  cc=mean(x,na.rm=TRUE)
  return(cc)
}
  
  
