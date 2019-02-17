kfold=function(n)
{
data=iris
set.seed(123)
x=data
y=sample(1:dim(x)[1])
x=x[y,] #shuffling data

accuracy1=NULL
MCC3=NULL
k=c(1,3,5)   # No of k in KNN
for (e in k){
  accuracy0=NULL
  MCCC=NULL
  for( i in 1:n){   # loop for kfold
    
    r = floor (dim(x)[1] / n )
    test = x[(r*(i-1)+1 ):(r*i), ]
    train= x[-((r*(i-1)+1 ):(r*i)),]
    dist=NULL
    for (i in 1:nrow(test))    # loops for distance
    {
      for (j in 1:nrow(train))
      {
        A= sqrt(sum( (test[i,-5]-train[j,-5])^2) )
        dist = c (dist,A)   
      }
    }
    B=matrix(dist,nrow(test),nrow(train),byrow=T)  # matrix for distances
    
    D=NULL
    for (i in 1: nrow(test))
    {
      C= as.numeric(train$Species[(order(B[i,]))[1:e]])  
      D =c(D,C)    # min distances for K=c(1,3,..)
    }
    mat= matrix(D,nrow(test),e,byrow=T)
    minimum=NULL
    for (v in 1:nrow(test))
    {
      min=as.numeric(summary(mat[v,])[3])
      minimum=c(minimum,min)  # majority class 
    }
    F=as.numeric(test$Species) 
    labels=(minimum==F)  # comparing test and training class
    acc= (sum(labels)/nrow(test))*100
    accuracy0=c(accuracy0,acc)
    TT=table(F,minimum)   # confusion Matrix
    TP=TT[2,2]
    TN=TT[1,1]
    FN=TT[2,1]
    FP=TT[1,2]
    MCC= ((TP*TN)-(FP*FN))/ sqrt( (TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)  )
    MCCC=c(MCCC,MCC)
  }
  accuracy=mean(accuracy0)  
  accuracy1=c(accuracy1,accuracy)   # best accuracy for K=1, K=3
  MCC2=mean(MCCC)
  MCC3=c(MCC3,MCC2)     # best Mcc for K=1, K=3      
}
s=data.frame(k,accuracy1,MCC3)
print(s)
}
