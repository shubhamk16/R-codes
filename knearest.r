#KNN for iris data
x        = iris[51:150,]          
y        = sample(1:100) 
x        = x[y,]            #shuffle the data
train    = x[c(1:80),]
test     =  x[c(81:100),]    #divide test and train data in 80/20 ratio
D        = NULL
MCC1=NULL
accuracy22=NULL
accuracy1= NULL
dist     = NULL
for (i in 1:20)           #for-loop for euclidian dist
{
  for (j in 1:80)
  {  A  =sqrt(sum((test[i,-5]-train[j,-5])^2))          
    dist=c(dist,A)
  }
}
B=matrix(c(dist),20,80,byrow=TRUE)   # matrix of distances
k=c(1,3,5,7,9)
for( n in k )     # selecting k=1,3,5,7,9
  
{  
  for(m in 1:20)         #for-loop for finding class 
 {
  c   =as.numeric(train$Species[(order(B[m,]))[1:n]])
  min =as.numeric(summary(c)[3]) 
  D   =c(D,min)
 }
  F = as.numeric(test$Species) 
  #TT<- table(D,F)
  TP<- TT[1,1]
  TN<- TT[2,2]
  FP<- TT[2,1]
  FN<- TT[1,2]
 # accuracy2<- ((TP+TN)/(TP+TN+FP+FN))*100
  
 # MCC<- ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
 # MCC1=c(MCC1,MCC)
# accuracy22=c(accuracy22,accuracy2)
  
  labels    <- (D==F)                 #compare class of test set and predicred values
  accuracy  = ((sum(labels))/length(D))*100    #find accuracy
  accuracy1 = c(accuracy1,accuracy)  
}
#s=data.frame(k,accuracy22,MCC1)
print(accuracy1)