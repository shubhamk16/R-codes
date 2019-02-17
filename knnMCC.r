#KNN for iris data
#set.seed(123)

x        = iris[51:150,]          
y        = sample(1:100) 
x        = x[y,]            #shuffle the data
train    = x[c(1:80),]
test     = x[c(81:100),]    #divide test and train data in 80/20 ratio

Bestaccuracy=NULL
MCC1      = NULL
accuracy2 = NULL
accuracy1 = NULL
dist      = NULL

#for-loop for euclidian dist 
for (i in 1:20)           
{
  for (j in 1:80)
  {  A  =sqrt(sum((test[i,-5]-train[j,-5])^2))          
    dist=c(dist,A)
  }
}
# matrix of distances 1600
B=matrix(c(dist),20,80,byrow=TRUE)   
# selecting k=1,3,5,7,9
k=c(1,3,5,7,9)

for( n in k ) 
{   D = NULL  
 
 #for-loop for finding class   
 for(m in 1:20)         
 {
    c   =train$Species[(order(B[m,]))[1:n]]
  #min =as.numeric(summary(c)[3]) 
  D   =c(D,c)
  }
 
  Matrix <-matrix(D,nrow(test),n,byrow=T)
  min   <-c()
  for( v in 1:nrow(test)) {
    
    min[v]<-as.numeric(summary(Matrix[v,]))[3]
         
  }
  #compare class of test set and predicred values
  F = as.numeric(test$Species) 
  min= as.numeric(min)
  labels    <- (min==F)                 
 accuracy  = ((sum(labels))/length(F))*100    #find accuracy
 accuracy1 = c(accuracy1,accuracy)
  
# finding accuracy and MCC by mathews co-releation coefficient
 
  TT<- table(min,F)
  TP<- TT[1,1]
  TN<- TT[2,2]
  FP<- TT[2,1]
  FN<- TT[1,2]
  accuracy2<- ((TP+TN)/(TP+TN+FP+FN))*100
  
  MCC<- ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  MCC1=c(MCC1,MCC)
 Bestaccuracy=c( Bestaccuracy,accuracy2)
  
}
s=data.frame(k,  Bestaccuracy, MCC1)
print(s)