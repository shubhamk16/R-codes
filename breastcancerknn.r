mydata = read.csv("/nfs/cms/mtech18/shubham.kothawade/Desktop/wdbc.csv",header=T)
data   = mydata[,-1]     #remove unwanted column
x      = data[1:568,]       
y      = sample(1:568)    
x      = x[y,]           #shuffle dataset
train  = x[c(1:450),]    #convert dataset into test and train data
test   = x[c(451:568),]
D      = NULL #class
accuracy=NULL
accuracy1=NULL
dist   = NULL
for (i in 1:nrow(test))    #use for loop for finding euclidian distancs    
{
 for (j in 1:nrow(train))
  {  
   A    =sqrt(sum((test[i,-1]-train[j,-1])^2))   #A=dist(rbind(test[1,-1],train[1,-1]))
   dist =c(dist,A)
  }
}
B = matrix(c(dist),nrow(test),nrow(train),byrow=TRUE) #make a matrix of euclidian distance

for( n in c( 1,3,5,7,9 ))     #use no of k 
{
 for(m in 1:nrow(test))       #for-loop for finding class
 {
  c   = as.numeric(train$M[(order(B[m,]))[1:n]])
  min = as.numeric(summary(c)[3]) 
  D   = c(D,min)
 }
   F        = as.numeric(test$M)    #compare the class of test set and predicted output
  labels   <- (D==F)
  accuracy =((sum(labels))/length(D))*100  # calculate accuracy
  accuracy1=c(accuracy1,accuracy)
}
print(accuracy1)
