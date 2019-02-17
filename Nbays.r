NBS = function(x){
data=iris[1:100,]
versicolor=data[which(as.numeric(data[,5])==2),]
setosa=data[which(as.numeric(data[,5])==1),]
mean_setosa=as.numeric(colMeans(setosa[,-5]))
mean_versicolor=as.numeric(colMeans(versicolor[,-5]))
var_setosa=c()
for(i in 1:ncol(setosa[,-5]) ){
  var_setosa[i]=var(setosa[,i])
}
var_versicolor=c()
for(i in 1:ncol(versicolor[,-5]) ){
  var_versicolor[i]=var(versicolor[,i])
}
prob_setosa=c()
for(i in 1:ncol(setosa[,-5])){
prob_setosa[i] = exp (-(x[i]- mean_setosa[i])^2 / (2*var_setosa[i]^2)) / (sqrt(2*pi*var_setosa[i]^2))
}
prob_versicolor=c()
for(i in 1:ncol(setosa[,-5])){
  prob_versicolor[i] = exp (-(x[i]- mean_versicolor[i])^2 / (2*var_versicolor[i]^2)) / (sqrt(2*pi*var_versicolor[i]^2))
}

prob_s=prod(prob_setosa)* length(setosa)/length(data)
prob_v=prod(prob_versicolor)* length(versicolor)/length(data)
prob_s=prob_s/(prob_v+prob_s)
prob_v=prob_v/(prob_s+prob_v)
if (prob_s > prob_v) 
  sprintf("given class Setosa")
else if(prob_v > prob_s)
  sprintf("given class is  versicolor")

}

