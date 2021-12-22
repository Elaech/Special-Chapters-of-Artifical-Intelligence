library(olsrr)
library(dplyr)
options(digits=20)
houses_dataset = function(){
  return(read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task4/houses.dat",
                  sep=',', header = TRUE))
}

make_plots = function(){
  plot(BEST_MODELS$P,BEST_MODELS$RSS)
  plot(BEST_MODELS$P,BEST_MODELS$R2)
  plot(BEST_MODELS$P,BEST_MODELS$R2a)
  plot(BEST_MODELS$P,BEST_MODELS$CP)
}

closer_to_cp = function(cp,p){
  return(abs(p+1-as.double(cp)))
}

make_as_formula = function(features){
  return( as.formula(paste("PRICE~", paste(features, collapse="+"))))
}


HOUSES = houses_dataset()
columns = colnames(HOUSES)
features = columns[-1]


HOUSES


forward_selection = function(dataset){
  
}

backward_selection = function(dataset){
  
}

stepwise_selection = function(dataset){
  
}