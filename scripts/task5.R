library(olsrr)
library(dplyr)
options(digits=20)
houses_dataset = function(){
  return(read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task4/houses.dat",
                  sep=',', header = TRUE))
}




make_as_formula = function(features){
  return( as.formula(paste("PRICE~", paste(features, collapse="+"))))
}


HOUSES = houses_dataset()


get_p_value =  function(model){
  f = names(summary(model))$fstatistic
}

forward_selection = function(dataset){
  features_list = colnames(HOUSES)[-1]
  global_selection = list()
  
  while(TRUE){
    model_worth = list()
    for(index in 1:length(features_list)){
      if(!(features_list[index] %in% global_selection)){
        # calculate p-value
        current_selection = global_selection
        current_feature = features_list[index]
        current_selection[length(current_selection)+1] = current_feature
        model = lm(make_as_formula(current_selection),data=dataset)
        p_value = summary(model)$coefficients[length(summary(model)$coefficients[,4])+1,4]
        model_worth[length(model_worth)+1] = p_value[current_feature]
      }
      else{
        model_worth[length(model_worth)+1] = 1
      }
    }
    
    best_p_value = min(unlist(model_worth))
    best_feature_index = which.min(model_worth)
    best_feature = features_list[best_feature_index]
    print(best_feature)
    print(best_p_value)
    
    if(best_p_value < 0.05){
      global_selection[length(global_selection)+1] = best_feature
    }
    else{
      break
    }
  }
  return(unlist(global_selection))
}

backward_selection = function(dataset){
  features_list = colnames(HOUSES)[-1]
  global_selection = colnames(HOUSES)[-1]
  
  while(length(global_selection)>1){
    
    model_worth = list()
    model = lm(make_as_formula(global_selection), data=dataset)
    
    p_values = summary(model)$coefficients[,4][-1]
    weakest_feature = names(which.max(p_values))
    weakest_p_value = max(p_values)
    print(weakest_feature)
    print(weakest_p_value)
    
    if(weakest_p_value>0.05){
      global_selection = global_selection[global_selection != weakest_feature]
    }
    else{
      break
    }
  }
  return(unlist(global_selection))
}

stepwise_selection = function(dataset){
  features_list = colnames(HOUSES)[-1]
  global_selection = list()
  while(TRUE){
    #FORWARDS
    model_worth = list()
    for(index in 1:length(features_list)){
      if(!(features_list[index] %in% global_selection)){
        # calculate p-value for current var
        current_selection = global_selection
        current_feature = features_list[index]
        current_selection[length(current_selection)+1] = current_feature
        model = lm(make_as_formula(current_selection),data=dataset)
        p_value = summary(model)$coefficients[,4]

        model_worth[length(model_worth)+1] = p_value[current_feature]
      }
      else{
        model_worth[length(model_worth)+1] = 1
      }
    }
    
    best_p_value = min(unlist(model_worth))
    best_feature_index = which.min(model_worth)
    best_feature = features_list[best_feature_index]
    print("FORWARD")
    print(best_feature)
    print(best_p_value)
    
    if(best_p_value < 0.05){
      global_selection[length(global_selection)+1] = best_feature
    }
    else{
      break
    }
    # BACKWARDS
    while(length(global_selection)>0){
      model_worth = list()
      
      model = lm(make_as_formula(global_selection), data=dataset)
      p_values = summary(model)$coefficients[,4][-1]
      
      weakest_feature = names(which.max(p_values))
      weakest_p_value = max(p_values)
      print("BACKWARD")
      print(weakest_feature)
      print(weakest_p_value)
      
      if(weakest_p_value>0.05){
        global_selection = global_selection[global_selection != weakest_feature]
      }
      else{
        print("NOT_DELETED")
        break
      }
    }
    
  }
  return(unlist(global_selection))
}

fs_features = forward_selection(HOUSES)
bs_features = backward_selection(HOUSES)
sw_features = stepwise_selection(HOUSES)
homework4_features = c("BDR", "FLR",  "FP" ,"RMS", "ST", "LOT", "GAR", "L1", "L2")

print(fs_features)
print(bs_features)
print(sw_features)
print(homework4_features)

fs_model = lm(make_as_formula(fs_features),data=HOUSES)
bs_model = lm(make_as_formula(bs_features),data=HOUSES)
sw_model = lm(make_as_formula(sw_features),data=HOUSES)
h4_model = lm(make_as_formula(homework4_features),data=HOUSES)

plot(HOUSES$PRICE,predict(fs_model),col="green",pch=15)
points(HOUSES$PRICE,predict(bs_model),col="red",pch=16)
points(HOUSES$PRICE,predict(sw_model),col="light blue",pch=17)
points(HOUSES$PRICE,predict(h4_model),col="dark blue",pch=18)
abline(0,1,col="black")
# BEST_MODELS
# x = lm(PRICE ~FLR+ST,data=HOUSES)
# summary(x)