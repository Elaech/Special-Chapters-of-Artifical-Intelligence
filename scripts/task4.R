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

reg_on_houses = function(features,p){
  reg_formula = make_as_formula(features)
  model = lm(reg_formula,data=HOUSES)
  model_summary = summary(model)
  MODEL_REGS[[length(MODEL_REGS)+1]] <<- c(P,
                                          toString(reg_formula),
                                          deviance(model),
                                          model_summary$r.squared,
                                          model_summary$adj.r.squared,
                                          ols_mallows_cp(model,FULL_MODEL))
}

# GLOBALS
HOUSES = houses_dataset()
P = 0
CONSTANT_MODEL = lm(PRICE ~ 0, data=HOUSES)
FULL_MODEL = lm(PRICE~., data=HOUSES)
BEST_MODELS = list()
MODEL_REGS = list()

# MAIN
main = function(){
  # GET FEATURE COLUMNS
  columns = colnames(HOUSES)
  features = columns[-1]
  for(p in 1:(length(features))){
    P <<- p
    # PROCESS ALL MODEL COMBINATIONS
    model_summaries = combn(features,p,FUN=reg_on_houses,simplify = FALSE)
    
    # APPEND THE STATISTICS TO A DATAFRAME
    model_data = as.data.frame(do.call(rbind, MODEL_REGS))
    colnames(model_data) =  c("P","Fields","RSS","R2","R2a","cp")
    
    # GET THE MODELS THAT EACH BEST OPTIMISE RSS R2 R2a or CP
    best_model_data =list(
      model_data[which.min(model_data$RSS),],
      model_data[which.max(model_data$R2),],
      model_data[which.max(model_data$R2a),],
      model_data[which.min(closer_to_cp(model_data$cp,p)),]
    )
    # DROP DUPLICATE MODELS AND CONCATENATE
    best_model_data = distinct(bind_rows(best_model_data))
    
    # ADD BEST MODELS FOR P PARAMS INTO A GLOBAL LIST
    BEST_MODELS[[length(BEST_MODELS)+1]] <<- best_model_data
    
    # RESET THE LIST FOR ALL THE MODEL COMBINATIONS
    MODEL_REGS <<- list()
  }
  
  # CONCATENATE ALL THE BEST MODELS DATAFRAME INTO A BIG ONE
  BEST_MODELS <<- bind_rows(BEST_MODELS)
}


main()

make_plots()

BEST_MODELS






