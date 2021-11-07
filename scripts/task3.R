

alchool_dataset = function(){
  return(read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task3/alcool.dat",
                 sep=',', header = TRUE))
}


iq_dataset = function(){
  return(read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task3/iq.dat",
                  sep=',', header = TRUE))
}

ex1 = function(){
  df = alchool_dataset()
  plot(df[,2:3])
  cor(df[,2:3])
}

ex2 = function(){
  df = iq_dataset()
  lmNota = lm(Nota~IQ, data= df)
  print(summary(lmNota))

  
  intercept = lmNota[[1]][1]
  x = lmNota[[1]][2]
  
  plot(df[,2:3])
  abline(intercept,x,lwd=3,col="red")
  
  
  iq1 = 115
  iq1_pred = predict(lmNota,data.frame(IQ=c(iq1)))
  cat(sprintf("Nota pentru %f IQ este %f\n",iq1,iq1_pred))
  
  iq2 = 130
  iq2_pred = predict(lmNota,data.frame(IQ=c(130)))
  cat(sprintf("Nota pentru %f IQ este %f\n",iq2,iq2_pred))
}


special_relation = function(x,a,b,e){
  return(a+x*b+e)
}

generate_observations= function(m,a,b,xmin,xmax,sigma){
  eps = rnorm(m,mean=0,sd=sigma)
  x = runif(m,min=xmin,max=xmax)
  y = special_relation(x,a,b,eps)
  return(do.call(rbind, Map(data.frame, X=x, Y=y)))
}


ex3 = function(){
  generate_observations(100,10,0.8,-200,200,1.5)
}

special_linear_regresion = function(df_data,a,b){
  lmY = lm(Y~X,data = df_data)
  ap = lmY[[1]][1]
  bp = lmY[[1]][2]
  cat(sprintf("a' is %f\nb' is %f\n",ap,bp))
  print(confint(lmY,level=0.95))
  return(c(ap,bp))
}

ex4 = function(){
  df_data = generate_observations(100,10,0.8,-200,200,1.5)
  params = special_linear_regresion(df_data,10,0.8)
}

special_plot = function(xmin,xmax,a1,b1,a2,b2,title){
  x_vals = seq(from=xmin,to=xmax,by=(xmax-xmin)/10000)
  y_vals = a1+b1*x_vals
  pdf(file = sprintf("output/task3/%s.pdf",title),
      width = 8,
      height = 8)
  plot(x=x_vals,
       y=y_vals,
       type="l",
       lwd=1,
       col=c("black"),
       )
  abline(a2,b2,lwd=1,col=c("blue"),lty=2)
  invisible(dev.off())
}

experiment = function(m,a,b,xmin,xmax,sigma,title){
  df_data = generate_observations(m,a,b,xmin,xmax,sigma)
  params = special_linear_regresion(df_data,a,b)
  special_plot(xmin,xmax,a,b,params[1],params[2],title)
}

ex5 = function(){
  cat("The statistics for experiment a)\n")
  experiment(100,10,0.8,-200,200,1.5,"a")
  
  cat("The statistics for experiment b)\n")
  experiment(10,10,0.8,-5,5,1,"b")
  
  cat("The statistics for experiment c)\n")
  experiment(10000,10,0.8,-5,5,1,"c")
  
  cat("The statistics for experiment d)\n")
  experiment(10,10,0.8,5,5.2,1,"d")
  
  cat("The statistics for experiment e)\n")
  experiment(10000,10,0.8,5,5.2,1,"e")
  
  cat("The statistics for experiment f)\n")
  experiment(10,10,0.8,5,5.2,0.01,"f")
  
}





main = function(){
  EXERCISE = strtoi(readline())
  switch (EXERCISE,
          ex1(),
          ex2(),
          ex3(),
          ex4(),
          ex5()
  )
}


main()