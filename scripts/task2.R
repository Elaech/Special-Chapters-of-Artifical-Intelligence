

ex1 = function(){
  a = scan(what="double", nmax=1)
  b = scan(what="double", nmax=1)
  range_x = seq(from=a,to=b,by=0.001)
  plot(x=range_x,
       y=log2(range_x),
       type="l",
       lwd=2,
       col=c("red"),
       main="f(x)= log2 x, x in [a,b]")
}


ex2 = function(){
  n = 20
  p = seq(from=0.1, to=0.9, by=0.1)
  a = 1
  b = 20
  range_x = a:b
  for(i in 1:length(p)){
    
    
    range_y = dbinom(range_x,
                     size=b,
                     prob=p[i]
    )
    
    pdf(file = sprintf("output/task2/%d.pdf",i),
        width = 4,
        height = 4) 
    
    
    plot(x=range_x,
         y=range_y,
         type="l",
         lwd=2,
         col=c("red"),
         main = i)
    
    dev.off()
  }
  
  
}

ex3 = function(){
  my_mean = 0
  stddev = c(0.5, 1, 2)
  colors = c("red","blue","green")
  range_x = seq(-7,7,length=10000)
  
  for(i in 1:length(stddev)){
    plot(range_x,
         dnorm(range_x, mean = 0, sd=stddev[i]),
         type="l",
         lwd=2,
         col = colors[i]
         )
    if(i != length(stddev)){
      par(new=TRUE)
    }
  }
}


runif_wrapper = function(n){
  return(runif(n,0,20))
}

rnorm_wrapper = function(n){
  return(rnorm(n,mean=20, sd=0.1))
}

CLT = function(n, method){
  x = c(0)
  for(i in 1:1000){
    x[i] = mean(method(n))
  }
  return(x)  
}

ex4 = function(){
  n = c(1,5,10,100)
  selected_method = rnorm_wrapper
  for(i in 1:length(n)){
    range_y = CLT(n[i],method=selected_method)
    hist(range_y,main=n[i])
  }
}











main = function(){
  EXERCISE = strtoi(readline())
  switch (EXERCISE,
          ex1(),
          ex2(),
          ex3(),
          ex4()
  )
}


main()