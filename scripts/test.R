
ex1 = function(){
  print("Hello World!")
}


f2 = function(x){
  return ((x*1.0-5)/2.624669)
}

ex2 = function(){
  x = c(1, 8, 2, 6, 2, 8, 8, 5, 5, 5)
  
  cat("Presenting the data:\n",x,"\n")
  cat(sprintf("a)Sum div 10: %f\n", sum(x)/10))
  cat("b)log2 over array\n",log2(x),"\n")
  cat(sprintf("c)Max - min: %d\n",max(x)-min(x)))
  y_list = f2(x)
  cat("d)Apply function over array:\n",y_list, "\n")
  cat(sprintf("e)Average of y_list: %f\n",mean(y_list)))
  cat(sprintf("e)DevStd of y_list: %f\n",sd(y_list)))
}



more_than_40 = function(x){
  return(x>40)
}

ex3 = function(){
  factura = c(46,33,39,37,36,30,48,32,49,35,30,48)
  
  cat("Factura:\n", factura, "\n")
  cat(sprintf("Suma facuta pe an: %d\n", sum(factura)))
  cat(sprintf("Factura maxima pe an: %d\n", max(factura)))
  cat(sprintf("Factura minima pe an: %d\n", min(factura)))
  nr_luni_gt_40 = length(factura[factura>40])
  cat(sprintf("Numarul de luni in care s-a depasit 40: %d\n", nr_luni_gt_40))
  cat(sprintf("Procentajul numarilor de luni in care s-a depasit 40 din total: %f%s\n",
              nr_luni_gt_40 * 100/ length(factura),"%"))
}


standardize = function(x){
  z = (x - mean(x)) / sd(x)
  return( z)
}

ex4 = function(){
  # 5 6.2 3 4 7 9.1
  x = scan()
  cat("The array is:\n",x,"\n")
  cat(sprintf("Min : %f\n", min(x)))
  cat(sprintf("Max : %f\n", max(x)))
  cat(sprintf("Mean: %f\n", mean(x)))
  cat(sprintf("Median: %f\n", median(x)))
  cat(sprintf("Standard deviation: %f\n", sd(x)))
  cat("The sorted array is:\n",sort(x),"\n")
  scaled_x = standardize(x)
  cat("The standardized array is:\n",scaled_x,"\n")
  cat(sprintf("The standardized array has a mean of %f and stdev of %f",
              mean(scaled_x),
              sd(scaled_x)))
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