library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

money_dataset = function(){
  raw_dataset = read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task6/bancnotes.dat",
                  sep=',', header = TRUE)
  dataset = subset(raw_dataset,select=-c(Index))
  return(dataset)
}


MONEY = money_dataset()

MONEY.pca = prcomp(MONEY,scale= TRUE)
MONEY.pca
summary(MONEY.pca)


ggbiplot(MONEY.pca,choices=c(1,2))
ggbiplot(MONEY.pca,choices=c(1,3))
ggbiplot(MONEY.pca, choices=c(2,3))

