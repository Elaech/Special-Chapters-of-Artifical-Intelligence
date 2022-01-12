library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)
library(rgl)
set.seed(42)

money_dataset = function(){
  raw_dataset = read.csv("D:/Coding/Master/AI/Special-Chapters-of-Artifical-Intelligence/input/task6/bancnotes.dat",
                  sep=',', header = TRUE)
  dataset = subset(raw_dataset,select=-c(Index))
  return(dataset)
}


MONEY = money_dataset()

MONEY.pca = prcomp(MONEY,scale. = TRUE)
MONEY.pca
summary(MONEY.pca)
fviz_eig(MONEY.pca)
cl <- kmeans(MONEY.pca$x[,1:2],2)
MONEY.cluster2d<- as.factor(cl$cluster)
ggbiplot(MONEY.pca,choices=c(1,2), col=MONEY.cluster2d)

cl <- kmeans(MONEY.pca$x[,1:3],2)
MONEY.cluster3d <- as.factor(cl$cluster)
plot3d(MONEY.pca$x[,1:3], col=MONEY.cluster3d, main="k-means clusters", size=7)