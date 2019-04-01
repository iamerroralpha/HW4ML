ads <- read.csv("ads2.csv")
target <- ads$V1559
ads[1557]
library(plyr)
library(FSelector)

target <- revalue(target, c("ad."=1))
target <- revalue(target, c("nonad."=0))
traindata <- ads[3:1556] # We leave out the last row because it contains the actual classification


ig3<-information.gain(target ~., data = traindata ,unit = "log2")
which(ig2==max(ig2)) 

library(tree)

tree.acutC1 <- tree(target ~., data = traindata, wts = TRUE)
plot(tree.acutC1)
text(tree.acutC1, splits = TRUE, label ="yval", pretty = TRUE)
o<-order(ig3$attr_importance)[length(ig2$attr_importance)]
o<-order(ig3$attr_importance)[length(ig2$attr_importance)-1]
o<-order(ig3$attr_importance)[length(ig2$attr_importance)-2]
o<-order(ig3$attr_importance)[length(ig2$attr_importance)-3]

#take out all the variables that have zer information gain
nads <- data.frame()
for (iterator in 1:length(ig3$attr_importance))
{
  if (ig3$attr_importance[iterator]!=0)
    nads <- rbind(nads, "e"= c(ig3$attr_importance[iterator],iterator+4))
}

names(nads)[1]<-'ig'
names(nads)[2]<-'num'

which(nads$num==1336)