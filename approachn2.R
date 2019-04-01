#trControl = trainControl(  method = "cv", number = 10,verboseIter = TRUE)
library(FSelector)
library(tree)
acutdfr <- read.delim2("acute.txt", header = FALSE, sep = "\t", dec = ",",fileEncoding="UCS-2LE")
par(mfrow=c(1,2))
acutdfr$V1r <- cut(acutdfr$V1,c(35,37.95,50))

#ads[3:1556]

tree.acutC1 <- tree(acutdfr$V7 ~  acutdfr$V1 +acutdfr$V2 + acutdfr$V3 + acutdfr$V4 + acutdfr$V5 + acutdfr$V6, data = acutdfr, wts = TRUE)
ig1<-information.gain(acutdfr$V7 ~  acutdfr$V1r + acutdfr$V3 + acutdfr$V2 + acutdfr$V4 + acutdfr$V5 + acutdfr$V6, data = acutdfr,unit = "log2")
plot(tree.acutC1,main='Decision tree for class 1')
text(tree.acutC1, splits = TRUE, label ="yval", pretty = TRUE)

predict.tree.1 <- data.frame(predict(tree.acutC1,acutdfr[1:6]))

tree.acutC2 <- tree(acutdfr$V8 ~  acutdfr$V1 +acutdfr$V2 + acutdfr$V3 + acutdfr$V4 + acutdfr$V5 + acutdfr$V6, data = acutdfr, wts = TRUE)
ig2<-information.gain(acutdfr$V8 ~  acutdfr$V1r + acutdfr$V2 + acutdfr$V3 + acutdfr$V4 + acutdfr$V5 + acutdfr$V6, data = acutdfr,unit = "log2")
plot(tree.acutC2,main='Decision tree for class 2')
text(tree.acutC2, splits = TRUE, label ="yval", pretty = TRUE)

predict.tree.2 <- data.frame(predict(tree.acutC2,acutdfr[1:6]))

for (iterator in 1:nrow(acutdf))
{
  buffer<-0
  for (column in c(4, 5, 3)) #this are the values that give us the best information for the class 1 in the dataset
  {
    #print(ig1$attr_importance[column])
    #print( acutdf[1,as.integer(column)])
    buffer<-buffer + acutdf[iterator,as.integer(column)] * 100*ig1$attr_importance[column]
  }
  acutdf$knn1[iterator]<-  buffer
  
}


for (iterator in 1:nrow(acutdf))
{
  buffer<-0
  for (column in c(1, 2, 3)) #this are the values that give us the best information for the class 2 in the dataset
  {
    buffer<-buffer + acutdf[iterator,as.integer(column)] * 100*ig2$attr_importance[column]
  }
  acutdf$knn2[iterator]<-  buffer
  
}

C1.1 <-data.frame(subset(acutdf$knn1, acutdf$C1==1))     #For the first class
C1.0 <-data.frame(subset(acutdf$knn1, acutdf$C1==0))

C2.1 <-data.frame(subset(acutdf$knn2, acutdf$C2==1) )    #For the second class
C2.0 <-data.frame(subset(acutdf$knn2, acutdf$C2==0))

par(mfrow=c(2,2))

boxplot(C1.0,main='Scatterplot of the distances of the C1=0')
boxplot(C1.1,main='Scatterplot of the distances of the C1=1')
plot(C1.0,main='Plot of the distances of the C1=0')
plot(C1.1,main='Plot of the distances of the C1=1')

par(mfrow=c(2,2))
boxplot(C2.0,main='Scatterplot of the distances of the C2=0')
boxplot(C2.1,main='Scatterplot of the distances of the C2=1')
plot(C2.0,main='Plot of the distances of the C2=0')
plot(C2.1,main='Plot of the distances of the C2=1')



#calculate the distances for the first class------------------------------------------------------------
dist1<-data.frame(1:120)

for (outerit in 1:nrow(acutdf))
{
  for (iterator in 1:nrow(acutdf))
  {
    buffer<-0
    for (column in c(4, 5, 3)) #this are the values that give us the best information for the class 1 in the dataset
    {
      #print(ig1$attr_importance[column])
      #print( acutdf[1,as.integer(column)])
      buffer<-buffer + ((acutdf[iterator,as.integer(column)] - acutdf[outerit,as.integer(column)]) * 100*ig1$attr_importance[column])**2
    }
    dis[iterator] <- buffer**0.5
  }
  dist1[outerit]<-dis
}
dist1$"index" <-1:120
#calculate the distances for the second class------------------------------------------------------------

dist2<-data.frame(1:120)

for (outerit in 1:nrow(acutdf))
{
  for (iterator in 1:nrow(acutdf))
  {
    buffer<-0
    for (column in c(1, 2, 3)) #this are the values that give us the best information for the class 1 in the dataset
    {
      #print(ig1$attr_importance[column])
      #print( acutdf[1,as.integer(column)])
      buffer<-buffer + ((acutdf[iterator,as.integer(column)] - acutdf[outerit,as.integer(column)]) * 100*ig2$attr_importance[column])**2
    }
    dis[iterator] <- buffer**0.5
  }
  dist2[outerit]<-dis
}
dist2$"index" <-1:120


















#overall knn implementation----------------------------------------------

error1<-data.frame()
error2<-data.frame()


kvals <- c(1,3,5,7,9,11,13,15)
for ( k in kvals)
{
  


#knn implementation for the first class------------------------------------------------------------
#the knn implementation
knn1<-data.frame(1:120)
for (outerit in 1:nrow(acutdf))
{
  
  decision <- 0
  for (iterator in 1:k)
  {
    decision <- decision + acutdf$C1[order(dist1[outerit])[iterator]]
    
  }
  knn1$two[outerit] <- decision
  if (decision > 4.5)
  {
    knn1$one[outerit]<-1
  }
  if (decision < 4.5)
  {
    knn1$one[outerit]<-0
  }
  knn1$three[outerit] <- outerit
}

#knn implementation for the first class------------------------------------------------------------
#the knn implementation
knn2<-data.frame(1:120)
for (outerit in 1:nrow(acutdf))
{
  
  decision <- 0
  for (iterator in 1:k)
  {
    decision <- decision + acutdf$C2[order(dist2[outerit])[iterator]]
    
  }
  knn2$two[outerit] <- decision
  if (decision > 4.5)
  {
    knn2$one[outerit]<-1
  }
  if (decision < 4.5)
  {
    knn2$one[outerit]<-0
  }
  knn2$three[outerit] <- outerit
}

#calculate the errors --------------------------------------------------

error1 <- rbind(error1, data.frame(k,(sum(knn1$one-acutdf$C1)**2)**0.5))
error2 <- rbind(error2, data.frame(k,(sum(knn2$one-acutdf$C2)**2)**0.5))
#  c(sum(knn1$one-acutdf$C1),k)
}


names(error1)[2]<-"err"
names(error2)[2]<-"err"


require(ggplot2)

#print(qplot(error1$k, error1$err, data=error1))
#print(qplot(error2$k, error2$err, data=error2))
q1 <- print(qplot(error1$k, error1$err, data=error1)) + geom_line()
q2 <- print(qplot(error2$k, error2$err, data=error2)) + geom_line()
library(gridExtra)
grid.arrange(q1, q2, nrow = 1)

#pred1 <- predict(tree.acutC1,acutdfr)
#error1 <- as.numeric(pred1)  - as.numeric(acutdfr$V7)

#pred2 <- predict(tree.acutC2,acutdfr)
#error2 <- as.numeric(pred2) - as.numeric(acutdfr$V8)
