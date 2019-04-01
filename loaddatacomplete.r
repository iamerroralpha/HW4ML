download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/acute/diagnosis.data", "acute.txt", quiet = FALSE)
library(rpart)
acutdf <- read.delim2("acute.txt", header = FALSE, sep = "\t", dec = ",",fileEncoding="UCS-2LE")
model <- rpart(
  V7 ~ V1 + V2 + V3 + V4 + V5 + V6, 
  data = acutdf, 
  control = rpart.control(minsplit = 2))

par(xpd = NA, mar = rep(0.7, 4)) 
plot(model, compress = TRUE)
text(model, cex = 0.7, use.n = TRUE, fancy = TRUE, all = TRUE)


library(plyr)

acutdf$V2 <- revalue(acutdf$V2, c("yes"=1))
acutdf$V2 <- revalue(acutdf$V2, c("no"=0))
acutdf$V2 <- as.numeric(as.character(acutdf$V2))

acutdf$V3 <- revalue(acutdf$V3, c("yes"=1))
acutdf$V3 <- revalue(acutdf$V3, c("no"=0))
acutdf$V3 <- as.numeric(as.character(acutdf$V3))

acutdf$V4 <- revalue(acutdf$V4, c("yes"=1))
acutdf$V4 <- revalue(acutdf$V4, c("no"=0))
acutdf$V4 <- as.numeric(as.character(acutdf$V4))

acutdf$V5 <- revalue(acutdf$V5, c("yes"=1))
acutdf$V5 <- revalue(acutdf$V5, c("no"=0))
acutdf$V5 <- as.numeric(as.character(acutdf$V5))

acutdf$V6 <- revalue(acutdf$V6, c("yes"=1))
acutdf$V6 <- revalue(acutdf$V6, c("no"=0))
acutdf$V6 <- as.numeric(as.character(acutdf$V6))

names(acutdf)[7] <- "C1"
names(acutdf)[8] <- "C2"

acutdf$C1 <- revalue(acutdf$C1, c("yes"=1))
acutdf$C1 <- revalue(acutdf$C1, c("no"=0))
acutdf$C1 <- as.numeric(as.character(acutdf$C1))

acutdf$C2 <- revalue(acutdf$C2, c("yes"=1))
acutdf$C2 <- revalue(acutdf$C2, c("no"=0))
acutdf$C2 <- as.numeric(as.character(acutdf$C2))

library(entropy)

ec1<- entropy(discretize(acutdf$C1,2),unit = c("log2"))
ec2<-entropy(discretize(acutdf$C2,2),unit = c("log2"))

#entropy for the first class

#ep1V1<- entropy(discretize(C1.1$V1,54),unit = c("log2")) #the ones that went to the first class
#ep2V1 <-entropy(discretize(C1.0$V1,60),unit = c("log2")) #the ones that went to the second class
# we check those that have the high and low temperature
V1.Low <-subset(acutdf, acutdf$V1<=37.95)

  C1V1.1 <-subset(V1.Low, V1.Low$C1==1)     #For the first class
  C2V1.1 <-subset(V1.Low, V1.Low$C2==1)     #For the second class
  C1V1.0 <-subset(V1.Low, V1.Low$C1==0)
  C2V1.0 <-subset(V1.Low, V1.Low$C2==0)


#Calculate entropy of the low temperature one
  C1e1Low<-c(entropy(c(length(C1V1.0$V1), length(C1V1.1$V1)),unit = c("log2")), length(C1V1.1$V1)+ length(C1V1.0$V1))
  C2e1Low<-c(entropy(c(length(C2V1.0$V1), length(C2V1.1$V1)),unit = c("log2")), length(C2V1.1$V1)+ length(C2V1.0$V1))

#Now for the high temperature
V1.High <-subset(acutdf, acutdf$V1>=37.95)
  
  C1V1.1 <-subset(V1.High, V1.High$C1==1)     #For the first class
  C2V1.1 <-subset(V1.High, V1.High$C2==1)     #For the second class
  C1V1.0 <-subset(V1.High, V1.High$C1==0)
  C2V1.0 <-subset(V1.High, V1.High$C2==0)
#Calculate entropy for the high temperature
  C1e1High<-c(entropy(c(length(C1V1.0$V1), length(C1V1.1$V1)),unit = c("log2")), length(C1V1.1$V1)+ length(C1V1.0$V1))
  C2e1High<-c(entropy(c(length(C2V1.0$V1), length(C2V1.1$V1)),unit = c("log2")), length(C2V1.1$V1)+ length(C2V1.0$V1))

C1e1 <- C1e1High[1]*C1e1High[2]/(C1e1High[2]+C1e1Low[2]) + C1e1Low[1]*C1e1Low[2]/(C1e1High[2]+C1e1Low[2])
C2e1 <- C2e1High[1]*C2e1High[2]/(C2e1High[2]+C2e1Low[2]) + C2e1Low[1]*C2e1Low[2]/(C2e1High[2]+C2e1Low[2])
C1ig1 <- ec1 - C1e1
C2ig1 <- ec2 - C2e1

#Calculate the entropy for the second variable.
#This are the ones taht were meant to be yes
# second variable--------------------------------------------------------------------------------
V2.yes <-subset(acutdf, acutdf$V2==1)
#Subsetting
  C1yes.V2yes <-subset(V2.yes, V2.yes$C1==1)     #For the first class
  C2yes.V2yes <-subset(V2.yes, V2.yes$C2==1)     #For the second class
  C1no.V2yes <-subset(V2.yes, V2.yes$C1==0)     #This one is for the second classification
  C2no.V2yes <-subset(V2.yes, V2.yes$C2==0)

  C1e2yes<-c(entropy(c(length(C1no.V2yes$V1), length(C1yes.V2yes$V1)),unit = c("log2")), length(C1yes.V2yes$V1)+ length(C1no.V2yes$V1))
  C2e2yes<-c(entropy(c(length(C2no.V2yes$V1), length(C2yes.V2yes$V1)),unit = c("log2")), length(C2yes.V2yes$V1)+ length(C2no.V2yes$V1))
  #This are the ones taht were meant to be no
V2.no <-subset(acutdf, acutdf$V2==0)
  C1no.V2no <-subset(V2.yes, V2.no$C1==1)     #For the first class
  C2no.V2no <-subset(V2.yes, V2.no$C2==1)     #For the second class
  C1yes.V2no <-subset(V2.yes, V2.no$C1==0)     #This one is for the second classification
  C2yes.V2no <-subset(V2.yes, V2.no$C2==0)
  
  C1e2no<-c(entropy(c(length(C1no.V2no$V1), length(C1yes.V2no$V1)),unit = c("log2")), length(C1no.V2no$V1)+ length(C1yes.V2no$V1))
  C2e2no<-c(entropy(c(length(C2no.V2no$V1), length(C2yes.V2no$V1)),unit = c("log2")), length(C2no.V2no$V1)+ length(C2yes.V2no$V1))

  C1e2 <- C1e2yes[1]*C1e2yes[2]/(C1e2yes[2]+C1e2no[2]) + C1e2no[1]*C1e2no[2]/(C1e2yes[2]+C1e2no[2])
  C2e2 <- C2e2yes[1]*C2e2yes[2]/(C2e2yes[2]+C2e2no[2]) + C2e2no[1]*C2e2no[2]/(C2e2yes[2]+C2e2no[2])
  C1ig2 <- ec1 - C1e2
  C2ig2 <- ec2 - C2e2
  
#third variable-------------------------------------------------------------------------------
V3.yes <-subset(acutdf, acutdf$V3==1)
  #Subsetting
  C1yes.V3yes <-subset(V3.yes, V3.yes$C1==1)     #For the first class
  C2yes.V3yes <-subset(V3.yes, V3.yes$C2==1)     #For the second class
  C1no.V3yes <-subset(V3.yes, V3.yes$C1==0)     #This one is for the second classification
  C2no.V3yes <-subset(V3.yes, V3.yes$C2==0)
  
  C1e3yes<-c(entropy(c(length(C1yes.V3yes$V1), length(C1no.V3yes$V1)),unit = c("log2")), length(C1yes.V3yes$V1)+ length(C1no.V3yes$V1))
  C2e3yes<-c(entropy(c(length(C2yes.V3yes$V1), length(C2no.V3yes$V1)),unit = c("log2")), length(C2yes.V3yes$V1)+ length(C2no.V3yes$V1))
  #This are the ones taht were meant to be no
V3.no <-subset(acutdf, acutdf$V3==0)
  C1yes.V3no <-subset(V3.no, V3.no$C1==1)     #For the first class
  C2yes.V3no <-subset(V3.no, V3.no$C2==1)     #For the second class
  C1no.V3no <-subset(V3.no, V3.no$C1==0)     #This one is for the second classification
  C2no.V3no <-subset(V3.no, V3.no$C2==0)
  
  C1e3no<-c(entropy(c(length(C1no.V3no$V1), length(C1yes.V3no$V1)),unit = c("log2")), length(C1no.V3no$V1)+ length(C1yes.V3no$V1))
  C2e3no<-c(entropy(c(length(C2no.V3no$V1), length(C2yes.V3no$V1)),unit = c("log2")), length(C2no.V3no$V1)+ length(C2yes.V3no$V1))
  
  #C1e3 <- C1e3yes[1]*C1e3yes[2]/(C1e3yes[2]+C1e3no[2]) + C1e3no[1]*C1e2no[2]/(C1e3yes[2]+C1e3no[2])
  #C2e3 <- C2e3yes[1]*C2e3yes[2]/(C2e3yes[2]+C2e3no[2]) + C2e3no[1]*C2e2no[2]/(C2e3yes[2]+C2e3no[2])
  #C1ig3 <- ec1 - C1e3
  #C2ig3 <- ec2 - C2e3  
  C1ig3 <- ec1 - (C1e3yes[1]*C1e3yes[2]/(C1e3yes[2]+C1e3no[2]) + C1e3no[1]*C1e3no[2]/(C1e3yes[2]+C1e3no[2]))
  print(ec1 - (C1e3yes[1]*C1e3yes[2]/(C1e3yes[2]+C1e3no[2]) + C1e3no[1]*C1e3no[2]/(C1e3yes[2]+C1e3no[2])))
  print(c('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=',C1ig3))
  C2ig3 <- ec2 - (C2e3yes[1]*C2e3yes[2]/(C2e3yes[2]+C2e3no[2]) + C2e3no[1]*C2e3no[2]/(C2e3yes[2]+C2e3no[2]))
#fourth variable--------------------------------

  
V4.yes <-subset(acutdf, acutdf$V4==1)
  #Subsetting
  C1yes.V4yes <-subset(V4.yes, V4.yes$C1==1)     #For the first class
  C2yes.V4yes <-subset(V4.yes, V4.yes$C2==1)     #For the second class
  C1no.V4yes <-subset(V4.yes, V4.yes$C1==0)     #This one is for the second classification
  C2no.V4yes <-subset(V4.yes, V4.yes$C2==0)
  
  C1e4yes<-c(entropy(c(length(C1no.V4yes$V1), length(C1yes.V4yes$V1)),unit = c("log2")), length(C1yes.V4yes$V1)+ length(C1no.V4yes$V1))
  C2e4yes<-c(entropy(c(length(C2no.V4yes$V1), length(C2yes.V4yes$V1)),unit = c("log2")), length(C2yes.V4yes$V1)+ length(C2no.V4yes$V1))
  #This are the ones taht were meant to be no
  
  
V4.no <-subset(acutdf, acutdf$V4==0)
  C1no.V4no <-subset(V4.no, V4.no$C1==1)     #For the first class
  C2no.V4no <-subset(V4.no, V4.no$C2==1)     #For the second class
  C1yes.V4no <-subset(V4.no, V4.no$C1==0)     #This one is for the second classification
  C2yes.V4no <-subset(V4.no, V4.no$C2==0)
  
  C1e4no<-c(entropy(c(length(C1no.V4no$V1), length(C1yes.V4no$V1)),unit = c("log2")), length(C1no.V4no$V1)+ length(C1yes.V4no$V1))
  C2e4no<-c(entropy(c(length(C2no.V4no$V1), length(C2yes.V4no$V1)),unit = c("log2")), length(C2no.V4no$V1)+ length(C2yes.V4no$V1))
  
  C1e4 <- C1e4yes[1]*C1e4yes[2]/(C1e4yes[2]+C1e4no[2]) + C1e4no[1]*C1e4no[2]/(C1e4yes[2]+C1e4no[2])
  C2e4 <- C2e4yes[1]*C2e4yes[2]/(C2e4yes[2]+C2e4no[2]) + C2e4no[1]*C2e4no[2]/(C2e4yes[2]+C2e4no[2])
  C1ig4 <- ec1 - C1e4
  C2ig4 <- ec2 - C2e4  
  
#fifth variable ---------------------------------------------

V5.yes <-subset(acutdf, acutdf$V5==1)
  #Subsetting
  C1yes.V5yes <-subset(V5.yes, V5.yes$C1==1)     #For the first class
  C2yes.V5yes <-subset(V5.yes, V5.yes$C2==1)     #For the second class
  C1no.V5yes <-subset(V5.yes, V5.yes$C1==0)     #This one is for the second classification
  C2no.V5yes <-subset(V5.yes, V5.yes$C2==0)
  
  C1e2yes<-c(entropy(c(length(C1no.V5yes$V1), length(C1yes.V5yes$V1)),unit = c("log2")), length(C1yes.V5yes$V1)+ length(C1no.V5yes$V1))
  C2e2yes<-c(entropy(c(length(C2no.V5yes$V1), length(C2yes.V5yes$V1)),unit = c("log2")), length(C2yes.V5yes$V1)+ length(C2no.V5yes$V1))
  #This are the ones taht were meant to be no
  
  
V5.no <-subset(acutdf, acutdf$V5==0)
  C1no.V5no <-subset(V5.yes, V5.no$C1==1)     #For the first class
  C2no.V5no <-subset(V5.yes, V5.no$C2==1)     #For the second class
  C1yes.V5no <-subset(V5.yes, V5.no$C1==0)     #This one is for the second classification
  C2yes.V5no <-subset(V5.yes, V5.no$C2==0)
  
  C1e2no<-c(entropy(c(length(C1no.V5no$V1), length(C1yes.V5no$V1)),unit = c("log2")), length(C1no.V5no$V1)+ length(C1yes.V5no$V1))
  C2e2no<-c(entropy(c(length(C2no.V5no$V1), length(C2yes.V5no$V1)),unit = c("log2")), length(C2no.V5no$V1)+ length(C2yes.V5no$V1))
  
  C1e5 <- C1e2yes[1]*C1e2yes[2]/(C1e2yes[2]+C1e2no[2]) + C1e2no[1]*C1e2no[2]/(C1e2yes[2]+C1e2no[2])
  C2e5 <- C2e2yes[1]*C2e2yes[2]/(C2e2yes[2]+C2e2no[2]) + C2e2no[1]*C2e2no[2]/(C2e2yes[2]+C2e2no[2])
  C1ig5 <- ec1 - C1e5
  C2ig5 <- ec2 - C2e5  
  
  
#Sixth varaible----------------------------------------
  
  
V6.yes <-subset(acutdf, acutdf$V6==1)
  #Subsetting
  C1yes.V6yes <-subset(V6.yes, V6.yes$C1==1)     #For the first class
  C2yes.V6yes <-subset(V6.yes, V6.yes$C2==1)     #For the second class
  C1no.V6yes <-subset(V6.yes, V6.yes$C1==0)     #This one is for the second classification
  C2no.V6yes <-subset(V6.yes, V6.yes$C2==0)
  
  C1e2yes<-c(entropy(c(length(C1no.V6yes$V1), length(C1yes.V6yes$V1)),unit = c("log2")), length(C1yes.V6yes$V1)+ length(C1no.V6yes$V1))
  C2e2yes<-c(entropy(c(length(C2no.V6yes$V1), length(C2yes.V6yes$V1)),unit = c("log2")), length(C2yes.V6yes$V1)+ length(C2no.V6yes$V1))
  #This are the ones taht were meant to be no
  
  
V6.no <-subset(acutdf, acutdf$V6==0)
  C1no.V6no <-subset(V6.yes, V6.no$C1==1)     #For the first class
  C2no.V6no <-subset(V6.yes, V6.no$C2==1)     #For the second class
  C1yes.V6no <-subset(V6.yes, V6.no$C1==0)     #This one is for the second classification
  C2yes.V6no <-subset(V6.yes, V6.no$C2==0)
  
  C1e2no<-c(entropy(c(length(C1no.V6no$V1), length(C1yes.V6no$V1)),unit = c("log2")), length(C1no.V6no$V1)+ length(C1yes.V6no$V1))
  C2e2no<-c(entropy(c(length(C2no.V6no$V1), length(C2yes.V6no$V1)),unit = c("log2")), length(C2no.V6no$V1)+ length(C2yes.V6no$V1))
  
  C1e6 <- C1e2yes[1]*C1e2yes[2]/(C1e2yes[2]+C1e2no[2]) + C1e2no[1]*C1e2no[2]/(C1e2yes[2]+C1e2no[2])
  C2e6 <- C2e2yes[1]*C2e2yes[2]/(C2e2yes[2]+C2e2no[2]) + C2e2no[1]*C2e2no[2]/(C2e2yes[2]+C2e2no[2])
  C1ig6 <- ec1 - C1e6
  C2ig6 <- ec2 - C2e6  

  
#showing the information-------------------
print("Category 1")
print(c("The information gain for varaible 1 is", C1ig1))
print(c("The information gain for varaible 2 is", C1ig2))
print(c("The information gain for varaible 3 is", C1ig3))
print(c("The information gain for varaible 4 is", C1ig4))
print(c("The information gain for varaible 5 is", C1ig5))
print(c("The information gain for varaible 6 is", C1ig6))

print("Category 2")
print(c("The information gain for varaible 1 is", C2ig1))
print(c("The information gain for varaible 2 is", C2ig2))
print(c("The information gain for varaible 3 is", C2ig3))
print(c("The information gain for varaible 4 is", C2ig4))
print(c("The information gain for varaible 5 is", C2ig5))
print(c("The information gain for varaible 6 is", C2ig6))
      
      