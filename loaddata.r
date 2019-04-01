library("rpart")
acut <- read.delim2("acute.txt", header = FALSE, sep = "\t", dec = ",",fileEncoding="UCS-2LE")
model <- rpart(
  V7 ~ V1 + V2 + V3 + V4 + V5 + V6, 
  data = acut, 
  control = rpart.control(minsplit = 2))

par(xpd = NA, mar = rep(0.7, 4)) 
plot(model, compress = TRUE)
text(model, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)