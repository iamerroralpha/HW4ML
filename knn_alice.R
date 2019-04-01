# Read data
ads2 <- read.csv("ads2.csv")

View(ads2)


# Transforming the dependent variable to a factor
ads2$V1559 = as.factor(ads2$V1559)

# load library
library(caret)
library(e1071)

#Partitioning the data into training and validation data
set.seed(101)
knn.index = createDataPartition(ads2$V1244, p = 0.7, list = F )
train = ads2[knn.index,]
validation = ads2[-knn.index,]

# Explore data
dim(train)
dim(validation)
names(train)
head(train)
head(validation)

# Setting levels for both training and validation data
levels(train$V1559) <- make.names(levels(factor(train$V1559)))
levels(validation$V1559) <- make.names(levels(factor(validation$V1559)))

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(V1559~. , data = train, method = "knn", preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

# Summary of model
model1
plot(model1)

# Validation
valid_pred <- predict(model1,validation, type = "prob")

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],validation$V1559)

# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,"auc")
perf_val

# Plot AUC
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks
