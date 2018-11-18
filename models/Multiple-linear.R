data <- read.csv("file:///C:/Users/Admin/Documents/DA_project/mich.csv")
library(caret)
library(e1071)
library(magrittr)
library(ROCR)
library(pROC)
#remove danceability nd energy as the values are all 0
data$Danceability <- NULL
data$Energy <- NULL
for(i in c(1:nrow(data)))
  {if(data$rank[i] >= 50)
      data$hit[i] = 0
  }
data <- na.omit(data)
data$KeySignature <- NULL
data$keyConfidence <- NULL
data$start_of_fade_out <- NULL
data$Duration <- NULL
#divide the data into train and test
smp_size <- floor(0.70 * nrow(data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[c(1:3500), ]
test <- data[c(3501:4562), ]
test$Year <- NULL
train$Year <- NULL
test$Title <- NULL
train$Title <- NULL
test$X <- NULL
train$X <- NULL
test$Tempo <- NULL
train$Tempo <- NULL
test$Hotness <- as.numeric(test$Hotness)
test$Hotness <- round(test$Hotness,digits = 2)
train$Hotness <- as.numeric(train$Hotness)
train$Hotness <- round(train$Hotness,digits = 2)
train$rank <- NULL
test$rank <- NULL
#perform multiple linear regression
model1 <- lm(hit ~  KeySignatureConfidence +  TimeSignature + TimeSignatureConfidence + ArtistFamiliarity + Hotness + end_of_fade_in + key + Loudness + mode + mode_confidence , data=train)
pred1 <- predict(model1, test)
pred1 <- as.integer(pred1)
l <- union(pred1, test$hit)
#plot ROC
roc = model1 %>%
  predict(newdata = test) %>%
  prediction(test$hit) %>%
  performance("tpr","fpr")
plot(roc,auc=TRUE)
print(auc)
#plot confusion matrix
l <- union(pred1, test$hit)
Table2 <- table(factor(pred1, l), factor(test$hit, l))
confusionMatrix(Table2)
accuracy(test$hit, pred1, threshold = 0.5)

