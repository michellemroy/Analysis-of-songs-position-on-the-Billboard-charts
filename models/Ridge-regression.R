mydata <- read.csv("file:///C:/Users/Admin/Documents/DA_project/mich.csv")
library(ridge)
library(SDMTools)
library(caret)
for(i in c(1:nrow(mydata)))
{if(mydata$rank[i] >= 50)
  mydata$hit[i] = 0
}
mydata$Danceability <- as.numeric(mydata$Danceability)
mydata$Duration <- as.numeric(mydata$Duration)
mydata$KeySignature <- as.numeric(mydata$KeySignature)
mydata$KeySignatureConfidence <- as.numeric(mydata$KeySignatureConfidence)
mydata$Tempo <- as.numeric(mydata$Tempo)
mydata$TimeSignature <- as.numeric(mydata$TimeSignature)
mydata$TimeSignatureConfidence <- as.numeric(mydata$TimeSignatureConfidence)
mydata$Energy <- as.numeric(mydata$Energy)
mydata$ArtistFamiliarity <- as.numeric(mydata$ArtistFamiliarity)
mydata$Hotness <- as.numeric(mydata$Hotness)
mydata$end_of_fade_in <- as.numeric(mydata$end_of_fade_in)
mydata$key <- as.numeric(mydata$key)
mydata$keyConfidence <-as.numeric(mydata$keyConfidence)
mydata$Loudness <- as.numeric(mydata$Loudness)
mydata$mode <- as.numeric(mydata$mode)
mydata$mode_confidence <- as.numeric(mydata$mode_confidence)
mydata$start_of_fade_out <- as.numeric(mydata$start_of_fade_out)
mydata$rank <- as.numeric(mydata$rank)
#remove danceability nd energy as the values are all 0
mydata$Danceability <- NULL
mydata$Energy <- NULL
mydata$KeySignature <- NULL
mydata$keyConfidence <- NULL
mydata$start_of_fade_out <- NULL
mydata$Duration <- NULL

mydata <- na.omit(mydata)

#divide the data into train and test
smp_size <- floor(0.70 * nrow(mydata))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)

test <- mydata[c(1:1062), ]
train <- mydata[c(1063:4562), ]


train <- na.omit(train)
test <- na.omit(test)
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

#seperating predictor variables
x1 = as.matrix(test[,c(1:10)])
y <- train$hit
x <-  as.matrix(train[,c(1:10)])
x <- na.omit(x)
x1 <- na.omit(x1)
lambdas <- 10^seq(0.001, -1, by = -.01)
library(glmnet)
fit <- glmnet(x, y, alpha = 0, lambda = 1)
summary(fit)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
y_predicted <- predict(fit,x1)
y_predicted <- as.integer(y_predicted)

l <- union(y_predicted, test$hit)
Table2 <- table(factor(y_predicted, l), factor(test$hit, l))
print("ridge")
confusionMatrix(Table2)
accuracy(test$hit, y_predicted, threshold = 0.5)
