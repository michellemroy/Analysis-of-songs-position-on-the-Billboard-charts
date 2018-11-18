#load the file
music <- read.csv(file="C:/Users/Mishal/Desktop/data sets for DA/mich.csv",header=T,na.strings=c(""))
#to analyse for which attributes NA is present
summary(music)
#duration and tempo have NAs that can be replaced by the mean values

#dealing with NAs
music$Duration <- as.numeric(as.character(music$Duration))
class(music$Duration)
music$Duration[is.na(music$Duration)] <- median(music$Duration, na.rm=TRUE)

music$Tempo <- as.numeric(as.character(music$Tempo))
class(music$Tempo)
music$Tempo[is.na(music$Tempo)] <- mean(music$Tempo, na.rm=TRUE)

#catagorising top 50 as hit ie 1 and below that as not hit ie 0
music$hit[music$rank<=50] <- 1
music$hit[music$rank>50] <- 0
music$hit=as.factor(music$hit)

library(caret)

#create a training and a test set
smp_size <- floor(0.70 * nrow(music))

## set the seed to make your partition reproductible
set.seed(123)
#get the index of the training data
train_ind <- sample(seq_len(nrow(music)), size = smp_size)

#divide into test and train
train <- music[train_ind, ]
test <- music[-train_ind, ]

library(e1071)
#make svm model 1
model_svm <- svm(as.factor(train$hit)~., train,kernel='radial',cost=100,type ="C-classification")
summary(model_svm)

#predict using model and test data
pred <- predict(model_svm, test)
pred #the predicted values for test

xtab <- table(pred,test$hit)
confusionMatrix(xtab)

set.seed(1)
#tuning to chose the best model among all combinations of class, kernel and gamma
#tune.out <- tune(svm,music$hit ~ as.numeric(unlist(music["Duration"])) + as.numeric(unlist(music["KeySignature"])) + as.numeric(unlist(music["KeySignatureConfidence"])) + as.numeric(unlist(music["Tempo"])) + as.numeric(unlist(music["TimeSignature"])) + as.numeric(unlist(music["Year"])) + as.numeric(unlist(music["Energy"])) + as.numeric(unlist(music["ArtistFamiliarity"])) + as.numeric(unlist(music["Hotness"])) + as.numeric(unlist(music["end_of_fade_in"])) + as.numeric(unlist(music["key"])) + as.numeric(unlist(music["keyConfidence"])) + as.numeric(unlist(music["Loudness"])) + as.numeric(unlist(music["mode"])) + as.numeric(unlist(music["mode_confidence"])) + as.numeric(unlist(music["rank"])), data=music, kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),epsilon = seq(0,1,0.01)))
#summary(tune.out)

#finetuned model 2
model_svm2 <- svm(as.factor(train$hit)~., train,kernel='radial',cost=5,epsilon=0,type="C-classification")
summary(model_svm2)
pred5 <- predict(model_svm2, test)
pred5
tab5 <- table(pred5,test$hit)
res <- confusionMatrix(tab5)
res #final confusion matrix result
precision <- res$byClass['Pos Pred Value']  #precision  
recall <- res$byClass['Sensitivity']  #sensitivity
precision
recall

################################################################################
#further we can check accuracy if we check for a particulr time interval


music$Year <- as.numeric(as.character(music$Year))
mus<-subset(music, Year>1960 && Year<1980)

#create a training and a test set
smp_size <- floor(0.50 * nrow(mus))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mus)), size = smp_size)

train <- mus[train_ind, ]
test <- mus[-train_ind, ]
dim(train)

library(e1071)
model_svm <- svm(as.factor(train$hit)~., train,kernel='radial',cost=100,type ="C-classification")
summary(model_svm)

pred <- predict(model_svm, test)
dim(pred)

xtab <- table(pred,test$hit)
res <- confusionMatrix(xtab)
precision <- res$byClass['Pos Pred Value']    
recall <- res$byClass['Sensitivity']
precision
recall

