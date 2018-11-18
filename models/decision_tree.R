songs <- read.csv("/Users/simrandhinwa/Documents/mich.csv")
library(magrittr)
library(ROCR)
library(pROC)
library(rpart)
library(caret)
library(party)

dat = songs
data = na.omit(dat)
data$hit_flop <- ifelse(data$rank > 50,0,1)
newData = data[1:500,]
test = data[501:700,]
set.seed(3333)
library(rpart)
library(rpart.plot)
training$Title = NULL


rtree_fit2 <- rpart(hit_flop ~ X + Danceability + Duration + Tempo + Year + Energy + Hotness + Loudness + KeySignature + KeySignatureConfidence + TimeSignature + TimeSignatureConfidence + ArtistFamiliarity + end_of_fade_in + key + keyConfidence + mode + mode_confidence + start_of_fade_out ,newData, parms = list(split = 'information'), maxdepth = 1, minsplit = 1, minbucket = 1, cp = -1)

print(rtree_fit2)

rpart.plot(rtree_fit2)


tree_roc2 <- rtree_fit2 %>%
predict(newdata = test) %>%
prediction(test$hit_flop) %>%
performance("tpr", "fpr")

#plot(tree_roc1, auc = TRUE)
print(auc)


