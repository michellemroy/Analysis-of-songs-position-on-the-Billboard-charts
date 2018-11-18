mydata <- read.csv("file:///C:/Users/Admin/Documents/DA_project/mich.csv")

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
count = 0
#way-1 ( which says attributes 2,9,1 are highly correlated)
set.seed(10)
num_data <- mydata[,c(2,3,4,5,6,7,8,11,12,13,14,15,16,17,18,19,20,21)]
num_data <- na.omit(num_data)
num_data$Energy <- NULL
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix taking only the numeric attributes into account and non missing values
correlationMatrix <- cor(num_data)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#way-2

#ridge regression,pipeline , logistic regression(hit or not hit) , svms , kmeans
#https://www.dezyre.com/article/top-10-machine-learning-algorithms/202
#plot the graphs for prediction
num_data <- data.frame(num_data)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(rank~., data=num_data, method="rf", preProcess="scale", trControl=control,importance = TRUE)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#way-3 (Random forest machine learning algortihm)
#lower value of rmse indicates better fit
#and RMSE is observed while performing regression
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(num_data[,c(1:17)], num_data[,18], sizes=c(1:17), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#plotting the correlation plot of all the numeric attributes
res <- cor(num_data ,method = "pearson", use = "complete.obs")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)


head(round(res,2))

