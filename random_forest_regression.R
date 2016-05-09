library(randomForest)
library(datasets)

setwd("C:/Users/Vikaasa/IdeaProjects/Spark-Workshop")
# yelp_test = read.csv("traindata_randomforests_ip.csv")
yelp_train = read.csv("traindata_randomforests_ip.csv")
colnames(yelp_train)
summary(yelp_train)
yelpData <- yelp_train[,c('clusters','regression_value',
                           
                           'after_Jan2010_count',
                           'after_Jan2014_count',
                           'after_Jun2014_count',
                           'after_Jan2015_count',
                           'weighted_product_users_rating',
                           'ratings_avg',
                           'users_sum'
                           )]

str(yelpData)
colnames(yelpData)

## setting the seed to make our partition reproducable exactly the same way:
set.seed(4410)
## Taking training dataset as 80% of the sample size:
smp_size <- floor(0.80 * nrow(yelpData))
train_ind <- sample(seq_len(nrow(yelpData)), size = smp_size)
train <- yelpData[train_ind, ]
test <- yelpData[-train_ind, ]

## randomizing the test dataset:
smp_size <- floor(1* nrow(test))
test_ind <- sample(seq_len(nrow(test)), size = smp_size)
test <- test[test_ind, ]
set.seed(4410)
yelp.rf <- randomForest(regression_value ~ ., data=train, mtry=6,
                         importance=TRUE, na.action=na.omit)
print(yelp.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(yelp.rf), 2)
# yelp.pred <- predict(yelp.rf, testData, predict.all=TRUE)
yelp.pred <- predict(yelp.rf, test, nodes=TRUE)
test[,"predicted_value"]=yelp.pred
table(observed = test[, "regression_value"], predicted = yelp.pred)
head(test, n=10)
write.csv(test, file = "rf_op_testonly.csv")

tuneresult<-tuneRF(train[,-2], train[,2], 3, ntreeTry=50, stepFactor=2, improve=0.05,
                  trace=TRUE, plot=TRUE, doBest=TRUE)
round(importance(tuneresult), 2)
yelptune.pred <- predict(tuneresult, test)
test[,"predicted_value"]=yelptune.pred
table(observed = test[, "regression_value"], predicted = yelptune.pred)
head(test, n=10)
write.csv(test, file = "rf_tune_op.csv")

yelp.pred <- predict(yelp.rf, yelpData, nodes=TRUE)
yelp_train[,"predicted_value"]=yelp.pred
table(observed = yelpData[, "regression_value"], predicted = ozone.pred)
head(test, n=10)
write.csv(yelp_train, file = "regression_model_op.csv")

varImpPlot(yelp.rf)
plot(yelp.rf)
plot(yelp_train$regression_value, yelp.pred, ylab="predicted_value", xlab="regression_value", main="Accuracy of RF model") 

