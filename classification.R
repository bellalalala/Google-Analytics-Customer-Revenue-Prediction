################Random Forest#############
library(tidyverse)
options(scipen = 200)
df_classification <-  read.csv("C:\\Users\\81468\\OneDrive\\Documents\\DS5220\\project\\dataset\\df_Classification.csv")
df_classification$fullVisitorId <- as.character(df_classification$fullVisitorId)
memory.limit(10000000000000)
#fill GDP
mean_gdp <- mean(df_classification$GDP, na.rm = T)
median_gdp <- median(df_classification$GDP, na.rm = T)
df_classification[is.na(df_classification)] <- median_gdp
write.csv(df_classification, "df_classification.csv", row.names = F)

library(modelr)
df_part <- resample_partition(df_classification, c(train = 0.6, evl = 0.2, test = 0.2))
train <- data.frame(df_part$train)
evl <- data.frame(df_part$evl)
test <- data.frame(df_part$test)

#########1. upsampling of training set############
train$if_tr <- train$transactionRevenue > 0
count(train, if_tr)
rep_ele <- train[train$if_tr, ]
 for(i in 1:37) { 
   train <- rbind(train, rep_ele) 
 }

count(train, if_tr)
train_new <- select(train, -if_tr)

getwd()
setwd("C:/Users/81468/OneDrive/Documents/DS5220/project/dataset")
write.csv(train_new, "train_classification.csv", row.names = F)

test$if_tr <- test$transactionRevenue > 0
count(test, if_tr)
test <- select(test, -if_tr)
write.csv(test, "test_classification.csv", row.names = F)

evl$if_tr <- evl$transactionRevenue > 0
count(evl, if_tr)
evl <- select(evl, -if_tr)
write.csv(evl, "evl_classification.csv", row.names = F)

##########2.random forest for classification############
train <- read.csv("train_classification.csv")

train$isMobile <- as.factor(train$isMobile)
train$date <- as.factor(train$date)
train$bounces <- as.factor(train$bounces)
train$newVisits <- as.factor(train$newVisits)
train$campaign <- as.factor(train$campaign)
train$isTrueDirect <- as.factor(train$isTrueDirect)
train$adContent <- as.factor(train$adContent)
train$adwordsClickInfo.isVideoAd <- as.factor(train$adwordsClickInfo.isVideoAd)
train$city <- as.factor(train$city)
train$GDP <- log(train$GDP)

train$if_tr <- train$transactionRevenue > 0
train$if_tr <- as.factor(train$if_tr)
count(train, if_tr)

evl$isMobile <- as.factor(evl$isMobile)
evl$date <- as.factor(evl$date)
evl$bounces <- as.factor(evl$bounces)
evl$newVisits <- as.factor(evl$newVisits)
evl$campaign <- as.factor(evl$campaign)
evl$isTrueDirect <- as.factor(evl$isTrueDirect)
evl$adContent <- as.factor(evl$adContent)
evl$adwordsClickInfo.isVideoAd <- as.factor(evl$adwordsClickInfo.isVideoAd)
evl$city <- as.factor(evl$city)
evl$GDP <- log(evl$GDP)

evl$if_tr <- evl$transactionRevenue > 0
evl$if_tr <- as.factor(test$if_tr)

test <- read.csv("test_classification.csv")

test$isMobile <- as.factor(test$isMobile)
test$date <- as.factor(test$date)
test$bounces <- as.factor(test$bounces)
test$newVisits <- as.factor(test$newVisits)
test$campaign <- as.factor(test$campaign)
test$isTrueDirect <- as.factor(test$isTrueDirect)
test$adContent <- as.factor(test$adContent)
test$adwordsClickInfo.isVideoAd <- as.factor(test$adwordsClickInfo.isVideoAd)
test$city <- as.factor(test$city)
test$GDP <- log(test$GDP)

test$if_tr <- test$transactionRevenue > 0
test$if_tr <- as.factor(test$if_tr)

library(randomForest)
library(caret)
im <- list(list(), list(), list(), list(), list())
new <- list(matrix(nrow = dim(evl)[1], ncol = 50),matrix(nrow = dim(evl)[1], ncol = 50),
            matrix(nrow = dim(evl)[1], ncol = 50),matrix(nrow = dim(evl)[1], ncol = 50),
            matrix(nrow = dim(evl)[1], ncol = 50))
# matrix(nrow = dim(test)[1], ncol = 50)
m <- matrix(c(2,4,6,8,10))
for(j in 1:5) {
  for(i in 1:50) {
    sampletrain <- sample_frac(train, size = 0.02, replace = FALSE)
    class.rf <- randomForest(if_tr ~ 
                               channelGrouping + date + visitNumber + browser + operatingSystem + 
                               isMobile + deviceCategory + hits + pageviews + bounces + newVisits + 
                               campaign + source + isTrueDirect + adContent + adwordsClickInfo.page + 
                               adwordsClickInfo.slot + adwordsClickInfo.adNetworkType + 
                               adwordsClickInfo.isVideoAd + GDP + city + subContinent, 
                             data = sampletrain, mtry = m[j], ntree = 10, nodesize = 5, 
                             importance=TRUE, proximity=TRUE)
    im[[j]][[i]] <- importance(class.rf)
    new[[j]][ ,i] <- predict(class.rf, newdata = evl, type = "response")
    print(i)
  }
  print(j)
}

###########MSE
MSE <- matrix(nrow = 5, ncol = 2)
for(i in 1:5) {
  MSE[i] <- mean((new[[i]] - evl$if_tr)^2)
}

MSE[ ,2] <- m


MSE <- as.data.frame(MSE)
names(MSE) <- c("mse","m")
 ggplot(data = MSE) +
   geom_point(aes(x=m, y=mse))
 
 im_0 <- list()
 new_0 <- matrix(nrow = dim(test)[1], ncol = 50)
########mtry = 10
 for(i in 1:50) {
   sampletrain <- sample_frac(train, size = 0.02, replace = FALSE)
   class.rf <- randomForest(if_tr ~ 
                              channelGrouping + date + visitNumber + browser + operatingSystem + 
                              isMobile + deviceCategory + hits + pageviews + bounces + newVisits + 
                              campaign + source + isTrueDirect + adContent + adwordsClickInfo.page + 
                              adwordsClickInfo.slot + adwordsClickInfo.adNetworkType + 
                              adwordsClickInfo.isVideoAd + GDP + city + subContinent, 
                            data = sampletrain, mtry = 10, ntree = 10, nodesize = 5, 
                            importance=TRUE, proximity=TRUE)
   im_0[[i]] <- importance(class.rf)
   new_0[ ,i] <- predict(class.rf, newdata = test, type = "response")
   print(i)
 }
 
# FALSE = 1; TRUE = 2
yhat <- as.data.frame(rowSums(new_0==2, dims =1))
names(yhat) <- "raw_pre"
yhat$yhat <- yhat$raw_pre >= 25

new1 <- cbind(yhat, test$if_tr) %>% mutate(acc = (`test$if_tr` == yhat))
accuracy <- sum(new1$acc)/dim(new1)[1]
new1$pre <- ifelse((new1$yhat=="TRUE") & (new1$acc == "TRUE"), "TRUE", "FALSE")
precision <- sum(new1$pre=="TRUE")/sum(new1$yhat=="TRUE")
recall <- sum(new1$pre=="TRUE")/sum(new1$`test$if_tr`=="TRUE")

accuracy
precision
recall

#####importance

im[[1]][[1]]
imp <- select(as.data.frame(im[[1]]), MeanDecreaseGini)
for(i in 2:50) {
  data <- select(as.data.frame(im[[i]]), MeanDecreaseGini)
  imp <- rbind(imp, data)
}

imp$feature <- gsub("\\d","", row.names(imp))
imp <- group_by(imp, fe) %>% 
  summarise(importance = mean(MeanDecreaseGini))

write.csv(imp,"importance_classification.csv", row.names = F)

#check precision
library(MLmetrics)
precision(test$if_tr, as.factor(new[ ,50]==2), positive = "TRUE")
precision(test$if_tr, as.factor(new1$yhat=="TRUE"), positive = "TRUE")

sum(test$if_tr=="TRUE")
sum(new1$pre=="TRUE")

#test plus prediction
#data for model2
test$yhat <- yhat$yhat
write.csv(test, "test_with_pred1.csv", row.names = F)

#importance plot
names(imp)[1] <- "feature"
imp_new <- mutate(imp, feature = reorder(feature, importance))

ggplot(imp_new) + 
  geom_col(aes(x=feature, y = importance)) + 
  coord_flip()
########check for logistic###########
logi_test <- read.csv("./checkpresicionoflogistic/test.csv")
logi_pred <- read.csv("./checkpresicionoflogistic/pred.csv")

logi_pred1 <- logi_pred>=0.5