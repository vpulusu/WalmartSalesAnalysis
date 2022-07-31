library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(rpart)
library(rattle)
library(car)
library(caret)
library(corrplot)
library(rpart.plot)

train<- read_csv("train.csv")
stores <- read_csv("stores.csv")
features <- read_csv("features.csv")

stores$Store <- factor(stores$Store)
train$Store <- factor(train$Store)
train <- full_join(train,stores,by=c("Store"))

train$WeekNum <- as.numeric(format(train$Date+3,"%U"))

train$Returns <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales < 0,sales,0)
})
train$Weekly_Sales <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales > 0,sales,0)
})



final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
aggregate_sales <- function(){
  for(i in 1:45){
    store_data <- train %>% filter(Store == i)
    dates <- unique(train$Date)
    for(next_date in seq_along(dates)){
      current_date <- unique(train$Date)[[next_date]]
      date_data <- store_data %>% filter(Date==current_date)
      #Add all the weekly sales
      net_sales <- sum(unlist(date_data$Weekly_Sales)) - sum(unlist(date_data$Returns))
      #Construct the data frame and append it
      next_row <- data.frame(Store=i,Date=current_date,Weekly_Sales=net_sales,IsHoliday=date_data$IsHoliday[[1]],Type=date_data$Type[[1]],WeekNum=date_data$WeekNum)
      next_row$Store <- factor(next_row$Store)
      final_data <- rbind(final_data,next_row)
    }
  }
  return(final_data)
}
# Sum the sales by store without taking into account each department
final_data <- aggregate_sales()



features$Store <- factor(features$Store)
#Merge our final_data with our features
train <- left_join(train,features,by=c("Store","Date","IsHoliday"))
# Make the NA markdown as 0
train$MarkDown1 <- sapply(train$MarkDown1, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown2 <- sapply(train$MarkDown2, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown3 <- sapply(train$MarkDown3, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown4 <- sapply(train$MarkDown4, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown5 <- sapply(train$MarkDown5, function(value){
  ifelse(is.na(value),0,value)
})


index <- createDataPartition(train$Weekly_Sales, p=0.8, list = FALSE)
train.train <-train[index,]
train.test <- train[-index,]

head(train.train)

aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Store"), drop=FALSE], mean)

aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Type"), drop=FALSE], mean)

aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Type"), drop=FALSE], max)

A_stores <- train.train %>% filter(Type=='A')
ggplot(A_stores,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=A_stores$IsHoliday)) + geom_smooth()
#Sales vary depending on the weeknum we are in

store_graph <- train.train %>% filter(Store == 20)
ggplot(store_graph,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=store_graph$IsHoliday)) + geom_smooth()


#Using a decision tree we will like to predict the Type of a store based on all the other parameters
train.rpart <-rpart(Type ~ Weekly_Sales + Size,data=train.train, control=rpart.control(minsplit=1,cp=0.05))
summary(train.rpart)

fancyRpartPlot(train.rpart)

dim(train.train)

rannk.rpart <- load("rank.Rdata")
head(train.train)
rank.rpart <- rpart(Rank~ .-Weekly_Sales-Size,data=train.train,control=rpart.control(minsplit=10,cp=0.03))
fancyRpartPlot(rank.rpart)

fit <- lm(Weekly_Sales ~.-Date-Type-CPI, data=train.train)
predict_fit_confidence <- predict(fit, newdata=train.test, interval="confidence", level=0.95)

summary(fit)


prediction <- predict(train.rpart,train.test, type="class")
train.test$Prediction <- prediction
#Find the percentage accuracy of our model
accur_table <- train.test %>% select(Type,Prediction) 
bool_vector <- accur_table$Type == accur_table$Prediction
length(which(bool_vector)) / length(bool_vector)



#Evaluate Results of the Model
prediction_rank <- predict(rank.rpart,train.test, type="class")
train.test$RankPred <- prediction_rank

accuracy_test <- train.test %>% select(Rank,RankPred)
values <- accuracy_test$Rank == accuracy_test$RankPred
length(which(values)) / length(values)


fit <- lm(Weekly_Sales ~.-Date-Type-CPI, data=train.train)
class(fit)


predict_fit_confidence <- predict(fit, newdata=train.test, interval="confidence", level=0.95)

predict_fit_pred <-predict(fit, newdata=train.test, interval="prediction", level=0.95)

predict_uncertainty <- cbind(train.test,predict_fit_confidence,predict_fit_pred[,-1])
train.test <- cbind(train.test,predict_fit_confidence)
results_vector <- ifelse(train.test$Weekly_Sales >= train.test$lwr & train.test$Weekly_Sales <= train.test$upr,TRUE,FALSE)
train.test <- cbind(train.test,results_vector)
sum(results_vector)/nrow(train.test) #Accuracy value is 20%

