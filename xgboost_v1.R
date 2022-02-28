rm(list = ls())
library(pROC)
library(tidyverse)
library(lubridate)
library(RTextTools)
library(maxent)
library(data.table)
library(tree)
library(randomForest)
require(xgboost)
require(Matrix)
require(data.table)
require(vcd)
library(randomForest)
library(ISLR)
library(gbm)



my_test = read.csv("Airbnb_test_clean.csv")
my_train = read.csv("Airbnb_training_clean.csv")

my_train$host_identity_verified = as.factor(my_train$host_identity_verified)

my_train$host_is_superhost = as.factor(my_train$host_is_superhost)


miss_airAll_func <- function(df1){
  airAll_missing = data.frame(blank_count_airAll = apply(df1, 2, 
                                                         function(x) sum(x == "", na.rm = F)),
                              missing_percent_airAll = apply(df1, 2,
                              function(x) x = (sum(x == "", na.rm = F)/(nrow(df1))*100)))
  return(airAll_missing)
}   

miss_airAll_func(my_train)
miss_airAll_func(my_test)

my_test = data.table(my_test)
my_train = data.table(my_train)

################################################################################################################
#Price train
my_train$price <- str_replace(my_train$price, "\\$", "")
my_train$price = as.numeric(my_train$price)
my_train[, .(.N), by = price]

my_train[,list(median(price, na.rm = T)), by = c("property_type", "market")]

#assigning missing value of price corresponding to mean for groups of property type and room type
my_train[, price_new := median(price, na.rm = T), by = c("property_type", "market") ]
my_train[is.na(price_new), price_new := 198]
my_train[is.na(price), price := price_new]
my_train = my_train[,- c("price_new")]



#price test
my_test$price <- str_replace(my_test$price, "\\$", "")
my_test$price = as.numeric(my_test$price)
my_test[, .(.N), by = price]

my_test[,list(median(price, na.rm = T)), by = c("property_type", "market")]

#assigning missing value of price corresponding to mean for groups of property type and room type
my_test[, price_new := median(price, na.rm = T), by = c("property_type", "market") ]
my_test[is.na(price_new), price_new := 198]
my_test[is.na(price), price := price_new]
my_test = my_test[,- c("price_new")]





#################################################################################################################
###### Cancellation_policy

my_train[, (.N), by = cancellation_policy]
#This suggests cancellation_policy does not have much impact on booking rate since 
#strict ones get the highest count.

my_train$cancellation_policy <- as.factor(my_train$cancellation_policy)
#imputing level 1.0 and 2.0 with level strict
#levels(my_train$cancellation_policy)[match(c("2.0", "5.0") , 
#                                         levels(my_train$cancellation_policy))] <- "strict"

#We can merge super_strict_30 and super_strict_60 to super_strict
levels(my_train$cancellation_policy)[match(c("super_strict_30", "super_strict_60") , 
                                         levels(my_train$cancellation_policy))] <- "super_strict"


#### test
my_test[, (.N), by = cancellation_policy]
my_test$cancellation_policy <- as.factor(my_test$cancellation_policy)
levels(my_train$cancellation_policy)[match(c("super_strict_30", "super_strict_60") , 
                                           levels(my_train$cancellation_policy))] <- "super_strict"

#################################################################################################################
#Security deposit - Prediction
my_train$security_deposit <- str_replace(my_train$security_deposit, "\\$", "")
my_train$security_deposit = as.numeric(my_train$security_deposit)
unique(my_train[, c("security_deposit"), with = F])

ggplot(my_train, aes(room_type, security_deposit, color = city_name))  +
  geom_boxplot(na.rm = T)

ggplot(my_train, aes(property_type, security_deposit, color = city_name)) +
  geom_boxplot(na.rm = T)

my_train[, security_dep := median(my_train$security_deposit, na.rm = T), 
       by = c("price" , "room_type" , "cancellation_policy")]

my_train[is.na(security_deposit), security_deposit := security_dep]
my_train = my_train[,-c("security_dep")]


######################## security_deposit 

my_test$security_deposit <- str_replace(my_test$security_deposit, "\\$", "")
my_test$security_deposit = as.numeric(my_test$security_deposit)
unique(my_test[, c("security_deposit"), with = F])

ggplot(my_test, aes(room_type, security_deposit, color = city_name))  +
  geom_boxplot(na.rm = T)

ggplot(my_test, aes(property_type, security_deposit, color = city_name)) +
  geom_boxplot(na.rm = T)

my_test[, security_dep := median(my_test$security_deposit, na.rm = T), 
          by = c("price" , "room_type" , "cancellation_policy")]

my_test[is.na(security_deposit), security_deposit := security_dep]
my_test = my_test[,-c("security_dep")]


################################################################################################################
#Numerical data type 

num_var = c("extra_people", "guests_included", "host_listings_count", "accommodates", "availability_30", "availability_60", 
            "availability_90", "availability_365", "cleaning_fee", "maximum_nights", "minimum_nights", "price" )

my_train$extra_people = as.numeric(my_train$extra_people)
my_train$guests_included = as.numeric(my_train$guests_included)
my_train$host_listings_count = as.numeric(my_train$host_listings_count)
my_train$accommodates = as.numeric(my_train$accommodates)
my_train$availability_30 = as.numeric(my_train$availability_30)
my_train$availability_60 = as.numeric(my_train$availability_60)
my_train$availability_90 = as.numeric(my_train$availability_90)
my_train$availability_365 = as.numeric(my_train$availability_365)
my_train$cleaning_fee = as.numeric(my_train$cleaning_fee)
my_train$maximum_nights = as.numeric(my_train$maximum_nights) #scale them down
my_train$minimum_nights = as.numeric(my_train$minimum_nights) #scale them down
my_train$price = as.numeric(my_train$price)


my_test$extra_people = as.numeric(my_test$extra_people)
my_test$guests_included = as.numeric(my_test$guests_included)
my_test$host_listings_count = as.numeric(my_test$host_listings_count)
my_test$accommodates = as.numeric(my_test$accommodates)
my_test$availability_30 = as.numeric(my_test$availability_30)
my_test$availability_60 = as.numeric(my_test$availability_60)
my_test$availability_90 = as.numeric(my_test$availability_90)
my_test$availability_365 = as.numeric(my_test$availability_365)
my_test$cleaning_fee = as.numeric(my_test$cleaning_fee)
my_test$maximum_nights = as.numeric(my_test$maximum_nights) #scale them down
my_test$minimum_nights = as.numeric(my_test$minimum_nights) #scale them down
my_test$price = as.numeric(my_test$price)
my_test$high_booking_rate = as.numeric(my_test$high_booking_rate)

#################################################################################################################
### Factor variable 

#my_train$host_identity_verified = as.factor(my_train$host_identity_verified)
#my_train$host_is_superhost = as.factor(my_train$host_is_superhost)
#my_train$host_response_time = as.factor(my_train$host_response_time)
#my_train$instant_bookable = as.factor(my_train$instant_bookable)
#my_train$bedrooms = as.factor(my_train$bedrooms)
#my_train$bathrooms = as.factor(my_train$bathrooms)
#my_train$is_location_exact = as.factor(my_train$is_location_exact)
#my_train$require_guest_phone_verification = as.factor(my_train$require_guest_phone_verification)
#my_train$require_guest_profile_picture = as.factor(my_train$require_guest_profile_picture)
#my_train$requires_license = as.factor(my_train$requires_license)
#my_train$room_type = as.factor(my_train$room_type)
#my_train$city_name = as.factor(my_train$city_name)

#my_test$host_identity_verified = as.factor(my_test$host_identity_verified)
#my_test$host_is_superhost = as.factor(my_test$host_is_superhost)
#my_test$host_response_time = as.factor(my_test$host_response_time)
#my_test$instant_bookable = as.factor(my_test$instant_bookable)
#my_test$bedrooms = as.factor(my_test$bedrooms)
#my_test$bathrooms = as.factor(my_test$bathrooms)
#my_test$is_location_exact = as.factor(my_test$is_location_exact)
#my_test$require_guest_phone_verification = as.factor(my_test$require_guest_phone_verification)
#my_test$require_guest_profile_picture = as.factor(my_test$require_guest_profile_picture)
#my_test$requires_license = as.factor(my_test$requires_license)
#my_test$room_type = as.factor(my_test$room_type)
#my_test$city_name = as.factor(my_test$city_name)

##################################################################################################################

#checking the data type
sapply(train_new, class)

###################################################################################################################
#choosing the variables for the model 



train_new = my_train[,c("extra_people","host_identity_verified","host_is_superhost","guests_included",
                        "host_listings_count","host_response_rate","host_response_time","instant_bookable","accommodates",
                        "availability_30","availability_60","availability_90","availability_365","bedrooms","beds","bathrooms",
                        "cleaning_fee","is_location_exact","maximum_nights","minimum_nights","require_guest_phone_verification",
                        "require_guest_profile_picture","requires_license","room_type","city_name", 
                        "host_since", "first_review", "security_deposit",
                        "cancellation_policy","high_booking_rate")]

test = my_test[, c("extra_people","host_identity_verified","host_is_superhost","guests_included",
                   "host_listings_count","host_response_rate","host_response_time","instant_bookable","accommodates",
                   "availability_30","availability_60","availability_90","availability_365","bedrooms","beds","bathrooms",
                   "cleaning_fee","is_location_exact","maximum_nights","minimum_nights","require_guest_phone_verification",
                   "require_guest_profile_picture","requires_license","room_type","city_name", 
                   "host_since", "first_review","security_deposit", "cancellation_policy")]

#####################################################################################################
#check missing value now

miss_airAll_func(train_new)
miss_airAll_func(test)

#####################################################################################################

train_inst = sample(nrow(train_new), .70*nrow(train_new))
train = train_new[train_inst,]
valid = train_new[-train_inst,]

######################################################################################################
#Feature Engineering












#xgboost
air.tb <- as.data.table(train)

air.tb.x <- air.tb[,1:26]
sparse_matrix <- sparse.model.matrix(~., data = air.tb.x)
head(sparse_matrix)

output_vector = air.tb[,high_booking_rate]

library(xgboost)
library(Matrix)
library(magrittr)


train_matrix<-xgb.DMatrix(data= sparse_matrix,label = output_vector)

#test_matrix<-xgb.DMatrix(data=as.matrix(testm),label=test_airbnb[,"high_booking_rate"])

parameterGrid = CJ(eta = c(0.2,0.3,0.4), lambda = c(10,20,30), alpha=c(5,10,20), depth=c(5,6,8))

getBoostModel = function(param){
  print(sprintf("running for :: eta: %s, lambda: %s, alpha: %s, depth: %s",param[1],param[2],param[3],param[4]))
  params = list(objective = "binary:logistic", eval_metric = 'logloss', eta=param[1], gamma=0, max_depth=param[4], min_child_weight=40, subsample=1, colsample_bytree=1)
  xgbcv <- xgb.cv( params = params, data = train_matrix, nrounds = 50, nfold = 5, showsd = T, stratified = T, maximize = F,nthreads=10, lambda=param[2], alpha=param[3])
  return(xgbcv)
}


xgbList = apply(parameterGrid,1,getBoostModel)
evalDT = rbindlist(lapply(xgbList,function(x){data.table(x$evaluation_log)[,param:=paste(unlist(x$params[c(3,10,11)]),collapse = " : ")]}))

evalDT[which(evalDT$test_logloss_mean == min(evalDT$test_logloss_mean)), ]


xgbst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 10, lambda = 20, colsample_bytree = 0.8, subsample = 0.7, 
                 min_child_weight = 40 ,eta = 0.4, nthreads = 10, nrounds = 100, objective = "binary:logistic")



importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = xgbst)
head(importance)

importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), 
                                model = xgbst, data = sparse_matrix, label = output_vector)

importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importance)

#y_valid <- sparse.model.matrix(high_booking_rate~.-1, data=valid)

## 
valid.x <- valid[,1:26]
y_valid <- sparse.model.matrix(~., data=valid.x)

#### accuracy on validation
y_pred <- predict(xgbst, y_valid)

classification <- ifelse(y_pred>0.6,1,0)
conf_table <- table(classification, valid$high_booking_rate)

accuracy <- (conf_table[1] + conf_table[4])/sum(conf_table)
accuracy

#### accuracy on training
x_pred <- predict(xgbst, sparse_matrix)
classification <- ifelse(x_pred>0.54,1,0)
conf_table <- table(classification, air.tb$high_booking_rate)

accuracy <- (conf_table[1] + conf_table[4])/sum(conf_table)
accuracy

##prediction on test set 
y_test = sparse.model.matrix(~., data = test)

test_predict = predict(xgbst, y_test)

classification <- ifelse(test_predict>0.50,1,0)
test_pred = sum(classification == 1)
test_pred

accuracy <- (conf_table[1] + conf_table[4])/sum(conf_table)
accuracy

#Random Forest
bag.mod <- randomForest(high_booking_rate ~., data = train, mtry=5, importance = TRUE) #defaults to 500 bags
bag_preds <- predict(bag.mod,newdata=OJ[-train,])
bag_acc <- sum(ifelse(bag_preds==OJ$Purchase[-train],1,0))/test_size

bag.mod
bag_acc



## Random Forest
rf.mod <- randomForest(as.factor(high_booking_rate)~., data = train, mtry = 5, ntree = 1000, importance=TRUE, 
                       shrinkage  = 0.1)

valid.x <- valid[,1:26]
y_valid <- valid[,27]

#### accuracy on validation
y_pred <- predict(rf.mod, valid.x)

classification <- ifelse(y_pred > 0.5 ,1,0)
conf_table <- table(classification, y_valid)

accuracy <- (conf_table[1] + conf_table[4])/sum(conf_table)
accuracy

##random forest example
rf.mod <- randomForest(Purchase~.,data=OJ,subset=train,mtry=4,ntree=1000,importance=TRUE)
rf_preds <- predict(rf.mod,newdata=OJ[-train,])
rf_acc <- sum(ifelse(rf_preds==OJ$Purchase[-train],1,0))/test_size
rf_acc


rf1 <- randomForest(as.factor(high_booking_rate) ~ ., data=train, mtry=5, importance = TRUE)


model_boost<- gbm(high_booking_rate ~ data = train_airbnb, distribution = "bernoulli", n.trees=5000,
                  interaction.depth = 7, shrinkage = 0.1)

## GBM 
library(gbm)
boost_data <- train
set.seed(1)
boost.mod <- gbm(high_booking_rate~., data = boost_data,
                 distribution="bernoulli",n.trees=1000,interaction.depth=4, shrinkage = 0.1)

boost_preds <- predict(boost.mod,newdata=boost_data[-train,],type='response',n.trees=1000)
boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==boost_data$Purchase[-train],1,0))/test_size
boost_acc




