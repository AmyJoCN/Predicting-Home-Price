## Load libraries
if(!require("randomForest")) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require("corrgram")) install.packages("corrgram", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require("reshape2")) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require("GGally")) install.packages("reshape2", repos = "http://cran.us.r-project.org")

library(corrgram)
library(randomForest)
library(GGally)
library(caret)
library(tidyverse)
library(ggplot2)

setwd("/Users/cocoapuffs/Desktop/UW Data Analytics/Course 3/Final Project")

#colleges = read.delim("colleges.tsv", sep = "\t",header = TRUE)

## Change the full path to the file. 
file = 'kc_house_data.csv'

#Load and Prepare the Data Set
#Start with loading and transforming data and verifying that column data types are as expected. 
#Note variables where median is significantly different from the mean and the maximum is farther 
#from the central tendencies (mean, median) than minimum, indicating skewed distributions. 
#Also recall density plots that you completed for your challenge project in the first part of this class.

house = read_csv(file)
house = house %>% mutate(  waterfront_fac = as.factor(waterfront), 
                           view_fac = as.factor(view), 
                           zipcode_fac = as.factor(zipcode))
house$id <- NULL
house$zipcode <- NULL

summary(house)
glimpse(house)

ggpairs(house[,c(3:7,2)])

#display price pust next set of columns from house that reasonably fit on the screen 
ggpairs(house[,c(8:12,2)])

ggpairs(house[,c(13:17,2)])

ggpairs(house[,c(18:21,2)])


#######################################################################################


#Variable Selection
#You have visually explored the data set and now you will evaluate variables to be included in the model.

#Steps:
  
  #1. Evaluate variable correlations
  #2. Remove highly correlated independent variables. Recall the hint about sqft_living15.
  #3. Evaluate variable significance.
      #A. For the final selection consider variables with the following:
          #a. be important for your decision (those may have low importance)
          #b. don't impact model overfitting
          #c. don't impact performance of the algorithm (keep in mind that each additional variable slows model training)
  #4. Remove variables with low significance

#top correlations
cor_level <- .7
correlationMatrix <- cor(house[,c(2:19)])
cor_melt <- arrange(melt(correlationMatrix), desc(value), Var1)

#hint: you can use %% 2 to remove every other row so we don't get duplicates, given 
#hint: several correlated variables interract with each other given us duplicates from melt
dplyr::filter(cor_melt, row_number() %% 2 == 1 & value != 1.0, value > cor_level)

#show variables that correlate to price only
cor_level <- .5
dplyr::filter(cor_melt, row_number() %% 2 == 1 & value != 1.0, Var1 == 'price', value > cor_level)

#remove variables that logically don't contribute to a good model
house$sqft_living15<- NULL
house$sqft_lot15<- NULL
house$waterfront<- NULL
house$view<- NULL
house$zipcode_fac<- NULL

glimpse(house)

tree_model <- randomForest(price ~ ., data=house)
summary(tree_model)

plot(tree_model)

var_imp <- varImpPlot(tree_model)
var_imp
varImpPlot(tree_model)

# Model Selection
# You have selected you variables and now you will select a model.
# The best model would have the lowest cross-validation error.
# 
# Steps:
#   
# 1. Evaluate a few models for initial performance. 
#    For example, you can chose linear regression and random forest.
# 2. Evaluate cross-validations to select the pest performing model.

print(tree_model)

lm_model <- lm(price ~ ., data = house)
summary(lm_model)
confint(lm_model)

train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
model_lm <- train(price ~ ., data=house, trControl=train_control, method="lm")
model_lm

train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
model_rf <- train(price ~ ., data=house, trControl=train_control, method="rf")
model_rf

# Prediction of Home Price
# You have explored the data set and tested the significance of some of the relationships in the data. 
# Now, you will compute and test a linear regresson model to predict the price of homes in King County.
# 
# As a first step, compute a linear regression model and print the summary and coeficient confidence 
# intervals to predict the log price of homes. Some features, such as ID are known to not be useful 
# in predicting price. Use the following features:
#   
# bedrooms
# bathrooms
# sqft_living
# sqft_lot
# floors
# waterfront
# view
# condition
# grade
# sqft_above
# square root of sqft_living
# log of sqft_living

# Hint: Ensure that you scale the features before you compute the model. 
# You can use lapply with the R scale function to iterate over the numeric feature columns.

set.seed(1234)

#more data wrangling
#- remove outlying values from vars?
#- remove vars where value is very strongly one option over other (e.g. basement)

#taking the log of price due to skew
house$log_price = log10(house$price + 1)

#take the log of other vars?

#feature engineering ideas
#- convert date into usable data and graph against price to find any patterns
#- create month column to reflect seasonality?
#- combine lat & long into one variable to get location (see Proj 1)
library(sf)
library(mapview)
house_sf <- st_as_sf(house, coords = c("long", "lat"), crs = 4326)
mapview(house_sf)
#map plot opens up in webview
house$coords = c(house$long, house$lat)
summary(house)
glimpse(house)

ggpairs(house[,c(14:15,2)])

library(lubridate)

house$dttm = house$date
house <- house %>% mutate(date = as_date(date))

glimpse(house)

ggplot(house, aes(x = date, y = price)) +
  geom_point()

summary(house)

house$yr_renovated

house$month = ifelse(str_detect(house$date, "^2014-05"), 'May14', 
              ifelse(str_detect(house$date, "^2014-06"), 'Jun14', 
              ifelse(str_detect(house$date, "^2014-07"), 'Jul14',
              ifelse(str_detect(house$date, "^2014-08"), 'Aug14',
              ifelse(str_detect(house$date, "^2014-09"), 'Sep14',
              ifelse(str_detect(house$date, "^2014-10"), 'Oct14',
              ifelse(str_detect(house$date, "^2014-11"), 'Nov14',
              ifelse(str_detect(house$date, "^2014-12"), 'Dec14',
              ifelse(str_detect(house$date, "^2015-01"), 'Jan15',
              ifelse(str_detect(house$date, "^2015-02"), 'Feb15',
              ifelse(str_detect(house$date, "^2015-03"), 'Mar15',
              ifelse(str_detect(house$date, "^2015-04"), 'Apr15',
              'May15'))))))))))))


house_date <- house %>% select(date, month)
house_date

house_date <- house %>% 
  group_by(month) %>%
  summarize(n = n(),
            price_mean = mean(price),
            price_sd = sd(price))

house_date

house$month = factor(house$month,
                  levels = c('May14', 'Jun14', 'Jul14', 'Aug14', 'Sep14', 'Oct14',
                             'Nov14', 'Dec14', 'Jan15', 'Feb15', 'Mar15', 'Apr15', 'May15'
                  ))


mean_price <- mean(house$price)

View(mean_price)

d <- ggplot(house, aes(month))
d + stat_summary_bin(aes(y = price), fun.y = "mean", geom = "bar")

ggplot(house, aes(x = month)) +
  geom_bar()

glimpse(house)

house_reno <- house %>% 
  group_by(yr_renovated) %>%
  summarize(n = n())
View(house_reno)

house_mod <- house %>% select(-date,-waterfront_fac,-view_fac,-sqft_basement,-dttm,-yr_renovated)
glimpse(house_mod)
summary(house_mod)

house_bthrm <- house %>% 
  group_by(bathrooms) %>%
  summarize(n = n())
house_bthrm

house_mod <- filter(house_mod, bathrooms != 0)
summary(house_mod)

#model matrix, and cbind, and selecting out original vars
month = model.matrix(~ month - 1, data = house_mod)

house_mod = cbind(house_mod, month)

glimpse(house_mod)

house_mod1 = select(house_mod, -month, -price)

#split data into train and test sets
in_train = createDataPartition(y = house_mod1$log_price,
                               p = 0.80,
                               list = FALSE)

house_train1 = house_mod1[in_train, ]
house_test1 = house_mod1[-in_train, ]

#center & scale data with preProcess (train and test sets)
preprocessing_steps = preProcess(select(house_train1, bedrooms, bathrooms, sqft_living, sqft_lot, floors,
                                        condition, grade, sqft_above, yr_built, lat, long),
                                 method = c('center', 'scale'))

house_train_proc = predict(preprocessing_steps, newdata = house_train1)
house_test_proc = predict(preprocessing_steps, newdata = house_test1)

#identify any vars w/near zero variance with nearZeroVar
nearZeroVar(house_mod1, saveMetrics = TRUE)

#run preProcess again on train and test sets?

preprocessing_steps = preProcess(select(house_train1, bedrooms, bathrooms, sqft_living, sqft_lot, floors,
                                        condition, grade, sqft_above, yr_built, lat, long),
                                 method = c('center', 'scale','nzv'))

house_train_proc = predict(preprocessing_steps, newdata = house_train1)
house_test_proc = predict(preprocessing_steps, newdata = house_test1)

glimpse(house_train_proc)

#monthJan15 and monthMay15 removed in pre-processing

lm_full_model = train(log_price ~ ., 
                 data = house_train_proc,
                 method = "lm", 
                 trControl = trainControl(method = "cv", number = 10))

summary(lm_full_model) # some more info on final model
summary(lm_full_model$finalModel)

### Predict on Test
predictions = predict(lm_full_model, newdata = house_test_proc)

# view the metric RMSE
postResample(pred = predictions, obs = house_test_proc$log_price)

# manual way of getting errors
errors = data.frame(pred = predictions, 
                    obs = house_test_proc$log_price,
                    error = predictions - house_test_proc$log_price)

# PLotting Errors - Also called residual plots
# Residual Plots: plot errors with predicted and observed
# if pattern appears in residual plots then 
# go back to make sure you have enough variables
# if random pattern then you are good.
ggplot(data = errors, aes(x = pred, y = obs)) +
  geom_point() 

#add xlim to make sure ggplot viz is not misleading

plot(errors$error)

plot(varImp(lm_full_model))

## lm model with top 4 importance variables

house_train_top = select(house_train_proc, lat, grade, yr_built, sqft_living, log_price)
glimpse(house_train_top)

lm_top_model = train(log_price ~ lat + grade + yr_built + sqft_living, 
                      data = house_train_top,
                      method = "lm", 
                      trControl = trainControl(method = "cv", number = 10))

summary(lm_top_model) # some more info on final model
summary(lm_top_model$finalModel)

### Predict on Test
predictions2 = predict(lm_top_model, newdata = house_test_proc)

# view the metric RMSE
postResample(pred = predictions2, obs = house_test_proc$log_price)

# manual way of getting errors
errors2 = data.frame(pred = predictions2, 
                    obs = house_test_proc$log_price,
                    error = predictions - house_test_proc$log_price)

# PLotting Errors - Also called residual plots
# Residual Plots: plot errors with predicted and observed
# if pattern appears in residual plots then 
# go back to make sure you have enough variables
# if random pattern then you are good.
ggplot(data = errors2, aes(x = pred, y = obs)) +
  geom_point() 

#add xlim to make sure ggplot viz is not misleading

plot(errors2$error)

plot(varImp(lm_top_model))

## lm model with top 4 importance variables + prediction variables
house_train_pred = select(house_train_proc, lat, grade, yr_built, sqft_living, sqft_above, sqft_lot, bedrooms, bathrooms, log_price,)
lm_pred_model = train(log_price ~ ., 
                      data = house_train_pred,
                      method = "lm", 
                      trControl = trainControl(method = "cv", number = 10))

summary(lm_pred_model) # some more info on final model
summary(lm_pred_model$finalModel)

### Predict on Test
predictions3 = predict(lm_pred_model, newdata = house_test_proc)

# view the metric RMSE
postResample(pred = predictions3, obs = house_test_proc$log_price)

# manual way of getting errors
errors3 = data.frame(pred = predictions3, 
                    obs = house_test_proc$log_price,
                    error = predictions - house_test_proc$log_price)

# PLotting Errors - Also called residual plots
# Residual Plots: plot errors with predicted and observed
# if pattern appears in residual plots then 
# go back to make sure you have enough variables
# if random pattern then you are good.
ggplot(data = errors3, aes(x = pred, y = obs)) +
  geom_point() 

#add xlim to make sure ggplot viz is not misleading

plot(errors3$error)

plot(varImp(lm_pred_model))

## QQ-plot? ##

## Build forward model ##

### Build random forest model ###



##############################################################################################
##You are able to have more variables in dataset vs. model, but not other way around or will throw error


View(house_mod1)
house_mod2 <- house_mod1

glimpse(house_mod2)

glimpse(house_train_top)

to_predict <- house_mod2[0,]
to_predict[1,]$bedrooms <- 4
to_predict[1,]$bathrooms <- 3

#to_predict[1,]$sqft_above <- 4000
to_predict[1,]$sqft_lot <- 5000
#add all variables we need for prediction
to_predict[1,]$condition <- 5

#to_predict[1,]$lat <- 47.3097
to_predict[1,]$sqft_living <- 4000
to_predict[1,]$grade <- 7
to_predict[1,]$yr_built <- 2004

preprocessing_steps <- preProcess(select(to_predict, bedrooms, bathrooms, sqft_living, sqft_lot, floors,
                                         condition, grade, sqft_above, yr_built, lat, long),
                                 method = c('center', 'scale'))

to_predict <- predict(preprocessing_steps, newdata = to_predict)

#str function to view dataset var data types
glimpse(to_predict)
summary(house)

str(to_predict)
#summary(to_predict)
predict(lm_top_model, newdata = to_predict)

antilog<-function(lx,base)    {
  lbx<-lx/log(exp(1),base=base)    
  result<-exp(lbx)    
  result    
  }      

antilog(5.667,10)

postResample(pred = predictprice, obs = to_predict$log_price)
print(predictprice)

# Run a quick prediction experiment with other models
# 1. Linear regression
# 2. You can experiment with other models here as well


# Summary
# In this final project you have done the following:
#   
# Loaded the data set on the prices and features of homes in King County, Washington USA.
# Perform some transformations on the label and features in the data set.
# Used visualizations to explore the relationships in the data set.
# Used feature selection methods to eliminate less useful independent variables.
# Created a linear regresson model and random forest model to predict the price of homes.
# Evaluated and selected the model
# Used the model to predict the price of the house that met your decision criteria: 
# 4 bedroom, 3 bathroom, 4000 square feet living area.
