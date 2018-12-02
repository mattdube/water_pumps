library(tidyverse)
library(caret)
library(here)
library(Metrics)
library(gbm)
library(MLmetrics)
library(vip)

# load data
blood.train <- read_csv(here("00_Data", "blood.train.csv"))
blood.test <- read_csv(here("00_Data", "blood.test.csv"))

# rename columns
names(blood.train)
new_names <- c("id", "months_last_donate", "number_donations", "total_volume", "months_first_donation", "donate_march")
new_names_test <- new_names[1:5]


names(blood.train) <- new_names
names(blood.test) <- new_names_test

# check for missing data
blood.train %>%
    select(everything()) %>%
    summarise_all(funs(sum(is.na(.))))

# check data
str(blood.train)

# make target variable a factor, with "Y" or "N" as values
blood.train <- 
    blood.train %>%
    mutate(donate_march = ifelse(donate_march == 0, "N", "Y"))

blood.train <- 
    blood.train %>%
    mutate(id = as.character(id))

# split data
set.seed(5678)
trainIndex <- createDataPartition(blood.train$donate_march, p = .8,
                                  list = FALSE,
                                  times = 1)
b.train <- blood.train[trainIndex,]
b.test <- blood.train[-trainIndex,]

# build base model using default features
trControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    savePredictions = TRUE,
    classProbs = TRUE
    )

b.train <- b.train %>% select(-id)
b.test <- b.test %>% select(-id)
model_gbm <- train(donate_march ~.,
                   data = b.train,
                   method = 'gbm',
                   trControl = trControl,
                   tuneLength = 3
                   )

model_gbm
gbmImp <- varImp(object = model_gbm)
xgbImp <- varImp(object = model_xgb)

predictions.gbm <- predict.train(object = model_gbm, b.test[,1:4], type="prob")

# now that we have a baseline, let's see if it can be improved
# first, an xgboost baseline
model_xgb <- train(donate_march ~., 
                   data=b.train,
                   method = "xgbTree",
                   trControl = trControl,
                   tuneLength = 3)

predictions.xgb <- predict.train(object = model_xgb, b.test[, 1:4], type = "prob")

# compare our 2 baseline models
# collect resamples
results <- resamples(list(GradBoostTree = model_gbm, xgbTree = model_xgb, xgbTree2 = model_xgb_2))
results$values

summary(results)

# plot models
par(mfrow = c(1,2))
plot(model_gbm)
plot(model_xgb)

bwplot(results)


# compare logloss
predictions.gbm$Y
b.test <- 
    b.test %>% mutate(donate_march = as.numeric(ifelse(donate_march == 'Y', 1, 0)))
gbm_logloss <- Metrics::logLoss(b.test$donate_march, predictions.gbm$Y)
xgb_logloss <- logLoss(b.test$donate_march, predictions.xgb$Y)
xgb_2_logloss <- 

# Feature engineering 
b.train <- b.train %>% 
    mutate(donate_per_month = (months_first_donation - months_last_donate) / number_donations,
           donor_length = months_first_donation - months_last_donate) %>%
    select(everything()) %>%
    select(-(total_volume))

b.test <- b.test %>% 
    mutate(donate_per_month = (months_first_donation - months_last_donate) / number_donations,
           donor_length = months_first_donation - months_last_donate) %>%
    select(everything()) %>%
    select(-(total_volume))

# let's see if our model improves with the new features, and removing total_volume
model_xgb_2 <- train(donate_march ~., 
                   data=b.train,
                   method = "xgbTree",
                   trControl = trControl,
                   tuneLength = 3)

predictions.xgb_2 <- predict.train(object = model_xgb_2, b.test[, 2:7], type = "prob")

varImp(object = model_xgb_2)
confusionMatrix()

names(getModelInfo())

vip(model_xgb_2) + ggtitle("xgb2: tuned")
