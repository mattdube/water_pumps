library(caret)
library(xgboost)

y = train_processed$status_group
train_clean <- train_processed %>%
  select(-status_group)

# preProcess data
# preProcess_model <- preProcess(train_clean, method = c("center", "scale"))
# train_clean <- predict(preProcess_model, newdata = train_clean)

# split data
split_index <- createDataPartition(train_clean$status_group, p=0.75, list = FALSE)
trainSet <- train_clean[split_index,]
testSet <- train_clean[-split_index,]

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

# train gbm model
gbm.fit <- train(status_group ~ ., data=trainSet, 
                 method='gbm',
                 trControl = fitControl,
                 tuneLength = 3)

##################
# dummy variables
##################
train_clean <- train_clean %>% mutate(status_group=as.numeric(status_group))
train_dummies <- dummyVars(" ~.", data=train_clean, fullRank=TRUE)
train_clean_dmy  <- data.frame(predict(train_dummies, newdata=train_clean))
train_clean_dmy %>% head(20) %>% View()
train_transformed <- train_clean_dmy %>% mutate(status_group=as.factor(status_group))

