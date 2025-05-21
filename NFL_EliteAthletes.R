install.packages(c("tidyverse", "caret", "randomForest", "pROC"))
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)

NFL_Draft_Prospects <- read.csv("C:/Users/ernes/OneDrive/Documents/Learning R/NFL_Draft_prospects2.csv")

DLine <- NFL_Draft_Prospects %>% 
  filter(POS == "EDGE"|POS == "DT") ##& !is.na(X40YD),!is.na(BENCH))
#Not all athletes perform certain activities
#Impute the missing data with column median
DLine <- DLine %>% 
  group_by(POS) %>% 
  mutate(
    X40YD = ifelse(is.na(X40YD),median(X40YD,na.rm = TRUE),X40YD),
    BENCH = ifelse(is.na(BENCH),median(BENCH,na.rm = TRUE),BENCH),
    VERT = ifelse(is.na(VERT),median(VERT,na.rm = TRUE),VERT)
  )


#DLine <- DLine %>% 
#  mutate(across(where(is.numeric),~ ifelse(is.na(.),median(., na.rm =TRUE), .)))

DLine$First.Round.Pick <- factor(DLine$First.Round.Pick, levels = c(0,1))

#Split data into testing and training datsets
set.seed(100)
train_index <- createDataPartition(DLine$First.Round.Pick, p = 0.7, list = FALSE)
train_data <- DLine[train_index,]
test_data <- DLine[-train_index,]

#create Logistic Regression Model, fit model
log_model <- glm(First.Round.Pick~ X40YD +BENCH+VERT,
                 data = train_data, family = "binomial")

summary(log_model)

#Predict probabilities using test data
log_probs <- predict(log_model, test_data, type = "response")

#predict class
log_pred <- ifelse(log_probs > 0.5,1,0)

#confusion matrix for log_model
confusionMatrix(as.factor(log_pred), test_data$First.Round.Pick)

roc_log <- roc(test_data$First.Round.Pick, log_probs)
auc(roc_log)
plot(roc_log, main = "Logistic Regression ROC Curve")




##Random Forest
rf_model <- randomForest(First.Round.Pick ~ X40YD + BENCH + VERT,
                         data = train_data, importance = TRUE, ntree = 500)

#predict
rf_pred <- predict(rf_model, test_data)

# confusion matrix
confusionMatrix(rf_pred, test_data$First.Round.Pick)

rf_probs <- predict(rf_model, test_data, type = "prob")[,2]
roc_rf <- roc(test_data$First.Round.Pick, rf_probs)
auc(roc_rf)
plot(roc_rf, main = "Random Forest ROC Curve")

# View importance
importance(rf_model)

# Plot importance
varImpPlot(rf_model, main = "Feature Importance - Random Forest", bg = 'Red')

Importance <- varImp(rf_model)
plot(Importance)


