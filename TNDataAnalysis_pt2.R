library(caret)
# split the data into training and test partitions
training <- createDataPartition(TN2019_final$Application_Status, p=0.7, list=FALSE)
set.seed(2020)
TN_train  <- TN2019_final[training,]
TN_test <- TN2019_final[-training,]

library(MASS)
library(tidyr)

full_model <- glm(Application_Status ~ ., family=binomial(link='logit'), data=TN_train)
coef(full_model)

stepwise_model <- full_model %>% stepAIC(trace=FALSE)
coef(stepwise_model)

# compare full and stepwise model accuracy
TN_test$Application_Status <- as.character(TN_test$Application_Status) 
TN_test$Application_Status[TN_test$Application_Status=='denied'] <- '0'
TN_test$Application_Status[TN_test$Application_Status=='approved']<- '1'

prob_full <- full_model %>% predict(TN_test, type='response')
predicted_classes_full <- ifelse(prob_full > 0.5, '1', '0')
observed_classes_full <- TN_test$Application_Status
mean(predicted_classes_full == observed_classes_full)
anova(full_model)

# compare full and stepwise model accuracy
TN_test$Application_Status <- as.character(TN_test$Application_Status) 
TN_test$Application_Status[TN_test$Application_Status=='denied'] <- '0'
TN_test$Application_Status[TN_test$Application_Status=='approved']<- '1'

prob_stepwise <- stepwise_model %>% predict(TN_test, type='response')
predicted_classes_stepwise <- ifelse(prob_stepwise > 0.5, '1', '0')
observed_classes_stepwise <- TN_test$Application_Status
mean(predicted_classes_stepwise == observed_classes_stepwise)
anova(stepwise_model)
