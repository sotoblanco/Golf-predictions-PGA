setwd("~/Courses/Upwork_Projects/Gareth/RBC/data")
library(tidyverse)
library(caret)
library(doParallel)
library(tibble)
library(plotly)
train_2021 <- read.csv("train_2021_RBC.csv")
test_2021 <- read.csv("test_2021_RBC.csv")


train_2021 <- train_2021 %>% mutate(pos = case_when(pos == 1 ~ "Winning",
                                                    pos > 1 & pos <= 5 ~ "top5",
                                                    pos > 5 & pos <= 10 ~ "top10",
                                                    pos > 10 & pos <= 20 ~ "top20",
                                                    pos > 20 & pos <= 60 ~ "top60",
                                                    pos > 60 ~ "cut"))

test_2021 <- column_to_rownames(test_2021, var = 'player_name')

train_2021 <- train_2021 %>% group_by(year) %>% mutate(score = scale(score)) %>% ungroup()

train_2021 <- train_2021 %>% dplyr::select(-c(player_name:money, rd1:rd4rank))

set.seed(100)
TrainingIndex <- createDataPartition(train_2021$score, p = 0.8, list = FALSE)
TrainingSet <- train_2021[TrainingIndex,]
TestingSet <- train_2021[-TrainingIndex,]

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
fitControl <- trainControl(method = "cv",
                           number = 10)
Model_rf <- train(score~., data = TrainingSet,
                  method = "rf",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = fitControl)


Model.training_rf <-predict(Model_rf, TrainingSet) # Apply model to make prediction on Training set
Model.testing_rf <-predict(Model_rf, TestingSet) # Apply model to make prediction on Testing set
cor(TrainingSet$score, Model.training_rf)
cor(TestingSet$score, Model.testing_rf)
par(mfrow=c(1,2))
plot(TrainingSet$score,Model.training_rf, col = "blue")
plot(TestingSet$score,Model.testing_rf, col = "blue")
# 0.98
#0.40
train_2021 <- read.csv("train_2021_RBC.csv")
set.seed(100)
TrainingIndex <- createDataPartition(train_2021$score, p = 0.8, list = FALSE)
TrainingSet <- train_2021[TrainingIndex,]
TestingSet <- train_2021[-TrainingIndex,]
TestingSet$pred <- predict(Model_rf, TestingSet)

