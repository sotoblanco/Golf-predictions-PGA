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

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)
Model_rf <- train(score~., data = train_2021,
                  method = "rf",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = fitControl)


Model.training_rf <-predict(Model_rf, train_2021) # Apply model to make prediction on Training set
cor(train_2021$score, Model.training_rf)
par(mfrow=c(1,2))
plot(train_2021$score,Model.training_rf, col = "blue")
test_2021$pred <- predict(Model_rf, test_2021)
# 0.98
#0.40
write.csv(test_2021, "~/Courses/Upwork_Projects/Gareth/RBC/data/scores_RBC.csv", row.names = FALSE)

