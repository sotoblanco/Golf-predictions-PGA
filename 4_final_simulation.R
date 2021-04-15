setwd("~/Courses/Upwork_Projects/Gareth/RBC/data")
library(tidyverse)
library(caret)
library(doParallel)
library(tibble)
library(plotly)
library(readxl)
library(janitor)
library(lubridate)

train_2021 <- read.csv("train_2021_RBC.csv")
test_2021 <- read.csv("test_2021_RBC.csv")
train_2021 <- train_2021 %>% group_by(year) %>% mutate(score = scale(score)) %>% ungroup()
train_2021 <- train_2021 %>% dplyr::select(-c(player_name:money, rd1:rd4rank))

## Random forest model
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
fitControl <- trainControl(method = "cv",
                           number = 10)
Model_rf <- train(score~., data = train_2021,
                  method = "rf",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = fitControl)


## Get the data with all scores

df_s <- read_excel("RBC Heritage Scores.xlsx", sheet = "Round4", na = c("", "-", "NaN"))
df_s <- clean_names(df_s)
df_s$date <- as.POSIXct(df_s$date, format = "%Y-%m-%d")
df_s$year <- year(df_s$date)
df_s <- df_s  %>% dplyr::select(year,first_name_surname, posn, rd1, rd2, rd3, rd4)
df_s[is.na(df_s)] <- 80
df_s$score <- df_s$rd1 +  df_s$rd2 + df_s$rd3 + df_s$rd4
df_s <- df_s %>% group_by(year) %>% mutate(score = scale(score)) %>% ungroup()
df_s <- df_s %>% group_by(first_name_surname) %>% 
  mutate(sd_score = sd(score)) %>% 
  mutate(mean_score = mean(score))
df_s <- df_s[!duplicated(df_s$first_name_surname),]

colnames(df_s)[2] <- 'player_name'
df_s <- df_s %>% select(-c(year, posn, rd1, rd2, rd3, rd4))
names_player <- test_2021 %>% select(player_name) 
df_s_2 <- merge(names_player, df_s ,by = 'player_name' ,all.x = TRUE)


sd_1 <- sd(df_s$score, na.rm = TRUE)
df_s_2[,3][is.na(df_s_2[,3])] <- sd_1 # sd for those that has NA
head(df_s_2)

#####

test_2021$pred <- predict(Model_rf, test_2021)
pred_s <- test_2021 %>% dplyr::select(player_name, year, pos, pred, sgtot)

df_all <- merge(pred_s, df_s_2, by = 'player_name', all.x = TRUE)
df_all$pred <- round(df_all$pred, 2)
df_all$sd_score <- round(df_all$sd_score,2)

## Simulation process to get the probabilities

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

df_34 <- df_all %>%
  uncount(100000, .id = "run") %>%
  rowwise() %>%
  mutate(sim_score = rnorm(1, mean = pred, sd = sd_score)) %>%
  
  group_by(run) %>%
  arrange(sim_score) %>%
  mutate(lowest_score = row_number() == 1) %>%
  
  group_by(player_name) %>%
  summarize(chance_lowest = mean(lowest_score),
            orig_score = first(pred),
            orig_sd = first(sd_score))

df_345 <- merge(df_34, test_2021, by = 'player_name', all.y = TRUE)

g <- ggplot(df_345, aes(x= sgtot, y = chance_lowest, label = player_name, color = pred))+
  geom_point()
ggplotly(g)

write.csv(df_345, "~/Courses/Upwork_Projects/Gareth/RBC/Results/simulation.csv")


train_2021 <- read.csv("train_2021_RBC.csv")
train_2021$pred <- predict(Model_rf, train_2021)
train_2021 <- train_2021 %>% mutate(pos = case_when(pos == 1 ~ "Winning",
                                                    pos > 1 & pos <= 5 ~ "top5",
                                                    pos > 5 & pos <= 10 ~ "top10",
                                                    pos > 10 & pos <= 20 ~ "top20",
                                                    pos > 20 & pos <= 60 ~ "top60",
                                                    pos > 60 ~ "cut"))

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
fitControl <- trainControl(method = "cv",
                           number = 10)

polr_m <- train(pos~pred, data = train_2021,
                method = "polr",
                trControl = fitControl)
polr_prob_train <- predict(polr_m, train_2021, type = "prob")$Winning
polr_prob_test <- predict(polr_m, test_2021, type = "prob")$Winning
train_2021$prob <- polr_prob_train/sum(polr_prob_train)
test_2021$prob <- polr_prob_test/sum(polr_prob_test)


g_1 <- ggplot(test_2021, aes(x= sgtot, y = round(prob,2), label = player_name))+
  geom_point()
ggplotly(g_1)