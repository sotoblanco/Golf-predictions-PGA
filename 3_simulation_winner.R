setwd("~/Courses/Upwork_Projects/Gareth/RBC/data")
library(tidyverse)
library(readxl)
df_s <- read_excel("RBC Heritage Scores.xlsx", sheet = "Round4", na = c("", "-", "NaN")) # read the spreadsheet including using na for blank and - spaces
library(janitor)
df_s <- clean_names(df_s)

df_s$date <- as.POSIXct(df_s$date, format = "%Y-%m-%d")
library(lubridate)
df_s$year <- year(df_s$date)

df_s <- df_s  %>% dplyr::select(year,first_name_surname, posn, rd1, rd2, rd3, rd4)
df_s[is.na(df_s)] <- 80
df_s$score <- df_s$rd1 +  df_s$rd2 + df_s$rd3 + df_s$rd4

df_s <- df_s %>% group_by(year) %>% mutate(score = scale(score)) %>% ungroup()


df_s <- df_s %>% group_by(first_name_surname) %>% 
  mutate(sd_score = sd(score)) %>% 
  mutate(mean_score = mean(score))

df_s <- df_s[!duplicated(df_s$first_name_surname),]
sd_1 <- sd(df_s$score, na.rm = TRUE)
df_s[,9][is.na(df_s[,9])] <- sd_1 # sd for those that has NA
colnames(df_s)[2] <- 'player_name'

#########

setwd("~/Courses/Upwork_Projects/Gareth/RBC/data")
test_2021 <- read.csv("scores_RBC.csv")
pred_s <- test_2021 %>% select(player_name, year, pos, pred, sgtot)
#colnames(pred_s)[1] <- 'player_name'

df_all <- merge(df_s, pred_s, by = 'player_name')
df_all$pred <- round(df_all$pred, 2)
df_all$sd_score <- round(df_all$sd_score,2)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

df_34 <- df_all %>%
  uncount(100000, .id = "run") %>%
  rowwise() %>%
  mutate(sim_mpg = rnorm(1, mean = pred, sd = sd_score)) %>%
  
  group_by(run) %>%
  arrange(sim_mpg) %>%
  mutate(lowest_mpg = row_number() == 1) %>%
  
  group_by(player_name) %>%
  summarize(chance_lowest = mean(lowest_mpg),
            orig_mpg = first(pred))# %>%
  ggplot(aes(orig_mpg, chance_lowest, label = player_name)) +
  geom_text(hjust = 0, check_overlap = TRUE) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.001), 
                     labels = scales::percent_format(accuracy = 1), 
                     breaks = c(0, 0.01, 0.1*(1:4)))



write.csv(df_34, "~/Courses/Upwork_Projects/Gareth/RBC/results/simulation results.csv")
