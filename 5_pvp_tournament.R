setwd("~/Courses/Upwork_Projects/Gareth/RBC/results")

df <- read.csv("simulation.csv")
df <- df %>% dplyr::select(player_name, orig_score, orig_sd)

df <- column_to_rownames(df, "player_name")

f <- function(n1, s1, n2, s2){
  mean(rnorm(100000, n1, s1) < rnorm(100000, n2, s2))
  
}

g <- Vectorize(f, c("n1", "s1", "n2", "s2"))
res <- outer(df$orig_score, df$orig_sd, df$orig_score, df$orig_sd, FUN = g)
dimnames(res) <- list(row.names(df), row.names(df))
res <- data.frame(res)
res <- tibble::rownames_to_column(res, 'p1')

datalong_2 <- tidyr::gather(res, 'p2', 'value', 2:134)
library(DT)
datatable(datalong_2, filter = "top")

write.csv(datalong_2, "~/Courses/Upwork_Projects/Gareth/RBC/results/pvp tournament.csv")
