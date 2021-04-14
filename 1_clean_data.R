###### Data preparation ####
setwd("~/Courses/Upwork_Projects/Gareth/RBC")
library(readxl)
df <- read_excel("2021 RBC Heritage Tournament.xlsx", sheet = "Historic", na = c("", "-", "NaN")) # read the spreadsheet including using na for blank and - spaces
library(janitor)
df <- clean_names(df)
library(dplyr)
df_2021 <- read_excel("2021 RBC Heritage Tournament.xlsx",sheet = "Current", na = c("", "-", "NaN"))
df_2021 <- clean_names(df_2021)
df_2021[names(df)]
df <- rbind(df_2021,df)

# explore missing values
sum(is.na(df))
#19191
naniar::gg_miss_var(df)
df <- df %>%
  group_by(player_name) %>%
  mutate(across(c(current:g3putt_avoid10000), ~replace(., is.na(.), median(., na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(across(c(current:g3putt_avoid10000), ~replace(., is.na(.), median(., na.rm = TRUE))))
sum(is.na(df))
naniar::gg_miss_var(df)
#18704
df_2 <- df
df <- df %>% select(player_name:g3putt_avoid10000)

sgtot <- grep("sgtot", names(df))
Xs_seq <- rev(seq_along(sgtot))
sgtot <- as.matrix(df[sgtot]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgtot <- sgtot

sgt2g <- grep("sgt2g", names(df))
Xs_seq <- rev(seq_along(sgt2g))
sgt2g <- as.matrix(df[sgt2g]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgt2g <- sgt2g

sgbs <- grep("sgbs", names(df))
Xs_seq <- rev(seq_along(sgbs))
sgbs <- as.matrix(df[sgbs]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgbs <- sgbs

sgsg <- grep("sgsg", names(df))
Xs_seq <- rev(seq_along(sgsg))
sgsg <- as.matrix(df[sgsg]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgsg <- sgsg

sgott <- grep("sgott", names(df))
Xs_seq <- rev(seq_along(sgott))
sgott <- as.matrix(df[sgott]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgott <- sgott

sgapp <- grep("sgapp", names(df))
Xs_seq <- rev(seq_along(sgapp))
sgapp <- as.matrix(df[sgapp]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgapp <- sgapp

sgarg <- grep("sgarg", names(df))
Xs_seq <- rev(seq_along(sgarg))
sgarg <- as.matrix(df[sgarg]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgarg <- sgarg

sg_px <- grep("sg_px", names(df))
Xs_seq <- rev(seq_along(sg_px))
sg_px <- as.matrix(df[sg_px]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sg_px <- sg_px

opps_gained <- grep("opps_gained", names(df))
Xs_seq <- rev(seq_along(opps_gained))
opps_gained <- as.matrix(df[opps_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$opps_gained <- opps_gained

bo_b_gained <- grep("bo_b_gained", names(df))
Xs_seq <- rev(seq_along(bo_b_gained))
bo_b_gained <- as.matrix(df[bo_b_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$bo_b_gained <- bo_b_gained

eagles_gained <- grep("eagles_gained", names(df))
Xs_seq <- rev(seq_along(eagles_gained))
eagles_gained <- as.matrix(df[eagles_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$eagles_gained <- eagles_gained

birdies_gained <- grep("birdies_gained", names(df))
Xs_seq <- rev(seq_along(birdies_gained))
birdies_gained <- as.matrix(df[birdies_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$birdies_gained <- birdies_gained

bogeys_avoided <- grep("bogeys_avoided", names(df))
Xs_seq <- rev(seq_along(bogeys_avoided))
bogeys_avoided <- as.matrix(df[bogeys_avoided]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$bogeys_avoided <- bogeys_avoided

doubles_avoided <- grep("doubles_avoided", names(df))
Xs_seq <- rev(seq_along(doubles_avoided))
doubles_avoided <- as.matrix(df[doubles_avoided]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$doubles_avoided <- doubles_avoided

dr_dist <- grep("dr_dist", names(df))
Xs_seq <- rev(seq_along(dr_dist))
dr_dist <- as.matrix(df[dr_dist]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$dr_dist <- dr_dist

fwys_gained <- grep("fwys_gained", names(df))
Xs_seq <- rev(seq_along(fwys_gained))
fwys_gained <- as.matrix(df[fwys_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$fwys_gained <- fwys_gained

good_drives_gained <- grep("good_drives_gained", names(df))
Xs_seq <- rev(seq_along(good_drives_gained))
good_drives_gained <- as.matrix(df[good_drives_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$good_drives_gained <- good_drives_gained

dr_left_avoid <- grep("dr_left_avoid", names(df))
Xs_seq <- rev(seq_along(dr_left_avoid))
dr_left_avoid <- as.matrix(df[dr_left_avoid]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$dr_left_avoid <- dr_left_avoid

gi_rs_gained <- grep("gi_rs_gained", names(df))
Xs_seq <- rev(seq_along(gi_rs_gained))
gi_rs_gained <- as.matrix(df[gi_rs_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$gi_rs_gained <- gi_rs_gained

sand_saves_gained <- grep("sand_saves_gained", names(df))
Xs_seq <- rev(seq_along(sand_saves_gained))
sand_saves_gained <- as.matrix(df[sand_saves_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sand_saves_gained <- sand_saves_gained

scrambling_gained <- grep("scrambling_gained", names(df))
Xs_seq <- rev(seq_along(scrambling_gained))
scrambling_gained <- as.matrix(df[scrambling_gained]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$scrambling_gained <- scrambling_gained

prox <- grep("prox", names(df))
Xs_seq <- rev(seq_along(prox))
prox <- as.matrix(df[prox]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$prox <- prox

g75100 <- grep("g75100", names(df))
Xs_seq <- rev(seq_along(g75100))
g75100 <- as.matrix(df[g75100]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g75100 <- g75100

g100125 <- grep("g100125", names(df))
Xs_seq <- rev(seq_along(g100125))
g100125 <- as.matrix(df[g100125]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g100125 <- g100125

g125150 <- grep("g125150", names(df))
Xs_seq <- rev(seq_along(g125150))
g125150 <- as.matrix(df[g125150]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g125150 <- g125150

g150175 <- grep("g150175", names(df))
Xs_seq <- rev(seq_along(g150175))
g150175 <- as.matrix(df[g150175]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g150175 <- g150175

g150175 <- grep("g150175", names(df))
Xs_seq <- rev(seq_along(g150175))
g150175 <- as.matrix(df[g150175]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g150175 <- g150175

g175200 <- grep("g175200", names(df))
Xs_seq <- rev(seq_along(g175200))
g175200 <- as.matrix(df[g175200]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g175200 <- g175200

g200 <- grep("g200", names(df))
Xs_seq <- rev(seq_along(g200))
g200 <- as.matrix(df[g200]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g200 <- g200

g200 <- grep("g200", names(df))
Xs_seq <- rev(seq_along(g200))
g200 <- as.matrix(df[g200]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g200 <- g200

sg_par3 <- grep("sg_par3", names(df))
Xs_seq <- rev(seq_along(sg_par3))
sg_par3 <- as.matrix(df[sg_par3]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sg_par3 <- sg_par3

p30150 <- grep("p30150", names(df))
Xs_seq <- rev(seq_along(p30150))
p30150 <- as.matrix(df[p30150]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p30150 <- p30150

p3150175 <- grep("p3150175", names(df))
Xs_seq <- rev(seq_along(p3150175))
p3150175 <- as.matrix(df[p3150175]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p3150175 <- p3150175

p3175200 <- grep("p3175200", names(df))
Xs_seq <- rev(seq_along(p3175200))
p3175200 <- as.matrix(df[p3175200]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p3175200 <- p3175200

p3200225 <- grep("p3200225", names(df))
Xs_seq <- rev(seq_along(p3200225))
p3200225 <- as.matrix(df[p3200225]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p3200225 <- p3200225

p3225 <- grep("p3225", names(df))
Xs_seq <- rev(seq_along(p3225))
p3225 <- as.matrix(df[p3225]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p3225 <- p3225

sg_par4 <- grep("sg_par4", names(df))
Xs_seq <- rev(seq_along(sg_par4))
sg_par4 <- as.matrix(df[sg_par4]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sg_par4 <- sg_par4

p40350 <- grep("p40350", names(df))
Xs_seq <- rev(seq_along(p40350))
p40350 <- as.matrix(df[p40350]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p40350 <- p40350

p4350400 <- grep("p4350400", names(df))
Xs_seq <- rev(seq_along(p4350400))
p4350400 <- as.matrix(df[p4350400]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p4350400 <- p4350400

p4400450 <- grep("p4400450", names(df))
Xs_seq <- rev(seq_along(p4400450))
p4400450 <- as.matrix(df[p4400450]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p4400450 <- p4400450

p4450500 <- grep("p4450500", names(df))
Xs_seq <- rev(seq_along(p4450500))
p4450500 <- as.matrix(df[p4450500]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p4450500 <- p4450500

p4500 <- grep("p4500", names(df))
Xs_seq <- rev(seq_along(p4500))
p4500 <- as.matrix(df[p4500]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p4500 <- p4500

sg_par5 <- grep("sg_par5", names(df))
Xs_seq <- rev(seq_along(sg_par5))
sg_par5 <- as.matrix(df[sg_par5]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sg_par5 <- sg_par5

p50500 <- grep("p50500", names(df))
Xs_seq <- rev(seq_along(p50500))
p50500 <- as.matrix(df[p50500]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p50500 <- p50500

p5500550 <- grep("p5500550", names(df))
Xs_seq <- rev(seq_along(p5500550))
p5500550 <- as.matrix(df[p5500550]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p5500550 <- p5500550

p5550600 <- grep("p5550600", names(df))
Xs_seq <- rev(seq_along(p5550600))
p5550600 <- as.matrix(df[p5550600]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p5550600 <- p5550600

p5600650 <- grep("p5600650", names(df))
Xs_seq <- rev(seq_along(p5600650))
p5600650 <- as.matrix(df[p5600650]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p5600650 <- p5600650

p5600 <- grep("p5600", names(df))
Xs_seq <- rev(seq_along(p5600))
p5600 <- as.matrix(df[p5600]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$p5600 <- p5600

g05ft <- grep("g05ft", names(df))
Xs_seq <- rev(seq_along(g05ft))
g05ft <- as.matrix(df[g05ft]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g05ft <- g05ft

g510ft <- grep("g510ft", names(df))
Xs_seq <- rev(seq_along(g510ft))
g510ft <- as.matrix(df[g510ft]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g510ft <- g510ft

g1015ft <- grep("g1015ft", names(df))
Xs_seq <- rev(seq_along(g1015ft))
g1015ft <- as.matrix(df[g1015ft]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g1015ft <- g1015ft

g1520ft <- grep("g1520ft", names(df))
Xs_seq <- rev(seq_along(g1520ft))
g1520ft <- as.matrix(df[g1520ft]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g1520ft <- g1520ft

g2025ft <- grep("g2025ft", names(df))
Xs_seq <- rev(seq_along(g2025ft))
g2025ft <- as.matrix(df[g2025ft]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g2025ft <- g2025ft

g25ft_plus <- grep("g25ft_plus", names(df))
Xs_seq <- rev(seq_along(g25ft_plus))
g25ft_plus <- as.matrix(df[g25ft_plus]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g25ft_plus <- g25ft_plus

g3putt_avoid <- grep("g3putt_avoid", names(df))
Xs_seq <- rev(seq_along(g3putt_avoid))
g3putt_avoid <- as.matrix(df[g3putt_avoid]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$g3putt_avoid <- g3putt_avoid

df <- df %>% dplyr::select(player_name:location,sgtot:g3putt_avoid)

write.csv(df, "~/Courses/Upwork_Projects/Gareth/RBC/data/data.csv", row.names = FALSE)

test_2021 <- df %>% filter(year == 2021)
write.csv(test_2021, "~/Courses/Upwork_Projects/Gareth/RBC/data/test_2021_RBC.csv", row.names = FALSE)

train_2021 <- df %>% filter(year != 2021)
write.csv(train_2021, "~/Courses/Upwork_Projects/Gareth/RBC/data/train_2021_RBC.csv", row.names = FALSE)
