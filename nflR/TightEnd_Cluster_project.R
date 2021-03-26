rm(list = ls())

library("nflfastR")
library("tidyverse")
library("caret")
library("cluster")

Last_three_season <- fast_scraper_schedules(seasons = 2018:2020)

Rosters <- fast_scraper_roster(seasons = 2018:2020)

TightEnds <-
  Rosters %>%
  filter(position == "TE")

TightEnds %>% filter(last_name == "Hooper") %>% select(gsis_id)

TightEnd_gsid <- TightEnds %>% group_by(gsis_id) %>% summarise(appearances = n())

pbp <- nflfastR::load_pbp(2018:2020, qs = TRUE)

view(Last_three_season)

schedule_object <- Last_three_season %>%
  filter(game_type == "REG") %>%
  select(game_id, season, week)

schedule_object_vector_twenty_eighteen <- schedule_object %>% filter(season == 2018 & week == 1) 
schedule_object_vector_twenty_eighteen <- as.vector(schedule_object_vector_twenty_eighteen$game_id)
pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_eighteen)
pbp <- pbp %>% filter(!is.na(receiver_player_id))
pbp <- decode_player_ids(pbp = pbp)
te_pbp <- pbp %>%
  inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
te_pbp_twenty_eighteen <- te_pbp

view(te_pbp)
closeAllConnections()
###2018 loop
for (i in 2:17) {
  schedule_object_vector_twenty_eighteen <- schedule_object %>% filter(season == 2018 & week == i) 
  schedule_object_vector_twenty_eighteen <- as.vector(schedule_object_vector_twenty_eighteen$game_id)
  pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_eighteen)
  pbp <- pbp %>% filter(!is.na(receiver_player_id))
  pbp <- decode_player_ids(pbp = pbp)
  te_pbp <- pbp %>%
    inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
  te_pbp_twenty_eighteen <- rbind(te_pbp, te_pbp_twenty_eighteen)
  closeAllConnections()
}

view(te_pbp_twenty_eighteen)

te_twenty_eighten_target_summary <- te_pbp_twenty_eighteen %>%
  filter(play_type == "pass" & !is.na(receiver_player_id)) %>%
  group_by(posteam, receiver_player_id) %>%
  summarise(
    receiver_player_name = paste(unique(receiver_player_name), collapse = ', '),
    targets = n(),
    receptions = sum(complete_pass, na.rm=TRUE),
    total_yards = sum(yards_gained, na.rm=TRUE),
    total_touchdowns = sum(touchdown, na.rm=TRUE),
    total_first_downs = sum(first_down_pass, na.rm = TRUE),
    yards_to_first_ratio = mean(yards_gained/ydstogo[ydstogo > 0], na.rm = TRUE),
    redzone_targets = sum(case_when(yardline_100 <= 20 ~ 1), na.rm = TRUE),
    redzone_catches = sum(complete_pass[yardline_100 <= 20], na.rm = TRUE),
    redzone_epa = sum(epa[yardline_100 <= 20], na.rm=TRUE),
    total_yac = sum(yards_after_catch, na.rm=TRUE),
    avg_yac_tgt = mean(yards_after_catch, na.rm=TRUE),
    avg_yac = mean(yards_after_catch[complete_pass == 1], na.rm=TRUE),
    total_air_yards = sum(air_yards, na.rm=TRUE),
    avg_air_yards = mean(air_yards, na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    total_epa = sum(epa, na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_air_epa = mean(air_epa, na.rm=TRUE),
    avg_yac_epa = mean(yac_epa, na.rm=TRUE),
    avg_cp = mean(cp, na.rm=TRUE),
    avg_cpoe = mean(cpoe, na.rm=TRUE)
  )

##scale all columns for te_target_summary

te_twenty_eighten_target_summary <- te_twenty_eighten_target_summary %>% filter(targets >= 25)

view(te_twenty_eighten_target_summary %>% arrange(desc(targets)))

te_twenty_eighten_target_summary_scaled <-
  te_twenty_eighten_target_summary %>% 
  ungroup() %>% 
  select(-posteam, -receiver_player_id,-receiver_player_name) %>%
  scale()

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = te_twenty_eighten_target_summary_scaled, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

te_cluster <- pam(te_twenty_eighten_target_summary_scaled, k = 8)

te_cluster_set <- te_cluster$cluster

te_cluster_set <- data.frame(matrix(unlist(te_cluster_set), nrow=length(te_cluster_set), byrow=TRUE))

names(te_cluster_set)[1] <- "te_cluster"

te_twenty_eighten_target_summary <- cbind.data.frame(te_cluster_set,te_twenty_eighten_target_summary)

view(te_twenty_eighten_target_summary %>% arrange(te_cluster))



#########################Begin 2019 Clustering



schedule_object_vector_twenty_nineteen <- schedule_object %>% filter(season == 2019 & week == 1) 
schedule_object_vector_twenty_nineteen <- as.vector(schedule_object_vector_twenty_nineteen$game_id)
pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_nineteen)
pbp <- pbp %>% filter(!is.na(receiver_player_id))
pbp <- decode_player_ids(pbp = pbp)
te_pbp <- pbp %>%
  inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
te_pbp_twenty_nineteen <- te_pbp

view(te_pbp)
closeAllConnections()
###2019 loop
for (i in 2:17) {
  schedule_object_vector_twenty_nineteen <- schedule_object %>% filter(season == 2019 & week == i) 
  schedule_object_vector_twenty_nineteen <- as.vector(schedule_object_vector_twenty_nineteen$game_id)
  pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_nineteen)
  pbp <- pbp %>% filter(!is.na(receiver_player_id))
  pbp <- decode_player_ids(pbp = pbp)
  te_pbp <- pbp %>%
    inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
  te_pbp_twenty_nineteen <- rbind(te_pbp, te_pbp_twenty_nineteen)
  closeAllConnections()
}

view(te_pbp_twenty_nineteen)

te_twenty_nineteen_target_summary <- te_pbp_twenty_nineteen %>%
  filter(play_type == "pass" & !is.na(receiver_player_id)) %>%
  group_by(posteam, receiver_player_id) %>%
  summarise(
    receiver_player_name = paste(unique(receiver_player_name), collapse = ', '),
    targets = n(),
    receptions = sum(complete_pass, na.rm=TRUE),
    total_yards = sum(yards_gained, na.rm=TRUE),
    total_touchdowns = sum(touchdown, na.rm=TRUE),
    total_first_downs = sum(first_down_pass, na.rm = TRUE),
    yards_to_first_ratio = mean(yards_gained/ydstogo[ydstogo > 0], na.rm = TRUE),
    redzone_targets = sum(case_when(yardline_100 <= 20 ~ 1), na.rm = TRUE),
    redzone_catches = sum(complete_pass[yardline_100 <= 20], na.rm = TRUE),
    redzone_epa = sum(epa[yardline_100 <= 20], na.rm=TRUE),
    total_yac = sum(yards_after_catch, na.rm=TRUE),
    avg_yac_tgt = mean(yards_after_catch, na.rm=TRUE),
    avg_yac = mean(yards_after_catch[complete_pass == 1], na.rm=TRUE),
    total_air_yards = sum(air_yards, na.rm=TRUE),
    avg_air_yards = mean(air_yards, na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    total_epa = sum(epa, na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_air_epa = mean(air_epa, na.rm=TRUE),
    avg_yac_epa = mean(yac_epa, na.rm=TRUE),
    avg_cp = mean(cp, na.rm=TRUE),
    avg_cpoe = mean(cpoe, na.rm=TRUE)
  )

te_twenty_nineteen_target_summary <- te_twenty_nineteen_target_summary %>% filter(targets >= 25)




view(te_twenty_nineteen_target_summary %>% arrange(desc(targets)))

te_twenty_nineteen_target_summary_scaled <-
  te_twenty_nineteen_target_summary %>% 
  ungroup() %>% 
  select(-posteam, -receiver_player_id,-receiver_player_name) %>%
  scale()

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = te_twenty_eighten_target_summary_scaled, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)


te_cluster <- pam(te_twenty_nineteen_target_summary_scaled, k = 8)

te_cluster_set <- te_cluster$cluster

te_cluster_set <- data.frame(matrix(unlist(te_cluster_set), nrow=length(te_cluster_set), byrow=TRUE))

names(te_cluster_set)[1] <- "te_cluster"

te_twenty_nineteen_target_summary <- cbind.data.frame(te_cluster_set,te_twenty_nineteen_target_summary)
#########################################start 2020


schedule_object_vector_twenty_twenty <- schedule_object %>% filter(season == 2020 & week == 1) 
schedule_object_vector_twenty_twenty <- as.vector(schedule_object_vector_twenty_twenty$game_id)
pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_twenty)
pbp <- pbp %>% filter(!is.na(receiver_player_id))
pbp <- decode_player_ids(pbp = pbp)
te_pbp <- pbp %>%
  inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
te_pbp_twenty_twenty <- te_pbp

view(te_pbp)
closeAllConnections()
###2019 loop
for (i in 2:17) {
  schedule_object_vector_twenty_twenty <- schedule_object %>% filter(season == 2020 & week == i) 
  schedule_object_vector_twenty_twenty <- as.vector(schedule_object_vector_twenty_twenty$game_id)
  pbp <- build_nflfastR_pbp(game_ids = schedule_object_vector_twenty_twenty)
  pbp <- pbp %>% filter(!is.na(receiver_player_id))
  pbp <- decode_player_ids(pbp = pbp)
  te_pbp <- pbp %>%
    inner_join(TightEnd_gsid, by = c("receiver_player_id" = "gsis_id"))
  te_pbp_twenty_twenty <- rbind(te_pbp, te_pbp_twenty_twenty)
  closeAllConnections()
}

view(te_pbp_twenty_twenty)

te_twenty_twenty_target_summary <- te_pbp_twenty_twenty %>%
  filter(play_type == "pass" & !is.na(receiver_player_id)) %>%
  group_by(posteam, receiver_player_id) %>%
  summarise(
    receiver_player_name = paste(unique(receiver_player_name), collapse = ', '),
    targets = n(),
    receptions = sum(complete_pass, na.rm=TRUE),
    total_yards = sum(yards_gained, na.rm=TRUE),
    total_touchdowns = sum(touchdown, na.rm=TRUE),
    total_first_downs = sum(first_down_pass, na.rm = TRUE),
    yards_to_first_ratio = mean(yards_gained/ydstogo[ydstogo > 0], na.rm = TRUE),
    redzone_targets = sum(case_when(yardline_100 <= 20 ~ 1), na.rm = TRUE),
    redzone_catches = sum(complete_pass[yardline_100 <= 20], na.rm = TRUE),
    redzone_epa = sum(epa[yardline_100 <= 20], na.rm=TRUE),
    total_yac = sum(yards_after_catch, na.rm=TRUE),
    avg_yac_tgt = mean(yards_after_catch, na.rm=TRUE),
    avg_yac = mean(yards_after_catch[complete_pass == 1], na.rm=TRUE),
    total_air_yards = sum(air_yards, na.rm=TRUE),
    avg_air_yards = mean(air_yards, na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    total_epa = sum(epa, na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_air_epa = mean(air_epa, na.rm=TRUE),
    avg_yac_epa = mean(yac_epa, na.rm=TRUE),
    avg_cp = mean(cp, na.rm=TRUE),
    avg_cpoe = mean(cpoe, na.rm=TRUE)
  )

te_twenty_twenty_target_summary <- te_twenty_twenty_target_summary %>% filter(targets >= 25)




view(te_twenty_twenty_target_summary %>% arrange(desc(targets)))

te_twenty_twenty_target_summary_scaled <-
  te_twenty_twenty_target_summary %>% 
  ungroup() %>% 
  select(-posteam, -receiver_player_id,-receiver_player_name) %>%
  scale()

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = te_twenty_twenty_target_summary_scaled, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)


te_cluster <- pam(te_twenty_twenty_target_summary_scaled, k = 8)

te_cluster_set <- te_cluster$cluster

te_cluster_set <- data.frame(matrix(unlist(te_cluster_set), nrow=length(te_cluster_set), byrow=TRUE))

names(te_cluster_set)[1] <- "te_cluster"

te_twenty_twenty_target_summary <- cbind.data.frame(te_cluster_set,te_twenty_twenty_target_summary)



###################################summarize all years
view(te_twenty_nineteen_target_summary %>% arrange(te_cluster))

view(te_twenty_twenty_target_summary %>% arrange(te_cluster))

te_twenty_nineteen_cluster_summary <- te_twenty_nineteen_target_summary %>%
  group_by(te_cluster) %>%
  summarise(
    tight_ends_in_cluster = n(),
    avg_targets = mean(targets),
    max_targets = max(targets),
    min_targets = min(targets),
    avg_targets = mean(receptions),
    max_targets = max(receptions),
    min_targets = min(receptions),
    avg_tot_yds = mean(total_yards),
    max_tot_yds = max(total_yards),
    min_tot_yds = min(total_yards),
    avg_yac_tgt = mean(total_yac/targets),
    max_yac_tgt = max(total_yac/targets),
    min_yac_tgt = min(total_yac/targets),
    avg_td = mean(total_touchdowns),
    max_td = max(total_touchdowns),
    min_td = min(total_touchdowns),
    avg_tot_epa = mean(total_epa),
    max_tot_epa = max(total_epa),
    min_tot_epa = min(total_epa),
    avg_cpoe = mean((receptions/targets) - avg_cp),
    max_cpoe = max((receptions/targets) - avg_cp),
    min_cpoe = min((receptions/targets) - avg_cp),
    avg_redzone_epa = mean(redzone_epa),
    max_redzone_epa = max(redzone_epa),
    min_redzone_epa = min(redzone_epa)
  )


te_twenty_eighten_cluster_summary <- te_twenty_eighten_target_summary %>%
  group_by(te_cluster) %>%
  summarise(
    tight_ends_in_cluster = n(),
    avg_targets = mean(targets),
    max_targets = max(targets),
    min_targets = min(targets),
    avg_targets = mean(receptions),
    max_targets = max(receptions),
    min_targets = min(receptions),
    avg_tot_yds = mean(total_yards),
    max_tot_yds = max(total_yards),
    min_tot_yds = min(total_yards),
    avg_yac_tgt = mean(total_yac/targets),
    max_yac_tgt = max(total_yac/targets),
    min_yac_tgt = min(total_yac/targets),
    avg_td = mean(total_touchdowns),
    max_td = max(total_touchdowns),
    min_td = min(total_touchdowns),
    avg_tot_epa = mean(total_epa),
    max_tot_epa = max(total_epa),
    min_tot_epa = min(total_epa),
    avg_cpoe = mean((receptions/targets) - avg_cp),
    max_cpoe = max((receptions/targets) - avg_cp),
    min_cpoe = min((receptions/targets) - avg_cp),
    avg_redzone_epa = mean(redzone_epa),
    max_redzone_epa = max(redzone_epa),
    min_redzone_epa = min(redzone_epa)
  )



te_twenty_twenty_cluster_summary <- te_twenty_twenty_target_summary %>%
  group_by(te_cluster) %>%
  summarise(
    tight_ends_in_cluster = n(),
    avg_targets = mean(targets),
    max_targets = max(targets),
    min_targets = min(targets),
    avg_targets = mean(receptions),
    max_targets = max(receptions),
    min_targets = min(receptions),
    avg_tot_yds = mean(total_yards),
    max_tot_yds = max(total_yards),
    min_tot_yds = min(total_yards),
    avg_yac_tgt = mean(total_yac/targets),
    max_yac_tgt = max(total_yac/targets),
    min_yac_tgt = min(total_yac/targets),
    avg_td = mean(total_touchdowns),
    max_td = max(total_touchdowns),
    min_td = min(total_touchdowns),
    avg_tot_epa = mean(total_epa),
    max_tot_epa = max(total_epa),
    min_tot_epa = min(total_epa),
    avg_cpoe = mean((receptions/targets) - avg_cp),
    max_cpoe = max((receptions/targets) - avg_cp),
    min_cpoe = min((receptions/targets) - avg_cp),
    avg_redzone_epa = mean(redzone_epa),
    max_redzone_epa = max(redzone_epa),
    min_redzone_epa = min(redzone_epa)
  )
