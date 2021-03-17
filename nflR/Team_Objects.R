rm(list = ls())

library("nflfastR")
library("tidyverse")
library("caret")

Twenty_Twenty <- fast_scraper_schedules(seasons = 2020)

Twenty_Twenty_CLE_schd <- Twenty_Twenty %>%
  filter(home_team == "CLE" | away_team == "CLE", game_type == "REG")

Twenty_Twenty_CLE_game <- as.vector(Twenty_Twenty_CLE_schd$game_id)
Twenty_Twenty_CLE_pbp <- build_nflfastR_pbp(game_ids = Twenty_Twenty_CLE_game)

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% group_by(game_id, game_half) %>%
  mutate(LastPlayOfHalf = as.numeric(play_id == max(play_id))) %>%
  ungroup()

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% group_by(game_id, game_half) %>%
  mutate(FirstPlayOfHalf = as.numeric(play_id == min(play_id))) %>%
  ungroup()

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% group_by(game_id) %>%
  mutate(FirstPlayOfGame = as.numeric(play_id == min(play_id))) %>%
  ungroup()

# Now, we get into what's most relevant:
Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% mutate(LastPlayOfDrive = ifelse(
  (is.na(td_team) &
     (punt_attempt %in% 1 | play_type %in% "punt" | (extra_point_attempt %in% 1 & (play_type != "no_play" | (str_detect(Twenty_Twenty_CLE_pbp$desc, "(enforced between downs)")))) |
        play_type %in% "extra_point" | two_point_attempt %in% 1 | (str_detect(Twenty_Twenty_CLE_pbp$desc, "(two-point)") & penalty %in% 0) |
        (str_detect(Twenty_Twenty_CLE_pbp$desc, "(TWO-POINT)") & penalty %in% 0) | (str_detect(Twenty_Twenty_CLE_pbp$desc, "(Two-point)") & penalty %in% 0) |
        (str_detect(Twenty_Twenty_CLE_pbp$desc, "(Two-Point)") & penalty %in% 0) | field_goal_result %in% "missed" | field_goal_result %in% "blocked" |
        field_goal_result %in% "made" | field_goal_attempt %in% 1 | interception %in% 1 | safety %in% 1 |
        fumble_lost %in% 1 | fourth_down_failed %in% 1 | LastPlayOfHalf %in% 1)), 1, 0)
)

# This is just a dummy variable that will be fixed later
Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>%
  mutate(NewDrive = 0)

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% group_by(game_id) %>%
  mutate(NewDrive = ifelse(FirstPlayOfGame == 1, 1,
                           ifelse(lag(LastPlayOfDrive == 1), 1 + lag(NewDrive), lag(NewDrive)))) %>%
  ungroup()

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>% group_by(game_id) %>%
  mutate(NewDrive = cumsum(NewDrive)) %>%
  ungroup()

Twenty_Twenty_CLE_pbp <- Twenty_Twenty_CLE_pbp %>%
  select(-drive) %>%
  rename(drive = NewDrive)
  
Twenty_Twenty_CLE_off <-
  Twenty_Twenty_CLE_pbp %>%
  filter(posteam == "CLE") %>%
  select(posteam, game_id, drive, series, play_id, play_type, play_clock, down, ydstogo, score_differential, goal_to_go, yardline_100, pass, pass_attempt, complete_pass, shotgun, qb_dropback,  pass_length, pass_location, passer_player_id, passer_player_name, receiver_player_id, receiver_player_name, rush_attempt, run_location, run_gap, rusher_player_id, rusher_player_name, yards_gained, air_yards, yards_after_catch, cp, cpoe, epa, air_epa, yac_epa, qb_epa, comp_air_epa, comp_yac_epa, qb_epa, xyac_epa, wp, wpa, touchdown, field_goal_attempt, field_goal_result, penalty, penalty_team, penalty_player_id, penalty_player_name, penalty_type, penalty_yards, fumble, fumble_lost, interception)

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  mutate(Pre_play_Outcome_Classification = case_when(wp > .67 ~ "Likely to Win", between(wp,.34,.67) ~ "Outcome Undecided", wp < .34 ~ "Likely to Lose"))

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  mutate(Play_Outcome_Classification = case_when(wp + wpa > .67 ~ "Likely to Win", between((wp + wpa),.34,.67) ~ "Outcome Undecided", wp + wpa < .34 ~ "Likely to Lose"))

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  mutate(Successful_Run = case_when(play_type == "run" & down == "1" & yards_gained/ydstogo >= .4 ~ 1, play_type == "run" & down == "2" & yards_gained/ydstogo >= .6 ~ 1, play_type == "run" & (down == "3"|down == "4") & yards_gained/ydstogo >= 1 ~ 1))

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  arrange(game_id, drive, play_id)

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  mutate(off_player_name = coalesce(receiver_player_name,rusher_player_name), off_player_id = coalesce(receiver_player_id,rusher_player_id))

Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  mutate(new_penalty_yards = case_when(posteam == penalty_team ~ -1*penalty_yards, posteam != penalty_team ~ penalty_yards))


Twenty_Twenty_CLE_off <- Twenty_Twenty_CLE_off %>%
  select(-penalty_yards) %>%
  rename(penalty_yards = new_penalty_yards)

Twenty_Twenty_CLE_target_summary <- Twenty_Twenty_CLE_off %>%
  filter(play_type == "pass" & !is.na(receiver_player_id)) %>%
  group_by(posteam, receiver_player_id) %>%
  summarise(
    receiver_player_name = paste(unique(receiver_player_name), collapse = ', '),
    targets = n(),
    receptions = sum(complete_pass, na.rm=TRUE),
    total_yards = sum(yards_gained, na.rm=TRUE),
    total_yac = sum(yards_after_catch, na.rm=TRUE),
    avg_yac_tgt = mean(yards_after_catch, na.rm=TRUE),
    avg_yac = mean(yards_after_catch[complete_pass == 1], na.rm=TRUE),
    total_air_yards = sum(air_yards, na.rm=TRUE),
    avg_air_yards = mean(air_yards, na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_air_epa = mean(air_epa, na.rm=TRUE),
    avg_yac_epa = mean(yac_epa, na.rm=TRUE),
    avg_cp = mean(cp, na.rm=TRUE),
    avg_cpoe = mean(cpoe, na.rm=TRUE)
  )

Twenty_Twenty_CLE_target_summary <-
  Twenty_Twenty_CLE_target_summary %>%
  filter(targets >= 15)

view(Twenty_Twenty_CLE_target_summary)

cpoe_epa_plot_CLE <- ggplot(Twenty_Twenty_CLE_target_summary, aes(x = avg_cpoe, y = avg_epa, colour = receiver_player_id, label = receiver_player_name, size = targets),show.legend = FALSE) +
  geom_text(show.legend = FALSE) +
  ggtitle("Receiver CPOE and EPA 2020 Season") +
  geom_hline(yintercept = mean(Twenty_Twenty_CLE_off$qb_epa[Twenty_Twenty_CLE_off$play_type=="pass"], na.rm=TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(Twenty_Twenty_CLE_off$cpoe[Twenty_Twenty_CLE_off$play_type=="pass"], na.rm=TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  xlab("Avg. CPOE") +
  ylab("Avg. EPA")


Twenty_Twenty_CLE_by_down <- Twenty_Twenty_CLE_off %>%
  filter(play_type == "pass" | play_type == "run") %>%
  group_by(posteam, down, play_type) %>%
  summarise(
    total_plays = n(),
    avg_yds_to_go = mean(ydstogo, na.rm=TRUE),
    total_yards_gained = sum(yards_gained, na.rm=TRUE),
    avg_yards_gained = mean(yards_gained, na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_gain_pct = mean((yards_gained/ydstogo), na.rm=TRUE)
  )


Twenty_Twenty_CLE_drive_summary <- Twenty_Twenty_CLE_off %>% 
  filter(play_type != "kickoff") %>%
  group_by(game_id, posteam, drive) %>%
  summarise(
    first_play = min(play_id),
    last_play = max(play_id),
    total_plays = n(),
    total_passes = sum(pass_attempt, na.rm=TRUE),
    total_rushes = sum(rush_attempt, na.rm=TRUE),
    total_successful_rushes = sum(Successful_Run, na.rm=TRUE),
    total_yards_gained = sum(yards_gained, na.rm=TRUE),
    total_pass_yards_gained = sum(yards_gained[play_type=="pass"], na.rm=TRUE),
    total_rush_yards_gained = sum(yards_gained[play_type=="run"], na.rm=TRUE),
    total_penalty_yards = sum(penalty_yards, na.rm=TRUE),
    avg_yards_gained = mean(yards_gained, na.rm=TRUE),
    avg_yards_gained_first_down = mean(yards_gained[down == 1], na.rm=TRUE),
    avg_pass_yards_gained = sum(yards_gained[play_type=="pass"], na.rm=TRUE)/sum(pass_attempt, na.rm=TRUE),
    avg_rush_yards_gained = sum(yards_gained[play_type=="run"], na.rm=TRUE)/sum(rush_attempt, na.rm=TRUE),
    total_wpa = sum(wpa, na.rm=TRUE),
    total_pass_wpa = sum(wpa[play_type=="pass"], na.rm=TRUE),
    total_run_wpa = sum(wpa[play_type=="run"], na.rm=TRUE),
    avg_wpa = mean(wpa, na.rm=TRUE),
    avg_pass_wpa = mean(wpa[play_type=="pass"], na.rm=TRUE),
    avg_run_wpa = mean(wpa[play_type=="run"], na.rm=TRUE),
    total_epa = sum(epa, na.rm=TRUE),
    total_pass_epa = sum(epa[play_type=="pass"], na.rm=TRUE),
    total_rush_epa = sum(epa[play_type=="run"], na.rm=TRUE),
    avg_epa = mean(epa, na.rm=TRUE),
    avg_pass_epa = mean(epa[play_type=="pass"], na.rm=TRUE),
    avg_run_epa = mean(epa[play_type=="run"], na.rm=TRUE),
    touchdown = max(touchdown, na.rm=TRUE),
    field_goal_attempt = max(field_goal_attempt, na.rm=TRUE),
    fumble_lost = max(fumble_lost),
    interception = max(interception)
  )

Twenty_Twenty_CLE_pbp_def <- Twenty_Twenty_CLE_pbp %>%
  filter(defteam == "CLE")


#Create dataframe of defensive plays only
defense_pbp <- Twenty_Twenty_CLE_pbp_def %>%
  filter(play_type != "punt" & play_type != "kickoff" & !is.na(play_type), penalty != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa, lateral_sack_player_id, lateral_sack_player_name, interception_player_id, interception_player_name, lateral_interception_player_id, lateral_interception_player_name, blocked_player_id, blocked_player_name, qb_hit_1_player_id, qb_hit_1_player_name, qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, forced_fumble_player_2_player_id, forced_fumble_player_2_player_name, solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle_2_player_id, solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_4_player_name, pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id, pass_defense_2_player_name, fumble_recovery_1_player_id, fumble_recovery_1_player_name, fumble_recovery_1_team, fumble_recovery_2_player_id, fumble_recovery_2_player_name, interception, solo_tackle_1_team, solo_tackle_2_team, assist_tackle_1_team, assist_tackle_2_team, assist_tackle_3_team, assist_tackle_4_team, fumble_recovery_1_team, fumble_recovery_2_team, forced_fumble_player_1_team, forced_fumble_player_2_team, fumble_lost)

#Create subdataframes for different types of plays made
interceptions_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(interception_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = interception_player_id, player_name = interception_player_name)

blocked_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(blocked_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = blocked_player_id, player_name = blocked_player_name)

qb_hit_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(qb_hit_1_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_1_player_id, player_name = qb_hit_1_player_name)

qb_hit_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(qb_hit_2_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_2_player_id, player_name = qb_hit_2_player_name)

forced_fumble_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_1_player_id), forced_fumble_player_1_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_1_player_id, player_name = forced_fumble_player_1_player_name)

forced_fumble_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_2_player_id), forced_fumble_player_2_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_2_player_id, player_name = forced_fumble_player_2_player_name)

solo_tackle_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(solo_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)

solo_tackle_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(solo_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)

assist_tackle_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)

assist_tackle_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)

pass_defense_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(pass_defense_1_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)

pass_defense_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(pass_defense_2_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)

fumble_recovery_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(fumble_recovery_1_player_id), fumble_recovery_1_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)

fumble_recovery_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(fumble_recovery_2_player_id), fumble_recovery_2_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)

#MERGE SUBDATAFRAMES including duplicates
defense_df_merged <- rbind(interceptions_pbp, 
                           blocked_pbp, 
                           qb_hit_1_pbp, qb_hit_2_pbp, 
                           forced_fumble_1_pbp, forced_fumble_2_pbp,
                           solo_tackle_1_pbp, solo_tackle_2_pbp,
                           assist_tackle_1_pbp, assist_tackle_2_pbp,
                           pass_defense_1_pbp, pass_defense_2_pbp,
                           fumble_recovery_1_pbp, fumble_recovery_2_pbp
)

#remove duplicate plays for individual players-same play can count for multiple players
defense_df_merged <- defense_df_merged %>%
  distinct(play_id, player_id, .keep_all = TRUE)

#Summarize plays made, playmaking EPA and run vs. pass breakdowns
Twenty_Twenty_CLE_def_playmaking_epa <- defense_df_merged %>%
  filter(epa < 0) %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"])), 
    playmaking_wpa = abs(sum(wpa[!is.na(wpa)]))
  )

#Summarize negative_plays_made made, playmaking EPA and run vs. pass breakdowns
Twenty_Twenty_CLE_neg_def_playmaking_epa <- defense_df_merged %>%
  filter(epa > 0) %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    negative_plays_made = length(epa), 
    negative_epa = abs(sum(epa)),
    pass_negative_epa = abs(sum(epa[play_type == "pass"])),
    rush_negative_epa = abs(sum(epa[play_type == "run"])), 
    negative_wpa = abs(sum(wpa[!is.na(wpa)]))
  )


rm(interceptions_pbp) 
rm(blocked_pbp) 
rm(qb_hit_1_pbp) 
rm(qb_hit_2_pbp) 
rm(forced_fumble_1_pbp) 
rm(forced_fumble_2_pbp)
rm(solo_tackle_1_pbp) 
rm(solo_tackle_2_pbp)
rm(assist_tackle_1_pbp) 
rm(assist_tackle_2_pbp)
rm(pass_defense_1_pbp) 
rm(pass_defense_2_pbp)
rm(fumble_recovery_1_pbp) 
rm(fumble_recovery_2_pbp)
