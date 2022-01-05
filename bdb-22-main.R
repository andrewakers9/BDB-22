
library(lubridate) #1.7.9.2
library(xgboost) #1.2.0.1
library(SHAPforxgboost) #0.0.4
library(keras) #2.4.0
library(themis) #0.1.4
library(tidymodels) #0.1.3
library(tidyverse) #1.3.0

#setwd("C:\\Users\\andre\\OneDrive\\Desktop\\BDB-22")

###############################################################################

##helper functions
deg_2_rad <- function(deg) {
  rad <- deg * pi /180
  return(rad)
}

angle_rotate <- function(a, rotation) {
  a <- a + rotation
  a <- ifelse(a < 0, a + 360,
              ifelse(a > 360, a - 360, a))
  return(a)
}

angle_diff <- function(a, b) {
  a <- a - b
  a <- (a + 180) %% 360 - 180
  return(a)
}

read_tracking_data <- function(yr) {
  file_name <- paste0("tracking", yr, ".csv")
  df <- read_csv(file_name, col_types = cols())
  return(df)
}

get_air_yards <- function(play_df) {
  
  if(!("punt_land" %in% play_df$event)) {
    return(NA_real_)
  }
  
  los_x <- play_df %>% 
    filter(display_name == "football") %>% 
    slice(1) %>%
    pull(x)
  
  punt_land_x <- play_df %>%
    filter(event == "punt_land", display_name == "football") %>%
    slice(1) %>%
    pull(x)
  
  return(as.integer(punt_land_x - los_x))
  
}

get_punt_end_frame <- function(kick_contact, 
                               hang_time,
                               op_time,
                               play_df) {
  
  if(kick_contact %in% c("DEZ", "OOB")) return(NA)
  if(is.na(hang_time)) return(NA)
  
  if("punt" %in% play_df$event) {
    
    snap_frame_id <- play_df %>%
      filter(event == "punt") %>%
      dplyr::slice(1) %>% 
      pull(frame_id)
    
    return(round(hang_time * 10) + snap_frame_id)
    
  } else if("ball_snap" %in% play_df$event) {
    
    snap_frame_id <- play_df %>%
      filter(event == "ball_snap") %>%
      dplyr::slice(1) %>% 
      pull(frame_id)
    
    return(round((hang_time + op_time) * 10) + snap_frame_id)
    
  }
  
}

get_cov_feats <- function(punt_team,
                          punt_end_frame,
                          play_df) {
  
  if(max(play_df$frame_id) < punt_end_frame) {
    if("punt_land" %in% play_df$event) {
      punt_end_frame <- play_df %>%
        filter(
          display_name == "football",
          event == "punt_land"
        ) %>%
        pull(frame_id)
    } else if("punt_received" %in% play_df$event) {
      punt_end_frame <- play_df %>%
        filter(
          display_name == "football",
          event == "punt_received"
        ) %>%
        pull(frame_id)
    } else if("fair_catch" %in% play_df$event) {
      punt_end_frame <- play_df %>%
        filter(
          display_name == "football",
          event == "fair_catch"
        ) %>%
        pull(frame_id)
    } else {
      df <- data.frame(
        dist_1 = NA,
        dist_2 = NA,
        dist_3 = NA,
        dist_4 = NA,
        #rel_x_1 = NA,
        #rel_x_2 = NA,
        #rel_x_3 = NA,
        #rel_x_4 = NA,
        #rel_y_1 = NA,
        #rel_y_2 = NA,
        #rel_y_3 = NA,
        #rel_y_4 = NA,
        speed_x_1 = NA,
        speed_x_2 = NA,
        speed_x_3 = NA,
        speed_x_4 = NA,
        speed_y_1 = NA,
        speed_y_2 = NA,
        speed_y_3 = NA,
        speed_y_4 = NA,
        a_1 = NA,
        a_2 = NA,
        a_3 = NA,
        a_4 = NA,
        yards_from_goal = NA,
        yards_from_side = NA
      )
      return(df)
    }
  }
  
  fb_coords <- play_df %>%
    filter(
      display_name == "football",
      frame_id == punt_end_frame
    ) %>%
    select(x, y) %>%
    unlist()
  
  yards_from_goal <- 110 - fb_coords[1]
  yards_from_side <- min(53.3 - fb_coords[2], fb_coords[2])
  
  df <- play_df %>%
    filter(
      team == punt_team,
      frame_id == punt_end_frame
    ) %>%
    mutate(
      rel_x = x - fb_coords[1],
      rel_y = y - fb_coords[2],
      dist = sqrt(rel_x^2 + rel_y^2),
      rel_angle = atan2(-rel_x, -rel_y),
      rel_angle = if_else(rel_angle < 0, rel_angle + 2*pi, rel_angle) * (180/pi),
      rel_angle = angle_diff(dir, rel_angle),
      speed_x = cos(deg_2_rad(rel_angle)) * s,
      speed_y = sin(deg_2_rad(rel_angle)) * s,
    ) %>%
    arrange(dist) %>%
    mutate(.row = row_number()) %>%
    dplyr::slice(1:4) %>%
    select(
      .row,
      dist,
      #rel_x,
      #rel_y,
      speed_x,
      speed_y,
      a
    ) %>%
    pivot_wider(
      names_from = .row,
      #values_from = c(rel_x, rel_y, speed_x, speed_y, a)
      values_from = c(dist, speed_x, speed_y, a)
    ) %>%
    mutate(
      yards_from_goal = yards_from_goal,
      yards_from_side = yards_from_side
    )
  
  return(df)
  
}

get_returner_id <- function(play_df) {
  
  if(!("ball_snap" %in% play_df$event)) {
    
    returner_id <- play_df %>%
      filter(frame_id == 11) %>%
      filter(x == max(x)) %>% 
      pull(nfl_id)
    
  } else {
    
    returner_id <- play_df %>%
      filter(event == "ball_snap") %>%
      filter(x == max(x)) %>%
      pull(nfl_id)
    
  }
  
  return(returner_id)
  
}

get_returner_feats <- function(punt_end_frame, 
                               play_df) {
  
  if(!("ball_snap" %in% play_df$event)) {
    
    returner_id <- play_df %>%
      filter(frame_id == 11) %>%
      filter(x == max(x)) %>% 
      pull(nfl_id)
    
    returner_start_coords <- play_df %>%
      filter(frame_id == 11) %>%
      filter(x == max(x)) %>%
      select(x, y) %>%
      unlist()
    
  } else {
    
    returner_id <- play_df %>%
      filter(event == "ball_snap") %>%
      filter(x == max(x)) %>%
      pull(nfl_id)
    
    returner_start_coords <- play_df %>%
      filter(event == "ball_snap") %>%
      filter(x == max(x)) %>%
      select(x, y) %>%
      unlist()
    
  }
  
  fb_coords <- play_df %>%
    filter(
      display_name == "football",
      frame_id == punt_end_frame
    ) %>%
    select(x, y) %>%
    unlist()
  
  returner_feats <- play_df %>%
    filter(
      frame_id == punt_end_frame,
      nfl_id == returner_id
    ) %>%
    mutate(
      ret_speed_x = -sin(deg_2_rad(dir)) * s,
      ret_speed_y = -cos(deg_2_rad(dir)) * s,
      ret_rel_x = returner_start_coords[1] - fb_coords[1],
      ret_rel_y = returner_start_coords[2] - fb_coords[2]
    ) %>%
    select(
      ret_speed_x,
      ret_speed_y,
      ret_rel_x,
      ret_rel_y
    )
  
  return(returner_feats)
  
}

get_cnn_feats <- function(punt_end_frame,
                          punt_team,
                          play_df,
                          mirror = FALSE) {
  
  if(mirror) {
    
    play_df <- play_df %>%
      mutate(
        y = 53.333 - y,
        dir = case_when(
          cos(deg_2_rad(angle_rotate(dir, 90))) - 0.001 < 0 ~ angle_rotate(dir, angle_diff(90, dir) * 2),
          TRUE ~ angle_rotate(dir, angle_diff(180, dir) * 2)
        )
      )
    
  }
  
  if(!("ball_snap" %in% play_df$event)) {
    
    ret_id <- play_df %>%
      filter(frame_id == 11) %>%
      filter(x == max(x)) %>% 
      pull(nfl_id)
    
  } else {
    
    ret_id <- play_df %>%
      filter(event == "ball_snap") %>%
      filter(x == max(x)) %>%
      pull(nfl_id)
    
  }
  
  punt_end_df <- play_df %>%
    filter(
      frame_id == punt_end_frame,
      display_name != "football"
    ) %>%
    mutate(
      speed_x = sin(deg_2_rad(dir)) * s,
      speed_y = cos(deg_2_rad(dir)) * s,
      team = if_else(punt_team == team, "punt", "receive")
    ) 
  
  ret_info <- punt_end_df %>%
    filter(nfl_id == ret_id) %>%
    select(x, y, speed_x, speed_y) %>%
    unlist()
  
  punt_end_df <- punt_end_df %>%
    filter(nfl_id != ret_id) %>%
    mutate(
      dist = sqrt((x - ret_info[1])^2 + (y - ret_info[2])^2)
    ) %>%
    #arrange(desc(team), dist) %>%
    arrange(desc(team)) %>%
    mutate(id = row_number()) #%>%
    #with_groups(team, ~ filter(.x, row_number() <= 6))
  
  punt_team_df <- punt_end_df %>%
    filter(team == "punt") %>%
    mutate(
      rel_x = x - ret_info[1],
      rel_y = y - ret_info[2],
      rel_angle = atan2(-rel_x, -rel_y),
      rel_angle = if_else(rel_angle < 0, rel_angle + 2*pi, rel_angle) * (180/pi),
      rel_angle = angle_diff(dir, rel_angle),
      speed_x = cos(deg_2_rad(rel_angle)) * s,
      speed_y = sin(deg_2_rad(rel_angle)) * s,
      rel_speed_x = speed_x - ret_info[3],
      rel_speed_y = speed_y - ret_info[4]
    )
  
  comb <- punt_end_df %>%
    pull(id) %>%
    combn(2) %>%
    t() %>%
    as.data.frame() %>%
    rename(id1 = V1, id2 = V2)
  
  comb <- comb %>%
    left_join(
      punt_end_df %>%
        select(id, team, x, y, speed_x, speed_y),
      by = c("id1" = "id")
    ) %>%
    left_join(
      punt_end_df %>%
        select(id, team, x, y, speed_x, speed_y),
      by = c("id2" = "id"),
      suffix = c("1", "2")
    ) %>%
    filter(
      team1 == "receive",
      team2 == "punt"
    ) %>%
    mutate(
      x = x1 - x2,
      y = y1 - y2,
      speed_x = speed_x1 - speed_x2,
      speed_y = speed_y1 - speed_y2
    ) %>%
    select(
      id1, id2,
      x, y,
      speed_x, speed_y
    )
  
  punt_s_x <- rep(punt_team_df$speed_x, each = 10) 
  punt_s_y <- rep(punt_team_df$speed_y, each = 10)
  punt_rel_s_x <- rep(punt_team_df$rel_speed_x, each = 10) 
  punt_rel_s_y <- rep(punt_team_df$rel_speed_y, each = 10)
  punt_rel_x <- rep(punt_team_df$rel_x, each = 10)
  punt_rel_y <- rep(punt_team_df$rel_y, each = 10)
  
  get_feat_vec <- function(df, feat) {
    
    vec <- df %>%
      select(id1, id2, !!sym(feat)) %>%
      pivot_wider(
        names_from = id2,
        values_from = !!sym(feat) 
      ) %>%
      select(-id1) %>%
      as.matrix() %>%
      as.numeric()
    
    return(vec)
    
  }
  
  pair_x <- get_feat_vec(comb, "x")
  pair_y <- get_feat_vec(comb, "y")
  pair_s_x <- get_feat_vec(comb, "speed_x")
  pair_s_y <- get_feat_vec(comb, "speed_y")
  
  feat_arr <- array(
    c(punt_s_x,
      punt_s_y,
      punt_rel_x,
      punt_rel_y,
      punt_rel_s_x,
      punt_rel_s_y,
      pair_x,
      pair_y,
      pair_s_x,
      pair_s_y),
    dim = c(10, 11, 10)
    #dim = c(6, 6, 10)
  )
  
  return(feat_arr)
  
}

train_wflow <- function(features,
                        rec,
                        model,
                        param_grid,
                        m_set = NULL,
                        seed = 2021) {
  
  set.seed(seed)
  
  if(is.null(m_set)) {
    m_set <- metric_set(rmse)
  }
  
  y_var <- rec$var_info %>%
    filter(role == "outcome") %>%
    pull(variable)
  
  if(model$mode == "regression") {
  
    cv_folds <- vfold_cv(features,
                         v = 5,
                         strata = !!sym(y_var),
                         breaks = 4)
    
  } else {
    
    cv_folds <- vfold_cv(features,
                         v = 5,
                         strata = !!sym(y_var))
    
  }
  
  wflow <- workflow() %>%
    add_model(model) %>%
    add_recipe(rec)
  
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  
  cv_tune <- wflow %>%
    tune_grid(
      cv_folds,
      grid = param_grid,
      metrics = m_set
    )
  
  parallel::stopCluster(cl)
  
  print(show_best(cv_tune))
  
  best_tune <- show_best(cv_tune) %>%
    select(all_of(colnames(param_grid))) %>%
    slice(1)
  
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  
  cv_tune <- wflow %>%
    tune_grid(
      cv_folds,
      grid = best_tune,
      metrics = m_set,
      control = control_grid(save_pred = TRUE)
    )
  
  parallel::stopCluster(cl)
  
  cv_preds <- cv_tune %>%
    collect_predictions(parameters = best_tune)
  
  if(model$mode == "regression") {
  
    features <- features %>%
      mutate(.row = row_number()) %>%
      left_join(
        cv_preds %>%
          select(.row, .pred),
        by = ".row"
      )
    
  } else {
    
    features <- features %>%
      mutate(.row = row_number()) %>%
      left_join(
        cv_preds %>%
          select(.row, .pred_class),
        by = ".row"
      )
    
  }
  
  train_wflow <- wflow %>%
    finalize_workflow(parameters = best_tune)
  
  train_wflow <- train_wflow %>%
    fit(features)
  
  ret_list <- list(
    features = features,
    wflow = train_wflow
  )
  
  return(ret_list)
  
}


###############################################################################

##read data
games_df <- read_csv("games.csv", col_types = cols()) %>%
  janitor::clean_names()

pff_df <- read_csv("PFFScoutingData.csv", col_types = cols()) %>%
  janitor::clean_names()

players_df <- read_csv("players.csv", col_types = cols()) %>%
  janitor::clean_names()

plays_df <- read_csv("plays.csv", col_types = cols()) %>%
  janitor::clean_names()

tracking_df <- map_df(2018:2020, read_tracking_data) %>%
  janitor::clean_names()

stadium_df <- read_csv("stadium_coordinates.csv", col_types = cols()) %>%
  janitor::clean_names()

games_weather_df <- read_csv("games_weather.csv", col_types = cols()) %>%
  janitor::clean_names()

##join stadium name to games_df
games_df <- games_df %>%
  left_join(
    games_weather_df %>% select(game_id, stadium_name),
    by = "game_id"
  )

##join stadium details to games_df
games_df <- games_df %>%
  left_join(
    stadium_df %>% 
      distinct(stadium_name, .keep_all = TRUE) %>%
      select(-home_team),
    by = "stadium_name"
  )

##read in weather related data
weather_df <- read_csv("weather.csv", col_types = cols()) %>%
  janitor::clean_names()

##filter for only relevant games
weather_df <- weather_df %>%
  semi_join(games_df, by = "game_id") %>%
  inner_join(
    games_weather_df %>% select(game_id, tz_offset),
    by = "game_id"
  )

##filter weather for only the closest station and convert all times to UTC
##rotate wind direction 180 so that it is in direction of wind, not where it originates
weather_df <- weather_df %>%
  arrange(game_id, distance_to_station) %>%
  group_by(game_id, distance_to_station) %>%
  mutate(game_station_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(game_id) %>%
  filter(game_station_id == min(game_station_id)) %>%
  ungroup() %>%
  mutate(
    time_measure = mdy_hm(time_measure) - hours(tz_offset),
    wind_direction = angle_rotate(wind_direction, 180)
  )

##filter data for only punts; remove fakes 
plays_df <- plays_df %>%
  filter(
    special_teams_play_type == "Punt",
    special_teams_result != "Non-Special Teams Result",
    special_teams_result != "Blocked Punt",
    !(str_detect(play_description, "block|deflect")),
    !(special_teams_result == "Muffed" & is.na(kick_return_yardage))
  ) %>%
  mutate(
    yards_to_goal = if_else(possession_team == yardline_side, 
                            100 - yardline_number,
                            yardline_number, 
                            missing = 50),
    game_clock = ms(str_match(game_clock, "\\d+:\\d{2}")),
    time_remaining = if_else(
      quarter %in% c(1, 3),
      as.numeric(game_clock) / 60 + 15,
      as.numeric(game_clock) / 60
    )  
  ) %>%
  select(
    -c(kick_blocker_id,
       penalty_codes,
       penalty_jersey_numbers,
       pass_result)
  ) %>%
  arrange(game_id, play_id)

##filter for only punt plays
pff_df <- pff_df %>%
  semi_join(
    plays_df,
    by = c("game_id", "play_id")
  ) %>%
  select(
    game_id,
    play_id,
    snap_detail,
    snap_time,
    operation_time,
    hang_time,
    kick_direction_intended,
    kick_direction_actual,
    return_direction_intended,
    return_direction_actual,
    gunners,
    punt_rushers,
    vises,
    kick_contact_type
  )

##add info to pff data
pff_df <- pff_df %>%
  mutate(
    snap_detail = case_when(
      snap_detail %in% c("<", ">") ~ "wide",
      snap_detail %in% c("H", "L") ~ "hl",
      TRUE ~ "ok"
    ),
    kick_dir_int_num = case_when(
      kick_direction_intended == "L" ~ 1L,
      kick_direction_intended == "C" ~ 2L,
      kick_direction_intended == "R" ~ 3L,
      TRUE ~ NA_integer_
    ),
    kick_dir_actual_num = case_when(
      kick_direction_actual == "L" ~ 1L,
      kick_direction_actual == "C" ~ 2L,
      kick_direction_actual == "R" ~ 3L,
      TRUE ~ NA_integer_
    ),
    kick_dir_diff = abs(kick_dir_int_num - kick_dir_actual_num),
    num_gunners = coalesce(str_count(gunners, ";") + 1, 0),
    num_rushers = coalesce(str_count(punt_rushers, ";") + 1, 0),
    num_vises = coalesce(str_count(vises, ";") + 1, 0)
  ) %>%
  select(-c(gunners, punt_rushers, vises))

##join pff_df to plays_df
plays_df <- plays_df %>%
  left_join(
    pff_df,
    by = c("game_id", "play_id")
  )

##filter tracking data for only punt plays 
tracking_df <- tracking_df %>%
  semi_join(
    plays_df,
    by = c("game_id", "play_id")
  )

##extract play direction and play time from tracking data 
play_details_df <- tracking_df %>%
  select(game_id, play_id, play_direction, play_time = time) %>%
  distinct(game_id, play_id, .keep_all = TRUE)

##join play direction and time of play to plays df
plays_df <- plays_df %>%
  left_join(play_details_df, by = c("game_id", "play_id"))

##join columns from games_df to plays_df; add column for score from punting team's perspective
plays_df <- plays_df %>%
  left_join(
    games_df %>% select(game_id, 
                        home_team = home_team_abbr,
                        stadium_angle = stadium_azimuth_angle_adj,
                        roof_type,
                        altitude,
                        artificial_turf),
    by = "game_id"
  ) %>%
  mutate(
    game_score = if_else(
      possession_team == home_team,
      pre_snap_home_score - pre_snap_visitor_score,
      pre_snap_visitor_score - pre_snap_home_score
    )
  )

##add net return yards where appropriate and subtract touchbacks from kick length
plays_df <- plays_df %>%
  mutate(
    kick_return_yardage = case_when(
      special_teams_result %in% c("Downed", 
                                  "Fair Catch", 
                                  "Out of Bounds") ~ 0,
      special_teams_result == "Touchback" ~ 20,
      TRUE ~ kick_return_yardage
    )
  ) %>%
  filter(!is.na(kick_return_yardage))

##join weather data to plays df 
plays_df <- plays_df %>%
  mutate(
    play_time = round_date(play_time, unit = "hour")
  ) %>%
  left_join(
    weather_df %>% 
      select(game_id, time_measure, dew_point, humidity, temperature, 
             pressure, precipitation, wind_speed, wind_direction),
    by = c("game_id", "play_time" = "time_measure")
  ) %>%
  group_by(game_id) %>%
  fill(temperature:wind_direction, .direction = "downup") %>%
  ungroup()

##rotate stadium angle when play is going to away endzone and calculate wind direction relative to play
plays_df <- plays_df %>%
  mutate(
    stadium_angle = if_else(play_direction == "right", 
                            angle_rotate(stadium_angle, 180),
                            stadium_angle),
    wind_direction2 = angle_rotate(wind_direction, 360 - stadium_angle),
    wind_x = cos(deg_2_rad(wind_direction2)) * wind_speed,
    wind_y = sin(deg_2_rad(wind_direction2)) * wind_speed
  )

##standardize tracking data direction and coordinates
tracking_df <- tracking_df %>%
  mutate(
    x = if_else(play_direction == "left", 120 - x, x),
    y = if_else(play_direction == "left", 160/3 - y, y),
    o = case_when(
      play_direction == "left" & o > 180 ~ o - 180,
      play_direction == "left" & o < 180 ~ o + 180,
      TRUE ~ o
    ),
    dir = case_when(
      play_direction == "left" & dir > 180 ~ dir - 180,
      play_direction == "left" & dir < 180 ~ dir + 180,
      TRUE ~ dir
    )
  )

##nest tracking data for each unique game and play IDs
tracking_df <- tracking_df %>%
  group_by(game_id, play_id) %>%
  nest() %>%
  ungroup()

tracking_df <- tracking_df %>%
  arrange(game_id, play_id)

tracking_df <- tracking_df %>%
  mutate(
    punt_air_yards = map_dbl(data, get_air_yards),
    returner_id2 = map_dbl(data, get_returner_id)
  )

plays_df <- plays_df %>%
  left_join(
    tracking_df %>%
      select(game_id, play_id, punt_air_yards),
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    punt_air_yards = case_when(
      is.na(punt_air_yards) ~ kick_length,
      punt_air_yards < 10 ~ kick_length,
      kick_contact_type %in% c("DEZ", "OOB") ~ kick_length,
      TRUE ~ punt_air_yards
    ),
    kick_return_yardage = case_when(
      is.na(punt_air_yards) ~ kick_return_yardage,
      punt_air_yards < 10 ~ kick_return_yardage,
      kick_contact_type %in% c("DEZ", "OOB") ~ kick_return_yardage,
      TRUE ~ punt_air_yards - kick_length + kick_return_yardage
    )
  )

tracking_df <- tracking_df %>%
  select(-punt_air_yards)

################################################################################
##air yards modeling 

##xgb regression model specs
xgb_spec <- 
  boost_tree(learn_rate = 0.05,
             trees = tune(),
             mtry = tune(),
             loss_reduction = tune(),
             tree_depth = tune()) %>%
  set_engine("xgboost",
             lambda = tune(),
             alpha = tune()) %>%
  set_mode("regression")

##select features to predict punt air yards
air_yards_feats <- plays_df %>%
  select(
    game_id,
    play_id,
    yards_to_goal,
    game_score,
    time_remaining,
    num_gunners,
    num_rushers,
    num_vises,
    altitude,
    artificial_turf,
    temperature,
    precipitation,
    dew_point,
    humidity,
    pressure,
    wind_x,
    wind_y,
    punt_air_yards
  ) %>%
  mutate(wind_y = abs(wind_y))

##hyperparamter grid for tuning
air_yards_grid <- crossing(
  mtry = c(4, 6, 8), 
  trees = seq(40, 150, by = 10),
  loss_reduction = 4:6, 
  tree_depth = c(4, 6, 8),
  lambda = c(1, 10, 20), 
  alpha = c(1, 10, 20)
)

air_yards_rec <- 
  recipe(punt_air_yards ~ ., data = air_yards_feats) %>%
  step_rm(game_id, play_id) %>%
  step_impute_median(all_numeric_predictors())

kick_length_mod <- train_wflow(features = air_yards_feats,
                               rec = air_yards_rec,
                               model = xgb_spec,
                               param_grid = air_yards_grid,
                               seed = 2021)

#xgb.importance(model = kick_length_mod$wflow$fit$fit$fit)

air_yards_bake <- air_yards_rec %>%
  prep() %>%
  bake(new_data = air_yards_feats) %>%
  select(-punt_air_yards) %>%
  as.matrix()

air_yards_shap <- predict(
  kick_length_mod$wflow$fit$fit$fit,
  newdata = air_yards_bake,
  predcontrib = TRUE
)

shap.plot.summary(
  shap.prep(
    shap_contrib = as.data.frame(air_yards_shap[,-ncol(air_yards_shap)]), 
    X_train = as.data.frame(air_yards_bake),
    top_n = 10
  )
) +
  ggtitle("Expected punt distance: SHAP value distributions")

ggsave("punt_distance_shap.jpeg", device = "jpeg", dpi = 400)

###############################################################################

plays_df <- plays_df %>%
  mutate(
    punt_team = if_else(possession_team == home_team, "home", "away")
  )

tracking_df <- tracking_df %>%
  left_join(
    plays_df %>%
      select(
        game_id,
        play_id,
        punt_team,
        operation_time,
        hang_time,
        kick_contact_type
      ),
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    punt_end_frame = pmap_dbl(
      list(
        kick_contact = kick_contact_type,
        hang_time = hang_time,
        op_time = operation_time,
        play_df = data
      ),
      .f = get_punt_end_frame
    )
  )

cov_feats <- tracking_df %>%
  filter(!is.na(punt_end_frame)) %>%
  mutate(
    feats = pmap(
      list(
        punt_team = punt_team,
        punt_end_frame = punt_end_frame,
        play_df = data
      ),
      get_cov_feats
    ),
    ret_feats = map2(
      punt_end_frame,
      data,
      get_returner_feats
    )
  ) %>%
  select(game_id, play_id, feats, ret_feats) %>%
  unnest(c(feats, ret_feats))

cov_feats <- cov_feats %>%
  filter(!is.na(dist_1)) %>%
  left_join(
    plays_df %>%
      select(
        game_id,
        play_id,
        kick_contact_type,
        operation_time,
        hang_time,
        num_gunners,
        num_vises,
        num_rushers,
        punt_air_yards,
        kick_return_yardage
      ),
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    catch = if_else(
      kick_contact_type %in% c("CC", "CFFG", "BC", "BOG", "MBDR"), 1, 0
    ),
    total_time = operation_time + hang_time
  ) %>%
  select(-c(kick_contact_type, operation_time))

cov_feats1 <- cov_feats %>% filter(catch == 1) %>% select(-catch)
cov_feats2 <- cov_feats %>% filter(catch == 0) %>% select(-catch)

cov_rec1 <- 
  recipe(kick_return_yardage ~ ., data = cov_feats1) %>%
  step_rm(game_id, play_id, ret_rel_x, ret_rel_y,
          num_gunners, num_vises, num_rushers, punt_air_yards,
          total_time, hang_time)

cov_param_grid1 <- crossing(
  mtry = c(6, 8, 10), 
  trees = seq(30, 90, by = 2),
  loss_reduction = 2:5, 
  tree_depth = c(6, 8, 10),
  lambda = c(1, 10, 15, 20), 
  alpha = c(1, 10, 20)
)

cov_model1 <- train_wflow(features = cov_feats1,
                          rec = cov_rec1,
                          model = xgb_spec,
                          param_grid = cov_param_grid1,
                          seed = 2021)

xgb.importance(model = cov_model1$wflow$fit$fit$fit)

cov_bake1 <- cov_rec1 %>%
  prep() %>%
  bake(new_data = cov_feats1) %>%
  select(-kick_return_yardage) %>%
  as.matrix()

cov_shap <- predict(
  cov_model1$wflow$fit$fit$fit,
  newdata = cov_bake1,
  predcontrib = TRUE
)
  
shap.plot.summary(
  shap.prep(
    shap_contrib = as.data.frame(cov_shap[,-ncol(cov_shap)]), 
    X_train = as.data.frame(cov_bake1),
    top_n = 10
  )
) +
  ggtitle("Expected punt return based on coverage: SHAP value distributions")

ggsave("exp_ret_cov_shap.jpeg", device = "jpeg", dpi = 400)

###############################################################################

reticulate::use_condaenv(condaenv = "tf-keras")

cnn_feats <- tracking_df %>%
  filter(
    kick_contact_type %in% c("BC", "BOG", "CC", "CFFG", "MBDR")
  ) %>%
  mutate(
    feat_arr = pmap(
      list(
        punt_end_frame = punt_end_frame,
        punt_team = punt_team,
        play_df = data
      ),
      safely(get_cnn_feats)
    ),
    feat_arr_mirror = pmap(
      list(
        punt_end_frame = punt_end_frame,
        punt_team = punt_team,
        play_df = data,
        mirror = TRUE
      ),
      safely(get_cnn_feats)
    )
  ) %>%
  select(-data) 

cnn_feats <- cnn_feats %>% 
  left_join(
    cov_feats %>%
      select(
        game_id,
        play_id,
        ret_speed_x,
        ret_speed_y,
        yards_from_side,
        yards_from_goal
      ),
    by = c("game_id", "play_id")
  ) 

cnn_feat_filt <- map_lgl(cnn_feats$feat_arr, ~ !is.null(.x$error))

cnn_feats <- cnn_feats %>%
  filter(!cnn_feat_filt)

features <- array(unlist(append(cnn_feats$feat_arr, cnn_feats$feat_arr_mirror)), 
                  dim = c(10, 11, 10, length(cnn_feats$feat_arr) * 2))

features <- aperm(features, c(4, 2, 1, 3))

cnn_y <- cnn_feats %>%
  select(game_id, play_id) %>%
  left_join(
    plays_df %>%
      select(game_id, play_id, kick_return_yardage),
    by = c("game_id", "play_id")
  ) %>%
  pull(kick_return_yardage) %>%
  rep(2)

set.seed(2021)
cov_folds <- vfold_cv(cov_feats1,
                      v = 5,
                      strata = kick_return_yardage,
                      breaks = 5)

res <- numeric(5)
cnn_cv_preds <- data.frame()

for(i in 1:5) {
  
  val_idx <- setdiff(1:nrow(cnn_feats), cov_folds$splits[[i]]$in_id)
  train_idx <- setdiff(1:dim(features)[1], c(val_idx, val_idx + nrow(cnn_feats)))
  
  train_x <- features[train_idx,,,]
  val_x <- features[val_idx,,,]
  
  train_y <- cnn_y[train_idx]
  val_y <- cnn_y[val_idx]
  
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(128, kernel_size = c(1, 1), strides = c(1, 1), input_shape = c(11, 10, 10)) %>%
    layer_activation("relu") %>%
    layer_conv_2d(160, kernel_size = c(1, 1), strides = c(1, 1)) %>%
    layer_activation("relu") %>%
    layer_conv_2d(128, kernel_size = c(1, 1), strides = c(1, 1)) %>%
    layer_activation("relu") %>%
    layer_average_pooling_2d(pool_size = c(1, 10)) %>%
    layer_lambda(f = function(x) k_squeeze(x, axis = 3)) %>%
    #layer_reshape(c(11, 128)) %>%
    layer_conv_1d(128, kernel_size = 1, strides = 1) %>%
    layer_activation("relu") %>%
    layer_batch_normalization() %>%
    layer_conv_1d(128, kernel_size = 1, strides = 1) %>%
    layer_activation("relu") %>%
    layer_batch_normalization() %>%
    layer_conv_1d(96, kernel_size = 1, strides = 1) %>%
    layer_activation("relu") %>%
    layer_batch_normalization() %>%
    layer_average_pooling_1d(pool_size = 11) %>%
    layer_flatten() %>%
    layer_dense(96, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(0.4) %>%
    layer_dense(128, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(0.4) %>%
    layer_dense(1, activation = "relu")
  
  model %>% 
    compile(
      loss = "mean_squared_error",
      optimizer = optimizer_adam(lr = 0.00001)
    )
  
  keras_fit <- model %>%
    fit(
      x = train_x,
      y = train_y,
      validation_data = list(val_x, val_y),
      epochs = 50,
      batch_size = 32
    )
  
  pred <- predict(model, x = val_x)
  sq_error <- mean((pred - val_y)^2)
  
  res[i] <- sq_error
  
  df <- data.frame(
    .row = val_idx,
    .pred = pred
  )
  
  cnn_cv_preds <- cnn_cv_preds %>%
    rbind(df)
  
}

# cov_feats1 <- cov_model1$features %>%
#   left_join(
#     cnn_cv_preds %>%
#       rename(.pred_cnn = .pred),
#     by = ".row"
#   ) %>%
#   mutate(
#     .pred_ensemble = 0.5 * .pred + 0.5 * .pred_cnn
#   )


###############################################################################

cov_rec2 <- 
  recipe(kick_return_yardage ~ ., data = cov_feats2) %>%
  step_rm(game_id, play_id, ret_rel_x, ret_rel_y,
          num_gunners, num_vises, num_rushers, total_time)

cov_param_grid2 <- crossing(
  mtry = c(6, 8, 10), 
  trees = seq(40, 120, by = 5),
  loss_reduction = 3:6, 
  tree_depth = c(3, 4, 6),
  lambda = c(1, 10, 15, 20), 
  alpha = c(1, 10, 15)
)

cov_model2 <- train_wflow(features = cov_feats2,
                          rec = cov_rec2,
                          model = xgb_spec,
                          param_grid = cov_param_grid2,
                          seed = 2021)

#xgb.importance(model = cov_model2$wflow$fit$fit$fit)

cov_feats_all <- cov_model1$features %>% 
  rbind(cov_model2$features) %>%
  rename(cov_pred = .pred) %>%
  select(-.row)

###############################################################################

ret_rec <- 
  recipe(cov_pred ~ ., data = cov_feats_all) %>%
  step_rm(contains("1"), contains("2"), contains("3"), contains("4"),
          game_id, play_id, contains("ret_speed"), hang_time, kick_return_yardage)

ret_param_grid <- crossing(
  mtry = c(4, 6, 8), 
  trees = seq(60, 150, by = 10),
  loss_reduction = 2:5, 
  tree_depth = c(4, 6, 8),
  lambda = c(1, 10, 15, 20), 
  alpha = c(0.1, 1, 10)
)

ret_model <- train_wflow(features = cov_feats_all,
                         rec = ret_rec,
                         model = xgb_spec,
                         param_grid = ret_param_grid,
                         seed = 2021)

#xgb.importance(model = ret_model$wflow$fit$fit$fit)

ret_bake <- ret_rec %>%
  prep() %>%
  bake(new_data = cov_feats_all) %>%
  select(-cov_pred) %>%
  as.matrix()

ret_shap <- predict(
  ret_model$wflow$fit$fit$fit,
  newdata = ret_bake,
  predcontrib = TRUE
)

shap.plot.summary(
  shap.prep(
    shap_contrib = as.data.frame(ret_shap[,-ncol(ret_shap)]), 
    X_train = as.data.frame(ret_bake)
  )
) +
  ggtitle("Expected coverage based on punt: SHAP value distributions")

ggsave("exp_cov_punt.jpeg", device = "jpeg", dpi = 400)

cov_feats_all <- cov_feats_all %>%
  left_join(
    ret_model$features %>%
      select(game_id, play_id, ret_pred = .pred),
    by = c("game_id", "play_id")
  )

###############################################################################

catch_feats <- cov_feats %>%
  mutate(catch = factor(catch))

catch_rec <- 
  recipe(catch ~ ., data = catch_feats) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_rm(contains("1"), contains("2"), contains("3"), contains("4"),
          game_id, play_id, contains("ret_speed"), hang_time, kick_return_yardage) %>%
  step_smote(catch, over_ratio = 0.8)

catch_param_grid <- crossing(
  mtry = c(3, 4, 6, 8),
  trees = seq(60, 170, by = 10),
  loss_reduction = 1:4,
  tree_depth = c(4, 6, 8)
)

xgb_class_spec <- 
  boost_tree(learn_rate = 0.1,
             trees = tune(),
             mtry = tune(),
             loss_reduction = tune(),
             tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

catch_model <- train_wflow(features = catch_feats,
                           rec = catch_rec,
                           model = xgb_class_spec,
                           param_grid = catch_param_grid,
                           m_set = metric_set(accuracy),
                           seed = 2021)

cov_preds_catch <- cov_model2$features %>%
  select(-.pred) %>%
  bind_cols(
    predict(cov_model1$wflow, new_data = cov_model2$features)
  ) %>%
  select(game_id,
         play_id,
         cov_pred2 = .pred)

###############################################################################

final_df <- plays_df %>%
  left_join(
    tracking_df %>%
      select(game_id, play_id, returner_id2),
    by = c("game_id", "play_id")
  ) %>%
  select(
    game_id, 
    play_id, 
    kicker_id,
    returner_id2,
    possession_team,
    yards_to_goal,
    num_vises,
    special_teams_result, 
    kick_contact_type,
    punt_air_yards,
    kick_return_yardage
  ) %>%
  mutate(
    possession_team = if_else(
      possession_team %in% c("OAK", "LV"), "OAK/LV", possession_team)
  ) %>%
  left_join(
    cov_feats_all %>%
      select(
        game_id,
        play_id,
        cov_pred,
        ret_pred
      ),
    by = c("game_id", "play_id")
  ) %>%
  left_join(
    kick_length_mod$features %>%
      select(
        game_id,
        play_id,
        air_yard_pred = .pred
      ),
    by = c("game_id", "play_id")
  ) %>%
  left_join(
    catch_model$features %>%
      select(
        game_id,
        play_id,
        catch,
        catch_pred = .pred_class
      ),
    by = c("game_id", "play_id")
  )

final_df <- final_df %>%
  left_join(cov_preds_catch, by = c("game_id", "play_id"))

final_df <- final_df %>%
  mutate(
    cov_pred = case_when(
      kick_contact_type == "OOB" ~ 0,
      kick_contact_type == "DEZ" ~ 20,
      TRUE ~ cov_pred
    ),
    ret_pred = case_when(
      kick_contact_type == "OOB" ~ 0,
      kick_contact_type == "DEZ" ~ 20,
      TRUE ~ ret_pred
    ),
    catch = replace_na(catch, 0),
    catch_pred = replace_na(catch_pred, 0),
    cov_pred = if_else(
      catch == 0 & catch_pred == 1, cov_pred2, cov_pred
    )
  ) %>%
  filter(!is.na(cov_pred)) 

avg_kick_ret_yard <- mean(final_df$kick_return_yardage)
avg_cov_pred <- mean(final_df$cov_pred)
avg_ret_pred <- mean(final_df$ret_pred)

final_df <- final_df %>%
  mutate(
    npy = punt_air_yards - kick_return_yardage,
    npyg = (punt_air_yards - air_yard_pred) + (avg_kick_ret_yard - kick_return_yardage),
    npyg_kick = punt_air_yards - air_yard_pred,
    npyg_exp_cov = avg_ret_pred - ret_pred,
    npyg_cov = ret_pred - cov_pred + (avg_cov_pred - avg_ret_pred),
    npyg_ret = cov_pred - kick_return_yardage - (avg_cov_pred - avg_kick_ret_yard)
  )

final_df <- final_df %>%
  mutate(
    npyg_bounce = case_when(
      catch == 0 & catch_pred == 0 ~ npyg_ret,
      TRUE ~ 0
    ),
    npyg_ret = case_when(
      catch == 0 & catch_pred == 0 ~ 0,
      TRUE ~ npyg_ret
    ),
    punter_cont = npyg_kick + npyg_exp_cov + npyg_bounce,
    cov_cont = npyg_cov,
    ret_cont = npyg_ret
  )


final_df <- final_df %>%
  left_join(
    players_df %>%
      select(nfl_id, punter_name = display_name),
    by = c("kicker_id" = "nfl_id")
  ) %>%
  left_join(
    players_df %>%
      select(nfl_id, returner_name = display_name),
    by = c("returner_id2" = "nfl_id")
  )

punter_npyg <- final_df %>%
  with_groups(kicker_id, ~ filter(.x, n() >= 20)) %>%
  group_by(punter_name) %>%
  summarise(
    n = n(),
    avg_npyg_kick = mean(npyg_kick),
    avg_npyg_exp_cov = mean(npyg_exp_cov),
    avg_npyg_bounce = mean(npyg_bounce),
    avg_punter_cont = mean(punter_cont),
    sd_punter_cont = sd(punter_cont),
    perc_positive = mean(punter_cont > 0)
  ) %>%
  arrange(desc(avg_punter_cont))

cov_npyg <- final_df %>%
  group_by(possession_team) %>%
  summarise(
    n = n(),
    avg_npyg_cov = mean(npyg_cov),
    avg_npyg_ret = mean(npyg_ret),
    avg_cont = mean(npyg_cov + npyg_ret),
    med_npyg_ret = median(npyg_ret),
    big_rets = sum(npyg_ret < -30),
    bad_initial_cov = sum(npyg_cov < -5)
  ) %>%
  arrange(desc(avg_cont))

ret_npyg <- final_df %>%
  filter(
    !is.na(returner_name),
    !(catch == 0 & catch_pred == 0)
  ) %>%
  with_groups(returner_name, ~ filter(.x, n() >= 20)) %>%
  group_by(returner_name) %>%
  summarise(
    n = n(),
    avg_npyg_ret = mean(-npyg_ret),
    med_npyg_ret = median(-npyg_ret),
    catch_perc = sum(catch == 1 & catch_pred == 1) / sum(catch_pred == 1)
  ) %>%
  arrange(desc(avg_npyg_ret))

###############################################################################

punter_npyg %>%
  slice(1:10) %>%
  mutate(
    punter_name = if_else(
      punter_name == "Jake Bailey",
      paste0(punter_name, " (n = ", n, ")"),
      paste0(punter_name, " (", n, ")")
    ),
    punter_name = factor(punter_name),
    punter_name = fct_reorder(punter_name, avg_punter_cont)
  ) %>%
  select(
    punter_name, 
    Punt = avg_npyg_kick,
    `Exp cov` = avg_npyg_exp_cov,
    Bounce = avg_npyg_bounce
  ) %>%
  pivot_longer(
    cols = -punter_name,
    names_to = "NPYG Component",
    values_to = "npyg"
  ) %>%
  ggplot(
    aes(npyg, punter_name, fill = `NPYG Component`)
  ) +
  geom_bar(
    stat = "identity"
  ) + 
  ggtitle("Top 10 punters based on average NPYG (min 20 punts)") +
  xlab("Average NPYG") + 
  ylab("") + 
  theme_minimal()

ggsave("./Plots/top_punters.jpeg", device = "jpeg", dpi = 400)

final_df %>%
  mutate(
    punter_name = if_else(punter_name == "Jake Bailey", "Jake Bailey", "Other"),
    npyg_kick = npyg_kick + npyg_bounce
  ) %>%
  select(punter_name, punter_cont, npyg_kick, npyg_exp_cov) %>%
  pivot_longer(
    cols = -punter_name,
    names_to = "component",
    values_to = "npyg"
  ) %>%
  ggplot(aes(npyg, component, fill = punter_name)) +
  ggridges::geom_density_ridges(alpha = 0.4, scale = 1.5) +
  #geom_density(alpha = 0.3) + 
  #facet_wrap(~ component) + 
  scale_x_continuous(
    limits = c(-20, 20), 
    expand = c(0, 0),
    name = "NPYG"
  ) +
  scale_y_discrete(
    expand = expansion(mult = c(0.01, .7)),
    labels = c("Exp. coverage", "Punt", "Total punter contribution"),
    name = ""
  ) +
  scale_fill_discrete(name = "", labels = c("Jake Bailey", "All")) +
  ggtitle("Distribution of NPYG: Jake Bailey vs. All") +
  ggridges::theme_ridges() +
  theme(
    plot.title = element_text(hjust = 1)
  )

ggsave("./Plots/npyg_ridges.jpeg", device = "jpeg", dpi = 400)

ret_npyg %>%
  slice(1:10) %>%
  mutate(
    returner_name = if_else(
      returner_name == "Nyheim Hines", 
      paste0(returner_name, " (n = ", n, ")"),
      paste0(returner_name, " (", n, ")")
    ),
    returner_name = factor(returner_name),
    returner_name = fct_reorder(returner_name, avg_npyg_ret)
  ) %>%
  ggplot(aes(avg_npyg_ret, returner_name)) +
  geom_bar(stat = "identity") +
  ylab("") +
  xlab("Average NPYG") +
  theme_minimal() +
  ggtitle("Top 10 returners based on average NPYG (min 20 returns)")

ggsave("./Plots/top_returners.jpeg", device = "jpeg", dpi = 400)

cov_npyg %>%
  slice(1:10) %>%
  mutate(
    possession_team = if_else(
      possession_team == "JAX", 
      paste0(possession_team, " (n = ", n, ")"),
      paste0(possession_team, " (", n, ")")
    ),
    possession_team = factor(possession_team),
    possession_team = fct_reorder(possession_team, avg_cont)
  ) %>%
  select(possession_team, avg_npyg_cov, avg_npyg_ret) %>%
  pivot_longer(
    cols = -possession_team,
    names_to = "NPYG Component",
    values_to = "Average NPYG"
  ) %>%
  ggplot(aes(`Average NPYG`, possession_team, fill = `NPYG Component`)) +
  geom_bar(stat = "identity") +
  ylab("") +
  scale_fill_discrete(labels = c("Coverage", "Return")) +
  ggtitle("Top 10 punt coverage units based on average NPYG") +
  theme_minimal()

ggsave("./Plots/top_cov_units.jpeg", device = "jpeg", dpi = 400)

