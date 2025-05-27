library(nhlCollection)
library(RCurl)
library(jsonlite)
library(dplyr)
library(gbm)
library(tidyr)
library(keras)
library(purrr)
library(mgcv)
library(glmnet)
library(fuzzyjoin)
library(brms)
library(glue)
process_player_ids <- function(shifts) {
  player_cols <- shifts %>% select(starts_with("player_")) %>% colnames()
  player_ids <- gsub("player_", "", player_cols)
  player_ids <- as.numeric(player_ids)  # ensure numeric IDs
  
  player_matrix <- as.matrix(shifts %>% select(all_of(player_cols)))
  max_players <- max(rowSums(player_matrix))
  
  on_ice_ids <- apply(player_matrix, 1, function(row) {
    ids <- player_ids[which(row == 1)]
    ids <- ids[1:min(length(ids), max_players)]
    if (length(ids) < max_players) {
      ids <- c(ids, rep(NA_real_, max_players - length(ids)))  # numeric NA
    }
    ids
  })
  
  on_ice_df <- as.data.frame(t(on_ice_ids))
  colnames(on_ice_df) <- paste0("player", 1:max_players)
  
  return(on_ice_df)
}


collect_season_shifts_data <- function(season1, season2 = 20242025) {
  # Fetch game metadata
  response <- RCurl::getURL("https://api.nhle.com/stats/rest/en/game")
  games_data <- jsonlite::fromJSON(response)$data
  
  # Filter for regular season games within range
  games <- games_data %>%
    filter(season >= season1, season <= season2, gameType == 2, gameStateId == 7) %>%
    pull(id)
  
  if (length(games) == 0) {
    stop("No games found for the specified seasons.")
  }
  
  # Load models once
  xg_nnet <- load_model_tf("models/xg_nnet.keras")
  xg_rebound <- load_model_tf("models/xg_rebound.keras")
  
  # Initialize containers
  all_for_shifts <- NULL
  all_against_shifts <- NULL
  all_summaries <- NULL
  all_shift_features <- NULL
  
  # Process the first game to initialize containers
  first_game_id <- games[1]
  season_year_first <- as.numeric(substr(first_game_id, 1, 4))
  
  message(glue("Initializing containers with the first game: {first_game_id}"))
  
  first_game_result <- tryCatch({
    get_game_summary(first_game_id, xg_nnet, xg_rebound)
  }, error = function(e) {
    stop(glue("⚠️ Error fetching the first game {first_game_id}: {e$message}"))
  })
  
  if (!is.null(first_game_result)) {
    home_shifts_first <- first_game_result$home_shifts
    away_shifts_first <- first_game_result$away_shifts
    
    all_for_shifts <- process_player_ids(combine_shifts(home_shifts_first, away_shifts_first))
    all_against_shifts <- process_player_ids(combine_shifts(away_shifts_first, home_shifts_first))
    all_summaries <- first_game_result$data
    
    home_shift_features_first <- home_shifts_first %>% select(-starts_with("player_"))
    away_shift_features_first <- away_shifts_first %>% select(-starts_with("player_"))
    all_shift_features <- rbind(home_shift_features_first, away_shift_features_first)
    all_shift_features$season <- season_year_first %>% as.numeric()
  } else {
    stop(glue("🚫 Error processing the first game {first_game_id}"))
  }
  
  # Loop through the rest of the games
  if (length(games) > 1) {
    for (i in 2:length(games)) {
      game_id <- games[i]
      season_year <- as.numeric(substr(game_id, 1, 4))
      
      game_result <- tryCatch({
        get_game_summary(game_id, xg_nnet, xg_rebound)
      }, error = function(e) {
        message(glue("⚠️ Error fetching game {game_id}: {e$message}"))
        return(NULL)
      })
      
      if (!is.null(game_result)) {
        home_shifts <- game_result$home_shifts
        away_shifts <- game_result$away_shifts
        
        for_shifts <- process_player_ids(combine_shifts(home_shifts, away_shifts))
        against_shifts <- process_player_ids(combine_shifts(away_shifts, home_shifts))
        summaries_data <- game_result$data # Assign to a temporary variable
        
     
        
        home_shift_features <- home_shifts %>% select(-starts_with("player_"))
        away_shift_features <- away_shifts %>% select(-starts_with("player_"))
        current_shift_features <- rbind(home_shift_features, away_shift_features)
        
        # Ensure the season column exists
        if (!"season" %in% colnames(current_shift_features)) {
          current_shift_features$season <- season_year
        }
        # Combine with existing data
        all_for_shifts <- combine_shifts(all_for_shifts, for_shifts)
        all_against_shifts <- combine_shifts(all_against_shifts, against_shifts)
        all_summaries <- combine_shifts(all_summaries, summaries_data) # Use the temp variable
        all_shift_features <- combine_shifts(all_shift_features, current_shift_features)
        home_shift_features <- home_shifts %>% select(-starts_with("player_"))

      } else {
        message(glue("🚫 Skipping game {game_id}"))
      }
      
      message(glue("✅ Processed game {i} of {length(games)}"))
    }
  } else {
    message("Only one game found, no further processing needed.")
  }
  
  return(list(
    data = all_summaries,
    shifts_for = all_for_shifts,
    shifts_against = all_against_shifts,
    shift_features = all_shift_features
  ))
}


season <- collect_season_shifts_data(20212022)

data <- season$data
shifts_for <- season$shifts_for[,1:18]
shifts_against <- season$shifts_against[,1:18]
shifts_features <- season$shift_features
 rm(season)
