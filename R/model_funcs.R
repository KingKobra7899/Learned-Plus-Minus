#' @export
create_formula <- function(df, output_var) {
  numeric_columns <- sapply(df, is.numeric)
  numeric_columns <- setdiff(names(df)[numeric_columns], output_var)
  formula <- stats::as.formula(paste(output_var, "~", paste(numeric_columns, collapse = " + ")))
  return(formula)
}
#' @export 
get_boxscore_shifts <- function(game_id){
url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play")
response <- httr::GET(url)

pbp_data <- tryCatch({
  httr::content(response, "parsed", simplifyVector = TRUE)
}, error = function(e) {
  warning(paste("Failed to parse JSON for game_id", game_id, ": ", e$message))
  return(NULL)
}) 

plays <- pbp_data$plays
plays$periodDescriptor <- plays$periodDescriptor$number
plays$timeInPeriod <- sapply(plays$timeInPeriod, mmss_to_decimal)

plays <- plays %>%mutate(period = periodDescriptor, time = timeInPeriod + 20 * (period - 1)) %>%select(
  period, 
  time,
  situationCode,
  typeDescKey,
  details
)

details <- plays$details


plays$details <- NULL
plays <- cbind(plays, details)

stops <- plays %>% filter(lead(typeDescKey) == "faceoff")

starts <- stops$time
ends <- lead(stops$time)
ends[length(ends)] <- max(plays$time)


shifts <- data.frame(startTime = starts, endTime = ends)
shifts$duration <- shifts$endTime - shifts$startTime
shifts$situation <- stops$situationCode

shift_code <- NULL

for(i in seq(from = 1, to = nrow(shifts))){
  start <- shifts[i,]$start
  end <- shifts[i,]$end
  
  rel_plays <- plays %>% filter(
    time > start,
    time < end
  )
  situation <- paste(unique(rel_plays$situationCode), collapse = " ")
  if(situation == ""){
    rel_plays <- plays %>% filter(
      time >= start,
      time <= end
    )
    situation <- paste(unique(rel_plays$situationCode), collapse = " ")
  }
  shift_code[i] <- situation
}

shifts$situation <- shift_code
unique(shift_code)

home_shifts <- shifts
home_shifts$situation <- sapply(home_shifts$situation, get_situation, is_home = T)

away_shifts <- shifts
away_shifts$situation <- sapply(away_shifts$situation, get_situation, is_home = F)


hits <- plays %>% filter(typeDescKey == "hit")

hitters <- hits %>% group_by(hittingPlayerId)%>%reframe(hits = n())
hit_players <- hits %>% group_by(hitteePlayerId)%>%reframe(times_hit = n())

hit_df <- merge(hitters, hit_players, by.x = "hittingPlayerId", by.y = "hitteePlayerId", all = T)
hit_df[is.na(hit_df)] <- 0
colnames(hit_df) <- c("playerId", "hits", "times_hit")
faceoffs <- plays %>% filter(typeDescKey == "faceoff")

faceoffs_win <- faceoffs %>% group_by(winningPlayerId)%>%reframe(faceoffWins = n())
faceoffs_lost <- faceoffs %>% group_by(losingPlayerId)%>%reframe(faceoffLosses = n())

faceoff_df <- merge(faceoffs_win, faceoffs_lost, by.x = "winningPlayerId", by.y = "losingPlayerId", all = T)

takeaways <- plays %>% filter(typeDescKey == "takeaway")

tw_df <- takeaways %>% group_by(playerId, zoneCode)%>%reframe(takeaways = n())

tw_df <- tw_df %>% pivot_wider(
  names_from = zoneCode,
  values_from = takeaways,
  values_fill = 0,
  names_glue = "{zoneCode}_takeaways"
)


giveaways <- plays %>% filter(typeDescKey == "giveaway")

gw_df <- giveaways %>% group_by(playerId, zoneCode)%>%reframe(giveaways = n())

gw_df <- gw_df %>% pivot_wider(
  names_from = zoneCode,
  values_from = giveaways,
  values_fill = 0,
  names_glue = "{zoneCode}_giveaways"
)

safe_group_count <- function(df, group_col, count_name) {
  if (nrow(df) == 0) {
    return(data.frame(!!group_col := character(0), !!count_name := integer(0)))
  }
  df %>% group_by(.data[[group_col]]) %>% reframe(!!count_name := n())
}

# HIT STATS
hits <- plays %>% filter(typeDescKey == "hit")
hitters <- safe_group_count(hits, "hittingPlayerId", "hits")
hit_players <- safe_group_count(hits, "hitteePlayerId", "times_hit")

hit_df <- merge(hitters, hit_players, by.x = "hittingPlayerId", by.y = "hitteePlayerId", all = TRUE)
colnames(hit_df) <- c("playerId", "hits", "times_hit")

# FACEOFF STATS
faceoffs <- plays %>% filter(typeDescKey == "faceoff")
faceoffs_win <- safe_group_count(faceoffs, "winningPlayerId", "faceoffWins")
faceoffs_lost <- safe_group_count(faceoffs, "losingPlayerId", "faceoffLosses")

faceoff_df <- merge(faceoffs_win, faceoffs_lost, by.x = "winningPlayerId", by.y = "losingPlayerId", all = TRUE)
colnames(faceoff_df)[1] <- "playerId"

# TAKEAWAYS
takeaways <- plays %>% filter(typeDescKey == "takeaway")
tw_df <- if (nrow(takeaways) > 0) {
  takeaways %>%
    group_by(playerId, zoneCode) %>%
    reframe(takeaways = n()) %>%
    pivot_wider(
      names_from = zoneCode,
      values_from = takeaways,
      values_fill = 0,
      names_glue = "{zoneCode}_takeaways"
    )
} else {
  data.frame(playerId = character(0))
}

# GIVEAWAYS
giveaways <- plays %>% filter(typeDescKey == "giveaway")
gw_df <- if (nrow(giveaways) > 0) {
  giveaways %>%
    group_by(playerId, zoneCode) %>%
    reframe(giveaways = n()) %>%
    pivot_wider(
      names_from = zoneCode,
      values_from = giveaways,
      values_fill = 0,
      names_glue = "{zoneCode}_giveaways"
    )
} else {
  data.frame(playerId = character(0))
}

# Merge TW and GW
tw_gw_df <- merge(tw_df, gw_df, by = "playerId", all = TRUE)

# GOALS AND ASSISTS
goals <- plays %>% filter(typeDescKey == "goal")

goals_scored <- if (nrow(goals) > 0) {
  goals %>% group_by(scoringPlayerId) %>%
    reframe(goals = n()) %>%
    rename(playerId = scoringPlayerId)
} else {
  data.frame(playerId = character(0), goals = integer(0))
}

prime_a_scored <- if (nrow(goals) > 0 && any(!is.na(goals$assist1PlayerId))) {
  goals %>% filter(!is.na(assist1PlayerId)) %>%
    group_by(assist1PlayerId) %>%
    reframe(primary_assists = n()) %>%
    rename(playerId = assist1PlayerId)
} else {
  data.frame(playerId = character(0), primary_assists = integer(0))
}

sec_a_scored <- if (nrow(goals) > 0 && any(!is.na(goals$assist2PlayerId))) {
  goals %>% filter(!is.na(assist2PlayerId)) %>%
    group_by(assist2PlayerId) %>%
    reframe(sec_assists = n()) %>%
    rename(playerId = assist2PlayerId)
} else {
  data.frame(playerId = character(0), sec_assists = integer(0))
}

scoring_df <- merge(goals_scored, prime_a_scored, by = "playerId", all = TRUE)
scoring_df <- merge(scoring_df, sec_a_scored, by = "playerId", all = TRUE)

# BLOCKS
blockedShots <- plays %>% filter(typeDescKey == "blocked-shot")

blocks <- if (nrow(blockedShots) > 0) {
  blockedShots %>%
    filter(reason == "blocked", !is.na(blockingPlayerId)) %>%
    mutate(playerId = blockingPlayerId) %>%
    group_by(playerId) %>%
    reframe(blockedShots = n())
} else {
  data.frame(playerId = character(0), blockedShots = integer(0))
}

shots_blocked <- if (nrow(blockedShots) > 0 && any(!is.na(blockedShots$shootingPlayerId))) {
  blockedShots %>%
    mutate(playerId = shootingPlayerId) %>%
    group_by(playerId) %>%
    reframe(shotsBlocked = n())
} else {
  data.frame(playerId = character(0), shotsBlocked = integer(0))
}

# FINAL MERGE
final_sum <- Reduce(function(x, y) merge(x, y, by = "playerId", all = TRUE), list(
  scoring_df,
  faceoff_df,
  blocks,
  shots_blocked,
  hit_df,
  tw_gw_df
))

# Fill NAs with 0
final_sum[is.na(final_sum)] <- 0

return(
  list(
    boxscore = final_sum,
    home_shifts = home_shifts,
    away_shifts = away_shifts
  )
)
}
flip_sign <- function(x) {
  ifelse(x < 0, -x, x)
}
is_on_ice <- function(shot_time, shift_start, shift_end) {
  return(shot_time >= shift_start && shot_time <= shift_end)
}
get_situation_time <- function(start_time, end_time, team, shots) {
  # Filter shots that occurred within the shift time range
  relevant_shots <- shots[shots$time >= start_time & shots$time <= end_time, ]
  
  # Determine the situation from the team's perspective
  if (nrow(relevant_shots) > 0) {
    situations <- apply(relevant_shots, 1, function(shot) {
      if (shot["eventOwnerTeamId"] == team) {
        return(shot["situation"])
      } else {
        # Reverse the situation (e.g., 5v4 becomes 4v5)
        situation_parts <- strsplit(shot["situation"], "v")[[1]]
        reversed_situation <- paste0(situation_parts[2], "v", situation_parts[1])
        return(reversed_situation)
      }
    })
    situation <- paste(unique(situations), collapse = ", ")
  } else {
    situation <- "Unknown"  # Default if no shots are found in the range
  }
  
  return(situation)
}
#' @export
get_situation <- function(situationCode, is_home) {
  # Split situationCode by spaces
  split_sections <- strsplit(situationCode, " ")[[1]]
  
  # Process each section
  processed_sections <- sapply(split_sections, function(section) {
    split_chars <- strsplit(section, "")[[1]] # Split the section into characters
    if (is_home) {
      # For home, switch positions 3 and 2
      paste(split_chars[3], split_chars[2], sep = "v")
    } else {
      # For away, switch positions 2 and 3
      paste(split_chars[2], split_chars[3], sep = "v")
    }
  })
  
  # Recombine all processed sections into one final string
  final_string <- paste(processed_sections, collapse = " ")
  return(final_string)
}

#' @export 
get_pbp_data <- function(game_id) {
  library(dplyr)
  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play")
  
  
  response <- httr::GET(url)
  
  pbp_data <- tryCatch({
    httr::content(response, "parsed", simplifyVector = TRUE)
  }, error = function(e) {
    warning(paste("Failed to parse JSON for game_id", game_id, ": ", e$message))
    return(NULL)
  })
  home <-pbp_data$homeTeam$id
  away<- pbp_data$awayTeam$id
  
  if (is.null(pbp_data) || !("plays" %in% names(pbp_data))) {
    return(NULL)
  }
  
  
  shots_goals <- c("goal", "shot-on-goal", "missed-shot")
  plays_cleaned <- dplyr::filter(pbp_data$plays, typeDescKey %in% shots_goals)
  plays_cleaned <- dplyr::filter(plays_cleaned, details$zoneCode %in% "O" )
  
  if (nrow(plays_cleaned) == 0) {
    return(NULL)
  }
  
  # Efficient data preparation
  play_details <- plays_cleaned$details
  play_details$shot_outcome <- plays_cleaned$typeDescKey
  play_details$situation <- plays_cleaned$situationCode
  times <- sapply(plays_cleaned$timeInPeriod, mmss_to_decimal)
  times <- times + (20 * (plays_cleaned$periodDescriptor$number - 1))
  times <- times
  
  
  # Calculate the difference between consecutive elements
  time_diff <- c(100, diff(times * 60))
  
  
  play_data <- dplyr::select(play_details, xCoord, yCoord, shotType, eventOwnerTeamId, situation) %>%
    dplyr::mutate(
      is_goal = as.numeric(!is.na(play_details$scoringPlayerId)),
      goalie = play_details$goalieInNetId,
      time = times,
      sec_since = time_diff,
      shot_outcome = plays_cleaned$typeDescKey,
      shotType = factor(shotType),
      shooter = dplyr::coalesce(play_details$scoringPlayerId, play_details$shootingPlayerId),
      xCoord = abs(as.numeric(xCoord)),
      yCoord = as.numeric(yCoord),
      distance = sqrt((xCoord - 89)^2 + yCoord^2),  # Inline distance calculation
      angle = atan2(yCoord, (xCoord - 89)),
      
    )
  
  play_data$is_home <- play_data$eventOwnerTeamId == home
  play_data$is_rebound <- play_data$sec_since < 2
  play_data$dX <- ifelse(lag(play_data$is_home) == play_data$is_home & play_data$sec_since < 2, lag(play_data$xCoord) - play_data$xCoord, 1000)
  play_data$dY <- ifelse(lag(play_data$is_home) == play_data$is_home & play_data$sec_since < 2, lag(play_data$yCoord) - play_data$yCoord, 1000)
  play_data$dAngle <- ifelse(lag(play_data$is_home) == play_data$is_home & play_data$sec_since < 2, lag(play_data$angle) - play_data$angle, 1000)
  play_data$dDist <- ifelse(lag(play_data$is_home) == play_data$is_home & play_data$sec_since < 2, lag(play_data$distance) - play_data$distance, 1000)
  
  play_data <- play_data %>%
    arrange(time) %>%
    group_by(is_home) %>%
    mutate(score = cumsum(is_goal)) %>%
    ungroup()
  play_data <- play_data %>%
    group_by(time) %>%
    mutate(
      home_score = if_else(is_home, score, NA_integer_),
      away_score = if_else(!is_home, score, NA_integer_)
    ) %>%
    ungroup()
  play_data <- play_data %>%
    mutate(
      home_score = zoo::na.locf(home_score, na.rm = FALSE),
      away_score = zoo::na.locf(away_score, na.rm = FALSE)
    ) 
  play_data$score <- NULL  
  play_data$home_score[is.na(play_data$home_score)] <- 0
  play_data$home_score[is.na(play_data$away_score)] <- 0
  play_data$diff <- ifelse(play_data$is_home, play_data$home_score - play_data$away_score, play_data$away_score - play_data$home_score)
  home_score <- sum(play_data$is_goal[play_data$eventOwnerTeamId==home])
away_score <- sum(play_data$is_goal[play_data$eventOwnerTeamId!=home])


play_data$shot_outcome = factor(play_data$shot_outcome)
play_data$situation <- mapply(get_situation, play_data$situation, play_data$is_home)
play_data$is_home <- as.numeric(play_data$is_home)

  play_data <- play_data %>% filter(
  situation %in% c("5v5", "4v4", "4v3", "3v3", "3v5", "4v5", "5v4", "5v3", "3v4", "6v5", "6v4", "4v6", "5v6")
)
  return(
    list(
      data = play_data,
      homeId = home,
      awayId = away
    )
  )
}
#' @export 
get_player_name <- function(player_id){
  url <- glue::glue("https://api-web.nhle.com/v1/player/{player_id}/landing")
  response <- RCurl::getURL(url)
  data <- jsonlite::fromJSON(response)

  first_name <- data$firstName$default
  last_name <- data$lastName$default
  name <- c(first_name, last_name)
  name <- paste(name, collapse = " ")
  return(name)
}

#' @export
get_team_name <- function(team_id){
  team_data <- jsonlite::fromJSON(RCurl::getURL("https://api.nhle.com/stats/rest/en/team"))$data
  return(team_data$triCode[team_data$id==team_id])
}

#' @export
mmss_to_decimal <- function(mmss) {
  parts <- try(strsplit(mmss, ":")[[1]], silent = TRUE)
  
  if (inherits(parts, "try-error") || length(parts) != 2) {
    return(NA)  # Handle errors or unexpected format
  }
  
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  
  if (is.na(minutes) || is.na(seconds)) {
    return(15)  # Handle non-numeric values
  } else {
    return(minutes + seconds / 60)
  }
}
scale_data <- function(data, mean, sd){
  return((data - mean) / sd)
}
#' @export
prep_pbp_data <- function(data) {
  # Scale numerical columns
  data$xCoord <- scale_data(data$xCoord, 61.6246151056552, 16.8019395755011)
  data$yCoord <- scale_data(data$yCoord, 0.000116272872980967, 18.8440029089781)
  data$angle <- scale_data(data$angle, 0.00469904200387261, 0.64901885914269)
  data$distance <- scale_data(data$distance, 33.0143960468627, 17.2297050245461)
  data$time <- scale_data(data$time, 31.7254593331691, 17.5149367826591)
  data$sec_since <- scale_data(data$sec_since, 43.5599445489081, 48.3817054216268)
  data$dX <- scale_data(data$dX, -6.50701818514144, 21.9539240039713)
  data$dY <- scale_data(data$dY, -0.435264552681976, 22.538968644329)
  data$dAngle <- scale_data(data$dAngle, -0.126431775084931, 3.47190468836782)
  data$dDist <- scale_data(data$dDist, 7.38187399416592, 22.560438167841)
  data$diff <- scale_data(data$diff, -0.0254378595227842, 1.61385573846185)
  
  # Ensure consistent factor levels
  data$shotType <- factor(data$shotType, levels = c("backhand", "slap", "snap", "tip-in",
                                                    "wrap-around", "wrist", "deflected",
                                                    "bat", "poke", "between-legs", "cradle"))
  data$situation <- factor(data$situation, levels = c("5v5", "4v4", "4v3", "3v3", "3v5", "4v5", "5v4", "5v3", "3v4", "6v5", "6v4", "4v6", "5v6"))
  
  # Ensure complete rows before encoding
  complete_rows <- complete.cases(data[c("is_goal", "xCoord", "yCoord", "angle", "distance",
                                         "dX", "dY", "dAngle", "dDist", "diff", "sec_since", 
                                         "time", "shotType", "situation")])
  if (!all(complete_rows)) {
    warning("Dropping rows with missing values.")
  }
  data <- data[complete_rows, ]
  
  # One-hot encoding
  x <- model.matrix(is_goal ~ xCoord + yCoord + angle + distance + dX + dY +
                      dAngle + dDist + sec_since + time + shotType + situation + diff,
                    data)
  
  # Remove intercept column
  
  
  # Replace NA values with 0 (shouldn't occur after complete.cases, but extra precaution)
  x[is.na(x)] <- 0
  
  return(as.matrix(x))
}

group_shifts <- function(shifts){
  num_clusters <- ceiling(nrow(shifts) / 6)
  cluster <- kmeans(shifts[,5:6], num_clusters)
  shifts$shift_cluster <- cluster$cluster
  cluster_sizes <- table(shifts$cluster)
  if (all(cluster_sizes <= 6)) {
  
  } else {
  
  
  # If some clusters are too large, increment the number of clusters and re-cluster
  while (any(cluster_sizes > 6)) {
    num_clusters <- num_clusters + 1  # Increment the number of clusters
    cluster <- kmeans(shifts[, 5:6], centers = num_clusters)  # Re-cluster
    shifts$shift_cluster <- cluster$cluster  # Assign new cluster labels
    cluster_sizes <- table(shifts$cluster)  # Recalculate cluster sizes
  }

}

shifts_reframed <- shifts %>%
  group_by(shift_cluster) %>%
  reframe(
    startTime = max(startTime),
    endTime = min(endTime),
    duration = endTime - startTime,
    players = list(playerId)
  )%>%
  mutate(
    binary_indicators = map(players, ~ {
      all_player_ids <- unique(shifts$playerId)
      sapply(all_player_ids, function(pid) as.integer(pid %in% .x))
    })
  ) %>%
  unnest_wider(binary_indicators, names_sep = "_") %>%
  rename_with(~ paste0("", unique(shifts$playerId)), starts_with("binary_indicators_"))%>%
  select(-players, -shift_cluster)
return(shifts_reframed)
}
#'@export
combine_shifts <- function(existing, new_data) {
  if(is.null(new_data)){
    return(existing)
  }
  all_columns <- union(colnames(existing), colnames(new_data))
  
  
  existing <- existing %>%
    mutate(!!!setNames(rep(list(0), length(setdiff(all_columns, names(existing)))), setdiff(all_columns, names(existing)))) %>%
    select(all_of(all_columns))
  
  new_data <- new_data %>%
    mutate(!!!setNames(rep(list(0), length(setdiff(all_columns, names(new_data)))), setdiff(all_columns, names(new_data)))) %>%
    select(all_of(all_columns))
  
  
  return(bind_rows(existing, new_data))
}
get_skaters <- function(game_id){
url <- glue::glue("https://api-web.nhle.com/v1/gamecenter/{game_id}/boxscore")
boxscore <- jsonlite::fromJSON(RCurl::getURL(url))


htf <- boxscore$playerByGameStats$homeTeam$forwards
rownames(htf) <- htf$playerId
htf <-htf%>% mutate(name = name$default) %>% select(playerId, name, position)

htd <- boxscore$playerByGameStats$homeTeam$defense
rownames(htd) <- htd$playerId
htd <-htd%>% mutate(name = name$default) %>% select(playerId, name, position)

atf <- boxscore$playerByGameStats$awayTeam$forwards
rownames(atf) <- atf$playerId
atf <-atf%>% mutate(name = name$default) %>% select(playerId, name, position)

atd <- boxscore$playerByGameStats$awayTeam$defense
rownames(atd) <- atd$playerId
atd <-atd%>% mutate(name = name$default) %>% select(playerId, name, position)

players <- rbind(atf, htf, atd, htd)
return(players)
}
#' @export 
get_regularized_metric <- function(shifts, metric, trace.it){
  library(glmnet)
  shift_y <- shifts[[metric]]
  shift_x <- shifts %>% select(starts_with("player"), starts_with("opp"),diff, is_home, duration)
  
  ridge_model <- cv.glmnet(shift_x %>% makeX(), 
                           shift_y,
                           weights = shifts$duration,
                           nfolds = 10,
                           trace.it = trace.it,
                           parallel = T,
                           alpha = 0)
  return(ridge_model)
}
fix_encoding_list <- function(x) {
  lapply(x, function(name) {
    bad_bytes <- iconv(name, from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]]
    corrected <- rawToChar(bad_bytes)
    Encoding(corrected) <- "UTF-8"
    corrected
  })
}

#' @export 
get_game_summary <- function(game_id, xg_nnet, xg_rebound){
  game <- get_pbp_data(game_id)
  skaters <- get_skaters(game_id)
  bs_shifts <- get_boxscore_shifts(game_id)
  homeId <- game$homeId
  awayId <- game$awayId
  game_data <- game$data
  home_gf = sum(game_data$is_goal[game_data$eventOwnerTeamId == homeId])
  away_gf = sum(game_data$is_goal[game_data$eventOwnerTeamId== awayId])
  game_data <- game_data %>% na.omit()
  
  
  game_data$xG <- predict_xg(game_data, xg_nnet, xg_rebound)
  home_xgf = sum(game_data$xG[game_data$eventOwnerTeamId == homeId])
  away_xgf = sum(game_data$xG[game_data$eventOwnerTeamId == awayId])
  player_summary <- game_data %>% 
  group_by(shooter) %>% 
  reframe(ixG = sum(xG),
          iF = n())
  player_summary <- merge(bs_shifts$boxscore,player_summary, by.x = "playerId", by.y = "shooter", all = T)
  
  
  
  shift_url <- glue::glue("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}")
  shift_data <- jsonlite::fromJSON(RCurl::getURL(shift_url))$data
  shift_data <- shift_data %>%
    select(all_of(c("playerId" ,"typeCode", "period","teamId", "startTime","endTime","shiftNumber")))
  shift_data$startTime <- (sapply(shift_data$startTime, mmss_to_decimal) + (shift_data$period-1) * 20) 
  shift_data$endTime <- (sapply(shift_data$endTime, mmss_to_decimal) + (shift_data$period-1) * 20) 
  
  shift_data <- shift_data %>%mutate(
    # startTime = round(startTime,1),
    #endTime = round(endTime,1),
    duration = endTime-startTime
  )%>%filter(
    playerId %in% skaters$playerId
  )
  
  toi <- shift_data %>% group_by(playerId)%>%reframe(toi = sum(duration))
  
  
  unique_pairs <- shift_data %>%
    select(playerId, teamId) %>%
    distinct()
  
  on_ice_stats <- data.frame(
    playerId = unique_pairs$playerId,
    teamId = unique_pairs$teamId,
    GF = numeric(nrow(unique_pairs)),
    GA = numeric(nrow(unique_pairs)),
    xGF = numeric(nrow(unique_pairs)),
    xGA = numeric(nrow(unique_pairs)),
    FF = numeric(nrow(unique_pairs)),
    FA = numeric(nrow(unique_pairs))
  )
  
  for (i in seq_len(nrow(shift_data))) {
    row <- shift_data[i, ]
    start <- row$startTime
    end <- row$endTime
    player <- row$playerId
    team <- row$teamId
    
    relevant_events <- game_data[game_data$time > start & game_data$time < end, ]
    
    for (j in seq_len(nrow(relevant_events))) {
      event <- relevant_events[j, ]
      event_team <- event$eventOwnerTeamId
      
      if (event_team == team) {
        on_ice_stats[on_ice_stats$playerId == player, "FF"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "FF"] + 1
        on_ice_stats[on_ice_stats$playerId == player, "xGF"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "xGF"] + event$xG
        on_ice_stats[on_ice_stats$playerId == player, "GF"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "GF"] + event$is_goal
      } else {
        on_ice_stats[on_ice_stats$playerId == player, "FA"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "FA"] + 1
        on_ice_stats[on_ice_stats$playerId == player, "xGA"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "xGA"] + event$xG
        on_ice_stats[on_ice_stats$playerId == player, "GA"] <- 
          on_ice_stats[on_ice_stats$playerId == player, "GA"] + event$is_goal
      }
    }
  }
  
  
  player_summary <- merge(on_ice_stats, player_summary, by = "playerId", all = T)
  player_summary[is.na(player_summary)]<- 0
  
  home_shifts <- bs_shifts$home_shifts
  away_shifts <- bs_shifts$away_shifts
  
  home_shifts$diff = mapply(get_team_score, 
                                  MoreArgs = list(data = game_data, team = homeId), 
                                  timestamp = home_shifts$startTime)

  away_shifts$diff =  mapply(get_team_score, 
                                  MoreArgs = list(data = game_data, team = awayId), 
                                  timestamp = away_shifts$startTime)                              
  
  home_shifts_stats <- data.frame(
    GF = numeric(nrow(home_shifts)), 
    GA = numeric(nrow(home_shifts)), 
    xGF = numeric(nrow(home_shifts)), 
    xGA = numeric(nrow(home_shifts)),
    FF = numeric(nrow(home_shifts)),
    FA = numeric(nrow(home_shifts))
  )
  
  away_shifts_stats <- data.frame(
    GF = numeric(nrow(away_shifts)), 
    GA = numeric(nrow(away_shifts)), 
    xGF = numeric(nrow(away_shifts)), 
    xGA = numeric(nrow(away_shifts)),
    FF = numeric(nrow(home_shifts)),
    FA = numeric(nrow(home_shifts))
  )
  
  for (i in seq_len(nrow(home_shifts))) {
    row <- home_shifts[i, ]
    start <- row$startTime
    end <- row$endTime
    team <- homeId
    
    relevant_events <- game_data[game_data$time >= start & game_data$time <= end, ]
    
    for (j in seq_len(nrow(relevant_events))) {
      event <- relevant_events[j, ]
      event_team <- event$eventOwnerTeamId
      
      if (event_team == team) {
        home_shifts_stats[i, "xGF"] <- home_shifts_stats[i, "xGF"] + event$xG
        home_shifts_stats[i, "GF"] <- home_shifts_stats[i, "GF"] + event$is_goal
        home_shifts_stats[i, "FF"] <- home_shifts_stats[i, "FF"] + 1
      } else {
        home_shifts_stats[i, "xGA"] <- home_shifts_stats[i, "xGA"] + event$xG
        home_shifts_stats[i, "GA"] <- home_shifts_stats[i, "GA"] + event$is_goal
        home_shifts_stats[i, "FA"] <- home_shifts_stats[i, "FA"] + 1
      }
    }
  }
  
  for (i in seq_len(nrow(away_shifts))) {
    row <- away_shifts[i, ]
    start <- row$startTime
    end <- row$endTime
    team <- awayId
    
    relevant_events <- game_data[game_data$time >= start & game_data$time <= end, ]
    
    for (j in seq_len(nrow(relevant_events))) {
      event <- relevant_events[j, ]
      event_team <- event$eventOwnerTeamId
      
      if (event_team == team) {
        away_shifts_stats[i, "xGF"] <- away_shifts_stats[i, "xGF"] + event$xG
        away_shifts_stats[i, "GF"] <- away_shifts_stats[i, "GF"] + event$is_goal
        away_shifts_stats[i, "FF"] <- away_shifts_stats[i, "FF"] + 1
      } else {
        away_shifts_stats[i, "xGA"] <- away_shifts_stats[i, "xGA"] + event$xG
        away_shifts_stats[i, "GA"] <- away_shifts_stats[i, "GA"] + event$is_goal
        away_shifts_stats[i, "FA"] <- away_shifts_stats[i, "FA"] + 1
      }
    }
  }
  
  home_shifts <- cbind(home_shifts, home_shifts_stats)
  away_shifts <- cbind(away_shifts, away_shifts_stats)
  
  home_shift <- shift_data %>% filter(teamId == homeId)
  away_shift <- shift_data %>% filter(teamId != homeId)
  
  home_shifts$is_home <- 1
  away_shifts$is_home <- 0
  
  player_ids_home <- unique(home_shift$playerId)  # Get player IDs from home_shift
for (player in player_ids_home) {
  # Create a column for each playerId, initialized to 0 in home_shifts
  home_shifts[[paste0("player_", player)]] <- 0
  
  # Loop through each row in home_shift for the specific playerId
  for (i in 1:nrow(home_shift)) {
    if (home_shift$playerId[i] == player) {
      # Check if the player's timeframe overlaps with the original timeframe in home_shifts
      for (j in 1:nrow(home_shifts)) {
        overlap_start <- max(home_shifts$startTime[j], home_shift$startTime[i])
        overlap_end <- min(home_shifts$endTime[j], home_shift$endTime[i])
        
        if (overlap_start < overlap_end) {  # Check if there is an overlap
          overlap_duration <- overlap_end - overlap_start
          shift_duration <- home_shifts$endTime[j] - home_shifts$startTime[j]
          
          # Calculate the percentage of the shift the player was on ice
          home_shifts[j, paste0("player_", player)] <- 
            1
        }
      }
    }
  }
}

player_ids_away <- unique(away_shift$playerId)  # Get player IDs from away_shift
for (player in player_ids_away) {
  # Create a column for each playerId, initialized to 0 in away_shifts
  away_shifts[[paste0("player_", player)]] <- 0
  
  # Loop through each row in away_shift for the specific playerId
  for (i in 1:nrow(away_shift)) {
    if (away_shift$playerId[i] == player) {
      # Check if the player's timeframe overlaps with the original timeframe in away_shifts
      for (j in 1:nrow(away_shifts)) {
        overlap_start <- max(away_shifts$startTime[j], away_shift$startTime[i])
        overlap_end <- min(away_shifts$endTime[j], away_shift$endTime[i])
        
        if (overlap_start < overlap_end) {  # Check if there is an overlap
          overlap_duration <- overlap_end - overlap_start
          shift_duration <- away_shifts$endTime[j] - away_shifts$startTime[j]
          
          # Calculate the percentage of the shift the player was on ice
          away_shifts[j, paste0("player_", player)] <- 
            1
        }
      }
    }
  }
}
  
  
  
  home_shifts <- home_shifts %>% mutate(
    xG_pct = xGF / (xGA + xGF),
    Fwick_pct = FF / (FF + FA),
    GF_rate = 60 * GF / duration,
    GA_rate = 60 * GA  / duration,
    xGF_rate = 60 * xGF / duration,
    xGA_rate = 60 * xGA  / duration,
    FF_rate =  60 * FF / duration,
    FA_rate = 60 * FA / duration
  )%>%filter(
    duration > 0
  )

   away_shifts <- away_shifts %>% mutate(
    xG_pct = xGF / (xGA + xGF),
    Fwick_pct = FF / (FF + FA),
    GF_rate = 60 * GF / duration,
    GA_rate = 60 * GA  / duration,
    xGF_rate = 60 * xGF / duration,
    xGA_rate = 60 * xGA  / duration,
    FF_rate =  60 * FF / duration,
    FA_rate = 60 * FA / duration
  )%>%filter(
    duration > 0
  )
  
  
  
  home_gf = sum(game_data$is_goal[game_data$eventOwnerTeamId == homeId])
  home_xgf = sum(game_data$xG[game_data$eventOwnerTeamId == homeId])
  
  away_gf = sum(game_data$is_goal[game_data$eventOwnerTeamId== awayId])
  away_xgf = sum(game_data$xG[game_data$eventOwnerTeamId == awayId])
  
  player_summary$teamGF = ifelse(player_summary$teamId == homeId, home_gf,away_gf)
  player_summary$teamGA = ifelse(player_summary$teamId == homeId, away_gf,home_gf)
  
  player_summary$teamxGF = ifelse(player_summary$teamId == homeId, home_xgf, away_xgf)
  player_summary$teamxGA = ifelse(player_summary$teamId == homeId, away_xgf,home_xgf)
  
  game_end = max(game_data$time)
  home_points = 0
  away_points = 0

  if(home_gf > away_gf){
    home_points = 1
    if(game_end > 60){
      away_points = 0.5
    }else{
      away_points = 0
    }
  }else if(home_gf < away_gf){
    away_points = 1
    if(game_end > 60){
      home_points = 0.5
    }else{
      home_points = 0
    }
  }
  player_summary <- merge(player_summary, toi, by = "playerId")
  player_summary <- merge(player_summary, skaters, by = "playerId")
  player_summary$game_outcome <- ifelse(player_summary$teamId == homeId, home_points, away_points)
  
  player_summary <- player_summary %>%
  mutate(
    faceoffWinningPct = faceoffWins / (faceoffWins + faceoffLosses),
    gameId = game_id,
    name = fix_encoding_list(name)
  )
 return(list(
  data = player_summary,
  away_shifts = away_shifts,
  home_shifts = home_shifts
))
}
#' @export 
get_game_score <- function(summaries, offense.model, defense.model){
  offense <- predict(offense.model, summaries)
  defense <- predict(defense.model, summaries)
  
  
  offense <- scale(offense, center = 2.716047, scale = 0.698286)
  defense <- -1 * scale(defense, center = 2.383336, scale = 0.8152321)
  gamescore <- offense + defense
  return(data.frame(
    offense = offense,
    defense = defense,
    gamescore = gamescore
  ))
}

#' @export
prep_summaries <- function(summaries){
  summaries <- summaries %>%
    reframe(
      
      goals = (goals - 0.1197927) / 0.3478451,
      bapm = (bapm - 0.001547731) / 0.01599692,
      blockedShots = (blockedShots - 0.07087692) / 0.3754932,
      primary_assists = (primary_assists - 0.1508666) / 0.3998862,
      sec_assists = (sec_assists - 0.1219792) / 0.3557028,
      faceoffWins = (faceoffWins - 1.580948) / 3.146262,
      faceoffLosses = (faceoffLosses - 1.58428) / 2.966689,
      
      hits = (hits - 1.250733) / 1.427926,
      times_hit = (times_hit - 1.244803) / 1.257364,
      N_takeaways = (N_takeaways - 0.08295466) / 0.2941611,
      D_takeaways = (D_takeaways - 0.1682145) / 0.4277739,
      O_takeaways = (O_takeaways - 0.1324945) / 0.3811339,
      N_giveaways = (N_giveaways - 0.06130291) / 0.2525989,
      D_giveaways = (D_giveaways - 0.2583634) / 0.5666064,
      O_giveaways = (O_giveaways - 0.1488844) / 0.4154918,
      ixG = (ixG - 0.1250912) / 0.2069007
      #xGF = (xGF - .8050644) / .7920592,
      #xGA = (xGA - 0.7807485) / 0.743609
    )
  summaries <- model.matrix(~., summaries) %>% data.frame() %>% select(-X.Intercept.)%>%as.matrix()
  return(summaries)
}

get_team_score <- function(data, team, timestamp) {
  data$time_from <- data$time - timestamp
  diff <- data$diff[which.min(abs(data$time_from))]

  if(data$eventOwnerTeamId[which.min(abs(data$time_from))] == team){
    return(diff)
  }else{
    return(diff * -1)
  }
}

prep_rebounds_data <- function(data) {
  # Scale numerical columns
  data$xCoord <- scale_data(data$xCoord, 61.6246151056552, 16.8019395755011)
  data$yCoord <- scale_data(data$yCoord, 0.000116272872980967, 18.8440029089781)
  data$angle <- scale_data(data$angle, 0.00469904200387261, 0.64901885914269)
  data$distance <- scale_data(data$distance, 33.0143960468627, 17.2297050245461)
  data$time <- scale_data(data$time, 31.7254593331691, 17.5149367826591)
  data$sec_since <- scale_data(data$sec_since, 43.5599445489081, 48.3817054216268)
  data$dX <- scale_data(data$dX, -7.33369648807994, 19.5968965167085)
  data$dY <- scale_data(data$dY, -0.807485260189723, 19.1183533710828)
  data$dAngle <- scale_data(data$dAngle, -0.163935519801121, 3.3445958624678)
  data$dDist <- scale_data(data$dDist, 8.40566822753027, 20.6353440909719)
  data$diff <- scale_data(data$diff, -0.0254378595227842, 1.61385573846185)
  
  # Ensure consistent factor levels
  data$shotType <- factor(data$shotType, levels = c("backhand", "slap", "snap", "tip-in",
                                                    "wrap-around", "wrist", "deflected",
                                                    "bat", "poke", "between-legs", "cradle"))
  
  
  # Ensure complete rows before encoding
  complete_rows <- complete.cases(data[c("is_goal", "xCoord", "yCoord", "angle", "distance",
                                         "dX", "dY", "dAngle", "dDist", "diff", "sec_since", 
                                         "time", "shotType")])
  if (!all(complete_rows)) {
    warning("Dropping rows with missing values.")
  }
  data <- data[complete_rows, ]
  
  # One-hot encoding
  x <- model.matrix(is_goal ~ xCoord + yCoord + angle + distance + dX + dY +
                      dAngle + dDist + sec_since + time + shotType + diff + is_rebound,
                    data)
  
  # Remove intercept column
  
  
  # Replace NA values with 0 (shouldn't occur after complete.cases, but extra precaution)
  x[is.na(x)] <- 0
  
  return(as.matrix(x))
}

prep_normal_data <- function(data) {
  # Scale numerical columns
  data <- data %>% select(-dX, -dY, -dAngle, -dDist)
  data$xCoord <- scale_data(data$xCoord, 61.6246151056552, 16.8019395755011)
  data$yCoord <- scale_data(data$yCoord, 0.000116272872980967, 18.8440029089781)
  data$angle <- scale_data(data$angle, 0.00469904200387261, 0.64901885914269)
  data$distance <- scale_data(data$distance, 33.0143960468627, 17.2297050245461)
  data$time <- scale_data(data$time, 31.7254593331691, 17.5149367826591)
  data$sec_since <- scale_data(data$sec_since, 43.5599445489081, 48.3817054216268)
  data$diff <- scale_data(data$diff, -0.0254378595227842, 1.61385573846185)
  
  # Ensure consistent factor levels
  data$shotType <- factor(data$shotType, levels = c("backhand", "slap", "snap", "tip-in",
                                                    "wrap-around", "wrist", "deflected",
                                                    "bat", "poke", "between-legs", "cradle"))
  
  
  # Ensure complete rows before encoding
  complete_rows <- complete.cases(data[c("is_goal", "xCoord", "yCoord", "angle", "distance",
                                         "diff", "sec_since", 
                                         "time", "shotType")])
  if (!all(complete_rows)) {
    warning("Dropping rows with missing values.")
  }
  data <- data[complete_rows, ]
  
  # One-hot encoding
  x <- model.matrix(is_goal ~ xCoord + yCoord + angle + distance + 
                      sec_since + time + shotType + diff,
                    data)
  
  # Remove intercept column
  
  
  # Replace NA values with 0 (shouldn't occur after complete.cases, but extra precaution)
  x[is.na(x)] <- 0
  
  return(as.matrix(x))
}

#' @export 
predict_xg <- function(pbp_data, xg.nnet, xg.rebound){
  rbd_data <- pbp_data[pbp_data$is_rebound,]
  norbd_data <- pbp_data[!pbp_data$is_rebound,]

  if(nrow(rbd_data) > 0){
  rbd_data$xG <- c(predict(xg.rebound, prep_rebounds_data(rbd_data), verbose = 0))
  }else{
    rbd_data = data.frame()
  }
  if(nrow(norbd_data) > 0){
  norbd_data$xG <- c(predict(xg.nnet, prep_normal_data(norbd_data), verbose = 0))
  }else{
    norbd_data = data.frame()
  }

pbp_data <- rbind(rbd_data, norbd_data)
pbp_data <- pbp_data[order(pbp_data$time),]
  return(pbp_data$xG)
}