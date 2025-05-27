shifts$shift_id <- rownames(shifts)
library(dplyr)

# Step 1: Summarize per-player-per-season stats (per game, not per 60)
data$season <- substring(data$gameId, 1, 4)

get_mean_sd <- function(vector){
  print(glue::glue("Mean: {mean(vector)}, SD: {sd(vector)}"))
}

player_stats <- data %>%
  group_by(season, playerId, position) %>%
  reframe(
    gp = n(),
    name = unique(name),
    toi = sum(toi),
    xGF = sum(xGF) / gp,
    xGA = sum(xGA) / gp,
    iF = sum(iF) / gp,
    primary_assists = sum(primary_assists) / gp,
    sec_assists = sum(sec_assists) / gp,
    ixG = sum(ixG) / gp,
    goals = sum(goals) / gp,
    O_takeaways = sum(O_takeaways) / gp,
    O_giveaways = sum(O_giveaways) / gp,
    blockedShots = sum(blockedShots) / gp,
    D_takeaways = sum(D_takeaways) / gp,
    D_giveaways = sum(D_giveaways) / gp,
    N_takeaways = sum(N_takeaways) / gp,
    N_giveaways = sum(N_giveaways) / gp,
    hits = sum(hits) / gp
  )%>%filter(gp > 15)
player_stats <- merge(player_stats, a3z_pergame, by = c("name", "season")) %>% na.omit()
player_stats <- merge(player_stats, metadata, by = "playerId")
player_stats$age <- as.numeric(player_stats$season) - as.numeric(player_stats$birthYear)
get_mean_sd(player_stats$weight)

get_player_season_summary <- function(p_data){
  return(p_data %>% select(xGF, xGA, iF, primary_assists, sec_assists, ixG, goals,
                              O_takeaways, O_giveaways, D_takeaways, D_giveaways, 
                              N_takeaways, N_giveaways, hits, blockedShots,shot_assists,
                              chance_assists,passes,pass_allowed, recoveries, clears,
                              forecheck_pressures, failed_exit, exits, dz_puck_touches,
                              forecheck_cycle_shots, dz_retrievals, denials, rebounds, height,
                              weight
                           )%>%
    mutate(
      iF = (iF - 1.73697178171072) / 0.613350649852211,
      ixG = (ixG - 0.10763534186252) / 0.0622514213950782,
      goals = (goals - 0.107306610461173) / 0.0771590598666977,
      primary_assists = (primary_assists - 0.100904833380185) / 0.0632862519530085,
      sec_assists = (sec_assists - 0.0806371924286831) / 0.0489108122710148,
      O_takeaways = (O_takeaways - 0.109141341007842) / 0.0760669058682906,
      O_giveaways = (O_giveaways - 0.175807512172212) / 0.12571687812024,
      N_takeaways = (N_takeaways - 0.0578680915072625) / 0.0461254630860795,
      N_giveaways = (N_giveaways - 0.0874882853576585) / 0.0461254630860795,
      D_giveaways = (D_giveaways - 0.259028613752256) / 0.20812086331882,
      D_takeaways = (D_takeaways - 0.105058002432476) / 0.0742761543391291,
      hits = (hits - 1.16728631019504) / 0.795382264805113,
      shot_assists = (shot_assists - 1.45719091803093) / 0.689574842996567,
      chance_assists = (chance_assists - 0.521995000975907) / 0.327736959477356,
      passes = (passes - 2.43787949839392) / 1.04786418967684,
      pass_allowed = (pass_allowed - 0.370058312920216) / 0.518220610890364,
      recoveries = (recoveries - 0.393317341625865) / 0.309967939105363,
      clears = (clears - 0.579553910353059) / 0.32438630231598,
      forecheck_pressures = (forecheck_pressures -0.923168576638035)/0.597648902025301,
      failed_exit = (failed_exit - 0.475164717922313) / 0.307823868732956,
      exits = (exits - 1.41492726039526) / 0.578110982349682,
      dz_puck_touches = (dz_puck_touches - 4.23779150741843) / 2.31591811095295,
      forecheck_cycle_shots = (forecheck_cycle_shots - 0.789940340279256) / 0.399470536985659,
      dz_retrievals = (dz_retrievals - 2.83246767069127) / 1.87097139899261,
      denials = (denials - 0.289110066915352) / 0.263666676072118,
      rebounds = (rebounds - 0.0875319081730215) /0.0895247714112177,
      height = (height - 73.3364825581395) / 2.18853437619768,
      weight = (weight - 201.11773255814) / 15.2948229273567
    ) %>% as.matrix())
}




get_player_stats <- function(pid, szn) {
  if (is.na(pid) || nrow(player_stats %>% filter(playerId == pid, season == szn)) == 0) {
    return(filler_vector)
  } else {
    return(player_stats %>% filter(playerId == pid, season == szn) )
  }
}
seasons <- shifts_features$season
get_x <- function(on_ice_df, player_stats){
num_shifts <- nrow(on_ice_df)
num_players <- ncol(on_ice_df)

num_stats <- ncol(get_player_season_summary(player_stats))

mask_val <- 1000
filler_vector <- matrix(mask_val, nrow = 1, ncol = num_stats)




shift_player_stats <- array(NA, dim = c(num_shifts, num_players, num_stats))
library(progress)


# Initialize the progress bar
pb <- progress_bar$new(
  format = "  Processing shifts [:bar] :percent | ETA: :eta",
  total = num_shifts, clear = FALSE, width = 60
)

for (shift_idx in 1:num_shifts) {
  current_season <- seasons[shift_idx]
  current_on_ice <- on_ice_df[shift_idx, ]
  
  for (player_idx in 1:num_players) {
    player_id <- current_on_ice[[player_idx]]
    
    if (!is.na(player_id)) {
      player_stats_row <- player_stats %>% 
        filter(playerId == player_id, season == current_season)
      
      if (nrow(player_stats_row) == 1) {
        player_stats_matrix <- get_player_season_summary(player_stats_row)
        shift_player_stats[shift_idx, player_idx, ] <- player_stats_matrix
      } else {
        shift_player_stats[shift_idx, player_idx, ] <- mask_val
      }
    } else {
      shift_player_stats[shift_idx, player_idx, ] <- mask_val
    }
  }
  
  pb$tick()
}
return(shift_player_stats)
}

for_shifts <- get_x(shifts_for, player_stats)
against_shifts <- get_x(shifts_against, player_stats)
shifts_features$situation <- as.factor(shifts_features$situation)
shift_x <- model.matrix(~situation + is_home + diff + duration - 1, data = shifts_features)
num_shift_inputs <- ncol(shift_x)


dim(shift_player_stats)

offense_loss <- function(y_true, y_pred) {
  
  huber_loss <- k_mean(loss_huber(y_true, y_pred))
  
  
  lambda <- 0.05  # Weight for KL term - adjust as needed
  combined_loss <- huber_loss #+ lambda * abs(y_pred)
  
  return(combined_loss)
}

defense_loss <- function(y_true, y_pred) {
  
  huber_loss <- k_mean(loss_huber(y_true, y_pred))
  
  
  lambda <- 0.05  # Weight for KL term - adjust as needed
  combined_loss <- huber_loss - lambda * y_pred
  
  return(combined_loss)
}

nnet_rapm <- function(stat, regularization, loss){
k_clear_session()
  
  
  
num_stats <- ncol(get_player_season_summary(player_stats))
player_input <- layer_input(shape = c(num_stats), name = "player_input")

# Mask inside the encoder
not_filler_mask <- layer_lambda(f = function(x) {
  k_cast(k_any(k_not_equal(x, 1000), axis = -1, keepdims = TRUE), dtype = "float32")
})(player_input)

# Process player stats
x <- player_input %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 14, activation = "relu") %>%
  layer_dense(units = 12, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu", name = "embedding") %>%
  layer_dense(units = 6, activation = "relu") %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 2, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear", kernel_regularizer = regularizer_l2(regularization))

# Apply the mask
masked_output <- layer_multiply(list(x, not_filler_mask))

player_encoder <- keras_model(inputs = player_input, outputs = masked_output)

shift_input_pos <- layer_input(shape = c(18, num_stats), name = "shift_pos_input")
shift_input_neg <- layer_input(shape = c(18, num_stats), name = "shift_neg_input")
shift_features_input <- layer_input(shape = c(num_shift_inputs), name = "shift_features_input")

# Encode players
encoded_pos <- shift_input_pos %>%
  time_distributed(player_encoder)

encoded_neg <- shift_input_neg %>%
  time_distributed(player_encoder)

# Aggregate each shift
agg_pos <- encoded_pos %>%
  layer_lambda(function(x) k_sum(x, axis = 2L))  # shape: (batch, 1)

agg_neg <- encoded_neg %>%
  layer_lambda(function(x) k_sum(x, axis = 2L))

# Player-based metric: pos - neg
player_metric <- list(agg_pos, agg_neg) %>%
  layer_lambda(function(tensors) tensors[[1]] - tensors[[2]])  # shape: (batch, 1)

# Weighted shift features (Dense layer with no bias for interpretability)
shift_metric <- shift_features_input %>%
  layer_dense(units = 1, name = "shift_weights", use_bias = F)


final_output <- layer_add(list(player_metric, shift_metric))

# Build model
model <- keras_model(
  inputs = list(shift_input_pos, shift_input_neg, shift_features_input),
  outputs = final_output
)

model%>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-5),
  loss = loss
)

model %>% fit(
  x = list(for_shifts,
           against_shifts,
           shift_x),
  y = shifts_features[[stat]],
  sample_weight =shifts_features$duration,
  epochs = 16
)

return(player_encoder)
}

player_x <-player_stats %>% get_player_season_summary()


offense <-nnet_rapm("xGF_rate", 0.01, offense_loss)
defense <-nnet_rapm("xGA_rate", 0.01, offense_loss)



player_stats$NxGF <- c(predict(offense, player_x))
player_stats$NxGA <- c(predict(defense, player_x))

hist(player_stats$NxGF, breaks = 50)

hist(player_stats$NxGA, breaks = 50)
player_stats <-player_stats %>% group_by(position, season)%>%mutate(offense_rank = percent_rank(NxGF))%>%ungroup()
player_stats <- player_stats %>% group_by(position, season)%>%mutate(defense_rank = 1-percent_rank(NxGA))%>%ungroup()
player_stats$NAPM <- player_stats$NxGF - player_stats$NxGA
player_stats <- player_stats %>% group_by(position, season)%>%mutate(overall_rank = percent_rank(NAPM))%>%ungroup()




get_player <- function(player_id){
  player_stats %>% filter(playerId == player_id)
}


plot_player_career <- function(player_id) {
  # Get player data using the player ID
  p <- get_player(player_id)
  
  # Get player name for the title
  player_name <- get_player_name(player_id) %>% fix_encoding_list()
  
  # Create the visualization
  ggplot(p, aes(x = season %>% as.numeric())) +
    # Add a subtle background gradient
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
              fill = "grey95", alpha = 0.5) +
    
    # Add subtle horizontal grid lines with custom styling
    geom_hline(yintercept = seq(0, 1, by = 0.25), color = "grey80", 
               linetype = "dashed", size = 0.3) +
    
    # Use thicker lines with custom styling and add points
    geom_line(aes(y = overall_rank, color = "Overall"), size = 1.2) +
    geom_point(aes(y = overall_rank, color = "Overall"), size = 3, shape = 19) +
    
    geom_line(aes(y = offense_rank, color = "Offense"), size = 1.2) +
    geom_point(aes(y = offense_rank, color = "Offense"), size = 3, shape = 17) +
    
    geom_line(aes(y = defense_rank, color = "Defense"), size = 1.2) +
    geom_point(aes(y = defense_rank, color = "Defense"), size = 3, shape = 15) +
    
    # Set custom colors with proper legend
    scale_color_manual(
      name = "Performance Metrics",
      values = c("Overall" = "#1A1A1A", "Offense" = "#3366CC", "Defense" = "#CC3333"),
      breaks = c("Overall", "Offense", "Defense")
    ) +
    
    # Better y-axis scaling with minor breaks
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      minor_breaks = seq(0, 1, by = 0.1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    
    # Add more informative labels
    labs(
      title = paste(player_name, "- Performance Rankings Over Time"),
      subtitle = "Tracking overall, offensive, and defensive percentile rankings",
      caption = "Data sources: All Three Zones Project and NHL API"
    ) +
    
    ylab("Percentile Rank") +
    xlab("Season") +
    
    # More unique theme with customizations
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "grey30", hjust = 0.5),
      legend.position = "bottom",
      legend.background = element_rect(fill = "grey95", color = "grey80"),
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      panel.border = element_rect(color = "grey80", fill = NA)
    )
}

# Example usage:
plot_player_career(8481656)


ggplot(player_stats, aes(x = age, y = NAPM))+
  geom_point()+
  geom_smooth()

teens <- player_stats %>% filter(age < 20)

passing <- lm(NAPM ~ position *(primary_assists + shot_assists + chance_assists + passes + missed_passes) - 1, player_stats)
shooting <- lm(NAPM ~ position *(iF + ixG + goals + forecheck_cycle_shots + rebounds) - 1, player_stats)
zone_exits <- lm(NAPM ~ position *(clears + failed_exit + exits) - 1, player_stats)
forecheck <- lm(NAPM ~ position *(rebounds + O_takeaways + forecheck_pressures + hits) - 1, player_stats)
defense_microstats <- lm(NAPM ~ position *(D_takeaways + pass_allowed + recoveries + clears + dz_retrievals + dz_puck_touches + denials) - 1, player_stats)

player_stats$passing <- predict(passing, player_stats)
player_stats$shooting <- predict(shooting, player_stats)
player_stats$zone_exits <- predict(zone_exits, player_stats)
player_stats$forecheck <- predict(forecheck, player_stats)
player_stats$defense_microstats <- predict(defense_microstats, player_stats)

# Load dplyr for easier grouping operations
library(dplyr)

# Calculate percent rank by position per season
player_stats <- player_stats %>%
  group_by(position, season) %>%
  mutate(
    passing = percent_rank(passing),
    shooting = percent_rank(shooting),
    zone_exits = percent_rank(zone_exits),
    forecheck = percent_rank(forecheck),
    defense_microstats = percent_rank(defense_microstats)
  ) %>%
  ungroup()
