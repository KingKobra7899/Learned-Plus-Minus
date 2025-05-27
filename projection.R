# Load required libraries
library(keras)
library(tensorflow)
library(dplyr)

# Set seeds for reproducibility
set.seed(42)
tensorflow::tf$random$set_seed(42)

# Enhanced data preparation with age features
prepare_age_aware_data <- function(df, max_seq_length = 4) {
  # Scale features globally
  napm_mean <- mean(df$NAPM, na.rm = TRUE)
  napm_sd <- sd(df$NAPM, na.rm = TRUE)
  age_mean <- mean(df$age, na.rm = TRUE)
  age_sd <- sd(df$age, na.rm = TRUE)
  
  df_scaled <- df %>%
    arrange(playerId, season) %>%
    mutate(
      napm_scaled = (NAPM - napm_mean) / napm_sd,
      age_scaled = (age - age_mean) / age_sd,
      # Age curve features - typical peak around 26-28
      age_prime = pmax(0, 1 - abs(age - 27) / 10),  # Peak performance window
      age_decline = pmax(0, (age - 30) / 10),       # Post-30 decline factor
      age_development = pmax(0, (25 - age) / 5),    # Pre-25 development potential
      # Experience proxy (season number for each player)
      experience = row_number()
    ) %>%
    group_by(playerId) %>%
    mutate(
      # Year-over-year changes
      napm_change = NAPM - lag(NAPM, default = NAPM[1]),
      age_trajectory = (age - min(age)) / pmax(1, max(age) - min(age)),
      # Career stage
      career_games = row_number(),
      is_rookie = ifelse(career_games == 1, 1, 0)
    ) %>%
    ungroup()
  
  # Create sequences for training
  sequences <- list()
  targets <- list()
  
  for(player in unique(df_scaled$playerId)) {
    player_data <- df_scaled %>% filter(playerId == player) %>% arrange(season)
    n_seasons <- nrow(player_data)
    
    if(n_seasons >= 2) {
      for(i in 1:(n_seasons - 1)) {
        seq_length <- min(i, max_seq_length)
        start_idx <- max(1, i - seq_length + 1)
        
        # Enhanced feature set including age dynamics
        seq_features <- player_data[start_idx:i, c("napm_scaled", "age_scaled", 
                                                   "age_prime", "age_decline", 
                                                   "age_development", "napm_change", 
                                                   "career_games", "is_rookie")]
        
        # Pad sequence
        padded_seq <- matrix(0, nrow = max_seq_length, ncol = 8)
        padded_seq[1:nrow(seq_features), ] <- as.matrix(seq_features)
        
        sequences[[length(sequences) + 1]] <- padded_seq
        targets[[length(targets) + 1]] <- player_data$napm_scaled[i + 1]
      }
    }
  }
  
  # Convert to arrays
  X <- array(0, dim = c(length(sequences), max_seq_length, 8))
  y <- array(0, dim = c(length(targets)))
  
  for(i in 1:length(sequences)) {
    X[i,,] <- sequences[[i]]
    y[i] <- targets[[i]]
  }
  
  list(X = X, y = y, napm_mean = napm_mean, napm_sd = napm_sd, 
       age_mean = age_mean, age_sd = age_sd, max_seq_length = max_seq_length,
       df_scaled = df_scaled)
}

# Build age-aware RNN model
build_age_aware_rnn <- function(max_seq_length = 4) {
  # Main sequence input
  sequence_input <- layer_input(shape = c(max_seq_length, 8), name = "sequence")
  
  # RNN processing
  masked <- sequence_input %>%
    layer_masking(mask_value = 0.0)
  
  lstm_out <- masked %>%
    layer_lstm(units = 64, return_sequences = TRUE, dropout = 0.2, recurrent_dropout = 0.1) %>%
    layer_lstm(units = 32, return_sequences = FALSE, dropout = 0.2, recurrent_dropout = 0.1)
  
  # Age-specific pathway - focus on the most recent age features  
  age_features <- masked %>%
    layer_lambda(function(x) x[, -1, 2:5]) %>%  # Extract age features from last timestep
    layer_dense(units = 16, activation = 'relu', name = "age_pathway") %>%
    layer_dropout(rate = 0.3)
  
  # Combine RNN output with age pathway
  combined <- layer_concatenate(list(lstm_out, age_features)) %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, activation = 'linear', name = "output")
  
  model <- keras_model(inputs = sequence_input, outputs = combined)
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = 'mse',
    metrics = c('mae')
  )
  
  return(model)
}

# Train and apply age-aware model
data_prep <- prepare_age_aware_data(player_stats)
model <- build_age_aware_rnn(data_prep$max_seq_length)

# Train model with more focus on age patterns
model %>% fit(
  x = data_prep$X,
  y = data_prep$y,
  epochs = 150,
  batch_size = 16,
  validation_split = 0.2,
  verbose = 0
)

# Make predictions for ALL players with enhanced age understanding
predictions <- c()

for(player in unique(player_stats$playerId)) {
  player_data <- player_stats %>% 
    filter(playerId == player) %>% 
    arrange(season)
  
  # Calculate enhanced age features
  player_enhanced <- player_data %>%
    mutate(
      napm_scaled = (NAPM - data_prep$napm_mean) / data_prep$napm_sd,
      age_scaled = (age - data_prep$age_mean) / data_prep$age_sd,
      age_prime = pmax(0, 1 - abs(age - 27) / 10),
      age_decline = pmax(0, (age - 30) / 10),
      age_development = pmax(0, (25 - age) / 5),
      napm_change = NAPM - lag(NAPM, default = NAPM[1]),
      career_games = row_number(),
      is_rookie = ifelse(career_games == 1, 1, 0)
    )
  
  # Create input sequence with all 8 features
  n_seasons <- nrow(player_enhanced)
  seq_features <- player_enhanced[, c("napm_scaled", "age_scaled", "age_prime", 
                                      "age_decline", "age_development", "napm_change",
                                      "career_games", "is_rookie")]
  
  # Create padded sequence
  input_seq <- array(0, dim = c(1, data_prep$max_seq_length, 8))
  actual_length <- min(n_seasons, data_prep$max_seq_length)
  input_seq[1, 1:actual_length, ] <- as.matrix(seq_features[1:actual_length, ])
  
  # Make prediction
  pred_scaled <- model %>% predict(input_seq, verbose = 0)
  pred_napm <- pred_scaled * data_prep$napm_sd + data_prep$napm_mean
  
  predictions <- c(predictions, as.numeric(pred_napm))
}

# Add projected_NAPM column
player_lookup <- player_stats %>%
  group_by(playerId) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(projected_NAPM = predictions) %>%
  select(playerId, projected_NAPM)

player_stats <- player_stats %>%
  left_join(player_lookup, by = "playerId")
