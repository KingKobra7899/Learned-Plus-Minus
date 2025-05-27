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
               linetype = "dashed", linewidth = 0.3) +
    
    # Use thicker lines with custom styling and add points
    geom_line(aes(y = overall_rank, color = "Overall Rank"), linewidth = 1.2) +
    geom_point(aes(y = overall_rank, color = "Overall Rank"), size = 3, shape = 19) +
    
    #geom_line(aes(y = offense_rank, color = "Offense"), linewidth = 1.2) +
    #geom_point(aes(y = offense_rank, color = "Offense"), size = 3, shape = 17) +
    
    #geom_line(aes(y = defense_rank, color = "Defense"), linewidth = 1.2) +
   # geom_point(aes(y = defense_rank, color = "Defense"), size = 3, shape = 15) +
    
    # Set custom colors with proper legend
    scale_color_manual(
      name = "",
      values = c("Overall Rank" = "#1A1A1A", "Offense" = "#3366CC", "Defense" = "#CC3333"),
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
      title = "",
      subtitle = "",
      caption = ""
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
plot_player_microstats <- function(player_id, focus_season) {
  # Get player data using the player ID
  p <- get_player(player_id)
  
  # Get player name for the title
  player_name <- get_player_name(player_id) %>% fix_encoding_list()
  
  # Convert focus_season to character if it's numeric
  if(is.numeric(focus_season)) {
    focus_season <- as.character(focus_season)
  }
  
  # Extract the season data
  season_data <- p %>% filter(season == focus_season)
  
  # Check if we have data for the specified season
  if(nrow(season_data) == 0) {
    stop(paste0("No data found for season ", focus_season, " for player ", player_name))
  }
  
  # Extract microstats for the specified season
  microstats <- data.frame(
    metric = c("Passing", "Shooting", "Zone Exits", "Forecheck", "DZ Microstats", "Defense", "Offense", "Overall"),
    value = c(
      season_data$passing, 
      season_data$shooting, 
      season_data$zone_exits, 
      season_data$forecheck, 
      season_data$defense_microstats,
      season_data$defense_rank,
      season_data$offense_rank,
      season_data$overall_rank
    )
  )
  
  # Add row/column positions for the grid layout
  microstats$row <- 1
  microstats$col <- 1:8
  
  # Create microstat visualization with colored squares
  ggplot(microstats, aes(x = col, y = row)) +
    # Create colored squares
    geom_tile(aes(fill = value), color = "black", width = 0.9, height = 0.4, linewidth = 0.25) +
    # Add percentile text
    geom_text(aes(label = percent(value, 3)), 
              fontface = "bold", size = 5, color = "black") +
    # Use a color gradient from red (low) to blue (high)
    scale_fill_gradient2(
      low = "#ca0020",      # red
      mid = "#f7f7f7",      # orange/yellow
      high = "#0571b0",     # green
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Percentile"
    )+
    # Use metric names for x-axis labels
    scale_x_continuous(
      breaks = 1:8,
      labels = microstats$metric,
      position = "bottom"
    ) +
    # Remove y-axis
    scale_y_continuous(breaks = NULL) +
    # Add title
    labs(
      title = paste0(player_name, " - ", focus_season, " Rankings"),
      x = "",
      y = ""
    ) +
    # Apply theming
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
}

player_card <- function(player, season){
grid.arrange(plot_player_microstats(player, season), plot_player_career(player) + geom_vline(xintercept = season))
}
player_card(8482073, 2021)
