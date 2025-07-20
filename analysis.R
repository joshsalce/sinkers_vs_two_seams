library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(tidyselect)
library(baseballr)
library(RMySQL)
library(overlapping)
library(tibble)
library(hrbrthemes)
library(ggpubr)
library(class)
library(mclust)
library(mgcv)
library(scales)

set.seed(909)

source("startup.R")

plot_gam_heatmap <- function(df,
                             x_col = "plate_x",
                             y_col = "plate_z",
                             response_col,
                             lg_avg_val,
                             plot_limits,
                             rect_bounds = c(-0.7083, 0.7083, 1.6, 3.4),
                             plot_midpt = 0.0,
                             title = "Smoothed GAM Heatmap",
                             xlabel = "Horizontal Location (in.)",
                             ylabel = "Vertical Location (in.)",
                             scale_value = "Value",
                             rev_color_scale = FALSE) {
  df <- df %>%
    mutate(
      x = !!sym(x_col),
      y = !!sym(y_col),
      z = !!sym(response_col)
    )
  
  low_color <- ifelse(rev_color_scale, "red", "blue")
  high_color <- ifelse(rev_color_scale, "blue", "red")
  
  # Step 1: Filter and rename columns
  # Step 2: Fit GAM model
  gam_model <- mgcv::gam(z ~ s(x, y), data = df)
  
  # Step 3: Create prediction grid
  x_seq <- seq(min(df$x), max(df$x), length.out = 100)
  y_seq <- seq(min(df$y), max(df$y), length.out = 100)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Step 4: Predict
  grid$z_pred <- ifelse(response_col != "wOBA", 100, 1)*predict(gam_model, newdata = grid, na.action = "na.omit")
  
  # Step 5: Plot
  plot <- ggplot(grid, aes(x = x, y = y, fill = z_pred - lg_avg_val)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(low = low_color, mid = "white", high = high_color,
                         midpoint = plot_midpt,
                         limits = plot_limits,
                         oob = squish,
                         name = scale_value) +
    coord_fixed() +
    theme_minimal() +
    labs(x = xlabel, y = ylabel, title = title) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 16),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
    )
  
  # Conditional annotation
  if (!all(is.na(rect_bounds))) {
    
    plot <- plot +
      annotate("rect",
               xmin = rect_bounds[1], xmax = rect_bounds[2],
               ymin = rect_bounds[3], ymax = rect_bounds[4],
               fill = NA, color = "black", size = 0.5)
  }
  
  plot
}

# Read in data
pitch_data <- dbGetQuery(myConn, read_sql_file(paste0(folder_path, "pitch_data.sql")))

pitcher_sample <- dbGetQuery(myConn, read_sql_file(paste0(folder_path, "pitcher_sample.sql")))

inside_pitch_data <- dbGetQuery(myConn, read_sql_file(paste0(folder_path, "inside_pitches.sql")))

gb_rates <- dbGetQuery(myConn, read_sql_file(paste0(folder_path, "gb_rates.sql")))

fg_guts <- read_csv(paste0(data_folder_path, "fangraphs-guts-data.csv"))
  
event_map <- c(
    "wBB"  = "walk",
    "wHBP" = "hit_by_pitch",
    "w1B"  = "single",
    "w2B"  = "double",
    "w3B"  = "triple",
    "wHR"  = "home_run"
)
  
fg_woba_weights <-
  fg_guts %>%
  select(Season, wBB, wHBP, w1B, w2B, w3B, wHR) %>%
  pivot_longer(
    cols = -Season,
    names_to = "events",
    values_to = "wOBA_value_fg"
  ) %>%
  mutate(
    events = recode(events, !!!event_map)
  ) %>%
  rename(game_year = Season)

pitch_data <- left_join(pitch_data, fg_woba_weights)

inside_pitch_data <- left_join(inside_pitch_data, fg_woba_weights)

stuff_preds <-  read_csv(paste0(data_folder_path, "stuff_standard_preds.csv"))

stuff_diff_preds <-  read_csv(paste0(data_folder_path, "stuff_diffs_preds.csv"))

sinker_data <- 
  pitch_data %>%
  filter(pitch_type == "SI" & !is.na(pfx_z) & !is.na(pfx_x) & !is.na(effective_speed)) %>%
  mutate(
    VMOV = pfx_z, 
    HMOV = ifelse(p_throws == "L", pfx_x, -1*pfx_x) 
  ) %>%
  select(
    pitcher, player_name, game_year, p_throws, stand, 
    effective_speed, HMOV, VMOV, pfx_x, pfx_z, arm_angle, zone, plate_x, plate_z,
    swing, whiff, bip, csw, launch_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, 
    wOBA_value_fg, woba_denom, delta_run_exp
  )

# Define an approximation of the Coolwarm colormap
coolwarm_palette <- c("#3B4CC0", "#5679D6", "#8FB5E2", "#C4DDEA", "#E8E8E8",
                      "#EAB89E", "#D78252", "#BE3C2B", "#962A20")

# Calculate league averages
lg_avg_si_swing_pct <- 
  sinker_data %>%
  summarise(Swing_pct = 100*round(mean(swing, na.rm = TRUE), 3)) %>%
  pull()

lg_avg_si_woba <- 
  sinker_data %>%
  summarise(wOBA = round(sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE), 3)) %>%
  pull()

lg_avg_si_xwoba <- 
  sinker_data %>% 
  summarise(xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 3)) %>%
  pull()

lg_avg_si_whiff_rate <- 
  sinker_data %>%
  summarise(Whiff_rate = 100*round(sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE), 3)) %>%
  pull()

lg_avg_si_foul_pct <- 
  sinker_data %>%
  summarise(Foul_pct = 100*round(sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE)), 3)) %>%
  pull()

lg_avg_si_stk_pct <- 
  sinker_data %>%
  summarise(stk_pct = 100*round(sum(ifelse(csw == 1 | !is.na(whiff), 1, 0), na.rm = TRUE) / n(), 3)) %>%
  pull()

lg_avg_si_csw_pct <- 
  sinker_data %>%
  summarise(CSW_pct = 100*round(mean(csw, na.rm = TRUE), 3)) %>%
  pull()

# Analysis =====================================================================================================================================================

# Fit GAM model for Whiff% for sinker movement
whiff_gam_model <- mgcv::gam(whiff ~ s(HMOV, VMOV), data = sinker_data)

whiff_x_seq <- seq(min(sinker_data$HMOV), max(sinker_data$HMOV), length.out = 100)
whiff_y_seq <- seq(min(sinker_data$VMOV), max(sinker_data$VMOV), length.out = 100)
whiff_grid <- expand.grid(HMOV = whiff_x_seq, VMOV = whiff_y_seq)

whiff_grid$Whiff_pct <- 100*predict(whiff_gam_model, newdata = whiff_grid, na.action = "na.omit")

# Plot Sinker Movement vs. Smoothed Whiff%
ggplot(whiff_grid, aes(x = HMOV, y = VMOV, fill = Whiff_pct)) +
   geom_hex(stat = "identity") +
   scale_fill_gradient2(
       low = "blue", mid = "white", high = "red", midpoint = lg_avg_si_whiff_rate, 
       limits = c(10, 20),
       oob = squish,
       name = "Whiff%"
     ) +
   labs(title = "Sinker Whiff%: Vertical vs. Horizontal Movement",
        x = "Horizontal Movement (in.)",
        y = "Vertical Movement (in.)") +
   theme_minimal() +
   scale_x_continuous(breaks = seq(min(sinker_data$HMOV), max(sinker_data$HMOV), by = 2)) +
   scale_y_continuous(breaks = seq(min(sinker_data$VMOV), max(sinker_data$VMOV), by = 2)) +
   theme(
       panel.grid = element_blank(), # Remove grid lines for clean look
       axis.text = element_text(size = 12),
       axis.title = element_text(size = 14),
       plot.title = element_text(size = 18),
       panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
       legend.title = element_text(size = 14),
       legend.text = element_text(size = 10),
     ) + 
  xlim(5, 22) + 
  ylim(-10, 20)

# Group By and aggregate mean four-seam movement
four_seam_movement_avgs <- 
  pitch_data %>%
  filter(pitch_type == "FF") %>%
  mutate(pfx_x = ifelse(p_throws == "L", pfx_x, -1*pfx_x)) %>%
  group_by(pitcher, game_year) %>%
  summarise(
    N = n(),
    FF_VMOV = mean(pfx_z, na.rm = TRUE),
    FF_HMOV = mean(pfx_x, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N >= 50) %>%
  select(-N)

# Add four-seam movement differentials
sinker_mvt_diffs <-
  merge(sinker_data, four_seam_movement_avgs, by = c("pitcher", "game_year")) %>%
  mutate(
    FF_SI_VMOV_diff = VMOV - FF_VMOV,
    FF_SI_HMOV_diff = HMOV - FF_HMOV 
  )

# Fit GAM model for Whiff% for sinker movement differences from four-seam
whiff_mvt_diff_gam_model <- mgcv::gam(whiff ~ s(FF_SI_HMOV_diff, FF_SI_VMOV_diff), data = sinker_mvt_diffs)

whiff_mvt_diff_x_seq <- seq(min(sinker_mvt_diffs$FF_SI_HMOV_diff), max(sinker_mvt_diffs$FF_SI_HMOV_diff), length.out = 100)
whiff_mvt_diff_y_seq <- seq(min(sinker_mvt_diffs$FF_SI_VMOV_diff), max(sinker_mvt_diffs$FF_SI_VMOV_diff), length.out = 100)
whiff_mvt_diff_grid <- expand.grid(FF_SI_HMOV_diff = whiff_mvt_diff_x_seq, FF_SI_VMOV_diff = whiff_mvt_diff_y_seq)

whiff_mvt_diff_grid$Whiff_pct <- 100*predict(whiff_mvt_diff_gam_model, newdata = whiff_mvt_diff_grid, na.action = "na.omit")

# Plot Sinker Movement Difference vs. Smoothed Whiff%
ggplot(whiff_mvt_diff_grid, aes(x = FF_SI_HMOV_diff, y = FF_SI_VMOV_diff, fill = Whiff_pct)) +
  geom_hex(stat = "identity") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = lg_avg_si_whiff_rate, 
    limits = c(10, 20),
    oob = squish,
    name = "Whiff%"
  ) +
  labs(title = "Sinker Whiff%: Vertical vs. Horizontal Movement \nDeviation From Four-Seam Average",
       x = "Horizontal Movement Deviation (in.)",
       y = "Vertical Movement Deviation (in.)") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  ) + 
  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  scale_y_continuous(breaks = seq(-14, 2, by = 2))  +
  xlim(-1, 16) + 
  ylim(-15, 1)


# Plot density of raw movement, movement deviations
ggplot(sinker_data, aes(HMOV, VMOV)) +
  geom_hex(bins = 30) + 
  scale_fill_gradient2(low = "#3B4CC0", mid = "#E8E8E8", high = "#962A20", midpoint = 4000, name = "Count") +
  labs(
    title = "Frequency: Sinker Vertical vs. Horizontal Movement", 
    x = "Horizontal Movement (in.)", 
    y = "Vertical Movement (in.)"
  ) + 
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  )

ggplot(sinker_mvt_diffs, aes(FF_SI_HMOV_diff, FF_SI_VMOV_diff)) +
  geom_hex(bins = 30) + 
  scale_fill_gradient2(low = "#3B4CC0", mid = "#E8E8E8", high = "#962A20", midpoint = 2000, name = "Count") +
  labs(
    title = "Frequency: Sinker Vertical vs. Horizontal Movement \nDeviation From Four-Seam Average", 
    x = "Horizontal Movement Deviation (in.)", 
    y = "Vertical Movement Deviation (in.)",
  ) + 
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  )

# Find median movement difference from average four-seam
med_hmov_diff <- 
  sinker_mvt_diffs %>%
  group_by(player_name, game_year) %>%
  summarise(
    FF_SI_HMOV_diff = mean(FF_SI_HMOV_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    med_hmov_diff = median(FF_SI_HMOV_diff),
    .groups = "drop"
  ) %>%
  pull()

med_vmov_diff <- 
  sinker_mvt_diffs %>%
  group_by(player_name, game_year) %>%
  summarise(
    FF_SI_VMOV_diff = mean(FF_SI_VMOV_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    med_vmov_diff = median(FF_SI_VMOV_diff),
    .groups = "drop"
  ) %>%
  pull()

# Add flag for low vertical movement difference from FF, a.k.a. "Two-Seams"
sinker_mvt_diffs <- 
  sinker_mvt_diffs %>%
  mutate(
    Flag = ifelse(FF_SI_HMOV_diff > 0 & FF_SI_VMOV_diff >= med_vmov_diff, 1, 0)
  )

# Calculate player-level stats for sinkers, including % of pitches that meet "two-seam" criteria 
sinker_agg_stats <- 
  sinker_mvt_diffs %>%
  group_by(
    player_name, game_year
  ) %>%
  summarise(
    Two_Seam_Freq = mean(Flag, na.rm = TRUE),
    Two_Seam = ifelse(Two_Seam_Freq > 0.5, 1, 0),
    N = n(),
    arm_angle = mean(arm_angle, na.rm = TRUE),
    Velo = mean(effective_speed, na.rm = TRUE),
    VMOV = mean(VMOV, na.rm = TRUE),
    HMOV = mean(HMOV, na.rm = TRUE),
    FF_SI_VMOV_diff = mean(FF_SI_VMOV_diff, na.rm = TRUE),
    FF_SI_HMOV_diff = mean(FF_SI_HMOV_diff, na.rm = TRUE),
    Whiff_rate = 100*mean(whiff, na.rm = TRUE),
    wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    CSW_pct = 100*mean(csw, na.rm = TRUE),
    Foul_pct = 100*(sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE))),
    SI_Bucket = case_when(
      FF_SI_HMOV_diff <= med_hmov_diff & FF_SI_VMOV_diff > med_vmov_diff ~ 1,
      FF_SI_HMOV_diff > med_hmov_diff & FF_SI_VMOV_diff > med_vmov_diff ~ 2,
      FF_SI_HMOV_diff <= med_hmov_diff & FF_SI_VMOV_diff < med_vmov_diff ~ 3, 
      FF_SI_HMOV_diff > med_hmov_diff & FF_SI_VMOV_diff < med_vmov_diff ~ 4,
    ),
    .groups = "drop"
  ) %>%
  filter(N > 100)

sinker_agg_stats %>%
  ggplot(aes(x = HMOV, y = VMOV)) + 
  geom_point(aes(colour = factor(Two_Seam))) +
  scale_color_discrete(name = "Pitch Type", labels = c("Sinkers", "Two-Seams")) +
  labs(
    x = "Horizontal Movement (in.)",
    y = "Vertical Movement (in.)",
    title = "Average Vertical vs. Horizontal Movement, by Pitch Type"
  ) + 
  scale_x_continuous(breaks = seq(7.5, 20, by = 2.5)) + 
  scale_y_continuous(breaks = seq(-10, 20, by = 2.5)) +  
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  )

two_seam_stats <-
  sinker_agg_stats %>%
  filter(Two_Seam_Freq > .50) 

true_sinker_stats <-
  sinker_agg_stats %>%
  filter(Two_Seam_Freq <= .50) 

# Summary stats bucketed by sinkers and "two-seams"
sinker_mvt_diffs %>%
  group_by(Flag) %>%
  summarise(
    N = n(),
    wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    Whiff_rate = 100*mean(whiff, na.rm = TRUE),
    CSW_pct = 100*mean(csw, na.rm = TRUE),
    Foul_pct = 100*(sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  rename(`Two-Seam` = Flag)

# Plots ========================================================================================================================================================

# Group by bins of movement/movement difference, calculate wOBA and Foul / Contact
sinker_gam_data <-
  sinker_data %>%
  filter(VMOV <= 20 & VMOV >= -2 & HMOV >= 5 & HMOV <= 20) %>%
  group_by(HMOV, VMOV) %>%
  summarise(
    wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    Foul_pct = (sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE))),
    .groups = "drop"
  )

sinker_mvt_diff_gam_data <-
  sinker_mvt_diffs %>%
  filter(FF_SI_VMOV_diff <= 1 & FF_SI_VMOV_diff >= -15 & FF_SI_HMOV_diff <= 15 & FF_SI_HMOV_diff >= 0) %>%
  group_by(FF_SI_HMOV_diff, FF_SI_VMOV_diff) %>%
  summarise(
    wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    Foul_pct = (sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE))),
    .groups = "drop"
  )

# Plot wOBA, Foul / Contact for movement/movement difference
plot_gam_heatmap(
  sinker_gam_data %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  response_col = "wOBA",
  lg_avg_val = lg_avg_si_woba,
  title = "Sinker Vertical vs. Horizontal Movement: wOBA",
  x_col = "HMOV",
  y_col = "VMOV",
  xlabel = "Horizontal Movement (in.)",
  ylabel = "Vertical Movement (in.)",
  scale_value = "wOBA vs. Average",
  rect_bounds = NA,
  plot_limits = c(-0.1, 0.1),
  rev_color_scale = TRUE
)

plot_gam_heatmap(
  sinker_mvt_diff_gam_data %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  response_col = "wOBA",
  lg_avg_val = lg_avg_si_woba,
  title = "Sinker Vertical vs. Horizontal Movement \nDeviation From Four-Seam Average: wOBA",
  x_col = "FF_SI_HMOV_diff",
  y_col = "FF_SI_VMOV_diff",
  xlabel = "Horizontal Movement Deviation (in.)",
  ylabel = "Vertical Movement Deviation (in.)",
  scale_value = "wOBA vs. Average",
  rect_bounds = NA,
  plot_limits = c(-0.1, 0.1),
  rev_color_scale = TRUE
)

plot_gam_heatmap(
  sinker_gam_data %>%
    filter(!is.na(Foul_pct)),
  response_col = "Foul_pct",
  lg_avg_val = lg_avg_si_foul_pct,
  title = "Sinker Vertical vs. Horizontal Movement: Foul / Contact%",
  x_col = "HMOV",
  y_col = "VMOV",
  xlabel = "Horizontal Movement (in.)",
  ylabel = "Vertical Movement (in.)",
  scale_value = "Foul / Contact% vs. Average",
  rect_bounds = NA,
  plot_limits = c(-10, 10),
  rev_color_scale = FALSE
)

plot_gam_heatmap(
  sinker_mvt_diff_gam_data %>%
    filter(!is.na(Foul_pct)),
  response_col = "Foul_pct",
  lg_avg_val = lg_avg_si_foul_pct,
  title = "Sinker Vertical vs. Horizontal Movement \nDeviation From Four-Seam Average: Foul / Contact%",
  x_col = "FF_SI_HMOV_diff",
  y_col = "FF_SI_VMOV_diff",
  xlabel = "Horizontal Movement Deviation (in.)",
  ylabel = "Vertical Movement Deviation (in.)",
  scale_value = "Foul / Contact% vs. Average",
  rect_bounds = NA,
  plot_limits = c(-10, 10),
  rev_color_scale = FALSE
)

# Aggregate "buckets" of sinkers into four groups, with aggregate summary stats
sinker_mvt_diffs %>%
  mutate(
    SI_Bucket = case_when(
      FF_SI_HMOV_diff <= med_hmov_diff & FF_SI_VMOV_diff > -5 ~ 1,
      FF_SI_HMOV_diff > med_hmov_diff & FF_SI_VMOV_diff > -5 ~ 2,
      FF_SI_HMOV_diff <= med_hmov_diff & FF_SI_VMOV_diff < -5 ~ 3, 
      FF_SI_HMOV_diff > med_hmov_diff & FF_SI_VMOV_diff < -5 ~ 4,
    )
  ) %>%
  filter(!is.na(SI_Bucket)) %>%
  group_by(SI_Bucket) %>%
  summarise(
    N = n(),
    Whiff_rate = 100*mean(whiff, na.rm = TRUE),
    CSW_pct = 100*mean(csw, na.rm = TRUE),
    wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
    wOBA_diff_avg = round(wOBA - lg_avg_si_woba, 3),
    Foul_pct = 100*(sum(ifelse(bip == 0, 1, 0), na.rm = TRUE) / (sum(swing, na.rm = TRUE) - sum(whiff, na.rm = TRUE))),
    .groups = "drop"
  )

## Locations ===================================================================================================================================================

# Add buckets to split plots by, standardize horizontal location 
sinker_locs <- sinker_mvt_diffs %>%
  filter(abs(plate_x) <= 2.25 & plate_z >= 0 & plate_z <= 5) %>%
  mutate(
    plate_x_tr = ifelse(p_throws == "L", plate_x * -1, plate_x),
    location_bucket = ifelse(p_throws == stand & Flag == 0, 1, 
                     ifelse(p_throws == stand & Flag == 1, 2, 
                            ifelse(p_throws != stand & Flag == 0, 3, 4)))
  )

# Plot CSW%
plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 1),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "csw",
  lg_avg_val = lg_avg_si_csw_pct,
  plot_limits = c(-30, 40),
  scale_value = "CSW% vs. Average",
  title = "Same-Handed Sinkers: CSW%"
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 2),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "csw",
  lg_avg_val = lg_avg_si_csw_pct,
  plot_limits = c(-30, 40),
  scale_value = "CSW% vs. Average",
  title = "Same-Handed Two-Seams: CSW%"
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 3),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "csw",
  lg_avg_val = lg_avg_si_csw_pct,
  plot_limits = c(-30, 40),
  scale_value = "CSW% vs. Average",
  title = "Opposite-Handed Sinkers: CSW%"
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 4),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "csw",
  lg_avg_val = lg_avg_si_csw_pct,
  plot_limits = c(-30, 40),
  scale_value = "CSW% vs. Average",
  title = "Opposite-Handed Two-Seams: CSW%"
)

# Plot Swing%
plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 1),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "swing",
  lg_avg_val = 0,
  scale_value = "Swing%",
  title = "Same-Handed Sinkers: Swing%",
  plot_midpt = lg_avg_si_swing_pct,
  plot_limits = c(0, 90)
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 2),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "swing",
  lg_avg_val = 0,
  scale_value = "Swing%",
  title = "Same-Handed Two-Seams: Swing%",
  plot_midpt = lg_avg_si_swing_pct,
  plot_limits = c(0, 90)
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 3),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "swing",
  lg_avg_val = 0.0,
  scale_value = "Swing%",
  title = "Opposite-Handed Sinkers: Swing%",
  plot_midpt = lg_avg_si_swing_pct,
  plot_limits = c(0, 90)
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 4),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "swing",
  lg_avg_val = 0.0,
  scale_value = "Swing%",
  title = "Opposite-Handed Two-Seams: Swing%",
  plot_midpt = lg_avg_si_swing_pct,
  plot_limits = c(0, 90) 
)

# Plot wOBA
plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 1) %>%
    group_by(plate_x_tr, plate_z) %>%
    summarise(wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "wOBA",
  lg_avg_val = lg_avg_si_woba,
  plot_limits = c(-0.2, 0.3),
  scale_value = "wOBA vs. Average",
  title = "Same-Handed Sinkers: wOBA",
  rev_color_scale = TRUE
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 2) %>%
    group_by(plate_x_tr, plate_z) %>%
    summarise(wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "wOBA",  
  lg_avg_val = lg_avg_si_woba,
  plot_limits = c(-0.2, 0.3),
  scale_value = "wOBA vs. Average",
  title = "Same-Handed Two-Seams: wOBA",
  rev_color_scale = TRUE
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 3) %>%
    group_by(plate_x_tr, plate_z) %>%
    summarise(wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "wOBA",  
  lg_avg_val = lg_avg_si_woba,
  plot_limits = c(-0.2, 0.3),
  scale_value = "wOBA vs. Average",
  title = "Opposite-Handed Sinkers: wOBA",
  rev_color_scale = TRUE
)

plot_gam_heatmap(
  sinker_locs %>%
    filter(location_bucket == 4) %>%
    group_by(plate_x_tr, plate_z) %>%
    summarise(wOBA = sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(wOBA) & if_all(everything(), is.finite)),
  x_col = "plate_x_tr",
  y_col = "plate_z",
  response_col = "wOBA",
  lg_avg_val = lg_avg_si_woba,
  plot_limits = c(-0.2, 0.3),
  scale_value = "wOBA vs. Average",
  title = "Opposite-Handed Two-Seams: wOBA",
  rev_color_scale = TRUE
)

# Sinker/Two-Seam Candidates ===================================================================================================================================

# Perhaps designated two-seams perform better outside to oppo-handed hitters, maybe inside from glove-side

# Identify pitchers who are 60%+ inside four-seams (Inside to same-handed hitters)
# Example: https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=1%7C4%7C7%7C&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2023%7C&hfSit=&player_type=pitcher&hfOuts=&hfOpponent=&pitcher_throws=&batter_stands=R&hfSA=&game_date_gt=&game_date_lt=&hfMo=&hfTeam=&home_road=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&pitchers_lookup%5B%5D=686613&metric_1=&group_by=pitch-type&min_pitches=0&min_results=0&min_pas=0&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc#results
sp_population <- 
  pitch_data %>%
    group_by(pitcher, player_name, game_year) %>%
    summarise(
      N = n(),
      arm_angle = round(mean(arm_angle, na.rm = TRUE)),
      n = sum(ifelse(inning <= 5, 1, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter((n / N) > .50 & N > 250) %>%
    select(-n, -N)

si_candidates_compt <-
  merge(
    sp_population,
    inside_pitch_data %>% 
      group_by(player_name, pitcher, game_year) %>% 
      summarise(
        N = n(),
        FF_n = sum(ifelse(pitch_type == "FF", 1, 0)),
        SI_n = sum(ifelse(pitch_type == "SI", 1, 0)),
        RV_100 = round(mean(delta_run_exp, na.rm = TRUE) * 100, 2),
        wOBA = round(sum(wOBA_value_fg, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE), 3),
        .groups = "drop"
      ),
  by = c("player_name", "pitcher", "game_year")
  ) %>%
    filter(N > 100) %>%
    mutate(
      FF_Usage = round((FF_n / N)*100, 1),
      SI_Usage = round((SI_n / N)*100, 1)
    )

# Add in four-seam movement, overall GB%
si_candidates_25 <-
  merge(
    si_candidates_compt,
    merge(
      pitch_data %>%
        filter(pitch_type == "FF" & game_year == 2025) %>%
        group_by(player_name, pitcher, game_year) %>%
        summarise(
          VMOV = round(mean(pfx_z, na.rm = TRUE), 1),
          HMOV = -1*round(mean(pfx_x, na.rm = TRUE), 1),
          .groups = "drop"
        ),
    gb_rates
  )
) %>%
  mutate(GB_pct = round(GB_pct*100, 1))

# Filter down to pitchers who throw majority four-seams inside to same-handed hitters, with high damage output
si_candidates_25 %>%
  filter((wOBA > lg_avg_si_woba) & FF_Usage >= 30 & SI_Usage <= 20) %>%
  arrange(desc(xwOBA)) %>%
  select(player_name, N, VMOV, HMOV, arm_angle, FF_Usage, SI_Usage, wOBA, GB_pct) %>%
  rename(
    FF_VMOV = VMOV,
    FF_HMOV = HMOV
  ) %>%
  View()

sinker_agg_stats %>%
  ggplot(aes(x = arm_angle)) + 
  geom_density(aes(colour = factor(Two_Seam))) +
  scale_color_discrete(name = "Legend", labels = c("Sinkers", "Two-Seams")) +
  labs(
    x = "Arm Angle",
    y = "Density",
    title = "Pitcher Arm Angle, by Pitch Type"
  ) + 
  scale_x_continuous(breaks = seq(-25, 55, by = 10)) +  
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  )

stuff_pred_comps <- 
  merge(
    stuff_preds %>%
      filter(pitch_type == "SI"),
    stuff_diff_preds %>%
      filter(pitch_type == "SI"),
    by = c("pitcher", "p_throws", "batter", "game_year", "game_date", "at_bat_number", "pitch_number", "pitch_type")
  ) %>%
  select(
    pitcher, p_throws, batter, game_year, game_date, at_bat_number, pitch_number,
    pfx_x_diff, pfx_z_diff, xRV_swing.x, xRV_take.x, xRV_stuff.x, xRV_swing.y, xRV_take.y, xRV_stuff.y
  ) %>%
  mutate(
    pfx_x_diff = ifelse(p_throws == "R", -1, 1)*round(pfx_x_diff*12),
    pfx_z_diff = round(pfx_z_diff*12),
    xRV_stuff_diff = xRV_stuff.y - xRV_stuff.x,
    xRV_swing_diff = xRV_swing.y - xRV_swing.x
  ) %>%
  filter(pfx_x_diff >= 0)

# Fit GAM model for xRV for sinker movement
stuff_diff_gam_model <- mgcv::gam(xRV_stuff_diff ~ s(pfx_x_diff, pfx_z_diff), data = stuff_pred_comps)

stuff_diff_x_seq <- seq(min(stuff_pred_comps$pfx_x_diff), max(stuff_pred_comps$pfx_x_diff), length.out = 100)
stuff_diff_y_seq <- seq(min(stuff_pred_comps$pfx_z_diff), max(stuff_pred_comps$pfx_z_diff), length.out = 100)
stuff_diff_grid <- expand.grid(pfx_x_diff = stuff_diff_x_seq, pfx_z_diff = stuff_diff_y_seq)

stuff_diff_grid$xRV_stuff_diff <- predict(stuff_diff_gam_model, newdata = stuff_diff_grid, na.action = "na.omit")

# Plot Sinker Movement vs. Smoothed Whiff%
ggplot(stuff_diff_grid, aes(x = pfx_x_diff, y = pfx_z_diff, fill = xRV_stuff_diff*100)) +
  geom_hex(stat = "identity") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0.0, 
    limits = c(-1, 1),
    oob = squish,
    name = "xRV/100 Diff"
  ) +
  labs(title = "Sinker Vertical vs. Horizontal Movement Deviation: xRV/100 Difference",
       x = "Horizontal Movement Deviation (in.)",
       y = "Vertical Movement Deviation (in.)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(stuff_pred_comps$pfx_x_diff), max(stuff_pred_comps$pfx_x_diff), by = 2)) +
  scale_y_continuous(breaks = seq(min(stuff_pred_comps$pfx_z_diff), max(stuff_pred_comps$pfx_z_diff), by = 2)) +
  theme(
    panel.grid = element_blank(), # Remove grid lines for clean look
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 17),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  ) + 
    scale_x_continuous(breaks = seq(0, 15, by = 2)) +
    scale_y_continuous(breaks = seq(-14, 2, by = 2)) +
    xlim(0, 15) +
    ylim(-14, 2)
  

# Fit GAM model for xRV for sinker movement
stuff_diff_swing_gam_model <- mgcv::gam(xRV_swing_diff ~ s(pfx_x_diff, pfx_z_diff), data = stuff_pred_comps)

stuff_diff_swing_x_seq <- seq(min(stuff_pred_comps$pfx_x_diff), max(stuff_pred_comps$pfx_x_diff), length.out = 100)
stuff_diff_swing_y_seq <- seq(min(stuff_pred_comps$pfx_z_diff), max(stuff_pred_comps$pfx_z_diff), length.out = 100)
stuff_diff_swing_grid <- expand.grid(pfx_x_diff = stuff_diff_swing_x_seq, pfx_z_diff = stuff_diff_swing_y_seq)

stuff_diff_swing_grid$xRV_swing_diff <- predict(stuff_diff_swing_gam_model, newdata = stuff_diff_swing_grid, na.action = "na.omit")

# Plot Sinker Movement vs. Smoothed Whiff%
ggplot(stuff_diff_swing_grid, aes(x = pfx_x_diff, y = pfx_z_diff, fill = xRV_swing_diff*100)) +
  geom_hex(stat = "identity") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0.0, 
    limits = c(-2, 2),
    oob = squish,
    name = "xRV/100 Diff"
  ) +
  labs(title = "Sinker Vertical vs. Horizontal Movement Deviation: xRV/100 Difference (Swings)",
       x = "Horizontal Movement Deviation (in.)",
       y = "Vertical Movement Deviation (in.)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(stuff_pred_comps$pfx_x_diff), max(stuff_pred_comps$pfx_x_diff), by = 2)) +
  scale_y_continuous(breaks = seq(min(stuff_pred_comps$pfx_z_diff), max(stuff_pred_comps$pfx_z_diff), by = 2)) +
  theme(
    panel.grid = element_blank(), # Remove grid lines for clean look
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 17),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
  ) + 
  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  scale_y_continuous(breaks = seq(-14, 2, by = 2)) +
  xlim(0, 15) +
  ylim(-14, 2)
  
