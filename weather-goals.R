#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
library(xgboost)
library(Matrix)
library(elo)
library(data.table)
library(purrr)
#####################################################
weather <- fetch_results_afl(2023:2025)
colnames(weather)
weather <- weather %>% select(
  match.date, round.year, round.roundNumber, match.homeTeam.name, match.awayTeam.name, weather.weatherType
)
#####################################################
seasons <- 2023:2025
player_details_specific <- map_dfr(seasons, ~{
   fetch_player_details(season = .x, source = "AFL")
 })

position <- player_details_specific %>%
  mutate(
    Player = paste(firstName, surname),
    is_key_forward = as.integer(position == "KEY_FORWARD")
  ) %>%
  select(Player, is_key_forward) %>% filter(is_key_forward == 1)
#####################################################
stats <- fetch_player_stats_afl(2023:2025)
colnames(stats)
forward_positions <- c("CHF", "FF", "FPL", "FPR", "HFFR", "HFFL")

stats <- stats %>% select(
  utcStartTime, round.roundNumber, player.player.player.givenName, player.player.player.surname, player.player.position,
  goals, behinds, shotsAtGoal, goalAccuracy
) %>% filter(player.player.position %in% forward_positions)

stats <- stats %>%
  mutate(match.date = as_datetime(utcStartTime))  

stats <- stats %>%
  mutate(
    Player = paste(player.player.player.givenName, player.player.player.surname)
  )

stats <- stats %>%
  left_join(position, by = "Player")

stats <- stats %>%
  left_join(weather, by = "match.date")
#####################################################
colnames(stats)

forwards <- stats %>% mutate(
  Date = match.date, Round = round.roundNumber.x, Name = Player,
  Position = player.player.position, Weather = weather.weatherType
) %>% select(
  Date, Round, Name, Position, is_key_forward, goals, behinds, shotsAtGoal, goalAccuracy,
  Weather
)

colSums(is.na(forwards))
forwards <- forwards %>% mutate(is_key_forward = replace_na(is_key_forward, 0))
colSums(is.na(forwards))
#####################################################
accuracy_by_weather <- forwards %>%
  group_by(Weather) %>%
  summarise(
    avg_goal_accuracy = mean(goalAccuracy, na.rm = TRUE),
    sd_goal_accuracy = sd(goalAccuracy, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(avg_goal_accuracy))

print(accuracy_by_weather)
#####################################################
ggplot(forwards, aes(x = Weather, y = goalAccuracy)) +
  geom_boxplot() +
  labs(
    title = "Goal Accuracy by Weather Type",
    x = "Weather",
    y = "Goal Accuracy (%)"
  ) +
  theme_minimal()
#####################################################
anova_result <- aov(goalAccuracy ~ Weather, data = forwards)
summary(anova_result)

TukeyHSD(anova_result)
#####################################################
# Key forwards vs forwards
forwards %>%
  group_by(Weather, is_key_forward) %>%
  summarise(
    avg_goal_accuracy = mean(goalAccuracy, na.rm = TRUE),
    sd_goal_accuracy = sd(goalAccuracy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_goal_accuracy))
#####################################################
ggplot(forwards, aes(x = Weather, y = goalAccuracy, fill = factor(is_key_forward))) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    title = "Goal Accuracy by Weather and Forward Type",
    x = "Weather",
    y = "Goal Accuracy (%)",
    fill = "Key Forward"
  ) +
  theme_minimal()
#####################################################
anova2 <- aov(goalAccuracy ~ Weather * is_key_forward, data = forwards)
summary(anova2)
#####################################################
# Weather alone does not significantly impact goal accuracy.
# Key forwards are significantly more accurate, regardless of weather.
# There's no evidence that key forwards handle weather conditions differently than others.