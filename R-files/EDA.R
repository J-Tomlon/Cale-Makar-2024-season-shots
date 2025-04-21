library(dplyr)
library(ggplot2)
library(sportyR)


setwd("./Colorado-Avalanche-2024-shots")
shots <- read.csv("./raw-data/shots_2024.csv")

CO_shots <- shots %>%
  mutate(shotType = as.factor(shots$shotType)) %>%
  mutate(isPlayoffGame = as.factor(shots$isPlayoffGame)) %>%
  mutate(period = as.factor(shots$period)) %>%
  mutate(team = as.factor(shots$team)) %>% 
  mutate(shotRush = as.factor(shots$shotRush)) %>%
  mutate(shotRebound = as.factor(shots$shotRebound)) %>%
  mutate(shootingTeamForwardsOnIce = as.factor(shots$shootingTeamForwardsOnIce)) %>%
  mutate(shootingTeamDefencemenOnIce = as.factor(shots$shootingTeamDefencemenOnIce)) %>%
  mutate(offWing = as.factor(shots$offWing)) %>%
  filter(shotType != "") %>%
  filter(teamCode == "COL") 

rink <- geom_hockey("nhl")
rink.right.o.zone <- geom_hockey("nhl", "offensive_zone", )

##########################
COL2024Shots <- rink.right.o.zone +
  geom_density_2d_filled(data = CO_shots,
                         aes(x = xCordAdjusted, y = yCordAdjusted), 
                         alpha = 0.3, 
                         bins = 6, 
                         show.legend = FALSE) +
  labs(title= "COL Shot Density for Current Season") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("#FFFFFF00",
                               "#D53E4F",
                               "#F46D43",
                               "#FDAE61",
                               "#FEE08B",
                               "#ABDDA4",
                               "#66C2A5",
                               "#3288BD",
                               "#5E4FA2"))

COL2024Shots

ggsave("./plots/Colorado_Shot_Density.jpg", plot= COL2024Shots, 
       width = 10, height = 10)

##########################
MacKinnon <- CO_shots %>%
  filter(shooterName == "Nathan MacKinnon")

MacKinnon_plot <- rink.right.o.zone +
  geom_density_2d_filled(data = MacKinnon,
                         aes(x = xCordAdjusted, y = yCordAdjusted), 
                         alpha = 0.3, 
                         bins = 6, 
                         show.legend = FALSE) +
  labs(title= "MacKinnon Shot Density for Current Season") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("#FFFFFF00",
                               "#D53E4F",
                               "#F46D43",
                               "#FDAE61",
                               "#FEE08B",
                               "#ABDDA4",
                               "#66C2A5",
                               "#3288BD",
                               "#5E4FA2"))

MacKinnon_plot

ggsave("./plots/MacKinnon_Shot_Density.jpg", plot= MacKinnon_plot, 
       width = 10, height = 10)

##########################
Makar <- CO_shots %>%
  filter(shooterName == "Cale Makar")

Makar_shots_plot <- rink.right.o.zone +
  geom_density_2d_filled(data = Makar,
                         aes(x = xCordAdjusted, y = yCordAdjusted), 
                         alpha = 0.3, 
                         bins = 6, 
                         show.legend = FALSE) +
  labs(title= "Makar Shot Density for Regular Current Season") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("#FFFFFF00",
                               "#D53E4F",
                               "#F46D43",
                               "#FDAE61",
                               "#FEE08B",
                               "#ABDDA4",
                               "#66C2A5",
                               "#3288BD",
                               "#5E4FA2"))

Makar_shots_plot

ggsave("./plots/Makar_Shot_Density.jpg", plot= Makar_shots_plot, 
       width = 8, height = 8)

Makar_goals <- Makar %>%
  filter(goal== 1)
  
Makar_goals_plot <- rink.right.o.zone +
  geom_density_2d_filled(data = Makar_goals,
                         aes(x = xCordAdjusted, y = yCordAdjusted), 
                         alpha = 0.3, 
                         bins = 6, 
                         show.legend = FALSE) +
  labs(title= "Makar Goal Density for Current Regular Season") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("#FFFFFF00",
                               "#D53E4F",
                               "#F46D43",
                               "#FDAE61",
                               "#FEE08B",
                               "#ABDDA4",
                               "#66C2A5",
                               "#3288BD",
                               "#5E4FA2"))
Makar_goals_plot

ggsave("./plots/Makar_Goals_Density.jpg", plot= Makar_goals_plot, 
       width = 8, height = 8)

