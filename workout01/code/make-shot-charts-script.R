#Title: make-shot-charts-script.R
#Author: Kyle McEvoy, Student Stat 133, Spring 2019
#Purpose: To create shot charts for 5 NBA players using shot-chart dataset from the 2016-2017 nba season
#Input(s): shots-data.csv, found in "workout01/data" directory, "nba-court.jpg" in "workout01/images" directory
#Output(s): Shotcharts for the Curry, Durant, Klay, Draymond and Iguodala

library(jpeg)
library(grid)
library(ggplot2)
library(readr)
library(dplyr)


# Importing Data ----------------------------------------------------------

shots_data <- read_csv(file = "data/shots-data.csv", col_names = TRUE, 
                       col_types = cols_only(
                         team_name = col_character(),
                         game_date = col_character(),
                         season = col_character(),
                         period = col_integer(),
                         minutes_remaining = col_integer(),
                         seconds_remaining = col_integer(),
                         shot_made_flag = col_character(),
                         action_type = col_character(),
                         shot_type = col_factor(),
                         shot_distance = col_integer(),
                         opponent = col_character(),
                         x = col_double(),
                         y = col_double(),
                         player = col_character(),
                         minute = col_double()
                       )
                      )


# Separating Data by Player -----------------------------------------------

klay <- filter(shots_data, player == "Klay Thompson")
curry <- filter(shots_data, player == "Stephen Curry")
durant <- filter(shots_data, player == "Kevin Durant")
iguodala <- filter(shots_data, player == "Andre Iguodala")
draymond <- filter(shots_data, player == "Draymond Green")




# Image Importing ---------------------------------------------------------

parquet_background <- "images/nba-court.jpg"

parquet_img <- rasterGrob(
  readJPEG(parquet_background),
  width = unit(1, "npc"),
  height = unit(1, "npc"))


# Shot Charts -------------------------------------------------------------


klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))

ggsave(filename = "images/klay-thompson-shot-chart.pdf",
       width = 6.5, height = 5)

kd_shot_chart <- ggplot(data = durant) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))


ggsave(filename = "images/kevin-durant-shot-chart.pdf",
       width = 6.5, height = 5)

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))


ggsave(filename = "images/stephen-curry-shot-chart.pdf",
       width = 6.5, height = 5)

iggy_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))


ggsave(filename = "images/andre-iguodala-shot-chart.pdf",
       width = 6.5, height = 5)

dray_shot_chart <- ggplot(data = draymond) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))


ggsave(filename = "images/draymond-green-shot-chart.pdf",
       width = 6.5, height = 5)

facet_shot_chart <- ggplot(data = shots_data) +
  annotation_custom(parquet_img, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  facet_wrap( ~ player) +
  ggtitle('Shot Charts: Golden State Warriors (2016-2017 NBA season)') +
  theme_minimal() +
  xlab("Distance from the Basket (in Inches)") +
  ylab("Distance from the Basket (in Inches)") +
  labs(color = "Shot Success") +
  scale_color_manual(labels = c("Shot Missed", "Shot Made"), values = c("red", "green"))

ggsave(filename = "images/warriors-shot-charts.pdf",
       width = 8, height = 7, unit = "in")

ggsave(filename = "images/warriors-shot-charts.png",
       width = 8, height = 7, units = "in", device = "png")
  
