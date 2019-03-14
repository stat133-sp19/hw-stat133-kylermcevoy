#title: make-shots-data-script.R
#description: This script is used to prepare the shot chart data for workout1 of the course Stat 133, Spring 2019.
#Five players shot chart data will be combined into a single data.frame for use in producing a report.
#input(s): Requires five .csv files located in the data directory of the workout1 directory.
#output(s): .csv file shots-data.csv containing the shot chart data for the five players.  Also a text file
#shots-data-summary.txt containing the summary function output of the new data set.
##IMPORTANT: Please set working directory to workout01 directory before running code.

library(dplyr)
library(readr)


# Importing Data ----------------------------------------------------------



curry <- read_csv(file = "data/stephen-curry.csv", col_names = TRUE, 
                  col_types = cols(
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
                    y = col_double()
                  )
                  )

klay <- read_csv(file = "data/klay-thompson.csv", col_names = TRUE, 
                 col_types = cols(
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
                   y = col_double()
                 )
                 )

draymond <- read_csv(file = "data/draymond-green.csv", col_names = TRUE, 
                     col_types = cols(
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
                       y = col_double()
                     )
                    )

durant <- read_csv(file = "data/kevin-durant.csv", col_names = TRUE, 
                   col_types = cols(
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
                     y = col_double()
                   )
                  )

iguodala <- read_csv(file = "data/andre-iguodala.csv", col_names = TRUE, 
                     col_types = cols(
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
                       y = col_double()
                     )
                    )




# Adding and Changing Variables -------------------------------------------



#Adding a player column to each dataframe.

iguodala <- mutate(iguodala, player = "Andre Iguodala")

curry <- mutate(curry, player = "Stephen Curry")

durant <- mutate(durant, player = "Kevin Durant")

klay <- mutate(klay, player = "Klay Thompson")

draymond <- mutate(draymond, player = "Draymond Green")


#Changing shot_made_flag to more readable value


curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"

curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"

klay$shot_made_flag[klay$shot_made_flag == "y"] <- "shot_yes"

klay$shot_made_flag[klay$shot_made_flag == "n"] <- "shot_no"

durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"

durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"

iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"

iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"

draymond$shot_made_flag[draymond$shot_made_flag == "y"] <- "shot_yes"

draymond$shot_made_flag[draymond$shot_made_flag == "n"] <- "shot_no"


#Adding Minute Variable


curry <- mutate(curry, minute = (period - 1) * 12 + (12 - minutes_remaining))

klay <- mutate(klay, minute = (period - 1) * 12 + (12 - minutes_remaining))

durant <- mutate(durant, minute = (period - 1) * 12 + (12 - minutes_remaining))

draymond <- mutate(draymond, minute = (period - 1) * 12 + (12 - minutes_remaining))

iguodala <- mutate(iguodala, minute = (period - 1) * 12 + (12 - minutes_remaining))




# Exporting Summaries---------------------------------------------------------------



sink(file = "output/andre-iguodala-summary.txt")

summary(iguodala)

sink()

sink(file = "output/draymond-green-summary.txt")

summary(draymond)

sink()

sink(file = "output/kevin-durant-summary.txt")

summary(durant)

sink()

sink(file = "output/stephen-curry-summary.txt")

summary(curry)

sink()

sink(file = "output/klay-thompson-summary.txt")

summary(klay)

sink()



# Combining Datasets and Exporting------------------------------------------------------


shots_data <- rbind(curry, durant, draymond, iguodala, klay)

write.csv(
  x=shots_data,
  file = "data/shots-data.csv"
  )

sink(file = "output/shots-data-summary.txt")

summary(shots_data)

sink()
