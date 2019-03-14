#Data Dictionary for workout01/data/shots-data.csv

Dataset derived from 5 individual player data .csv files contained in the data folder of workout01. Dataset created using the script make-shots-data-script.R in workout01/code directory.

**team_name**: Name of the Player's team. Character variable. 30 teams make up the NBA. 
  
**game_date**: Date the game was played for the shot. Character variable.  
  
**season**: Season in which the game was played.  Only the year the season began in is listed. Character.  All observations in this data set take value "2016"
  
**period**: Quarter of the game during which the shot was taken. There are four quarters each of twelve minutes. Integer values from 1 to 4.
  
**minutes_remaining**: When the shot was taken how many minutes remained in the current period the seconds in addition to the minutes records in seconds_remaining.  Integer values.
  
**seconds_remaning**: When the shot was taken how many seconds remained in addition to the minutes remaining in the current period. Integer values from 0 to 60 seconds.
  
**shot_made_flag**: Categorical value imported as a character. Takes value "shot_yes" if shot was made and "shot_no" if shot was missed. Converted values from "y" and "n" respectively in original data.
  
**action_type**: Description of the type of shot attempted and the circumstances under which it was taken.  
Can include type of basketball move, or teammate actions that led to the shot. Character variable.
  
**shot_type**: variable indicates whether the shot was a 2-point field goal attempt or 3-point field goal attempt.  String used as factor.
  
**shot_distance**: Straight-line distance to the basket from the shot location (measured in feet).  
Takes integer values from 0 to 71.  Mean of 15.58.  
  
**opponent**: Opposing team against whom the shot was taken. Character.
    
**x and y**: Refer to the coordinates in inches on the court from where the shot was taken. In reference to the basket, shots to the left of the basket are assigned negative values.  
* Shots taken on the court but behind the basket are also assigned negative values.  Integer.
* x takes values from -248 to 246.
* y takes values from -39 to 717. Extremely large values of y are rare. They relate to end of quarter heaves.

**minute**:  The minute of the game when the shot was taken from the first minute to minute 48. Integer values.  

**player**: Character variable.  The name of the player taking the shot.  
* In this data set, there are five players "Stephen Curry", "Kevin Durant", "Klay Thompson", "Andre Iguodala" and "Draymond Green"  
	