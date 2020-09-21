# =========================================================================
# Title: Merging features from NBA tables
#
# Description:
# This script contains R code to clean & merge features from each table:
# 'Roster', 'Totals', 'Salaries', and 'Team Misc'. Once all needed 
# features are ready, they're merged into a data.frame and then saved as a 
# csv file.
#
# =========================================================================

# a couple libraries needed for cleaning strings (stringr) and manipulating 
# dataframes (dplyr)
library(dplyr)
library(stringr)

# column names for each table when reading in csv files
roster_col_names <- c('index', 'No.', 'Player', 'Position', 'Height', 'Weight',
                     'Birthdate', 'Country', 'Experience', 'College')

salary_col_names <- c('Index', 'Rank','Player', 'Salary')
total_col_names <- c('index', 'rank', 'player', 'age', 'games',
                                'games_started', 'minutes_played', 'field_goals', 
                                'field_goal_attempts', 'field_goal_%', 
                                '3P_field_goals', '3P_attempts', '3P_%',
                                '2P_field_goals', '2P_attempts', '2P_%', 
                                'effective_FG_%', 'free_throws',
                                'free_throw_attempts', 'FT_%','offensive_rebounds',
                                'defensive_rebounds', 'total_rebounds', 'assists',
                                'steals', 'blocks', 'turnovers', 'personal_fouls', 
                                'pts')
misc_col_names <-  c('index', 'label', 'wins', 'losses', 'PW', 'PL', 'MOV', 'SOS', 
                    'SRS', 'ORtg', 'DRtg', 'pace', 'FTr', '3PAr', 'eFG%', 'TOV%', 
                    'ORB%', 'FT/FGA', 'eFG%2', 'TOV%2', 'DRB%', 'FT/FGA2', 'arena',
                    'attendance')

# vectors to hold data from needed features over ten years
total_team_experience <- c()
num_players <- c()
salaries <- c()
salaries_min <- c()
salaries_max <- c()
salaries_median <- c()
total_points <- c()
wins <- c()
losses <- c()

# set the file path to the data and initialize a vector that holds the file names
file_path <- '../data/rawdata/'
files = c('roster', 'salaries', 'totals', 'team_misc')

# initialize a vector with team names in abbreviated format
team_names <- c(
  'ATL', 'BOS', 'BRK', 'CHI', 'CHO', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
  'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK',
  'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS'
)

# need list of seasons
seasons <- 2010:2019

# ========================================= #
# Looping through each team's seasonal info #
# ========================================= #


for (file in files) {
  for (season in seasons) {
    
    # do correct set of operations based on the file
    if (file == 'roster') {
      
      for (i in 1:length(team_names)) {
        #grab the roster csv and read in
        roster <- read.csv(paste0(file_path, file, '/', season, '/', 
                                 team_names[i], '-', season, '-', file, 
                                 '.csv'), 
                          stringsAsFactors = FALSE, 
                          col.names=roster_col_names) 
        
        # also need salary table because we need to only consider roster players 
        # present on salaries table 
        salary <- read.csv(paste0(file_path, file, '/', season, '/', 
                                 team_names[i], '-', season, '-', file, '.csv'),
                          stringsAsFactors=FALSE,
                          col.names=sal_col_names)
        
        # clean Salary column & remove some erroneous entries (salary = $0 or Salary = NaN), 
        # then grab the Player column of remaining entries
        salary$Salary <- as.numeric(str_remove_all(salary$Salary, regex('[\\$,]')))
        salary <- salary[!is.na(salary$Salary), ]
        salary <- salary[salary$Salary != 0, ]
        salary_players <- salary$Player
        
        # consider only the experience of those on the salary table
        roster <- roster[roster$Player %in% salary_players, ]
        experience <- roster %>% select('Experience')
        
        # clean the experience column
        cleaned_experience <- as.numeric(str_replace_all(experience$Experience, 'R', 
                                                        '0'))
        # append to our vector holding experience
        total_team_experience <- c(total_team_experience, 
                                  sum(cleaned_experience))
      }
      
    } else if (file == 'salaries') {
      
      for (i in 1:length(team_names)) {
        #grab the salary table and read in 
        salary <- read.csv(paste0(file_path, file, '/', season, '/', team_names[i], 
                                 '-', season, '-', file, '.csv'),
                          stringsAsFactors=FALSE,
                          col.names=sal_col_names)
        
        #cleaned salary column
        salary$Salary <- as.numeric(str_remove_all(salary$Salary, regex('[\\$,]')))
        
        #remove players with 0 as salary and then count how many players remain for
        # our official team headcount
        
        #first, we remove players with NA as salary; this is only for one player
        # on the chicago bulls team for season 2015 who's contract got waived - 
        # he is not present on the roser list for this particular year so there
        # is no need to subtract his headcount from number of players
        salary <- salary[!is.na(salary$Salary), ]
        
        #then remove players with zero salary
        salary <- salary[salary$Salary != 0, ]
        
        # append the sum and min, max to our vectors
        salaries <- c(salaries, sum(salary$Salary))
        salaries_min <- c(salaries_min, min(salary$Salary))
        salaries_max <- c(salaries_max, max(salary$Salary))
        salaries_median <- c(salaries_median, median(salary$Salary))
        num_players <- c(num_players, nrow(salary))
      }
      
    } else if (file == 'totals') {
      
      for (i in 1:length(team_names)) {
        #grab the totals csv and read in 
        totals = read.csv(paste0(file_path, file, '/', season, '/', team_names[i], 
                                 '-', season, '-', file, '.csv'),
                          stringsAsFactors = FALSE,
                          col.names=total_col_names)
        
        # append total points to vector
        total_points = c(total_points, sum(totals$pts))
      }
      
    } else {
      
      # if we've reached here then file is "team_misc"
      for (i in 1:length(team_names)) {
        # grab the team_misc csv and read in 
        misc = read.csv(paste0(file_path, file, '/', season, '/', team_names[i], 
                               '-', season, '-misc.csv'), 
                        stringsAsFactors = FALSE,
                        col.names=misc_col_names)
        
        #extract first element from each column (team win and losses)
        wins = c(wins, misc$wins[1])
        losses = c(losses, misc$losses[1])
      }
      
    }
    
  } #end looping through seasons
  
} #end looping through files

# ==================================================== #
# Last minute touches before building final data frame #
# ==================================================== #

# build vector of seasons for final dataframe - essentially repeat 
# each season entry 30 times to cover each team for each season
season_col = rep(seasons, each = 30)

#build vector of team names for final dataframe - same as above but 
# repeat sequence 10 times instead of each entry
teams_col = rep(team_names, times = 10)

#build our final dataframe!
nba_frame = data.frame(season_col, teams_col, total_team_experience, num_players, salaries,
                       salaries_min, salaries_max, salaries_median, total_points, wins, losses, 
                       stringsAsFactors = FALSE)

# set column names that I want to work with
names(nba_frame) = c('season', 'team', 'total_experience', 'num_players', 'total_salaries',
                     'min_salary', 'max_salary', 'median_salary', 'total_points', 'wins', 'losses')

# save final dataframe as csv file for analysis :)
write.csv(nba_frame, '../data/clean_data/nba_frame.csv')