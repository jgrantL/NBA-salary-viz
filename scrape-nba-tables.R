# =========================================================================
# Title: Scraping tables from raw html files
#
# Description:
# This script contains R code to scrape the tables 'Roster', 'Totals', 
# 'Salaries', and 'Team Misc' for a specific NBA team. Each table is read 
# as a data.frame, which is then exported as a csv file to the 
# corresponding subdirectory in the 'rawdata/' folder
#
# Note: the totals, salary, and team_misc tables are commented out in 
# source html file, thus we use a scraping method by identifying the start
# and end of a table and reading html file based on table range.
#
# =========================================================================

# we'll need the XML library to scrape tables
library(XML)

# make sure subdirectories exist inside 'rawdata/'
data_dirs <- c("roster", "totals", "salaries", "team_misc")

for (d in data_dirs) {
  subdir <- paste0("rawdata/", d)
  if (!dir.exists(subdir)) {
    dir.create(subdir)
  }
}


# name abbrevitions of 30 NBA teams
team_names <- c(
  'ATL', 'BOS', 'BRK', 'CHI', 'CHO', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
  'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK',
  'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS'
)

# seasons covered
seasons <- 2010:2019


# extract html tables Roster, Totals, Salaries, and Team Misc
for (year in seasons) {
  
  # make sure folder exist inside subdirectories
  for (d in data_dirs) {
    subdir <- paste0("rawdata/", d, '/', year)
    if (!dir.exists(subdir)) {
      dir.create(subdir)
    }
  }
  
  html_files <- paste0(team_names, '-', year, '.html')
  
  # loop that will scrape each table for each team during given season
  for (team in 1:length(team_names)) {
    # Read html document (as a character vector) for a given team
    print(paste('scraping data for', team_names[team],
                'season', year))
    html_doc <- readLines(paste0('rawdata/html-files/', 
                                 year, '/', html_files[team]))
    
    
    # ====================================================
    # Scrape Roster table
    # ====================================================
    # initial line position of roster html table
    begin_roster <- grep('id="roster"', html_doc)
    
    # find the line where the html ends
    line_counter <- begin_roster
    
    while (!grepl("</table>", html_doc[line_counter])) {
      line_counter <- line_counter + 1
    }
    # read in table as data.frame and export it as csv
    roster_tbl <- readHTMLTable(html_doc[begin_roster:line_counter])
    roster_csv <- paste0(team_names[team], '-', year, '-roster.csv')
    write.csv(roster_tbl, 
              file = paste0('rawdata/roster/', year, 
                            '/', roster_csv))
    
    # ====================================================
    # Scrape Totals table
    # ====================================================
    # initial line position of totals html table
    begin_totals <- grep('id="totals"', html_doc)
    
    # find the line where the html ends
    line_counter <- begin_totals
    while (!grepl("</table>", html_doc[line_counter])) {
      line_counter <- line_counter + 1
    }
    
    # read in table as data.frame and export it as csv
    totals_tbl <- readHTMLTable(html_doc[begin_totals:line_counter])
    totals_csv <- paste0(team_names[team], '-', year, '-totals.csv')
    write.csv(totals_tbl, 
              file = paste0('rawdata/totals/', year, 
                            '/', totals_csv))
    
    
    # ====================================================
    # Scrape Salaries table
    # ====================================================
    # initial line position of salaries html table
    begin_salaries <- grep('id="salaries"', html_doc)
    
    # find the line where the html ends
    line_counter <- begin_salaries
    while (!grepl("</table>", html_doc[line_counter])) {
      line_counter <- line_counter + 1
    }
    
    # read in table as data.frame and export it as csv
    salaries_tbl <- readHTMLTable(html_doc[begin_salaries:line_counter])
    salaries_csv <- paste0(team_names[team], '-', year, '-salaries.csv')
    write.csv(salaries_tbl, 
              file = paste0('rawdata/salaries/', year, 
                            '/', salaries_csv))
    
    
    # ====================================================
    # Scrape Team Misc table
    # ====================================================
    # initial line position of team_misc html table
    begin_misc <- grep('id="team_misc"', html_doc)
    
    # find the line where the html ends
    line_counter <- begin_misc
    while (!grepl("</table>", html_doc[line_counter])) {
      line_counter <- line_counter + 1
    }
    
    # read in table as data.frame and export it as csv
    misc_tbl <- readHTMLTable(html_doc[begin_misc:line_counter])
    misc_csv <- paste0(team_names[team], '-', year, '-misc.csv')
    write.csv(misc_tbl, 
              file = paste0('rawdata/team_misc/', year, 
                            '/', misc_csv))
    
  } # end looping through teams in a given season
  
} # end looping through seasons



