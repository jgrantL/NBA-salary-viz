# =============================================================================
# Title: Download raw html files
#
# Description:
# Downloads html files from basketball-reference.com
# Each html file corresponds to a team, and it's downloaded to 
# the corresponding season folder, in subdirectory 'html-files/' in 'rawdata/'
#
# The desired html tables are in URL's having this form:
# "http://www.basketball-reference.com/teams/ATL/2019.html"
# "http://www.basketball-reference.com/teams/BOS/2019.html"
#
# =============================================================================

# commonality in URLs
basketref <- 'https://www.basketball-reference.com'

# 30 NBA team abbreviations to be used in URL
team_names <- c(
  'ATL', 'BOS', 'BRK', 'CHI', 'CHO', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
  'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK',
  'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS'
)

# vector of seasons
seasons <- 2010:2019


# number of teams 
num_teams <- length(team_names)


# downloading html documents into subdirectory 'rawdata/html-files/year'
for (year in seasons) {
  
  # make sure subdirectory for each season exists and if not yet, create it
  subdir <- paste0("rawdata/html-files/", year)
  if (!dir.exists(subdir)) {
    dir.create(subdir)
  }
  
  for (team in 1:num_teams) {
    
    # below's sequence of if-statements account for name changes among certain 
    # teams during seasons 2009-2019
    if (year <= 2012 & team_names[team] == 'BRK') {
      print(paste('downloading html file for NJN (former BRK)', 
                  year))
      url_doc <- paste0(basketref, '/teams/NJN/', year, '.html')
    } else if (year <= 2014 & team_names[team] == 'CHO') {
      print(paste('downloading html file for CHA (former CHO)', 
                  year))
      url_doc <- paste0(basketref, '/teams/CHA/', year, '.html')
    } else if (year <= 2013 & team_names[team] == 'NOP') {
      print(paste('downloading html file for NOH (former NOP)', 
                  year))
      url_doc <- paste0(basketref, '/teams/NOH/', year, '.html')
    } else {
      print(paste('downloading html file for', team_names[team], 
                  'season', year))
      url_doc <- paste0(basketref, '/teams/', team_names[team], 
                        '/', year, '.html')
    }
    
    # download html file from url
    download.file(
      url = url_doc,
      destfile = paste0('rawdata/html-files/', year, '/',
                        team_names[team], '-', year, '.html'))
  } # end looping thorugh teams
  
}# end looping through seasons