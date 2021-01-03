# Install the relevant packages
install.packages(c("tidyverse", "readr"))
install.packages("remotes")
remotes::install_github("ewenme/understatr")

# Load the packages
library(tidyverse)
library(readr)
library(understatr)

# Create a vector of years
year_vec <- 2014:2020

# Read all the player data available
all_years <- map_df(year_vec, function(v) {
  i<-1
  team_stats <- get_league_teams_stats("EPL",v)
  teams <- unique(team_stats$team_name)
  shotx <- get_team_players_stats(teams[i],v)
  
  for (i in 2:length(teams)) {
    shoti<-get_team_players_stats(teams[i], v)
    
    shotx<-bind_rows(shotx,shoti)
  }
  return(shotx)
})

# Write to a csv file 
write_csv(all_years, "Player Statistics 2014 - 2020.csv")