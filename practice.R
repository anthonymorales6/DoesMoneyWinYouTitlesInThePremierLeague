library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(readr)
library(dplyr, warn.conflicts = FALSE)

# Reading in .csv files
# Premier_League_Transfer_Activity is the transfer balances of 
# every Premier League team from the 16/17 season to the 20/21 season.

Premier_League_Transfer_Activity <- read_csv("Premier League Transfer Activity Data - Sheet1.csv",
col_types = 
    cols(
    Club = col_character(),
    `20/21` = col_character(),
    `19/20` = col_character(),
    `18/19` = col_character(),
    `17/18` = col_character(),
    `16/17` = col_character(),
    Total = col_character()))
                                             
# Premier_League_Dataset is every game from the 00/01 season to the 17/18 season.

Premier_League_Dataset <- read_csv("Premier League Dataset.csv",
col_types = 
    cols(
    .default = col_double(),
    Date = col_character(),
    HomeTeam = col_character(),
    AwayTeam = col_character(),
    FTR = col_character(),
    HM1 = col_character(),
    HM2 = col_character(),
    HM3 = col_character(),
    HM4 = col_character(),
    HM5 = col_character(),
    AM1 = col_character(),
    AM2 = col_character(),
    AM3 = col_character(),
    AM4 = col_character(),
    AM5 = col_character(),
    HTFormPtsStr = col_character(),
    ATFormPtsStr = col_character())) 

totalspending <- as.numeric(Premier_League_Transfer_Activity_Data_Sheet1$`16/17`[1])
goalsscored <- as.numeric(Premier_League_Dataset$FTHG[6298])

totalspending/goalsscored

# EPLStandings is the standings of every Premier League team from the
# 99/00 season to the 15/16 season.

EPLStandings <- read_csv("EPLStandings.csv",
col_types = 
  cols(
    Team = col_character(),
    `2000` = col_double(),
    `2001` = col_double(),
    `2002` = col_double(),
    `2003` = col_double(),
    `2004` = col_double(),
    `2005` = col_double(),
    `2006` = col_double(),
    `2007` = col_double(),
    `2008` = col_double(),
    `2009` = col_double(),
    `2010` = col_double(),
    `2011` = col_double(),
    `2012` = col_double(),
    `2013` = col_double(),
    `2014` = col_double(),
    `2015` = col_double(),
    `2016` = col_double()))

# Calculating the number of goals Arsenal has scored at home from the 00/01 season
# to the 17/18 season.

arsenalhomegoals <- Premier_League_Dataset %>%
select(HomeTeam, FTHG) %>%
filter(HomeTeam == "Arsenal")
arsenalhomegoals

# Calculating the number of goals Arsenal has scored away from home from the 00/01 season
# to the 17/18 season.

arsenalawaygoals <- Premier_League_Dataset %>%
  select(AwayTeam, FTAG) %>%
  filter(AwayTeam == "Arsenal")
arsenalawaygoals

# Selecting all the home teams that have played in
# the Premier League from the 00/01 season to the 17/18 season.
# Considering dropping duplicates.

teams <- Premier_League_Dataset %>%
select(HomeTeam) 
teams

# Selecting Leeds United from teamsinpremierleague.

teamsinpremierleague <- unique(teams)
vectorofteamsinpremierleague <- teamsinpremierleague$HomeTeam
vectorofteamsinpremierleague[5]

# Setting the number of total goals in the Premier League from the 00/01 season to
# the 17/18 season to 0.

goals = c()
teamsinpremierleague$totalgoals <- 0

# Calculating the number of goals scored from each Premier League
# team from the 00/01 season to 17/18 season.
# Make a list/array 

  for (i in 1:length(vectorofteamsinpremierleague)) {
    homegoals <- Premier_League_Dataset %>%
      select(HomeTeam, FTHG) %>%
      filter(HomeTeam == vectorofteamsinpremierleague[i])
    
    awaygoals <- Premier_League_Dataset %>%
      select(AwayTeam, FTAG) %>%
      filter(AwayTeam == vectorofteamsinpremierleague[i])
      totalgoals = sum(homegoals$FTHG) + sum(awaygoals$FTAG)
      teamsinpremierleague$totalgoals[i] <- totalgoals
  }
homegoals
awaygoals
totalgoals

Premier_League_Dataset %>%
  select(HomeTeam, AwayTeam, FTHG, FTAG)
  rename("Home" = "HomeTeam") %>%
  rename("Away" = "AwayTeam") %>%
  rename("Home team goals" = "FTHG") %>%
  rename("Away team goals" = "FTAG")

# Each season ---------------------------------------------------------------
    
    season1 <- read.csv("2000-01.csv")

    teamsinseason1 <- season1 %>%
      select(HomeTeam) 
    teamsinseason1
    
    # Selecting Leeds United from teamsinpremierleague.
    
    teamsinpremierleague <- unique(teamsinseason1)
    vectorofteamsinpremierleague <- teamsinpremierleague$HomeTeam
    vectorofteamsinpremierleague[5]
    
    # Setting the number of total goals in the Premier League from 
    # the 00/01 season to 0.
    
    goals = c()
    teamsinpremierleague$totalgoals <- 0
    
    # Calculating the total number of goals scored by
    # each Premier League team in the 00/01 season.
    
    for (i in 1:length(vectorofteamsinpremierleague)) {
      homegoals <- season1 %>%
        select(HomeTeam, FTHG) %>%
        filter(HomeTeam == vectorofteamsinpremierleague[i])
      homegoals
      
      awaygoals <- season1 %>%
        select(AwayTeam, FTAG) %>%
        filter(AwayTeam == vectorofteamsinpremierleague[i])
      awaygoals
      
      totalgoals = sum(homegoals$FTHG) + sum(awaygoals$FTAG)
      teamsinpremierleague$totalgoals[i] <- totalgoals
      totalgoals
    }
    
    teamsinpremierleague$season <- "00/01"
      
    # Create a function to read in .csv files from each season and 
    # run it through the loop written.
    # Make sure to append at the end of the tibble.
    # Filtering out by season and year to group by.
    
    season1 <- read.csv("2000-01.csv")
    season1$Season <- 2000
    
    datamaker <- function(csv, year){
      season <- read.csv(csv)
      season$Season <- year
    }
    datamaker("2000-01.csv", "00/01")
    
    test <- "2000-01.csv"
    testdata <- read.csv(test)
    testdata$season <- "2000-01"
    
    #

    

