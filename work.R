library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(rsample)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)

# Reading in .csv files ------------------------------------------------------

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

Transfers <- read.csv("Transfers - Premier_League_Transfer_Activity.csv")
Expenditures <- read.csv("Expenditures - Premier_League_Standings.csv")
                                             
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

Premier_League_Standings <- read_csv("EPLStandings.csv",
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

# Fix columns in this dataset.

Wins <- read.csv("Wins - Premier_League_Standings.csv")
Losses <- read.csv("Losses - Premier_League_Standings.csv")


# Example manipulations of datasets  --------------------------------------------------

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
    

# Attempting to create a function to loop through .csv files --------------
  
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
    

# Creating the function for goals for and against ----------------------------------------------------------------

    datamaker <- function(csv, s, year){
      season <- read.csv(csv)
      
      teamsinseason1 <- season %>%
        select(HomeTeam) 
      
      teamsinpremierleague <- unique(teamsinseason1)
      vectorofteamsinpremierleague <- teamsinpremierleague$HomeTeam
      
      # Setting the number of total goals in the Premier League from 
      # the 00/01 season to 0.
      
      teamsinpremierleague$totalgoals <- 0
      teamsinpremierleague$totalgoals_conceded <- 0
      teamsinpremierleague$total_away_goals <- 0
      teamsinpremierleague$total_homegoals_goals <- 0
      teamsinpremierleague$total_away_goals_conceded <- 0 #How many goals they conceded when they are the away team
      teamsinpremierleague$total_home_goals_conceded <- 0 #How many goals they conceded when they are the home team
      
      # Calculating the total number of goals scored by
      # each Premier League team in the 00/01 season.
      
      for (i in 1:length(vectorofteamsinpremierleague)) {
        homegoals <- season %>%
          select(HomeTeam, FTHG) %>%
          filter(HomeTeam == vectorofteamsinpremierleague[i])
        teamsinpremierleague$total_homegoals_goals[i] <- sum(homegoals$FTHG)
        
        awaygoals <- season %>%
          select(AwayTeam, FTAG) %>%
          filter(AwayTeam == vectorofteamsinpremierleague[i])
        teamsinpremierleague$total_away_goals[i] <- sum(awaygoals$FTAG)
        
        totalgoals = sum(homegoals$FTHG) + sum(awaygoals$FTAG)
        teamsinpremierleague$totalgoals[i] <- totalgoals
        
      }
      
      for (i in 1:length(vectorofteamsinpremierleague)) {
        homegoals_conceded <- season %>%
          select(HomeTeam, FTAG) %>%
          filter(HomeTeam == vectorofteamsinpremierleague[i])
        teamsinpremierleague$total_home_goals_conceded[i] <- sum(homegoals_conceded$FTAG)
        
        awaygoals_conceded <- season %>%
          select(AwayTeam, FTHG) %>%
          filter(AwayTeam == vectorofteamsinpremierleague[i])
        teamsinpremierleague$total_away_goals_conceded[i] <- sum(awaygoals_conceded$FTHG)
        
        totalgoals_conceded = sum(homegoals_conceded$FTAG) + sum(awaygoals_conceded$FTHG)
        teamsinpremierleague$totalgoals_conceded[i] <- totalgoals_conceded
        
      }
      
      teamsinpremierleague$Season <- s
      teamsinpremierleague$goal_difference <- teamsinpremierleague$totalgoals - teamsinpremierleague$totalgoals_conceded
      teamsinpremierleague$year <- year
      return(teamsinpremierleague)
      
    }
    
    # Testing to see if it works 
    
    D1 <- datamaker(csv = "2000-01.csv", s = "00/01", year = 2001)
    D1
    D2 <- datamaker(csv = "2001-02.csv", s = "01/02", year = 2002)    
    D3 <- datamaker(csv = "2002-03.csv", s = "02/03", year = 2003) 
    
  
    # Setting up the code -----------------------------------------------------
    
    ## Setting up the year format for the csv and season
    y1 <- 2000:2019
    y2 <- 01:20
    y1
    y2
    
    y2 <- sprintf("%02d",y2)
    y2
    
    season_years <- paste0(y1,"-",y2) # setting the season year
    season_years
    
    ## Setting up a vector of csvs to read in
    data_to_read <- paste0(season_years, ".csv") # you can remove the "Premier League Datasets/" if that is not needed
    data_to_read
    
    years <- 2001:2020
    
    # Testing to make sure it works
    D5 <- datamaker(csv = data_to_read[5], s = season_years[5], year = years[5])    
    
    # Making the data ---------------------------------------------------------
    
    Prem_data <- c()
    
    for (i in 1:length(data_to_read)) {
      d <- datamaker(csv = data_to_read[i], s = season_years[i], year = years[i])
      
      Prem_data <- rbind(Prem_data, d)
    }
    

# Renaming columns in D1, Premier_League_Standings ----------------------------------------------------------------

    Prem_data$Season <- NULL
    
    Goals <- Prem_data %>%
      rename("Team" = "HomeTeam") %>%
      rename("Goals For" = "totalgoals") %>%
      rename("Goals Against" = "totalgoals_conceded") %>%
      rename("Away Goals For" = "total_away_goals") %>%
      rename("Home Goals For" = "total_homegoals_goals") %>%
      rename("Away Goals Against" = "total_away_goals_conceded") %>%
      rename("Home Goals Against" = "total_home_goals_conceded") %>%
      rename("Goal Difference" = "goal_difference") %>%
      rename("Season" = "year")
      
    Wins <- Wins %>%
      rename("2001" = "X00.01") %>%
      rename("2002" = "X01.02") %>%
      rename("2003" = "X02.03") %>%
      rename("2004" = "X03.04") %>%
      rename("2005" = "X04.05") %>%
      rename("2006" = "X05.06") %>%
      rename("2007" = "X06.07") %>%
      rename("2008" = "X07.08") %>%
      rename("2009" = "X08.09") %>%
      rename("2010" = "X09.10") %>%
      rename("2011" = "X10.11") %>%
      rename("2012" = "X11.12") %>%
      rename("2013" = "X12.13") %>%
      rename("2014" = "X13.14") %>%
      rename("2015" = "X14.15") %>%
      rename("2016" = "X15.16") 
    
    Expenditures <- Expenditures %>%
      rename("2001" = "X2001") %>%
      rename("2002" = "X2002") %>%
      rename("2003" = "X2003") %>%
      rename("2004" = "X2004") %>%
      rename("2005" = "X2005") %>%
      rename("2006" = "X2006") %>%
      rename("2007" = "X2007") %>%
      rename("2008" = "X2008") %>%
      rename("2009" = "X2009") %>%
      rename("2010" = "X2010") %>%
      rename("2011" = "X2011") %>%
      rename("2012" = "X2012") %>%
      rename("2013" = "X2013") %>%
      rename("2014" = "X2014") %>%
      rename("2015" = "X2015") %>%
      rename("2016" = "X2016") 
    
    Losses <- Losses %>%
      rename("2001" = "X2001") %>%
      rename("2002" = "X2002") %>%
      rename("2003" = "X2003") %>%
      rename("2004" = "X2004") %>%
      rename("2005" = "X2005") %>%
      rename("2006" = "X2006") %>%
      rename("2007" = "X2007") %>%
      rename("2008" = "X2008") %>%
      rename("2009" = "X2009") %>%
      rename("2010" = "X2010") %>%
      rename("2011" = "X2011") %>%
      rename("2012" = "X2012") %>%
      rename("2013" = "X2013") %>%
      rename("2014" = "X2014") %>%
      rename("2015" = "X2015") %>%
      rename("2016" = "X2016") 
    
    Wins$`X99.00` <- NULL


# Plots -------------------------------------------------------------------

plot1 <- Premier_League_Standings %>%
    pivot_longer(!Team, 
                   names_to = "Year", 
                   values_to = "Place")
    plot1$Year <- as.numeric(plot1$Year)
    plot1$Place <- factor(plot1$Place, levels = rev(levels(factor(plot1$Place))))
    select_team <- unique(plot1$Team)    
    
output$plot <- renderPlot({
      plot1 %>% 
        filter(Team == input$team & !is.na(Place)) %>%
        ggplot(aes(x = Year, y = Place, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Standings",
             subtitle = "From the 1999-2000 season to the 2015-2016 season",
             x = "Year", 
             y = "Standings") +
        theme_hc()
    }
    )

plot2 <- WinsPivoted 
    plot2$Season <- as.numeric(plot2$Season)
    plot2$Wins <- factor(plot2$Wins, levels = rev(levels(factor(plot2$Wins))))
    select_team <- unique(plot2$Team)
    
plot3 <- LossesPivoted
    plot1$Year <- as.numeric(plot1$Year)
    plot1$Place <- factor(plot1$Place, levels = rev(levels(factor(plot1$Place))))
    select_team <- unique(plot1$Team)


# Regression model --------------------------------------------------------

WinsPivoted <- Wins %>%
  pivot_longer(names_to = "Season",
               values_to = "Wins",
               cols = !Team)

ExpendituresPivoted <- Expenditures %>%
  pivot_longer(names_to = "Season",
               values_to = "Money Spent",
               cols = !Team) %>%
  rename("Expenses" = "Money Spent")

LossesPivoted <- Losses %>%
  pivot_longer(names_to = "Season",
               values_to = "Losses",
               cols = !Team)
  

Semi <- merge(Goals, WinsPivoted, by = c("Team", "Season"))
Almost <- merge(Semi, LossesPivoted, by = c("Team", "Season"))
FULLDATASET <- merge(Almost, ExpendituresPivoted, by = c("Team", "Season"))

MODELFULLDATASET <- FULLDATASET %>%
  rename("For" = "Goals For") %>%
  rename("Against" = "Goals Against") %>%
  rename("expenditures" = "Money Spent")

model <- stan_glm(formula = expenditures ~ Wins + Losses + For + Against,
                  data = MODELFULLDATASET,
                  refresh = 0,
                  family = gaussian())

print(model, digits = 3)

tbl_regression(model) %>%
  as_gt() %>%
  tab_header(title = "Regression of Money Spent",
             subtitle = "The Effect of Money Spent on Wins, Losses, Goals For, and Goals Against")







