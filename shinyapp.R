library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(ggthemes)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(rsample)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)

# Reading in .csv files ---------------------------------------------------

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

# Transfers is how much money each Premier League Team has spent from the 00/01 season
# to the 15/16 season.

Transfers <- read.csv("Transfers - Premier_League_Transfer_Activity.csv")

# Expenditures is how much money each Premier League Team has spent each season from the 00/01 season
# to the 15/16 season.

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

# Premier_League_Standings is the standings of every Premier League team from the
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

# Wins is the total amount of wins of every Premier League team from the 00/01 season to 
# the 15/16 season.

Wins <- read.csv("Wins - Premier_League_Standings.csv")

# Losses is the total amount of losses of every Premier League team from the 00/01 season to 
# the 15/16 season.

Losses <- read.csv("Losses - Premier_League_Standings.csv")

# Setting up plots --------------------------------------------------------

# Creating plot1, plot2, plot3, plot4, plot5, plot6, and plot7.
# These plots are the plots for standings, money spent, wins, losses, goals for, 
# and goals against.
# I used base R code from the help of my TF, Dan Baissa.

plot1 <- Premier_League_Standings %>%
    pivot_longer(!Team, 
                 names_to = "Year", 
                 values_to = "Place")
    plot1$Year <- as.numeric(plot1$Year)
    plot1$Place <- factor(plot1$Place, levels = rev(levels(factor(plot1$Place))))
    select_team <- unique(plot1$Team)
    
    
plot2 <- WinsPivoted 
    plot2$Season <- as.numeric(plot2$Season)
    plot2$Wins <- factor(plot2$Wins, levels = rev(levels(factor(plot2$Wins))))
    select_team <- unique(plot2$Team)
    
plot3 <- LossesPivoted
    plot3$Season <- as.numeric(plot3$Season)
    plot3$Losses <- factor(plot3$Losses, levels = rev(levels(factor(plot3$Losses))))
    select_team <- unique(plot3$Team)
    
plot4 <- ExpendituresPivoted
    plot4$Season <- as.numeric(plot4$Season)
    plot4$Expenses <- factor(plot4$Expenses, levels = rev(levels(factor(plot4$Expenses))))
    select_team <- unique(plot4$Team)
    
plot5 <- MODELFULLDATASET %>%
    select(Team, Season, For) 
    plot5$Season <- as.numeric(plot5$Season)
    plot5$For <- factor(plot5$For, levels = rev(levels(factor(plot5$For))))
    select_team <- unique(plot5$Team)
    
plot6 <- MODELFULLDATASET %>%
    select(Team, Season, Against) 
    plot6$Season <- as.numeric(plot6$Season)
    plot6$Against <- factor(plot6$Against, levels = rev(levels(factor(plot6$Against))))
    select_team <- unique(plot6$Against)
    
# Organizing shinyapp tabs ------------------------------------------------
    
# This is where I edit the user interface of my shinyapp.
# I inserted images, descriptions, and other features to make my shinyapp
# more interesting and professional.
    
ui <- navbarPage(
    "Does Money Win You Titles in the Premier League?",
    tabPanel("Standings",
             titlePanel("Standings of each Premier League team"),
             p("The Premier League is the top-flight of English football. After 38 games, teams look
             to qualify for European competition such as the UEFA Champions League and the UEFA Europe League.
             The ultimate goal is to have the most points at the end of the season and win the Premier League. 
             3 points are awarded for a win, 1 point for a tie, and 0 points for a loss. The top 4 teams qualify for the UEFA Champions League, 
             the 5th team qualifies for the UEFA Europa League, and the bottom 3 teams have been relegated to the Championship,
             the second division of English football. The Premier League takes place each year, from August to July."),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot1$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot1"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("PremierLeagueLogo")
             )),
    tabPanel("Money Spent",
             titlePanel("Money spent from each Premier League team"),
             p("The Premier League has a wide range of teams when it comes to finances.
               Some teams spend less than $10 million a year while others spend over $60 million a year.
               Regardless, more money is being spent on transfers of new players each season. The
               transfer market has become inflated over the years and teams with less money seem to struggle the most.
               In the 2000-01 season, the highest amount a Premier League team spent was $61.93 million.
               In 2015-16 season, the highest amount a Premier League team spent was $229.01 million. See the difference?"),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot4$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot4"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("KaiHavertz"),
                 imageOutput("VirgilVanDijk"),
                 imageOutput("KevinDeBruyne")
            )),
    tabPanel("Wins",
             titlePanel("Wins from each team in the Premier League"),
             p("Each weekend, the goal of each Premier League team is to win. However, 
               not everyone can win. The only team, alongside Preston North End in the 1888-89 season,
               that has gone an undefeated season in the Premier League is Arsenal is the 2003-04 season.
               `The Invincibles` managed to win 26 games and tie 12 games, losing 0 and reaching a total of 90 points.
               No team has accomplished this feat since and they will remain in history for a long time to come."),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot2$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot2"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("Arsenal"),
                 imageOutput("Liverpool"),
                 imageOutput("TottenhamHotspurs")
             )),
    tabPanel("Loses",
             titlePanel("Loses from each team in the Premier League"),
             p("As much as many Premier League teams would like to succeed and win more than half of their games during the season,
               some struggle to win and even tie. Many say the battle to avoid relegation between the bottom 5 teams in the Premier League
               is just as exciting as the top 6 teams trying to win the league. Teams that are relegated to the the second division of 
               English football, or the Championship, have the opportunity to be promoted back into the Premier League by placing in the top 2 
               of the Championship or winning the Championship playoffs, a bracket played between those who place 3rd to 6th."),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot3$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot3"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("Bournemouth"),
                 imageOutput("Norwich"),
                 imageOutput("Fulham")
             )),
    tabPanel("Goals For",
             titlePanel("Goals scored from each team in the Premier League"),
             p("Everyone loves watching goals. Whether it would be a late equalizer, or even an own-goal,
               goals are what makes us fans happy and is ultimatelt what wins games. Scoring more goals, leads to more wins,
               which leads to titles. Top teams manage to score more than 60 goals in a single season, where those who
               struggle usually manage to score 30 goals or less."),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot5$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot5"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("JamieVardy")
             )),
    tabPanel("Goals Against",
             titlePanel("Goals conceded from each team in the Premier League"),
             p("There's a saying that offense wins games and defense wins championships. In some way,
               that's very true and applicable in this case. Typically, the teams that have less goals against them
               throughout the course of the season finish higher up in the standings, whereas those who
               have more goals againts and concede much more finish lower."),
             fluidPage(
                 selectInput("team", "Team", choices = unique(plot6$Team)),
                 selectInput("geom", "View", c("Point", "Line", "Smooth")),
                 plotOutput("plot6"),
                 p("Please note that the year 2001 refers to the 2000-01 season, and so on."),
                 imageOutput("PetrCech")
             )),
    tabPanel("Relationships",
             titlePanel("Correlations within this study"),
             p("The Premier League has a wide range of teams when it comes to finances.
               Some teams spend less than $10 million a year while others spend over $60 million a year.
               Regardless, more money is being spent on transfers of new players each season. The
               transfer market has become inflated over the years and teams with less money seem to struggle the most.
               In the 2000-01 season, the highest amount a Premier League team spent was $61.93 million.
               In 2015-16 season, the highest amount a Premier League team spent was $229.01 million. See the difference?"),
             fluidPage(
                 imageOutput("model")
             )),
    tabPanel("About", 
             titlePanel("About my project"),
             h3("My study"),
             p("Without a doubt, the Premier League is best football league in the world.
               More money is being spent each transfer window on players with the goal of winning the Premier League at the end of the season.
               The question is, does money win you titles in the Premier League? Does money help reach the Champions League? Europa League? 
               Does money help score more and concede less? Win more and lose less? This study will look at
               how much money each team in the Premier League has spent every transfer window from the 00/01 season up until the 17/18 season, 
               alongside taking a look at wins, losses, goals scored, goals conceded, and standings at the end of the Premier League season."),
             h3("Acknowledgements"),
             p("I would like to acknowledge Dan Baissa. He has helped me throughout this project and has always pushed me forward.
               He always offered to help and "),
             h3("About Me"),
             p("My name is Anthony Morales and I am a student at Harvard College. I'm studying economics and statistics, with a specialization in quantitative finance.
             I'm from Bay Shore, New York. My email is anthonymorales@college.harvard.edu."),
             uiOutput("tab")
    )
)
    
# Defining server logic ------------------------------------------
    
server <- function(input, output, session){
    url <- a("Github", href="https://www.github.com/anthonymorales6")
    output$tab <- renderUI({
        tagList("My", url)
    })
    plot_geom <- reactive({
        switch(input$geom,
               Point = geom_point(),
               Smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               Jitter = geom_jitter(),
               Line = geom_line())
    })
    
    # Printing out images.
    # Changing respective filenames, widths, and heights.
    # Thank you to Evelyn Cai for helping me with this.
    
    output$PremierLeagueLogo <- renderImage({
        filename <- "PremierLeagueLogo.png"
        list(src = filename, width = 550, height = 250, deleteFile = FALSE)
    }) 
    
    output$KaiHavertz <- renderImage({
        filename1 <- "KaiHavertz.png"
        list(src = filename1, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$Arsenal <- renderImage({
        filename2 <- "Arsenal.jpg"
        list(src = filename2, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$Bournemouth <- renderImage({
        filename3 <- "Bournemouth.jpg"
        list(src = filename3, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$Fulham <- renderImage({
        filename4 <- "Fulham.jpg"
        list(src = filename4, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$JamieVardy <- renderImage({
        filename5 <- "JamieVardy.jpg"
        list(src = filename5, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$KevinDeBruyne <- renderImage({
        filename6 <- "KevinDeBruyne.png"
        list(src = filename6, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$Liverpool <- renderImage({
        filename7 <- "Liverpool.jpg"
        list(src = filename7, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$Norwich <- renderImage({
        filename8 <- "Norwich.jpg"
        list(src = filename8, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$PetrCech <- renderImage({
        filename9 <- "PetrCech.png"
        list(src = filename9, width = 400, height = 250, deleteFile = FALSE)
    })
    
    output$TottenhamHotspurs <- renderImage({
        filename10 <- "TottenhamHotspurs.jpeg"
        list(src = filename10, width = 350, height = 250, deleteFile = FALSE)
    })
    
    output$VirgilVanDijk <- renderImage({
        filename11 <- "VirgilVanDijk.jpg"
        list(src = filename11, width = 300, height = 250, deleteFile = FALSE)
    })
    
    output$model <- renderImage({
        filename12 <- "model.png"
        list(src = filename12, width = 350, height = 250, deleteFile = FALSE)
    })
    

# Rendering plots ---------------------------------------------------------
    
# Creating a plot with standings from the 99/00 season to the 15/16 season.

output$plot1 <- renderPlot({
    plot1 %>% 
        filter(Team == input$team & !is.na(Place)) %>%
        ggplot(aes(x = Year, y = Place, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Standings",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Standings") +
        theme_hc()
    }
    )
    
# Creating a plot with wins from the 99/00 season to the 15/16 season.
 
output$plot2 <- renderPlot({
    plot2 %>% 
        filter(Team == input$team & !is.na(Wins)) %>%
        ggplot(aes(x = Season, y = Wins, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Wins",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Wins") +
        theme_hc()
    }
    )

# Creating a plot with losses from the 99/00 season to the 15/16 season.

output$plot3 <- renderPlot({
    plot3 %>% 
        filter(Team == input$team & !is.na(Losses)) %>%
        ggplot(aes(x = Season, y = Losses, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Losses",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Losses") +
        theme_hc()
    }
    )

# Creating a plot to show transfer activity from 99/00 season to 15/16 season.

output$plot4 <- renderPlot({
    plot4 %>% 
        filter(Team == input$team & !is.na(Expenses)) %>%
        ggplot(aes(x = Season, y = Expenses, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Expenses",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Expenses") +
        theme_hc()
    }
    )

# Creating a plot to show goals for from 99/00 season to 15/16 season.

output$plot5 <- renderPlot({
    plot5 %>% 
        filter(Team == input$team & !is.na(For)) %>%
        ggplot(aes(x = Season, y = For, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Goals For",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Goals For") +
        theme_hc()
    }
    )

# Creating a plot to show goals against from 99/00 season to 15/16 season.

output$plot6 <- renderPlot({
    plot6 %>% 
        filter(Team == input$team & !is.na(Against)) %>%
        ggplot(aes(x = Season, y = Against, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Goals Against",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Goals Against") +
        theme_hc()
    }
    )
}


# Running the shinyapp ----------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)



