library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(readr)
library(dplyr, warn.conflicts = FALSE)


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

d <- EPLStandings %>%
    pivot_longer(!Team, 
                 names_to = "Year", 
                 values_to = "Place")
    d$Year <- as.numeric(d$Year)
    d$Place <- factor(d$Place, levels = rev(levels(factor(d$Place))))
    select_team <- unique(d$Team)
    

# Manipulating data for plots ---------------------------------------------


# Organizing shinyapp tabs ------------------------------------------------

# Consider changing c("Point", "Line", "Smooth").
    
ui <- navbarPage(
    "Does Money Win You Titles in the Premier League?",
    tabPanel("Standings",
             fluidPage(
                 selectInput("team", "Team", choices = unique(d$Team)),
                 selectInput("y", "Y variable", choices = names(EPLStandings)),
                 selectInput("geom", "Select Viewing Option", c("Point", "Line", "Smooth")),
                 plotOutput("plot")
             )),
    tabPanel("Money Spent",
             titlePanel("Money spent from each team in the Premier League"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Wins",
             titlePanel("Wins from each team in the Premier League"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Loses",
             titlePanel("Loses from each team in the Premier League"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Goals Scored",
             titlePanel("Goals scored from each team in the Premier League"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Goals Conceded",
             titlePanel("Goals conceded from each team in the Premier League"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("My study"),
             p("Without a doubt, the Premier League is best football league in the world.
               More money is being spent each transfer window on players with the goal of winning the Premier League at the end of the season.
               In the 00/01 season, the highest transfer fee was at a price of €26.00m euros for Rio Ferdinand to Leeds United. 
               In the 17/18 season, the highest transfer fee was at a price of €84.65m euros for Virgil Van Dijk to Liverpool.
               The question is, does money win you titles in the Premier League? Does money help reach the Champions League? Europa League? 
               Does money help score more and concede less? Win more and lose less? This study will look at
               how much money each team in the Premier League has spent every transfer window from the 00/01 season up until the 17/18 season, 
               alongside taking a look at wins, losses, goals scored, goals conceded, and standings at the end of the Premier League season."),
             h3("About Me"),
             p("My name is Anthony Morales and I am a student at Harvard College. I'm studying economics and statistics, with a specialization in quantitative finance.
             I'm from Bay Shore, New York. My email is anthonymorales@college.harvard.edu."))
             )


# Creating a plot with standings ------------------------------------------

# Define server logic required to draw a histogram
    
server <- function(input, output, session){
    plot_geom <- reactive({
        switch(input$geom,
               Point = geom_point(),
               Smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               Jitter = geom_jitter(),
               Line = geom_line() 
        )
    })
    
# Creating a plot with standings from the 99/00 season to the 15/16 season.

output$plot <- renderPlot({
    d %>% 
        filter(Team == input$team & !is.na(Place)) %>%
        ggplot(aes(x = Year, y = Place, group = 1)) +
        scale_x_continuous(breaks = (2000:2016)) +
        plot_geom() +
        labs(title = "Premier League Standings",
             subtitle = "From the 2000-2001 season to the 2015-2016 season",
             x = "Year", 
             y = "Standings") +
        theme_bw()
    }
    )
}
# Run the application 
shinyApp(ui = ui, server = server)



