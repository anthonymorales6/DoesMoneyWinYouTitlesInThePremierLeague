library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(readr)
library(dplyr)

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
                                           ATFormPtsStr = col_character())
) 

ui <- navbarPage(
    "Does money win you titles in the Premier League?",
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(Premier_League_Dataset)),
                 selectInput("y", "Y variable", choices = names(Premier_League_Dataset)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             )),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("My study"),
             p("The Premier League has been the best football league in the world
               for years now, but as more transfer records are set and as more and more money is being spent on 
               players each transfer window, the question is, does money win you titles? In this study we'll take a look at 
               how much money each team has spent every transfer window for the past five years 
               and how each team stands at the end of the season."),
             h3("About Me"),
             p("My name is Anthony Morales and I study economics, business, and finance. 
             You can reach me at anthonymorales@college.harvard.edu."))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(Premier_League_Dataset, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
