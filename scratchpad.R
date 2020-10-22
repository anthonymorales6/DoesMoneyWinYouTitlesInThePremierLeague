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
                             `2016` = col_double())
) 

x <- EPLStandings$Team[1] %>%
  ggplot(aes(x = ))
