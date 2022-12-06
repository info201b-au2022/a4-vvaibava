library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_df)


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# finding max and min year

latest_year <- max(incarceration_df$year)
max_year <- filter(incarceration_df, incarceration_df$year == latest_year)
max_year

earliest_year <- min(incarceration_df$year)
min_year <- filter(incarceration_df, incarceration_df$year == earliest_year)
View(min_year)

#finding average black population in 2018 and 1990

total_black_pop_2018 <- function() {
  avg_black <- incarceration_df %>%
    filter(year == 2018) %>%
    summarise(avg_black = mean(black_pop_15to64, na.rm = TRUE))
    return(avg_black)
    
}
total_black_pop_2018()

total_black_pop_1990 <- function() {
  avg_black <- incarceration_df %>%
    filter(year == 1990) %>%
    summarise(avg_black = mean(black_pop_15to64, na.rm = TRUE))
  return(avg_black)
  
}
total_black_pop_1990()

# finding average white population in 2018 and 1990

total_white_pop_2018 <- function() {
  avg_white <- incarceration_df%>%
    filter(year == 2018) %>%
    summarise(avg_white = mean(white_pop_15to64, na.rm = TRUE))
    return(avg_white)
}
total_white_pop_2018()

total_white_pop_1990 <- function() {
  avg_white <- incarceration_df%>%
    filter(year == 1990) %>%
    summarise(avg_white = mean(white_pop_15to64, na.rm = TRUE))
  return(avg_white)
}
total_white_pop_1990()

# difference between black population from 2018 and 1990

black_diff <- function() {
  black_2018 <- incarceration_df%>%
    filter(year == 2018) %>%
    summarise(pop_2018 = round(sum(black_pop_15to64, na.rm = TRUE)))
  black_1990 <- incarceration_df%>%
    filter(year == 1990) %>%
    summarise(pop_1990 = round(sum(black_pop_15to64, na.rm = TRUE)))
  diff <- (black_2018 - black_1990)
  return(diff)
}

black_diff()

# difference between white population from 2018 and 1990

white_diff <- function() {
  white_2018 <- incarceration_df%>%
    filter(year == 2018) %>%
    summarise(pop_2018 = round(sum(white_pop_15to64, na.rm = TRUE)))
  white_1990 <- incarceration_df%>%
    filter(year == 1990) %>%
    summarise(pop_1990 = round(sum(white_pop_15to64, na.rm = TRUE)))
  diff <- (white_2018 - white_1990)
  return(diff)
}

white_diff()



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns the jail population dataframe from 1970 to 2018
get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_df%>%
    group_by(year)%>%
    summarise(jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(jail_pop_df)   
}
View(get_year_jail_pop())

# This function plots a bar chart of jail population from 1970 to 2018
plot_jail_pop_for_us <- function()  {
  jail_pop_us <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = jail_pop)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Jail Population in US from 1970 to 2018",
      caption = "Overall Jail Population has increased since 1970",
      x = "year",
      y = "Total Jail Population"
      )
  return(jail_pop_us)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function takes the states vector and and returns a data frame
get_jail_pop_by_states <- function(states) {
  pop_states <- incarceration_df%>%
    filter(state %in% states) %>%
    group_by(state,year) %>%
    summarise(jail_state_pop = sum(total_jail_pop, na.rm = TRUE))
  return(pop_states)
}
get_jail_pop_by_states(c("WA", "CA"))

#This function creates a chart of the previous data frame

plot_jail_pop_by_states <- function(states) {
  jail_pop_state <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = jail_state_pop, color = state)) +
    labs(
      title = "Jail Population in US from 1970 to 2018",
      caption = "Overall Jail Population within states from 1970 to 2018",
      x = "year",
      y = "Total Jail Population"
    )
  return(jail_pop_state)
}

plot_jail_pop_by_states(c("WA", "CA", "TX", "FL"))



## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#This function creates a dataframe that will help better visualize items
jail_pop_black_white <- function(){
  compare_bw <- incarceration_df %>%
    group_by(year)%>%
    filter(year >= 1800)%>%
    select(year, black_jail_pop, white_jail_pop)%>%
    summarise(avg_black = mean(black_jail_pop, na.rm = TRUE), avg_white = mean(white_jail_pop, na.rm = TRUE))
  return(compare_bw)
}
jail_pop_black_white()

#This function creates a chart of the data frame above

plot_jail_pop_black_white <- function(){
  jail_pop_bw <- ggplot(jail_pop_black_white()) +
    geom_line(mapping = aes(x = year, y = avg_black, color = "black")) +
    geom_line(mapping = aes(x = year, y = avg_white, color = "white")) +
    scale_x_continuous(limits = c(1985,2020), breaks = seq(1985,2020, 10)) +
    labs(
      title = "Jail population of Blacks and Whites",
      caption = "Comparison of the black and white people within Jail from 1985 to 2018",
      x = "year",
      y = "jailing population"
    )
  return(jail_pop_bw)
}

plot_jail_pop_black_white()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#This function creates a dataframe that will help better visualize items
black_jailing_pop <- function() {
  map_pop <- incarceration_df %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    select(state, black_pop_15to64)%>%
    summarise(black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE)) %>%
    mutate(state = state.name[match(state,state.abb)]) %>% 
    mutate(state = tolower(state))
  return(map_pop)
}
black_jailing_pop()

state_size <- map_data("state") %>%
  rename(state = region)%>%
  left_join(black_jailing_pop(), by = "state")
View(state_size)

#This function creates a map from the above data frame

plot_black_jailing_pop <- function() {
  map_plot <- ggplot(state_size) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_pop_15to64), color = "white", size = 0.5) +
    coord_map() +
    labs(
      title = "Black Inmates Jailing Population",
      caption = "Percentage of black inmates throughout the Country") +
    theme_bw() +
    theme(
      axis.line = element_blank(),        
      axis.text = element_blank(),        
      axis.ticks = element_blank(),       
      axis.title = element_blank(),       
      plot.background = element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank() 
    )
  return(map_plot)
    
}

plot_black_jailing_pop()




## Load data frame ---- 




