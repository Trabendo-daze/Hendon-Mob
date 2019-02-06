
# Data and Library Load ---------------------------------------------------

library(tidyverse)
library(lubridate)

#Distribution of countries in DB
#Distribution of total earnings
#Distribution of number of outliers
#Distribution of average time between cashes
#Distribution of time between outlier and next cash
#Buy-ins? (This is difficult with Hendon Mob)
#Cashes/year
#What can we say and what can we not say with Hendon Mob Data??

hendon_summaries_df <- read_csv("hendon_summaries.csv")

hendon_summaries_df <- hendon_summaries_df %>%
  mutate(new_name = case_when(is.na(name) == TRUE ~ nationality, is.na(name) == FALSE ~ name)) %>%
  mutate(new_nationality = case_when(is.na(name) == TRUE ~ "Unlisted", is.na(name) == FALSE ~ nationality)) %>%
  mutate(average_cash = round(sum_of_cashes/number_of_cashes, 2)) %>%
  mutate(average_placement = round(average_placement), 2) %>%
  mutate(average_buy_in = round(average_buy_in), 2) %>%
  mutate(last_date = as_date(last_date)) %>%
  mutate(first_date = as_date(first_date)) %>%
  mutate(years_played = round((last_date - first_date)/365.25,2)) %>%
  mutate(average_time_btwn_cash = round((last_date - first_date)/number_of_cashes), 2) %>%
  mutate(average_time_btwn_cash = as.numeric(average_time_btwn_cash)) %>%
  mutate(binks_proportion = round(number_of_binks/number_of_cashes * 100, 2)) %>%
  select(name = new_name, nationality = new_nationality, average_buy_in, 
         number_of_cashes, sum_of_cashes, average_cash, average_placement, number_of_binks,
         binks_proportion, number_of_countries_cashed, first_date, last_date, years_played, 
         average_time_btwn_cash, unique_views) %>%
  arrange(desc(sum_of_cashes)) %>%
  mutate(quantile = ntile(sum_of_cashes, 4)) 

#average time between cash interpreted as "he has a cash once every X days on average"

first_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 1)

second_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 2)

third_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 3)

fourth_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 4)



# Nationalities in the dataset --------------------------------------------


nationality_summary <- hendon_summaries_df %>%
  group_by(nationality) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>%
  top_n(10, number)

top_nationality_plot <- ggplot(data=nationality_summary, aes(x=reorder(nationality, -number), y = number))

top_nationality_plot + geom_bar(stat = "identity", fill = "cadetblue2", col = "orangered1" ) + 
  labs(x = "Nationality", y = "Number of Players", 
       title = "Top 10 Nationalities of 1,000 Randomly Sampled Poker Players", caption = "Source: The Hendon Mob") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) 



# Distribution of unique views --------------------------------------------

range(hendon_summaries_df$unique_views)

unique_views_plot <- ggplot(hendon_summaries_df, aes(unique_views))

unique_views_plot + 
  theme_bw() + 
  geom_histogram(breaks=seq(0, 15000, by=1500), col="red", fill="steelblue", alpha = 1) + 
  scale_x_continuous(breaks = seq(0,15000, by = 1500), limits = c(0,17500)) + 
  labs(x = "Number of Views", y = "Number of Players", title = "Distribution of Unique Views per Player") + 
  theme(plot.title = element_text(hjust = 0.5))



# Relationship between views and cashes -----------------------------------

range(hendon_summaries_df$sum_of_cashes)

cor(hendon_summaries_df$sum_of_cashes, hendon_summaries_df$unique_views, method = "pearson") 

cashes_and_views_plot <- ggplot(hendon_summaries_df, aes(x = sum_of_cashes, y = unique_views))

cashes_and_views_plot + 
  theme_bw() +
  geom_point(col = "blue", alpha = .5) +
  scale_x_continuous(breaks = seq(0,10000000, by = 1000000), limits = c(0,11000000)) +
  labs(x = "Live Earnings", y = "Unique Views", title = "Relationship between Live Earnings and Unique Views on Hendon Mob", caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm") +
  annotate("text", x = 1200000, y = 30000, label = "R = 0.86")


cashes_and_views_plot + 
  theme_bw() +
  geom_point(col = "blue", alpha = .5) +
  scale_x_continuous(breaks = seq(0,200000, by = 10000), limits = c(0,210000)) +
  labs(x = "Live Earnings", y = "Unique Views", title = "Relationship between Live Earnings and Unique Views on Hendon Mob", caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm")

 
#slope of line gets smaller as we reduce live earnings. Unique views increase more as you increase live earnings,
#somewhat exponential growth.


# Unique views per country ------------------------------------------------


unique_views_country_summary <- hendon_summaries_df %>%
  group_by(nationality) %>%
  summarise(average_unique_views = mean(unique_views), number_of_players = n())


unique_views_country_summary_top_10 <- unique_views_country_summary %>%
  top_n(10, number_of_players)

#average unique views for top 10 countries by number_of_players

average_unique_views_plot <- ggplot(data = unique_views_country_summary_top_10 , aes(x=reorder(nationality, - average_unique_views), y = average_unique_views))

average_unique_views_plot + geom_bar(stat = "identity", fill = "steelblue", col = "orange") + 
  labs(x = "Nationality of Player", y = "Average Number of Views per Player", 
       title = "Average Number of Views for Players by Nationality (Top 10 Countries by # of Players only)", subtitle = "People search German players most often on average", caption = "Source: The Hendon Mob") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) 



# Average Total Earnings per Country among Players ------------------------


live_earnings_country_summary <- hendon_summaries_df %>%
  group_by(nationality) %>%
  summarise(average_live_earnings = mean(sum_of_cashes), number_of_players = n())


live_earnings_country_summary_top_10 <- live_earnings_country_summary %>%
  top_n(10, number_of_players)

#average unique views for top 10 countries by number_of_players

average_live_earnings_plot <- ggplot(data = live_earnings_country_summary_top_10 , aes(x=reorder(nationality, - average_live_earnings), y = average_live_earnings))

average_live_earnings_plot + geom_bar(stat = "identity", fill = "aquamarine4", col = "darkorange2") + 
  labs(x = "Nationality of Player", y = "Average Live Earnings per Player", 
       title = "Average Live Earnings for Players by Nationality (Top 10 Countries by # of Players only)", subtitle = "German players have the highest earnings on average", caption = "Source: The Hendon Mob") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) 



# Notes and possible analyses ---------------------------------------------

#Ideas: 

#1: Try to tell the story of what happens to players after a big score.
#What is a big score? How many players experience a big score? What happens afterwards?
#How many players follow the big score up with another big score? 
#How many go on a huge drought?
#Metrics needed: Definition of big score, usage of cashes summary in computation of this


#2 Try to find what makes a player famous. Regression models for "unique views" including random forests, 
#other machine learning decision tree models

#x variables: live earnings, country, number of cashes, average placement, years played
#y variable unique views


#3 Look at distribution of sample of Hendon Mob players, turn it into a Markdown/Shiny project
#and post on Github as creation of portfolio

#Note ~ 600,000 players on hendon mob

#1% sample is 6,000

#Things to look at - 

#Distribution of countries in DB
#Distribution of total earnings
#Distribution of number of outliers
#Distribution of average time between cashes
#Distribution of time between outlier and next cash
#Disribution in amount of countries cashed in
#Buy-ins? (This is difficult with Hendon Mob)
#Cashes/year
#What can we say and what can we not say with Hendon Mob Data??


#Separate into groups
#High, medium, low cashes
#What do these populations look like with the parameters above?
#Are there different average stories for each population?
#Nature of differences between groups?

#We are defining bink as any score more than 100x average buy in


#Need a plan for analyses and how to present them in my markdown

#Probably something about the entire population about the specific differences 
#between quartiles - how do these categories/players differ

#Rmarkdown - no code just graphs/text

#Entire Population
#How many players total?
#How many players in this sample?
#Average Earnings?
#Distribution of earnings?
#Distribution of countries in the sample?
#Average earnings per country?
#Average number of years played in the sample?
#Average number of cashes?
#Average buy-in?/Average buy-in distribution

#Between Quartiles
#Average earnings diffserences/distributional differences - Interpretation
#Countries cashed differences
#Average buy-in differences
#Average amount of time played?
#Average binks/cash? (do high stakes players just get lucky to get there?)
#Average amount of countries played in?
#Average time between cash? (who plays more often?)


#Summary section about differences


#


# Distribution of total earnings ------------------------------------------

display_dist_tot_earnings <- function(hendon_summaries){ 
  
  max_cash <- signif(max(hendon_summaries$sum_of_cashes), 2)
  
  total_earnings_plot <- ggplot(data = hendon_summaries, aes(sum_of_cashes))
  
  total_earnings_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(0, max_cash, by=max_cash/10), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(0,max_cash, by = max_cash/10), limits = c(0,max_cash)) + 
    labs(x = "Total Live Earnings", y = "Number of Players", title = "Distribution of Live Earnings per Player") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
}

display_dist_tot_earnings(first_hendon_mob_quantile)
display_dist_tot_earnings(second_hendon_mob_quantile)
display_dist_tot_earnings(third_hendon_mob_quantile)
display_dist_tot_earnings(fourth_hendon_mob_quantile)

#Conclusion - live earnings are more and more right skewed in higher quartiles of players


# Distribution of Amount of Countries Cashed in ---------------------------

display_dist_countries_cashed <- function(hendon_summaries){
  
  max_countries_cashed <- max(hendon_summaries$number_of_countries_cashed)
  
  countries_cashed_plot <- ggplot(data = hendon_summaries, aes(number_of_countries_cashed))
  
  
  countries_cashed_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(0, max_countries_cashed + 2, by=2), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(0,max_countries_cashed + 2, by = 2), limits = c(0,max_countries_cashed + 1)) + 
    labs(x = "Number of Countries Cashed", y = "Number of Players", 
         title = "Distribution of Number of Countries Cashed") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
}

display_dist_countries_cashed(hendon_summaries_df)
display_dist_countries_cashed(first_hendon_mob_quantile)
display_dist_countries_cashed(second_hendon_mob_quantile)
display_dist_countries_cashed(third_hendon_mob_quantile)
display_dist_countries_cashed(fourth_hendon_mob_quantile)


earnings_and_countries_plot <- ggplot(hendon_summaries_df, aes(x = sum_of_cashes, y = number_of_countries_cashed))

earnings_and_countries_plot  + 
  theme_bw() +
  geom_point(col = "blue", alpha = .5) +
  scale_x_continuous(breaks = seq(0,10000000, by = 1000000), limits = c(0,11000000)) +
  labs(x = "Live Earnings", y = "Countries Cashed", title = "Relationship between Live Earnings and Countries Cashed on Hendon Mob", caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm") 


# Average Time between Cashes Distribution --------------------------------


display_dist_time_btwn_cashes <- function(hendon_summaries){ 
  
  hendon_summaries <- hendon_summaries %>%
    filter(number_of_cashes > 1)
  
  time_cashes_plot <- ggplot(data = hendon_summaries, aes(average_time_btwn_cash))
  
  max_time <- signif(max(hendon_summaries$average_time_btwn_cash), 2)
  
  time_cashes_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(0, max_time, by=max_time/10), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(0,max_time, by = max_time/10), limits = c(0,max_time)) + 
    labs(x = "Time between Cashes (days)", y = "Number of Players", title = "Distribution of Days Between Cashes") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
}

display_dist_time_btwn_cashes(hendon_summaries_df)
display_dist_time_btwn_cashes(first_hendon_mob_quantile)
display_dist_time_btwn_cashes(second_hendon_mob_quantile)
display_dist_time_btwn_cashes(third_hendon_mob_quantile)
display_dist_time_btwn_cashes(fourth_hendon_mob_quantile)

#people with more cashes play more often?

#average time for each quartile -


hendon_summaries_more_one_cash <- hendon_summaries_df %>%
  filter(number_of_cashes > 1)

hendon_summaries_average_time_between <- mean(hendon_summaries_more_one_cash$average_time_btwn_cash)

first_hendon_mob_quantile_more_one_cash <- first_hendon_mob_quantile %>%
  filter(number_of_cashes > 1)

first_hendon_mob_quantile_average_time_between <- mean(first_hendon_mob_quantile$average_time_btwn_cash)

second_hendon_mob_quantile_more_one_cash <- second_hendon_mob_quantile_df %>%
  filter(number_of_cashes > 1)

second_hendon_mob_quantile_average_time_between <- mean(second_hendon_mob_quantile$average_time_btwn_cash)

third_hendon_mob_quantile_more_one_cash <- third_hendon_mob_quantile_df %>%
  filter(number_of_cashes > 1)

third_hendon_mob_quantile_quantile_average_time_between <- mean(third_hendon_mob_quantile$average_time_btwn_cash)

fourth_hendon_mob_quantile_more_one_cash <- fourth_hendon_mob_quantile_df %>%
  filter(number_of_cashes > 1)

fourth_hendon_mob_quantile_average_time_between <- mean(fourth_hendon_mob_quantile$average_time_btwn_cash)

time_between_vec <- c(first_hendon_mob_quantile_average_time_between, second_hendon_mob_quantile_average_time_between,
                      third_hendon_mob_quantile_quantile_average_time_between, fourth_hendon_mob_quantile_average_time_between)

time_between_df <- tibble(quartile = seq(1:4), average_time = time_between_vec)

time_between_plot <- ggplot(time_between_df, aes(quartile, average_time))

time_between_plot +
  theme_bw() +
  geom_bar(stat = "identity", col = "red", fill = "steelblue") +
  labs(x = "Quartile", y = "Average Time Between Cashes (days)", title = "Average Time Between Cashes", caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) 

#People with more cashes have longer between them

# Testing -----------------------------------------------------------------

max_time <- max(hendon_summaries_df$average_time_btwn_cash)

