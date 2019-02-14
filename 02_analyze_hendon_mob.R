#This script provides the code for the analysis in hendon_mob_analysis, the 
#PDF with final analyses/findings

# Data and Library Load ---------------------------------------------------

library(tidyverse) #for ggplot2 visualizations and wrangling data
library(lubridate) #for analysis of dates
library(gridExtra) #for displaying plots nicely

hendon_summaries_df <- read_csv("hendon_summaries.csv")

#The following resolves some formatting issues with the data and creates extra 
#necessary fields 


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
  mutate(quantile = ntile(sum_of_cashes, 4)) %>%
  replace_na(list(average_buy_in = 0))

#average time between cash interpreted as "he has a cash once every X days on average"

first_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 1)

second_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 2)

third_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 3)

fourth_hendon_mob_quantile <- hendon_summaries_df %>%
  filter(quantile == 4)



# Fields
# 
# Some fields in the summary dataframe are scraped directly, others are modified
# using post-hoc manipulation. The list of all of the fields which are in the 
# summary dataframe are is the following. 
# 
# 
# 
# 1. **name** - Playér's name
# 2. **nationality** - Player's nationality
# 3. **average_buy_in** - Player's average buy-in, in USD
# 
# i) Note that this item is imperfect because buy-in is not always listed,
# and when it is, sometimes the currency is difficult to guess. I assume here that
# the currency of the buy-in is in the currency of the country of the tournament,
# however this is not always true (and there is currently no better way). For example,
# some events in Ukraine transacted with USD, others with Ukrainian Hryvnia. Since
# I automatically convert all foreign currency to USD based on 2017 exchange rates,
# this results in some values being converted which actually did not need to be, and 
# therefore in the buy-in values being wrong. 
# 
# 4. **number_of_cashes** - Number of events cashed in career
# 5. **sum_of_cashes** - Total amount of money cashed for in poker career, in USD
# 6. **average_cash** - Average amount cashed for per tournament
# 7. **average_placement** - Average placement in tournaments
# 8. **number_of_binks** - A bink is poker slang for a sizeable tournament poker score.
# There's no agreed upon definition of a bink, but I define it here as any cash
# above 20 times the average buy-in. This field counts all of those cashes. For
# the reasons noted above, this field is somewhat unreliable since it depends on
# average buy-in.
# 
# 9. **binks_proportion** - The percentage of tournament cashes that were binks
# 10. **number_of_countries_cashed** - The number of distinct countries that a player
# had a tournament cash in.
# 11. **first_date** - The date of the earliest tournament cash for a player.
# 12. **last_date** - The date of the most recent tournament cash for a player.
# 13. **years_played** - The difference between the date of a player's most recent cash
# and their first cash, in years. Note that this does not assume that the player 
# has continued to play since their last cash.
# 14. **average_time_btwn_cash** - The average number of days between a player's cashes,
# determined by using the first and last cashes as the endpoints
# 15. **unique_views** - Number of unique views of the player's profile
# 16. **quantile** - Players are separated into 4 quartiles based on their total cashes.
# Players in quartile 1 are the 25% of players with the least amount of cashes, 
# while players in quartile 4 are the 25% of players with the most amount of cashes. 



# Total - Average Earnings ------------------------------------------------

mean(hendon_summaries_df$sum_of_cashes)


# Total - Distribution of Average Earnings --------------------------------


max_cash <- signif(max(hendon_summaries_df$sum_of_cashes), 2)

total_earnings_plot <- ggplot(data = hendon_summaries_df, aes(sum_of_cashes))

total_earnings_plot + 
  theme_bw() + 
  geom_histogram(breaks=seq(0, max_cash, by=max_cash/10), col="red", fill="steelblue", alpha = 1) + 
  scale_x_continuous(breaks = seq(0,max_cash, by = max_cash/10), limits = c(0,max_cash)) + 
  labs(x = "Total Live Earnings", y = "Number of Players", title = "Distribution of Live Earnings per Player",
       caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5))


# Total - Distribution of Nationalities -----------------------------------

nationality_summary <- hendon_summaries_df %>%
  group_by(nationality) %>%
  summarise(number = n()) %>%
  mutate(proportion = round( number/sum(number) * 100, 2)) %>%
  arrange(desc(proportion)) %>%
  top_n(10, proportion)

top_nationality_plot <- ggplot(data=nationality_summary, 
                               aes(x=reorder(nationality, -proportion), y = proportion))

top_nationality_plot + 
  theme_bw() +
  geom_bar(stat = "identity", col="red", fill="steelblue", alpha = 1) + 
  labs(x = "Nationality", y = "Percentage of Players", 
       title = "Top 10 Nationalities of Poker Players in the Hendon Mob", caption = "Source: The Hendon Mob") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) 



# Total - Average Earnings per Nationality --------------------------------

live_earnings_country_summary <- hendon_summaries_df %>%
  group_by(nationality) %>%
  summarise(average_live_earnings = mean(sum_of_cashes), number_of_players = n())


live_earnings_country_summary_top_10 <- live_earnings_country_summary %>%
  top_n(10, number_of_players) %>%
  arrange(desc(number_of_players))

average_live_earnings_plot <- ggplot(data = live_earnings_country_summary_top_10 , aes(x=reorder(nationality, - average_live_earnings), y = average_live_earnings))

average_live_earnings_plot + geom_bar(stat = "identity", col="red", fill="steelblue", alpha = 1) + 
  labs(x = "Nationality of Player", y = "Average Live Earnings per Player", 
       title = "Average Live Earnings for Players by Nationality",
       subtitle = "Top 10 Countries by # of Players", caption = "Source: The Hendon Mob") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 10)) 



# Total - Average Years Played --------------------------------------------

hendon_summaries_df_more_one_cash <- hendon_summaries_df %>%
  filter(number_of_cashes > 1)

mean(hendon_summaries_df_more_one_cash$years_played)


# Total - Average Number of Cashes ----------------------------------------

mean(hendon_summaries_df$number_of_cashes)


# Total - Average Buy-in --------------------------------------------------

hendon_summaries_for_average_bi <- hendon_summaries_df %>%
  filter(average_buy_in > 10)

mean(hendon_summaries_for_average_bi$average_buy_in)


# Total - Average Buy-in Distribution -------------------------------------

max_average_buy_in <- signif(max(hendon_summaries_for_average_bi$average_buy_in), 2)

total_buy_in_plot <- ggplot(data = hendon_summaries_for_average_bi, aes(average_buy_in))

total_buy_in_plot + 
  theme_bw() + 
  geom_histogram(breaks=seq(0, max_average_buy_in, by=max_average_buy_in/10), 
                 col="red", fill="steelblue", alpha = 1) + 
  scale_x_continuous(breaks = seq(0, max_average_buy_in, by = max_average_buy_in/10), 
                     limits = c(0,max_average_buy_in)) + 
  labs(x = "Average Buy-in", y = "Number of Players", title = "Distribution of Average Buy-in per Player",
       caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5))


# Quartile - Earnings Distribution ----------------------------------------

display_dist_tot_earnings <- function(hendon_summaries, title_string){ 
  
  max_cash <- signif(max(hendon_summaries$sum_of_cashes), 2)
  min_cash <-  signif(min(hendon_summaries$sum_of_cashes), 2)
  
  total_earnings_plot <- ggplot(data = hendon_summaries, aes(sum_of_cashes))
  
  total_earnings_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(min_cash, max_cash, by=max_cash/10), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(min_cash,max_cash, by = max_cash/10), limits = c(min_cash, max_cash)) + 
    labs(x = "Total Live Earnings", y = "Number of Players", title = title_string) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
  
}

dte1 <- display_dist_tot_earnings(first_hendon_mob_quantile, "Distribution of Live Earnings (Q1)")
dte2 <- display_dist_tot_earnings(second_hendon_mob_quantile, "Distribution of Live Earnings (Q2)")
dte3 <- display_dist_tot_earnings(third_hendon_mob_quantile, "Distribution of Live Earnings (Q3)")
dte4 <- display_dist_tot_earnings(fourth_hendon_mob_quantile, "Distribution of Live Earnings (Q4)")

grid.arrange(dte1, dte2, dte3, dte4, nrow = 2)


# Quantile - Countries Cashed Distribution --------------------------------

display_dist_countries_cashed <- function(hendon_summaries, title_string){
  
  max_countries_cashed <- max(hendon_summaries$number_of_countries_cashed)
  
  countries_cashed_plot <- ggplot(data = hendon_summaries, aes(number_of_countries_cashed))
  
  
  countries_cashed_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(0, max_countries_cashed + 2, by=2), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(0,max_countries_cashed + 2, by = 2), limits = c(0,max_countries_cashed + 1)) + 
    labs(x = "Number of Countries Cashed", y = "Number of Players", 
         title = title_string) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
}


dcc1 <- display_dist_countries_cashed(first_hendon_mob_quantile, "Number of Countries Cashed (Q1)")
dcc2 <- display_dist_countries_cashed(second_hendon_mob_quantile, "Number of Countries Cashed (Q2)")
dcc3 <-display_dist_countries_cashed(third_hendon_mob_quantile, "Number of Countries Cashed (Q3)")
dcc4 <-display_dist_countries_cashed(fourth_hendon_mob_quantile, "Number of Countries Cashed (Q4)")


grid.arrange(dcc1, dcc2, dcc3, dcc4, nrow = 2)


# Quartile - Countries Cashed Correlation ---------------------------------

earnings_and_countries_plot <- ggplot(hendon_summaries_df, aes(x = sum_of_cashes, y = number_of_countries_cashed))

earnings_and_countries_plot  + 
  theme_bw() +
  geom_point(col = "blue", alpha = .5) +
  scale_x_continuous(breaks = seq(0,10000000, by = 1000000), limits = c(0,11000000)) +
  labs(x = "Live Earnings", y = "Countries Cashed", title = "Relationship between Live Earnings and Countries Cashed on Hendon Mob", caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm") 


# Quartile - Average Time Between Cashes Distribution ----------------------------------

display_dist_time_btwn_cashes <- function(hendon_summaries, title_string){ 
  
  hendon_summaries <- hendon_summaries %>%
    filter(number_of_cashes > 1)
  
  time_cashes_plot <- ggplot(data = hendon_summaries, aes(average_time_btwn_cash))
  
  max_time <- signif(max(hendon_summaries$average_time_btwn_cash), 2)
  
  time_cashes_plot + 
    theme_bw() + 
    geom_histogram(breaks=seq(0, max_time, by=max_time/10), col="red", fill="steelblue", alpha = 1) + 
    scale_x_continuous(breaks = seq(0,max_time, by = max_time/10), limits = c(0,max_time)) + 
    labs(x = "Time between Cashes (days)", y = "Number of Players", title = title_string ) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
  
}

dtbc1 <- display_dist_time_btwn_cashes(first_hendon_mob_quantile, "Distribution of Days btwn Cashes (Q1)")
dtbc2 <- display_dist_time_btwn_cashes(second_hendon_mob_quantile, "Distribution of Days btwn Cashes (Q2)")
dtbc3 <- display_dist_time_btwn_cashes(third_hendon_mob_quantile, "Distribution of Days btwn Cashes (Q3)")
dtbc4 <- display_dist_time_btwn_cashes(fourth_hendon_mob_quantile, "Distribution of Days btwn Cashes (Q4)")

grid.arrange(dtbc1, dtbc2, dtbc3, dtbc4, nrow = 2)


# Quartile - Average Time Between Cashes Statistic ------------------------

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


# Quartile - Average Buy-ins ----------------------------------------------


first_hendon_mob_quantile_for_average_bi <- first_hendon_mob_quantile %>%
  filter(average_buy_in > 10)

first_hendon_mob_quantile_average_buy_in <- mean(first_hendon_mob_quantile_for_average_bi$average_buy_in)

second_hendon_mob_quantile_for_average_bi <- second_hendon_mob_quantile %>%
  filter(average_buy_in > 10)

second_hendon_mob_quantile_average_buy_in <- mean(second_hendon_mob_quantile_for_average_bi$average_buy_in)

third_hendon_mob_quantile_for_average_bi <- third_hendon_mob_quantile %>%
  filter(average_buy_in > 10)

third_hendon_mob_quantile_average_buy_in <- mean(third_hendon_mob_quantile_for_average_bi$average_buy_in)

fourth_hendon_mob_quantile_for_average_bi <- fourth_hendon_mob_quantile %>%
  filter(average_buy_in > 10)

fourth_hendon_mob_quantile_average_buy_in <- mean(fourth_hendon_mob_quantile_for_average_bi$average_buy_in)

average_buy_in_vec <- c(first_hendon_mob_quantile_average_buy_in ,        
                        second_hendon_mob_quantile_average_buy_in,
                        third_hendon_mob_quantile_average_buy_in, 
                        fourth_hendon_mob_quantile_average_buy_in)

average_buy_in_df <- tibble(quartile = seq(1:4), average_buy_in = average_buy_in_vec )

average_buy_in_plot <- ggplot(average_buy_in_df, aes(quartile, average_buy_in))

average_buy_in_plot +
  theme_bw() +
  geom_bar(stat = "identity", col = "red", fill = "steelblue") +
  labs(x = "Quartile", y = "Average Buy-In ($)", title = "Average Buy-in per Quartile", 
       caption = "Source: The Hendon Mob") + 
  theme(plot.title = element_text(hjust = 0.5)) 


