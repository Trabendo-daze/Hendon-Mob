

# Library Loading/Introductory Information ---------------------------------------------
#test change

library(rvest) #for html scraping
library(stringr) #for string manipulation
library(lubridate) #for date reading/manipulation
library(tidyverse) #for pipe usage/other miscellaneous functions, just good to have
library(janitor)

currency_conversion_df <- read_csv("currency_conversion.csv")
#Exchange rates based on the end of 2017

country_currency_mapping <- read_csv("country_currency_mapping.csv")
country_currency_mapping <- country_currency_mapping %>%
  select(countries_cashed_in = Country, code = Code, everything()) %>%
  left_join(currency_conversion_df) %>%
  replace_na(list(rate = 1))

#Information on using "selectorgadget" accessed with: 

vignette("selectorgadget")

#PURPOSE OF THIS SCRIPT

#This script was created for scraping information off of the live poker database, thehendonmob.com.
#The Hendon Mob is a website which keeps records for all live poker tournament cashes for players around the world
#and is seen as the go-to resource for understanding a poker player's live poker tournament background. 

#The website provides information for players such as their name, nationality, hometown, and any and all live tournament cashes 
#along with event name, event date, placement, dollars earned.

#By scraping this information from The Hendon Mob, we can perform analyses to answer questions such as:

#Is there a relationship between live earnings and unique profile views?
#What is the distribution of nationalities in the database?
#Do players of different nationalities exhibit different live cash distributions?

#This script contains functions for:
#1. Reading a specific player's Hendon Mob profile into R/csv.
#2. Creating a summary dataframe for as many Hendon Mob profiles as desired (each row is a player, 
#   columns for limited summary information)



# HTML Codes for Selector Gadget reading ----------------------------------

# ".date" - date of tournament
# ".currency+ .currency" - List of cashes
# ".currency" - List of cashes (alternate)
# "td.place" - place in tournament
# "event_name a" - event name
# ".popularity" - unique views
# ".player-profile-info-total-live__value" - total cashes
# ".text-wrap""- hometown
# ".player-profile-layout__name" - name and nationality


# Functions for creating dataframe for a specific poker player/summary dataframe of entire database ---------------

make_hendon_df <- function(url){
  
  #reads in url
  url <- read_html(url) 
  
  #reads in date column
  date <- url %>%  
    html_nodes(".date") %>%
    html_text() %>%
    parse_date_time(c("dmy", "my"))
  
  #reads in cashes column
  #there are two possibilities for the cashes html code depending on if the player cashed an event which is not in USD
  #the function deals with this by first seeing if there are events in non-USD currencies, and if there aren't it uses
  #an alternate html code
  
  cashes_double_currency <- url %>%
    html_nodes(".currency+ .currency") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[$]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  cashes_single_currency <- url %>%
    html_nodes(".currency") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[$]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  #only uses the alternate code if the first cashes code doesn't work (i.e. length of the associated vector == 0)
  
  if (length(cashes_double_currency > 0)) {
    cashes <- cashes_double_currency
  } else {
    cashes <- cashes_single_currency
  }
  
  #reads in placement column 
  
  placement <- url %>%
    html_nodes("td.place") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[st]" , "") %>%
    str_replace_all("[th]" , "") %>%
    str_replace_all("[nd]" , "") %>%
    str_replace_all("[rd]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  #reads in event name column, adjusts because there are extra blank events that get added because of html weirdness
  
  event_name <- url %>%
    html_nodes(".event_name a") %>%
    html_text()
  
  event_name <- event_name[!event_name == ""]
  
  #reads in hometown
  
  hometown <- url %>%
    html_nodes(".text-wrap") %>%
    html_text()
  
  #reads in name and country which come as a package in one html code
  #splits the name and country apart and stores them separately
  
  name_and_country <- url %>%
    html_nodes(".player-profile-layout__name") %>%
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  
  name_and_country <- unlist(strsplit(name_and_country, split="              "))
  name_and_country <- str_trim(name_and_country)
  
  nationality <- name_and_country[1]
  name <- name_and_country[2]
  
  #reads in unique_views, does some separating since unique_view html code also includes the player name
  
  popularity_html <- url %>%
    html_nodes(".popularity") %>%
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim()
  
  popularity_list <- unlist(strsplit(popularity_html, split="              "))
  popularity_list <- str_trim(popularity_list)
  
  unique_views <- as.numeric(popularity_list[2])
  
  #produces the dataframe for output, assigns a 0 to any events which don't have a cash listed
  
  cashes_df <- data.frame(name, nationality, unique_views, date, event_name, cashes, placement, stringsAsFactors = FALSE)
  cashes_df <- cashes_df %>%
    replace_na(list(cashes = 0))
  
  cashes_df
}

make_hendon_summary <- function(url){
  
  #this function does a lot which is similar to the previous function,
  #but attempts to only offer a summary for each player, intended for input into a larger summary dataframe
  
  url <- read_html(url)
  
  name_and_country <- url %>%
    html_nodes(".player-profile-layout__name") %>%
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>%
    str_trim()
  
  name_and_country <- unlist(strsplit(name_and_country, split="              "))
  name_and_country <- str_trim(name_and_country)
  
  nationality <- name_and_country[1]
  name <- name_and_country[2]
  
  sum_of_cashes <- url %>%
    html_nodes(".player-profile-info-total-live__value") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "")  %>%
    str_replace_all("[,]" , "") %>%
    str_replace_all("[$]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  popularity_html <- url %>%
    html_nodes(".popularity") %>%
    html_text() %>% 
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim()
  
  popularity_list <- unlist(strsplit(popularity_html, split="              "))
  popularity_list <- str_trim(popularity_list)
  
  unique_views <- as.numeric(popularity_list[2])
  
  cashes_double_currency <- url %>%
    html_nodes(".currency+ .currency") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[$]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  cashes_single_currency <- url %>%
    html_nodes(".currency") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[$]" , "") %>%
    str_replace_all("[,]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  #only uses the alternate code if the first cashes code doesn't work (i.e. length of the associated vector == 0)
  
  if (length(cashes_double_currency > 0)) {
    cashes <- cashes_double_currency
  } else {
    cashes <- cashes_single_currency
  }
  
  
  #cashes is a vector of the cashes and we can determine number of binks from them
  #binks should be defined here.
  
  number_of_cashes <- length(cashes)
  
  placement <- url %>%
    html_nodes("td.place") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_replace_all("[st]" , "") %>%
    str_replace_all("[th]" , "") %>%
    str_replace_all("[nd]" , "") %>%
    str_replace_all("[rd]" , "") %>%
    str_trim() %>%
    as.numeric()
  
  average_placement <- mean(placement)
  
  
  date <- url %>%  
    html_nodes(".date") %>%
    html_text() %>%
    parse_date_time(c("dmy", "my"))
  
  date_sorted <- sort(date)
  first_date <- date_sorted[1]
  last_date <- tail(date_sorted, n = 1)
  
  
  countries_cashed_in <- url %>%  
    html_nodes(".venue_flag") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim() 
  
  
  number_of_countries_cashed <- length(unique(countries_cashed_in))
  
  event_names <- url %>%  
    html_nodes(".event_name a") %>%
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    str_trim() 
  
  event_names <- event_names[event_names != ""]
  buy_in_df <- tibble(event_names, countries_cashed_in, cashes)
  buy_in_df <- buy_in_df %>%
    mutate(buy_in = parse_number(event_names)) %>%
    left_join(country_currency_mapping) %>%
    replace_na(list(buy_in = 0, code = "USD", rate = 1)) %>%
    select(countries_cashed_in, buy_in, cashes, code, rate) %>%
    mutate(buy_in_usd = buy_in/rate) %>%
    mutate(buy_in_usd = replace(buy_in_usd, buy_in_usd > cashes, cashes/10))
  
  
  #if buy-in is greater than or equal to cash, then buy-in = 1/10 of cash
  
  
  average_buy_in <- mean(buy_in_df$buy_in_usd, na.rm = TRUE)
  
  bink_threshold <- 20 * average_buy_in
  
  number_of_binks <- sum(cashes > bink_threshold, na.rm = TRUE)
  
  hendon_summary <- data.frame(name, nationality, average_buy_in, number_of_cashes, 
                               sum_of_cashes, average_placement, number_of_binks,
                               number_of_countries_cashed, first_date, last_date, 
                               unique_views, stringsAsFactors = FALSE)
  
  hendon_summary
  

}

# Creating Entire Database ------------------------------------------------

#this function compiles summaries using the make_hendon_summary function
#then binds them all together in one dataframe.

#the one variable for this function is "n" or the number of randomly selected summaries.

#note the try statement in this function which was created because some links will return empty sites.
#the code will let you know about the missing profile numbers but will proceed on.
#if you'd like to turn these errors off, you can add the parameter "silent = TRUE" to the try statement.

compile_hendon_summaries <- function(n){
  
  hendon_website_root <- "http://pokerdb.thehendonmob.com/player.php?a=r&n="
  
  hendon_total_df <- data_frame()
  
  random_hendon_vector <- sample(1:630000,n,replace=F)
  
  for (x in random_hendon_vector) {
    temp_hendon_url <- paste(hendon_website_root,x, sep = "")
    try(temp_hendon_df <- make_hendon_summary(temp_hendon_url))
    hendon_total_df <- bind_rows(temp_hendon_df, hendon_total_df)
    
  } 
  
  hendon_total_df <- distinct(hendon_total_df)
  
  hendon_total_df
  
}



# Creating and saving CSVs ------------------------------------------------

hendon_summary_csv <- compile_hendon_summaries(1000)

write_csv(hendon_summary_csv, path = "hendon_summaries.csv")



# Testing Section ---------------------------------------------------------

#Section for testing/outputting summaries for analysis. 


#Things to add to summary data frame



url <- "http://pokerdb.thehendonmob.com/player.php?a=r&n=64849"

url <- read_html(url)




test <- make_hendon_summary(url)

#We want a dataframe with the event name and the country played in
#We want to make columns with the buy-in and the currency of that country
#buy-in comes from parse_number of the event name
#currency comes from a join between the country_currency df and the country
#then we create a mutate to convert into USD, if not USD already
#then we get average buy-in and bink threshold 

event_names <- event_names[event_names != ""]
buy_in_df <- tibble(event_names, countries_cashed_in, cashes)
buy_in_df <- buy_in_df %>%
  mutate(buy_in = parse_number(event_names)) %>%
  left_join(country_currency_mapping) %>%
  replace_na(list(buy_in = 0, code = "USD", rate = 1)) %>%
  select(countries_cashed_in, buy_in, cashes, code, rate) %>%
  mutate(buy_in_usd = buy_in/rate) %>%
  mutate(buy_in_usd = replace(buy_in_usd, buy_in_usd > cashes, cashes/10))

average_buy_in <- mean(buy_in_df$buy_in_usd, na.rm = TRUE)


