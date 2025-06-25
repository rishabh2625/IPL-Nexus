library(tidyverse)
library(rvest)
library(dplyr)
library(tibble)
library(stringr)


url <- 'https://www.espncricinfo.com/series/indian-premier-league-2024-1410320/match-schedule-fixtures-and-results'
website <- read_html(url)

## Retrieving the relative path links to access the website of each match for that particular season
matchlinks <- website %>% 
  html_elements ('.ds-grow.ds-px-4.ds-border-r.ds-border-line-default-translucent') %>% 
  html_element('a') %>% 
  html_attr('href')

## Taking a base url and and concatenating the relative path links to create the original path links
base_url <- 'https://www.espncricinfo.com'
matchlinks <- str_c(base_url, matchlinks)

## Total number of matches for that season
total_matches <- length(matchlinks)

## Assigning variable vectors
dates <- character()
match_no <- c(202401:(202400+total_matches))
match_venue <- character()
toss <- character()
decision <- character()
player.of.the.match <- character()
team1 <- character()
team2 <- character()
winner <- character()
time.of.match <- character()
result <- c()
score_team1 <- character()
score_team2 <- character()

## Looping to go through each match details to retrieve the required data
for (i in 1:total_matches){
  ## Loading each match details
  match <- read_html(matchlinks[i])
  
  ## Scraping the match number, venue and date 
  match_details <- match %>% 
    html_element('.ds-text-tight-m.ds-font-regular.ds-text-typo-mid3') %>% 
    html_text()
  match_details <- strsplit(match_details, ',')[[1]]
  match_date <- paste(match_details[3],match_details[4],sep = ',')
  dates <- c(dates,match_date)
  
  ## Scraping data for the teams playing the match
  playing_teams <- match %>% 
    html_elements('.ds-text-tight-l.ds-font-bold.ds-text-typo') %>%
    html_text()
  team1 <- c(team1,playing_teams[1])
  team2 <- c(team2,playing_teams[2])
  
  ## Scraping all the tables in the site
  other_details <- match %>% 
    html_elements('table') %>%
    html_table()
  
  ## Checking if the match has been abandoned so it lacks the batting and bowling tables for both the innings
  if (length(other_details) < 9){
    ## Assigning 'Match Abandoned to required vectors'
    toss <- c(toss,'Match Abandoned')
    decision <- c(decision, ' Match Abandoned')
    player.of.the.match <- c(player.of.the.match, 'Match Abandoned')
    winner <- c(winner, 'Match Abandoned')
    score_team1 <- c(score_team1, "Match Abandoned")
    score_team2 <- c(score_team2, "Match Abandoned")
    
    ## Scraping the time at which the match was played Night or Daynight
    some_other_details <- other_details[3][[1]]
    match_time_ind<- which(some_other_details$X1 == 'Match days', arr.ind = TRUE)
    match_time <- some_other_details$X2[match_time_ind]
    match_time <- strsplit(match_time,'-')[[1]]
    clr_match_time <- str_extract(match_time[2], '[a-z]+') %>% str_to_title()
    time.of.match <- c(time.of.match,clr_match_time)
    
    match_venue <- c(match_venue,some_other_details$X2[1])
  }
  else{
    ## Scraping some other relevant details from the required table in the list of tables
    some_other_details <- other_details[5][[1]]
    
    ## Toss winner and their decision
    toss_details <- some_other_details$X2[2]
    toss_details <- strsplit(toss_details,',')[[1]]
    toss <- c(toss,toss_details[1])
    deci <- str_to_sentence(toss_details[2])
    decision <- c(decision,deci)
    
    ## Player of the match
    MOM <- some_other_details$X2[5]
    player.of.the.match <- c(player.of.the.match, MOM)
    
    ## Time the match was being played i.e. Night or Daynight
    match_time_ind<- which(some_other_details$X1 == 'Match days', arr.ind = TRUE)
    match_time <- some_other_details$X2[match_time_ind]
    match_time <- strsplit(match_time,'-')[[1]]
    clr_match_time <- str_extract(match_time[2], '[a-z]+') %>% str_to_title()
    time.of.match <- c(time.of.match,clr_match_time)
    
    score_t1 <- match %>% 
      html_elements('.ds-text-compact-m.ds-text-typo.ds-text-right.ds-whitespace-nowrap') %>% 
      html_text()
    score_team1 <- c(score_team1,score_t1[1])
    
    score_t2 <- strsplit(score_t1[2],')')[[1]]
    score_t2 <- score_t2[2]
    score_team2 <- c(score_team2,score_t2)
    
    match_venue <- c(match_venue,some_other_details$X2[1])
    ## The names of the winning team are stored under different row names in playoffs and league stages
    ## Applying conditionals to overcome this issue
    ## During League stages
    if (i <= total_matches - 4){
      ## Last row has name of the winning team 
      winning_team <- some_other_details$X2[nrow(some_other_details)]
      winning_team <- strsplit(winning_team,'2')[[1]]
      winning_team <- trimws(winning_team[1])
      winner <- c(winner,winning_team)
    }
    ## During Playoffs
    else {
      ## The row with name Series result has the name of the winning team 
      winning_team_ind <- which(some_other_details$X1 == 'Series result', arr.ind = TRUE)
      winning_team <- strsplit(some_other_details$X2[winning_team_ind], 'won')[[1]]
      winning_team <- strsplit(winning_team, 'advanced')[[1]]
      ## Removing extra white spaces
      winning_team <- trimws(winning_team[1])
      winner <- c(winner,winning_team)
    }
  }
  result[i] <- match %>% html_element(".ds-text-tight-s.ds-font-medium.ds-truncate.ds-text-typo span") %>% html_text()
}

data_2024 <- data.frame("Match_Id" = match_no, "Match_Date" = dates, "Match_Timing" = time.of.match,
                        "Venue" = match_venue, "Team_1" = team1, "Team_2" = team2,
                        "Toss_Winner" = toss, "Toss_Decision" = decision, "Team_1_Score" = score_team1, 
                        "Team_2_Score" = score_team2, "Winner" = winner, "Result" = result,
                        "POTM" = player.of.the.match)
