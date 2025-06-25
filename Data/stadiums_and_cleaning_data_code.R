library(dplyr)
library(rvest)
library(tidyr)


load("Match Data/total_match_data.RData")


colnames(total_match_data)
 dat <- total_match_data 

dat <- dat %>%
  mutate(
    # Ensure Team_1_Score and Team_2_Score are treated as character strings
    Team_1_Score = as.character(Team_1_Score),
    Team_2_Score = as.character(Team_2_Score),
    
    # Handle abandoned matches (preserve original values)
    Team_1_Score_Abandoned = ifelse(Team_1_Score == "Match Abandoned", "Match Abandoned", NA),
    Team_2_Score_Abandoned = ifelse(Team_2_Score == "Match Abandoned", "Match Abandoned", NA),
    
    # Extract score and wickets for Team_1 only for non-abandoned matches
    Team_1_Score_split = strsplit(Team_1_Score, "/"),
    Team_1_Score = ifelse(is.na(Team_1_Score_Abandoned), sapply(Team_1_Score_split, function(x) as.numeric(x[1])), NA),  # Extract score
    Team_1_Wickets = ifelse(is.na(Team_1_Score_Abandoned), sapply(Team_1_Score_split, function(x) ifelse(length(x) == 2, as.numeric(x[2]), 10)), NA),  # Extract wickets or set to 10
    
    # Extract score and wickets for Team_2 only for non-abandoned matches
    Team_2_Score_split = strsplit(Team_2_Score, "/"),
    Team_2_Score = ifelse(is.na(Team_2_Score_Abandoned), sapply(Team_2_Score_split, function(x) as.numeric(x[1])), NA),  # Extract score
    Team_2_Wickets = ifelse(is.na(Team_2_Score_Abandoned), sapply(Team_2_Score_split, function(x) ifelse(length(x) == 2, as.numeric(x[2]), 10)), NA),  # Extract wickets or set to 10
    
    # For abandoned matches, retain the original 'Match Abandoned' values
    Team_1_Score = ifelse(!is.na(Team_1_Score_Abandoned), Team_1_Score_Abandoned, Team_1_Score),
    Team_2_Score = ifelse(!is.na(Team_2_Score_Abandoned), Team_2_Score_Abandoned, Team_2_Score),
    Team_1_Wickets = ifelse(!is.na(Team_1_Score_Abandoned), Team_1_Score_Abandoned, Team_1_Wickets),
    Team_2_Wickets = ifelse(!is.na(Team_2_Score_Abandoned), Team_2_Score_Abandoned, Team_2_Wickets)
  )


colnames(dat)

unique(dat$Venue)

#deleting non needed columns and arranging them in order
dat <- dat %>%
  # Remove the unwanted columns
  select(-Team_1_Score_Abandoned, -Team_2_Score_Abandoned, -Team_1_Score_split, -Team_2_Score_split) %>%
  
  # Reorder columns to place 'Team_1_Wickets' after 'Team_1_Score' and 'Team_2_Wickets' after 'Team_2_Score'
  select(Match_Id, Match_Date, Match_Timing, Venue, Team_1, Team_2, Toss_Winner, Toss_Decision,
         Team_1_Score, Team_1_Wickets, Team_2_Score, Team_2_Wickets, Winner, Result, 
         POTM, Sixes, Fours, Dots)

################################
dat$Team_1_Score[60]=157
dat <- dat %>%
  # Replace 'Match Abandoned' with 0 for Team_1_Score, Team_2_Score, Team_1_Wickets, and Team_2_Wickets
  mutate(
    Team_1_Score = ifelse(Team_1_Score == "Match Abandoned", 0, as.numeric(Team_1_Score)),
    Team_2_Score = ifelse(Team_2_Score == "Match Abandoned", 0, as.numeric(Team_2_Score)),
    Team_1_Wickets = ifelse(Team_1_Wickets == "Match Abandoned", 0, as.numeric(Team_1_Wickets)),
    Team_2_Wickets = ifelse(Team_2_Wickets == "Match Abandoned", 0, as.numeric(Team_2_Wickets))
  )
#saving the modiofied data
dat <- dat %>%
  # Replace all NA values with 0 in the entire dataframe
  mutate_all(~ifelse(is.na(.), 0, .))
total_match_data <- dat

save(total_match_data,file="Match Data/total_match_data.RData")
##############################################################################################################################
## agin loading the data so that we dont get any error 
load("Match Data/total_match_data.RData")
dat <- total_match_data

gr1 <- dat %>% group_by(Venue) %>% summarise(Matches=n(),Total_dots =sum(Dots),Total_fours=sum(Fours),Total_sixes=sum(Sixes),Average_score=(mean(as.numeric(Team_1_Score))+mean(as.numeric(Team_2_Score)))/2,Average_wickets=(mean(as.numeric(Team_1_Wickets))+mean(as.numeric(Team_2_Wickets)))/2)

stadium_data<- gr1
save(stadium_data,file="Match Data/stadium_data.RData")
