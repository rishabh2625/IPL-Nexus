library(tidyverse)
library(rvest)
library(dplyr)


# Function to clean the batting data
clean_batting_data <- function(df) {
  df %>%
    filter(!is.na(Name) & Name != "") %>%  # Remove rows with missing names
    { 
      if ("Extras" %in% .$Name) {
        slice(., 1:(which(.$Name == "Extras") - 1))
      } else {
        .
      }
    }
}

# Initialize an empty list to store data frames
all_matches_data <- list()

# URL to get match schedule
url <- 'https://www.espncricinfo.com/series/ipl-2018-1131611/match-schedule-fixtures-and-results'
website <- read_html(url)

# Extract match links
matchlinks <- website %>% 
  html_elements('.ds-grow.ds-px-4.ds-border-r.ds-border-line-default-translucent') %>% 
  html_element('a') %>% 
  html_attr('href')

# Base URL to complete match links
base_url <- 'https://www.espncricinfo.com'
matchlinks <- str_c(base_url, matchlinks)

# Loop through match URLs
for (i in seq_along(matchlinks)) {
  # Read the webpage
  webpage <- read_html(matchlinks[i])
  #webpage <- read_html("https://www.espncricinfo.com/series/ipl-2019-1165643/chennai-super-kings-vs-mumbai-indians-final-1181768/full-scorecard")
  # Extract all tables
  tables <- webpage %>% html_table()
  
  # Extract team names from the specified class
  team_names <- webpage %>%
    html_elements('.ds-text-title-xs.ds-font-bold.ds-capitalize') %>%
    html_text(trim = TRUE)
  
  # Check if the expected tables exist
  if (length(tables) >= 3) {
    # Extract batting tables
    batting_table1 <- tables[[1]]  # First team
    batting_table2 <- tables[[3]]   # Second team
    
    # Ensure the tables have enough columns
    if (ncol(batting_table1) >= 8 && ncol(batting_table2) >= 8) {
      # Set column names
      colnames(batting_table1) <- c("Name", "Dismissal", "Runs", "Balls", "Other", "4s", "6s", "Strike Rate")
      colnames(batting_table2) <- c("Name", "Dismissal", "Runs", "Balls", "Other", "4s", "6s", "Strike Rate")
      
      
      # Convert to tibbles
      team1_batting_df <- as_tibble(batting_table1[1:dim(batting_table1)[1], 1:8])
      team2_batting_df <- as_tibble(batting_table2[1:dim(batting_table2)[1], 1:8])
      
      
      # Clean the data for both teams
      team1_batting_df <- clean_batting_data(team1_batting_df)
      team2_batting_df <- clean_batting_data(team2_batting_df)
      
      team1_batting_df <- mutate(team1_batting_df, Match_ID = 201800 + i)
      team2_batting_df <- mutate(team2_batting_df, Match_ID = 201800 + i)
      # Assign team names to data frames
      if (length(team_names) >= 2) {
        team1_name <- team_names[1]
        team2_name <- team_names[2]
        
        team1_batting_df <- mutate(team1_batting_df, Team = team1_name)
        team2_batting_df <- mutate(team2_batting_df, Team = team2_name)
        
        # Combine the data for this match
        match_data <- bind_rows(team1_batting_df, team2_batting_df)
        
        # Store the match data in the list
        all_matches_data[[length(all_matches_data) + 1]] <- match_data
      } else {
        message("Team names could not be extracted for URL: ", matchlinks[i])
      }
    } else {
      message("Not enough columns in batting tables for URL: ", matchlinks[i])
    }
  } else {
    message("Not enough tables found for URL: ", matchlinks[i])
  }
}
# Combine all matches into a single data frame
combined_batting_data_2018 <- bind_rows(all_matches_data)

combined_batting_data_2018 <- combined_batting_data_2018 %>% select(Match_ID, Team, everything())

save(combined_batting_data_2018,file="batting_data_2018.RData")
