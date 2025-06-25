library(tidyverse)
library(rvest)
library(dplyr)

# Function to clean the bowling data
clean_bowling_data <- function(df) {
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
url <- 'https://www.espncricinfo.com/series/ipl-2019-1165643/match-schedule-fixtures-and-results'
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
    bowling_table1 <- tables[[2]]  # First team
    bowling_table2 <- tables[[4]]   # Second team
    
    # Ensure the tables have enough columns
    if (ncol(bowling_table1) >= 11 && ncol(bowling_table2) >= 11) {
      # Set column names
      colnames(bowling_table1) <- c("Name", "Overs", "Maidens", "Runs", "Wickets Taken", "Economy", "Dots", "4s", "6s", "Wides", "No Balls")
      colnames(bowling_table2) <- c("Name", "Overs", "Maidens", "Runs", "Wickets Taken", "Economy", "Dots", "4s", "6s", "Wides", "No Balls")
      
      
      # Convert to tibbles
      
      team1_bowling_df <- as_tibble(bowling_table1[1:length(bowling_table1), 1:11])
      team2_bowling_df <- as_tibble(bowling_table2[1:length(bowling_table2), 1:11])
      
      
      # Clean the data for both teams
      team1_bowling_df <- clean_bowling_data(team1_bowling_df)
      team2_bowling_df <- clean_bowling_data(team2_bowling_df)
      
      
      team1_bowling_df <- mutate(team1_bowling_df, Match_ID = 201900 + i)
      team2_bowling_df <- mutate(team2_bowling_df, Match_ID = 201900 + i)
      
      rows_to_keep_1 <- rep(TRUE, nrow(team1_bowling_df))
      
      # Loop through the data frame to mark rows for deletion
      for (j in 1:(nrow(team1_bowling_df))) {
        # Check if the current row's 'Wickets' column (5th column) is greater than 0
        if (nchar(team1_bowling_df[j, 5]) > 1) {
          # Mark the next row for deletion
          rows_to_keep_1[j] <- FALSE
        }
      }
      
      # Subset the data frame to keep only the marked rows
      team1_bowling_df <- team1_bowling_df[rows_to_keep_1, ]
      
      
      rows_to_keep_2 <- rep(TRUE, nrow(team2_bowling_df))
      
      # Loop through the data frame to mark rows for deletion
      for (k in 1:(nrow(team2_bowling_df))) {
        # Check if the current row's 'Wickets' column (5th column) is greater than 0
        if (nchar(team2_bowling_df[k, 5]) > 1) {
          # Mark the next row for deletion
          rows_to_keep_2[k] <- FALSE
        }
      }
      
      # Subset the data frame to keep only the marked rows
      team2_bowling_df <- team2_bowling_df[rows_to_keep_2, ]
      
      
      ## Chaging data types of the columns to bind them properly
      team1_bowling_df$Overs <- as.numeric(team1_bowling_df$Overs)
      team2_bowling_df$Overs <- as.numeric(team2_bowling_df$Overs)
      
      team1_bowling_df$Maidens <- as.numeric(team1_bowling_df$Maidens)
      team2_bowling_df$Maidens <- as.numeric(team2_bowling_df$Maidens)
      
      team1_bowling_df$Runs <- as.numeric(team1_bowling_df$Runs)
      team2_bowling_df$Runs <- as.numeric(team2_bowling_df$Runs)
      
      team1_bowling_df$`Wickets Taken` <- as.numeric(team1_bowling_df$`Wickets Taken`)
      team2_bowling_df$`Wickets Taken` <- as.numeric(team2_bowling_df$`Wickets Taken`)
      
      team1_bowling_df$Economy <- as.numeric(team1_bowling_df$Economy)
      team2_bowling_df$Economy <- as.numeric(team2_bowling_df$Economy)
      
      team1_bowling_df$Dots <- as.numeric(team1_bowling_df$Dots)
      team2_bowling_df$Dots <- as.numeric(team2_bowling_df$Dots)
      
      team1_bowling_df$'4s' <- as.numeric(team1_bowling_df$'4s')
      team2_bowling_df$'4s' <- as.numeric(team2_bowling_df$'4s')
      
      team1_bowling_df$'6s' <- as.numeric(team1_bowling_df$'6s')
      team2_bowling_df$'6s' <- as.numeric(team2_bowling_df$'6s')
      
      team1_bowling_df$Wides <- as.numeric(team1_bowling_df$Wides)
      team2_bowling_df$Wides <- as.numeric(team2_bowling_df$Wides)
      
      team1_bowling_df$'No Balls' <- as.numeric(team1_bowling_df$'No Balls')
      team2_bowling_df$'No Balls' <- as.numeric(team2_bowling_df$'No Balls')
      
      # Assign team names to data frames
      if (length(team_names) >= 2) {
        team1_name <- team_names[1]
        team2_name <- team_names[2]
        
        team1_bowling_df <- mutate(team1_bowling_df, Team = team1_name)
        team2_bowling_df <- mutate(team2_bowling_df, Team = team2_name)
        
        # Combine the data for this match
        match_data <- bind_rows(team1_bowling_df, team2_bowling_df)
        
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
combined_bowling_data_2019 <- bind_rows(all_matches_data)

combined_bowling_data_2019 <- combined_bowling_data_2019 %>% select(Match_ID,Team,everything())

save(combined_bowling_data_2019,file="BowlingData_2019.RData")
