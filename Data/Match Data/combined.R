load("total_match_data.RData")

df <- total_match_data

df <- df %>%
  mutate(Team_1 = case_when(
    Team_1 == "Royal Challengers Bengaluru" ~ "RCB",
    Team_1 == "Chennai Super Kings" ~ "CSK",
    Team_1 == "Delhi Capitals" ~ "DC",
    Team_1 == "Punjab Kings" ~ "PBKS",
    Team_1 == "Kolkata Knight Riders" ~ "KKR",
    Team_1 == "Sunrisers Hyderabad" ~ "SRH",
    Team_1 == "Rajasthan Royals" ~ "RR",
    Team_1 == "Lucknow Super Giants" ~ "LSG",
    Team_1 == "Gujarat Titans" ~ "GT",
    Team_1 == "Mumbai Indians" ~ "MI",
    Team_1 == "Royal Challengers Bangalore" ~ "RCB",
    Team_1 == "Kings XI Punjab" ~ "PBKS",
    Team_1 == "Delhi Daredevils" ~ "DC",
    Team_1 == "Rising Pune Supergiant" ~ "RPS",
    Team_1 == "Gujarat Lions" ~ "GT",
    Team_1 == "Rising Pune Supergiants" ~ "RPS",
    
  ))
df <- df %>%
  mutate(Team_2 = case_when(
    Team_2== "Royal Challengers Bengaluru" ~ "RCB",
    Team_2 == "Chennai Super Kings" ~ "CSK",
    Team_2 == "Delhi Capitals" ~ "DC",
    Team_2 == "Punjab Kings" ~ "PBKS",
    Team_2 == "Kolkata Knight Riders" ~ "KKR",
    Team_2 == "Sunrisers Hyderabad" ~ "SRH",
    Team_2 == "Rajasthan Royals" ~ "RR",
    Team_2 == "Lucknow Super Giants" ~ "LSG",
    Team_2 == "Gujarat Titans" ~ "GT",
    Team_2 == "Mumbai Indians" ~ "MI",
    Team_2 == "Royal Challengers Bangalore" ~ "RCB",
    Team_2 == "Kings XI Punjab" ~ "PBKS",
    Team_2 == "Delhi Daredevils" ~ "DC",
    Team_2 == "Rising Pune Supergiant" ~ "RPS",
    Team_2 == "Gujarat Lions" ~ "GT",
    Team_2 == "Rising Pune Supergiants" ~ "RPS",
    
  ),
  Toss_Winner = case_when(
    Toss_Winner == "Royal Challengers Bengaluru" ~ "RCB",
    Toss_Winner == "Chennai Super Kings" ~ "CSK",
    Toss_Winner == "Delhi Capitals" ~ "DC",
    Toss_Winner == "Punjab Kings" ~ "PBKS",
    Toss_Winner == "Kolkata Knight Riders" ~ "KKR",
    Toss_Winner == "Sunrisers Hyderabad" ~ "SRH",
    Toss_Winner == "Rajasthan Royals" ~ "RR",
    Toss_Winner == "Lucknow Super Giants" ~ "LSG",
    Toss_Winner == "Gujarat Titans" ~ "GT",
    Toss_Winner == "Mumbai Indians" ~ "MI",
    Toss_Winner == "Royal Challengers Bangalore" ~ "RCB",
    Toss_Winner == "Kings XI Punjab" ~ "PBKS",
    Toss_Winner == "Delhi Daredevils" ~ "DC",
    Toss_Winner == "Rising Pune Supergiant" ~ "RPS",
    Toss_Winner == "Gujarat Lions" ~ "GT",
    Toss_Winner == "Rising Pune Supergiants" ~ "RPS",
    
  ),
  Winner = case_when(
    Winner == "Royal Challengers Bengaluru" ~ "RCB",
    Winner == "Chennai Super Kings" ~ "CSK",
    Winner == "Delhi Capitals" ~ "DC",
    Winner == "Punjab Kings" ~ "PBKS",
    Winner == "Kolkata Knight Riders" ~ "KKR",
    Winner == "Sunrisers Hyderabad" ~ "SRH",
    Winner == "Rajasthan Royals" ~ "RR",
    Winner == "Lucknow Super Giants" ~ "LSG",
    Winner == "Gujarat Titans" ~ "GT",
    Winner == "Mumbai Indians" ~ "MI",
    Winner == "Royal Challengers Bangalore" ~ "RCB",
    Winner == "Kings XI Punjab" ~ "PBKS",
    Winner == "Delhi Daredevils" ~ "DC",
    Winner == "Rising Pune Supergiant" ~ "RPS",
    Winner == "Gujarat Lions" ~ "GT",
    Winner == "Rising Pune Supergiants" ~ "RPS",
    
  )
  )
colnames(df)

df <- df %>%
  drop_na()

# Create the team statistics dataset
team_dataset1 <-
  # Calculate for Team_1
  df %>%
              select(Match_Id,Team_1, Toss_Winner,
                     Team_1_Score, Team_1_Wickets, Winner) %>%
              rename(Team = Team_1, Score = Team_1_Score, Wickets = Team_1_Wickets)
  # Calculate for Team_2
  team_dataset2 <-
    df %>%
              select(Match_Id,Team_2, Toss_Winner,
                     Team_2_Score, Team_2_Wickets, Winner) %>%
              rename(Team = Team_2, Score = Team_2_Score, Wickets = Team_2_Wickets) 
  
  
  
  
  
  team_dataset1 <- team_dataset1 %>% group_by(Team) %>%
    summarise(
      Matches_Played = n(),
     
    
      Highest_Score = max(Score, na.rm = TRUE),
      Lowest_Score = min(Score[Score != 0 & !is.na(Score)], na.rm = TRUE),
      Total_Runs = sum(Score, na.rm = TRUE),
      Total_Wickets = sum(Wickets, na.rm = TRUE)
    )
  team_dataset2 <- team_dataset2 %>% group_by(Team) %>%
    summarise(
      Matches_Played = n(),
     
      
      Highest_Score = max(Score, na.rm = TRUE),
      Lowest_Score = min(Score[Score != 0 & !is.na(Score)], na.rm = TRUE),
      Total_Runs = sum(Score, na.rm = TRUE),
      Total_Wickets = sum(Wickets, na.rm = TRUE)
    )
  
  
  
  colnames(team_dataset1) 
  
  combined_team_data <- team_dataset1 %>%
    full_join(team_dataset2, by = "Team", suffix = c("_1", "_2")) %>%
    mutate(
      Matches_Played = coalesce(Matches_Played_1, 0) + coalesce(Matches_Played_2, 0),
      Highest_Score = pmax(coalesce(Highest_Score_1, 0), coalesce(Highest_Score_2, 0)),
      Lowest_Score = pmin(coalesce(Lowest_Score_1, Inf), coalesce(Lowest_Score_2, Inf)),
      Total_Runs = coalesce(Total_Runs_1, 0) + coalesce(Total_Runs_2, 0),
      Total_Wickets = coalesce(Total_Wickets_1, 0) + coalesce(Total_Wickets_2, 0)
    ) %>%
    select(Team, Matches_Played, Highest_Score, Lowest_Score, Total_Runs, Total_Wickets)
  
  
  df_wins <- df %>% group_by(Winner) %>% summarise(Won=n())
  df_wins$Team <- df_wins$Winner
  df_wins <- df_wins %>% select(-Winner)
  combined_team_data <- bind_cols(combined_team_data,df_wins)
  combined_team_data <- combined_team_data %>% select(-Team...8)
  
  combined_team_data$Win_percentage <- (combined_team_data$Won/combined_team_data$Matches_Played)*100     

  combined_team_data <- combined_team_data %>% rename(Team =Team...1)
  
  
  combined_team_data$Image <- c("Chennai.png", "Delhi.jpg", "Gujarat.png", "Kolkata.png",
                                "Lucknow.png", "Mumbai1.png", "Punjab.png", "Banglore.png","pune.png",
                                "Rajasthan.png", "Hyderabad.png")
  
 
  combined_team_data <- combined_team_data %>% rename(Matches=Matches_Played,Wins=Won,Win_Percentage=Win_percentage)
  
  save(combined_team_data,file="combined_team_data.RData")   
  










