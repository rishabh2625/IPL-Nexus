library(dplyr)
library(tidyr)
library(rvest)

load("../Match Data/total_match_data.RData")

dat <- total_match_data

load("total_bowling_data.RData")

bowldat <- total_bowling_data
colnames(bowldat)
bowldat$Name <- bowldat$Name %>%
  # Remove "(c)" from names
  gsub("\\(c\\)", "", .) %>%
  # Remove any non-alphabet characters, keeping spaces
  gsub("[^a-zA-Z ]", "", .) %>%
  # Remove any extra spaces that might be left
  trimws()

length(unique(bowldat$Name))

unique(bowldat$Team)

colnames(bowldat)

bowldat <- bowldat %>%
  mutate(Team = case_when(
    Team == "Royal Challengers Bengaluru" ~ "RCB",
    Team == "Chennai Super Kings" ~ "CSK",
    Team == "Delhi Capitals" ~ "DC",
    Team == "Punjab Kings" ~ "PBKS",
    Team == "Kolkata Knight Riders" ~ "KKR",
    Team == "Sunrisers Hyderabad" ~ "SRH",
    Team == "Rajasthan Royals" ~ "RR",
    Team == "Lucknow Super Giants" ~ "LSG",
    Team == "Gujarat Titans" ~ "GT",
    Team == "Mumbai Indians" ~ "MI",
    Team == "Royal Challengers Bangalore" ~ "RCB",
    Team == "Kings XI Punjab" ~ "PBKS",
    Team == "Delhi Daredevils" ~ "DC",
    Team == "Rising Pune Supergiant" ~ "RPS",
    Team == "Gujarat Lions" ~ "GT",
    Team == "Rising Pune Supergiants" ~ "RPS",
    
  ))


unique(bowldat$Team)
bowldat <- bowldat %>%
  rename(Fours = `4s`, Sixes = `6s`)

bowldat[is.na(bowldat)] <- 0
bowldat[bowldat=="-"] <- "0"

total_bowl_data <- bowldat

save(total_bowl_data,file="total_bowl_data.RData")

####################################################################################
#do not touch above it
####################################################################################
bowldat <- total_bowl_data

bowldat1 <- bowldat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Runs = sum(as.numeric(Runs), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Runs,       # The values are the runs
    names_glue = "runs_{Year}" # Column names like runs_2015, runs_2016, etc.
  )

bowldat1_overs <- bowldat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Overs = sum(as.numeric(Overs), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Overs,      # The values are the balls faced
    names_glue = "overs_{Year}" # Column names like balls_2015, balls_2016, etc.
  )

combined_data <- left_join(bowldat1, bowldat1_overs, by = "Name")

bowldat1_wickets <- bowldat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Wickets = sum(as.numeric(`Wickets Taken`), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Wickets,      # The values are the balls faced
    names_glue = "wickets_{Year}" # Column names like balls_2015, balls_2016, etc.
  )

combined_data <- left_join(combined_data, bowldat1_wickets, by = 'Name')

bowldat1_dots <- bowldat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Dots = sum(as.numeric(Dots), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Dots,      # The values are the balls faced
    names_glue = "dots_{Year}" # Column names like balls_2015, balls_2016, etc.
  )

combined_data <- left_join(combined_data, bowldat1_dots, by = "Name")

years <- 2015:2024

for (year in years) {
  run_col <- paste0("runs_", year)  # Construct run column name, e.g. runs_2015
  overs_col <- paste0("overs_", year)  # Construct ball column name, e.g. balls_2015
  economy_rate_col <- paste0("economy_rate_", year)  # Construct new column name for strike rate
  
  # Add a new strike rate column by calculating (runs / balls) * 100
  combined_data <- combined_data %>%
    mutate(!!economy_rate_col := ifelse(!is.na(get(run_col)) & !is.na(get(overs_col)),
                                       (get(run_col) / get(overs_col)), NA))
}
################################################################################################################################################################################################################################################################
bowldat_main <- bowldat %>% group_by(Name) %>% summarise(Runs=sum(as.numeric(Runs)),Overs=sum(as.numeric(Overs)), Wickets = sum(`Wickets Taken`), Dots = sum(Dots), Fours=sum(as.numeric(Fours)),Sixes=sum(as.numeric(Sixes)),Teams=list(unique(Team)),Matches=n())

bowldat_main["Economy Rate"]=(bowldat_main$Runs/bowldat_main$Overs)



# Reorder columns in 'bowldat'
bowldat_main <- bowldat_main %>%
  select(Name, Matches, Teams, Runs, Overs, Wickets, Dots, Fours, Sixes, `Economy Rate`)

bowldat_main <-arrange(bowldat_main,desc(Runs))

combined_bowldata_main <- left_join(bowldat_main,combined_data,by="Name")

save(combined_bowldata_main,file="combined_bowldata_main.RData")
