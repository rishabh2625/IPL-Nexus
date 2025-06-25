library(dplyr)
library(tidyr)
library(rvest)

load("total_match_data.RData")

dat <- total_match_data

load("../Batting Data/total_bat_data.RData")

batdat <- total_bat_data
colnames(batdat)
batdat$Name <- batdat$Name %>%
  # Remove "(c)" from names
  gsub("\\(c\\)", "", .) %>%
  # Remove any non-alphabet characters, keeping spaces
  gsub("[^a-zA-Z ]", "", .) %>%
  # Remove any extra spaces that might be left
  trimws()

length(unique(batdat$Name))

unique(batdat$Team)

colnames(batdat)

batdat <- batdat %>%
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


unique(batdat$Team)
batdat <- batdat %>%
  rename(Fours = `4s`, Sixes = `6s`)

batdat[is.na(batdat)] <- 0
batdat[batdat=="-"] <- "0"

total_bat_data <- batdat

save(total_bat_data,file="../Batting Data/total_bat_data.RData")

####################################################################################
#do not touch above it
####################################################################################
batdat <- total_bat_data

batdat1 <- batdat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Runs = sum(as.numeric(Runs), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Runs,       # The values are the runs
    names_glue = "runs_{Year}" # Column names like runs_2015, runs_2016, etc.
  )

batdat1_balls <- batdat %>%
  mutate(Year = substr(Match_ID, 1, 4)) %>%  # Extract the year from Match_ID
  group_by(Name, Year) %>%  # Group by player and year
  summarise(Balls = sum(as.numeric(Balls), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Year,        # Year becomes the column names
    values_from = Balls,      # The values are the balls faced
    names_glue = "balls_{Year}" # Column names like balls_2015, balls_2016, etc.
  )

combined_data <- left_join(batdat1, batdat1_balls, by = "Name")

years <- 2015:2024

for (year in years) {
  run_col <- paste0("runs_", year)  # Construct run column name, e.g. runs_2015
  ball_col <- paste0("balls_", year)  # Construct ball column name, e.g. balls_2015
  strike_rate_col <- paste0("strike_rate_", year)  # Construct new column name for strike rate
  
  # Add a new strike rate column by calculating (runs / balls) * 100
  combined_data <- combined_data %>%
    mutate(!!strike_rate_col := ifelse(!is.na(get(run_col)) & !is.na(get(ball_col)),
                                       (get(run_col) / get(ball_col)) * 100, NA))
}
################################################################################################################################################################################################################################################################
batdat_main <- batdat %>% group_by(Name) %>% summarise(Ducks = sum(as.numeric(Runs) == 0),Runs=sum(as.numeric(Runs)),Balls=sum(as.numeric(Balls)),Fours=sum(as.numeric(Fours)),Sixes=sum(as.numeric(Sixes)),Teams=list(unique(Team)),Matches=n())

batdat_main["Strike Rate"]=(batdat_main$Runs/batdat_main$Balls)*100



# Reorder columns in 'batdat'
batdat_main <- batdat_main %>%
  select(Name, Matches, Teams, Runs, Balls, Fours, Sixes, Ducks, `Strike Rate`)

batdat_main <-arrange(batdat_main,desc(Runs))

combined_batdata_main <- left_join(batdat_main,combined_data,by="Name")

save(combined_batdata_main,file="../Batting Data/combined_batdata_main.RData")
