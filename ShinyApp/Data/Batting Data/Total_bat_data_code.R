library(dplyr)

# COMBINING THE ALL YEARS BATTING DATA

load("batting_data_2024.RData")
load("batting_data_2023.RData")
load("batting_data_2022.RData")
load("batting_data_2021.RData")
load("batting_data_2020.RData")
load("batting_data_2019.RData")
load("batting_data_2018.RData")
load("batting_data_2017.RData")
load("batting_data_2016.RData")
load("batting_data_2015.RData")



total_bat_data<-bind_rows(combined_batting_data_2024, combined_batting_data_2023, combined_batting_data_2022, combined_batting_data_2021, combined_batting_data_2020, combined_batting_data_2019, combined_batting_data_2018, combined_batting_data_2017, combined_batting_data_2016, combined_batting_data_2015)
save(total_bat_data, file = "total_bat_data.RData")


load("total_bat_data.RData")


match_id<-unique(total_bat_data$Match_ID)


sixes_in_each_match <- numeric(length(unique(total_bat_data$Match_ID)))
fours_in_each_match <- numeric(length(unique(total_bat_data$Match_ID)))


k<-1
for(i in unique(total_bat_data$Match_ID) ){
 
  sum6 <- 0
  sum4 <- 0
  for(j in which(i==total_bat_data$Match_ID)){
    sum6= sum6+as.numeric(total_bat_data$"6s"[j])
    sum4= sum4+as.numeric(total_bat_data$"4s"[j])
  }
  sixes_in_each_match[k]<-sum6
  fours_in_each_match[k]<-sum4
  k<-k+1
}


foo <- data.frame(Match_Id=match_id,Sixes=sixes_in_each_match,Fours=fours_in_each_match)


#####################################################################################################################################################

#COMBINING THE MATCH DATA


load("../Match Data/MatchData_2024.RData")
load("../Match Data/MatchData_2023.RData")
load("../Match Data/MatchData_2022.RData")
load("../Match Data/MatchData_2021.RData")
load("../Match Data/MatchData_2020.RData")
load("../Match Data/MatchData_2019.RData")
load("../Match Data/MatchData_2018.RData")
load("../Match Data/MatchData_2017.RData")
load("../Match Data/MatchData_2016.RData")
load("../Match Data/MatchData_2015.RData")

total_match_data<-bind_rows(data_2024,data_2023,data_2022,data_2021,data_2020,data_2019,data_2018,data_2017,data_2016,data_2015)
save(total_match_data, file = "../Match Data/total_match_data.RData")

######################################################################################################################################################

#ADDING THE COLUNS SIXEX AND FOURS

load("../Match Data/total_match_data.RData")

total_match_data <- total_match_data %>%
  left_join(foo, by = "Match_Id")


######################################################################################################################################################

#COMBINING BOWLING DATA


load("../Bowling Data/BowlingData_2024.RData")
load("../Bowling Data/BowlingData_2023.RData")
load("../Bowling Data/BowlingData_2022.RData")
load("../Bowling Data/BowlingData_2021.RData")
load("../Bowling Data/BowlingData_2020.RData")
load("../Bowling Data/BowlingData_2019.RData")
load("../Bowling Data/BowlingData_2018.RData")
load("../Bowling Data/BowlingData_2017.RData")
load("../Bowling Data/BowlingData_2016.RData")
load("../Bowling Data/BowlingData_2015.RData")

total_bowling_data<-bind_rows(combined_bowling_data_2024, combined_bowling_data_2023, combined_bowling_data_2022, combined_bowling_data_2021, combined_bowling_data_2020, combined_bowling_data_2019, combined_bowling_data_2018, combined_bowling_data_2017, combined_bowling_data_2016, combined_bowling_data_2015)

save(total_bowling_data, file = "../Bowling Data/total_bowling_data.RData")

#############################################################################################################################################################################

#ADDING TOTAL DOSTS COLUMNS


match_id<-unique(total_bowling_data$Match_ID)
dots_in_each_match <- numeric(length(unique(total_bowling_data$Match_ID)))
k<-1
for(i in unique(total_bowling_data$Match_ID) ){
  
  dots <- 0
  for(j in which(i==total_bowling_data$Match_ID)){
    dots = dots + total_bowling_data$"Dots"[j]
    
  }
  dots_in_each_match[k]<-dots
  k<-k+1
}

foo2 <- data.frame(Match_Id=match_id,Dots=dots_in_each_match)
total_match_data <- total_match_data %>%
  left_join(foo2, by = "Match_Id")


# Replace NA values with 0 in total_match_data using base R
total_match_data[is.na(total_match_data)] <- 0
save(total_match_data, file = "../Match Data/total_match_data.RData")

# Replace NA values with 0 in total_match_data using base R
total_bat_data[is.na(total_bat_data)] <- 0
save(total_bat_data, file = "total_bat_data.RData")

# Replace NA values with 0 in total_match_data using base R
total_bowling_data[is.na(total_bowling_data)] <- 0
save(total_bowling_data, file = "../Bowling Data/total_bowling_data.RData")



