library(dplyr)
library(ggplot2)
library(tidyr)

library(cowplot)
library(magick)





load("Match Data/total_match_data.RData")

colnames(total_match_data)

#############################################################################################################################################################
match_data <- total_match_data %>%
  mutate(Season = substr(Match_Id, 1, 4)) %>%  # Extract the year from Match_Id
  separate(Team_1_Score, into = c("Team_1_Runs", "Team_1_Wickets"), sep = "/", convert = TRUE) %>%
  separate(Team_2_Score, into = c("Team_2_Runs", "Team_2_Wickets"), sep = "/", convert = TRUE)

# Step 2: Convert runs to numeric and calculate total runs per season
match_data <- match_data %>%
  mutate(
    Team_1_Runs = as.numeric(Team_1_Runs),
    Team_2_Runs = as.numeric(Team_2_Runs)
  ) %>%
  group_by(Season) %>%
  summarize(Total_Runs = sum(Team_1_Runs + Team_2_Runs, na.rm = TRUE))

# Step 3: Plotting total runs per season
ggplot(match_data, aes(x = as.numeric(Season), y = Total_Runs)) +
  geom_line(color = "#2980b9", size = 1.5) +
  geom_point(color = "#2980b9", size = 3) +
  labs(title = "Total Runs Scored in Each IPL Season", x = "Season", y = "Total Runs") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2020, 2024, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######################################################################################################################################################################

bg_image <- image_read("www/peakpx.jpg")

toss_match_relation <- total_match_data %>%
  mutate(Win_Toss = ifelse(Toss_Winner == Winner, "Toss Winner Wins", "Toss Winner Loses")) %>%
  group_by(Season = substr(Match_Id, 1, 4), Result = Win_Toss) %>%
  summarise(Count = n(), .groups = "drop")

# Step 2: Plotting the results
plot <- ggplot(toss_match_relation, aes(x = Season, y = Count, fill = Result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Toss Winner vs Match Winner (2020-2024)", 
       x = "Seasons", 
       y = "Number of Matches") +
  theme_minimal() +
  scale_fill_manual(values = c("Toss Winner Wins" = "#2ecc71", "Toss Winner Loses" = "#e74c3c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank() )

ggdraw() +
  draw_image(bg_image, scale = 2) +   # Adjust scale as needed
  draw_plot(plot)

###########################################################################################################################################################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggbackground)


# Assuming 'total_match_data' and 'toss_match_relation' are defined
toss_match_relation <- total_match_data %>%
  mutate(Win_Toss = ifelse(Toss_Winner == Winner, "Toss Win", "Toss Loss")) %>%
  group_by(Season = substr(Match_Id, 1, 4), Result = Win_Toss) %>%
  summarise(Count = n(), .groups = "drop")

# Create your ggplot chart
plot <- ggplot(toss_match_relation, aes(x = Season, y = Count, fill = Result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Toss Winner vs Match Winner (2019-2024)", 
       x = "Season", 
       y = "Number of Matches") +
  theme_minimal() +
  scale_fill_manual(values = c("Toss Win" = "#27ae60", "Toss Loss" = "#e74c3c")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add the background image
ggbackground(plot, background = "path/to/your/image.jpg")

#########################################################################################################################################
library(dplyr)
library(ggplot2)

# Step 1: Extract the season from Match_Id
match_data <- total_match_data %>%
  mutate(Season = substr(Match_Id, 1, 4))

# Step 2: Calculate total dots and total matches per season
match_summary <- match_data %>%
  group_by(Season) %>%
  summarize(
    Total_Dots = sum(Dots, na.rm = TRUE),
    Total_Matches = n()  # Count the number of matches
  ) %>%
  mutate(Average_Dots = Total_Dots / Total_Matches)  # Calculate average dots per match

# Step 3: Plotting average dots per match using a bar graph with a blank background
ggplot(match_summary, aes(x = as.factor(Season), y = Average_Dots, fill = Average_Dots)) +  # Fill based on Average_Dots
  geom_bar(stat = "identity", color = "black", size = 0.5) +  # Black outline for bars
  labs(title = "Average Dots per Match in Each IPL Season", x = "Season", y = "Average Dots per Match") +
  scale_fill_gradientn(colors = c("#1E90FF", "#8A2BE2", "#DDA0DD", "#BA55D3", "#8A2BE2", "#4B0082")) +  # Gradient through blue, violet, and purple shades
  theme_minimal(base_size = 15) +  # Use minimal theme
  theme(panel.background = element_blank(),  # Remove panel background
        plot.background = element_blank(),   # Remove plot background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis text
        legend.position = "none") +  # Remove legend since it's not needed
  geom_text(aes(label = round(Average_Dots, 2)), vjust = -0.5, size = 4, color = "#2c3e50")  # Add data labels above bars
#######################################################################################################################################################################################



########################################################################################################################################################
load("total_bat_data.RData")

total_bat_data
