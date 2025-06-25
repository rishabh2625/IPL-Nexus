library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(purrr)

##################################################
#loading the needed data

load("Data/Match Data/total_match_data.RData")
load("Data/Batting Data/combined_batdata_main.RData")
load("Data/Bowling Data/combined_bowldata_main.RData")
load("Data/Match Data/combined_team_data.RData")
load("Data/Match Data/stadium_data.RData")

###################################################

teams_data <- combined_team_data

###################################################




# UI

ui <- dashboardPage(
  dashboardHeader(title = "IPL Data Analysis", titleWidth = 250),
  
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(
        "Introduction and Winners",
        tabName = "intro",
        icon = icon("info-circle")
      ),
      menuItem(
        "Match Data Analysis",
        tabName = "match",
        icon = icon("chart-line")
      ),
      menuItem(
        "Pitch Wise Analysis",
        tabName = 'pitch',
        icon = icon('baseball-ball')
      ),
      menuItem(
        "Batting Data Analysis",
        tabName = "batting",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Bowling Data Analysis",
        tabName = "bowling",
        icon = icon("bowling-ball")
      ),
      menuItem(
        "Team Comparison",
        tabName = "team_comparison",
        icon = icon("balance-scale")
      )
    )
  ),
  
  
  dashboardBody(
    tags$head(tags$style(
      HTML(
        "
        /* Sidebar Styling */
        .skin-blue .main-sidebar {
          background-color: #2c3e50;
          background: url('peakpx4.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #3498db; /* Active background color */

          color: white; /* Active text color */

        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #ecf0f1; /* Default text color */
          border-radius: 8px; /* Rounded corners */
          padding: 10px; /* Padding for better spacing */

        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #2980b9; /* Hover background color */

        }
        .skin-blue .main-header {
          background-color: #2980b9; /* Header background color */

        }
        .skin-blue .main-header .logo {
          background-color: #3498db; /* Logo background color */
          color: white;
          font-weight: bold;
           background: url('peakpx4.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
        }
        .skin-blue .main-header .navbar {
          background-color: #34495e; /* Navbar background color */
           background: url('peakpx4.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
        }

        /* Background Image Styling */
        .content-wrapper {
          background: url('peakpx4.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
        }

        /* Tab Item Background Color and Border */
        .tab-content {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 20px;
          border-radius: 8px; /* Rounded corners for tab content */
          border: 2px solid #2980b9; /* Border for tabs */
          margin: 20px 0; /* Space between tabs and edges */

        }

        /* Tab Heading Styling */
        .tab-pane h2 {
          color: #2980b9;
          font-weight: bold;
          border-bottom: 2px solid #2980b9; /* Underline heading */
          padding-bottom: 10px; /* Space below heading */
        }

        /* Header and Text Color */
        .box {
          color:#2980b9 ;
        }

        /* Button Styling */
        .btn {
          background-color: #2980b9; /* Button background color */
          color: white; /* Button text color */
          border-radius: 5px; /* Rounded corners */
        }
        .btn:hover {
          background-color: #3498db; /* Button hover color */
        }

        /* Card Styling */
        .team-card {
          background-color: rgba(255, 255, 255, 0.8);
          background: url('peakpx4.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
          color: white;
          padding: 20px;
          text-align: center;
          border: 1px solid #2980b9; /* Card border */
          height: 70vh; /* Card height */


          display: flex; /* Flexbox for centering content */
          flex-direction: column; /* Stack elements vertically */
          justify-content: center; /* Center elements vertically */
        }

        .team-card img {
          width: 100px; /* Set image width */
          height: 100px; /* Maintain aspect ratio */
          border-radius: 5px; /* Round image corners */
        }
        .tab1{
         background: url('peakpx.jpg') no-repeat center center;
          background-size: cover;
          background-color: transparent;
        }
      "
      )
    )),
    
    ################################################################################
    
    tabItems(
      # Introduction and Winners Tab
      tabItem(
        tabName = "intro",
        h2("Introduction and Winners"),
        tabsetPanel(
          tabPanel(
            "Introduction",
            h2("Introduction", style = "color:#2980b9 ; font-weight: bold;"),
            p(
              "This app provides a comprehensive analysis of IPL data over the last ten years from 2015-2024."
            ),
            h3(" Team:"),
            h4("1. Harsh Agrawalla"),
            h4("2. Rishabh Yadav"),
            h4("3. Ekta Kumari"),
            h4("4. Shekhar Suman")
          ),
          
          tabPanel(
            "Winners",
            h3("Winners of Last Ten Years"),
            tableOutput("winnersTable")
          ),
          
          tabPanel(
            "Orange Cap Holders",
            h3("Orange Cap Winners of Last Ten Years"),
            tableOutput("orangewinnersTable")
          ),
          
          tabPanel(
            "Purple Cap Holders",
            h3("Purple Cap Winners of Last Ten Years"),
            tableOutput("purplewinnersTable")
          )
        )
      ),
      
      ################################################################################
      
      # Match Data Analysis Tab
      
      tabItem(tabName = "match", tabItem(
        tabName = "match",
        h2("Match Data Analysis"),
        
        tabsetPanel(
          tabPanel(
            "Total Runs Analysis",
            selectInput(
              "matchRunsAnalysis",
              "Select Type:",
              choices = c("Total Runs Scored", "Average Runs per Match")
            ),
            plotOutput("matchRunsPlot")
          ),
          tabPanel(
            "Boundary Analysis",
            selectInput(
              "matchSixesAnalysis",
              "Select Type:",
              choices = c("Total Sixes", "Average Sixes", "Total Fours", "Average Fours")
            ),
            plotOutput("matchSixesPlot")
          ),
          tabPanel(
            "Some Other Analysis",
            selectInput(
              "matchFoursAnalysis",
              "Select Type:",
              choices = c("Toss Winner Wins", "Dots")
            ),
            plotOutput("matchFoursPlot")
          )
        )
        
      )),
      
      ################################################################################
      
      # Pitch wise data analysis tab
      tabItem(
        tabName = 'pitch',
        titlePanel("Pitch Wise Analysis"),
        
        fluidRow(column(
          12,
          selectInput(
            'pitchy',
            "Select Pitch",
            choices = stadium_data$Venue,
            selected = (stadium_data$Venue)[1],
            width = '100%'
          )
        )),
        
        fluidRow(column(
          12,
          h3(textOutput("pitch_name"), style = 'font-weight: bold;'),
          tableOutput("pitch_table")
        ))
      ),
      
      ################################################################################
      
      # Batting Data Analysis Tab
      
      
      tabItem(
        tabName = "batting",
        
        titlePanel("Batting Data Analysis"),
        
        
        fluidRow(column(
          12,
          selectInput(
            "player",
            "Select Player",
            choices = unique(combined_batdata_main$Name),
            selected = unique(combined_batdata_main$Name)[1],
            width = "100%"
          )
        )),
        
        
        fluidRow(
          column(
            12,
            
            h3(textOutput("player_name"), style = "font-weight: bold;"),
            
            
            tableOutput("player_table"),
            
            
            
            h4("Runs Scored Over Years"),
            plotOutput("runs_barplot"),
            
            
            h4("Strike Rate Over Years"),
            plotOutput("strike_rate_barplot")
          )
        )
        
        
      ),
      
      ################################################################################
      
      # Bowling Data Analysis Tab
      tabItem(
        tabName = "bowling",
        
        titlePanel("Bowling Data Analysis"),
        
        
        fluidRow(column(
          12,
          selectInput(
            "playerb",
            "Select Player",
            choices = unique(combined_bowldata_main$Name),
            selected = unique(combined_bowldata_main$Name)[1],
            width = "100%"
          )
        )),
        
        
        fluidRow(
          column(
            12,
            
            h3(textOutput("playerb_name"), style = "font-weight: bold;"),
            
            
            tableOutput("playerb_table"),
            
            
            h4("Runs Conceded Over Years"),
            plotOutput("runbs_barplot"),
            
            
            h4("Wickets Taken Over Years"),
            plotOutput("wickets_barplot"),
            
            
            h4("Economy rate over the years"),
            plotOutput('economy_rate_lineplot'),
            
            
            h4("Dots Bowled Over years"),
            plotOutput("dots_barplot")
          )
        )
      ),
      
      ################################################################################
      
      # Team Comparison Tab
      tabItem(tabName = "team_comparison", h2("Team Comparison"), fluidRow(
        column(
          width = 6,
          box(
            title = "Team 1",
            status = "primary",
            solidHeader = TRUE,
            class = "team-card",
            width = 12,
            selectInput("team1", "Select Team 1:", choices = teams_data$Team),
            uiOutput("team1Info")
          )
        ), column(
          width = 6,
          box(
            title = "Team 2",
            status = "primary",
            solidHeader = TRUE,
            class = "team-card",
            width = 12,
            selectInput("team2", "Select Team 2:", choices = teams_data$Team),
            uiOutput("team2Info")
          )
        )
      ))
    )
  )
)



################################################################################
############SERVER#############
################################################################################



server <- function(input, output, session) {
  # Data for Winners Table
  
  output$winnersTable <- renderTable({
    winners_data <- data.frame(
      Year = 2024:2015,
      Winner = c(
        "Kolkata Knight Riders",
        "Chennai Super Kings",
        "Gujarat Titans",
        "Chennai Super Kings",
        "Mumbai Indians",
        "Mumbai Indians",
        "Chennai Super Kings",
        "Mumbai Indians",
        "Sunrisers Hyderabad",
        "Mumbai Indians"
      ),
      Image = c(
        "Kolkata.png",
        "Chennai.png",
        "Gujarat.png",
        "Chennai.png",
        "Mumbai1.png",
        "Mumbai1.png",
        "Chennai.png",
        "Mumbai1.png",
        "Hyderabad.png",
        "Mumbai1.png"
      )
    )
    
    winners_data %>% mutate(Winners = paste0("<img src='", Image, "' height='50'/> ", Winner)) %>%
      select(Year, Winners) %>%
      mutate(Winners = lapply(Winners, htmltools::HTML))
  }, sanitize.text.function = function(x)
    x)
  
  
  ################################################################################
  
  #Table 2
  
  output$orangewinnersTable <- renderTable({
    #Data for orange cap winners
    
    datao <- data.frame(
      Year = 2024:2015,
      Player_Team = rev(
        c(
          "David Warner (SRH)",
          "Virat Kohli (RCB)",
          "David Warner (SRH)",
          "Kane Williamson (SRH)",
          "David Warner (SRH)",
          "KL Rahul (KXIP)",
          "Ruturaj Gaikwad (CSK)",
          "Jos Buttler (RR)",
          "Shubman Gill (GT)",
          "Virat Kohli (RCB)"
        )
      ),
      Matches = rev(c(14, 16, 14, 17, 12, 14, 16, 17, 17, 15)),
      Runs = rev(c(
        562, 973, 641, 735, 692, 670, 635, 863, 890, 741
      ))
    )
    
    
    final_data <- datao %>%
      separate(
        Player_Team,
        into = c("Player", "Team"),
        sep = " \\(",
        extra = "drop"
      ) %>%
      mutate(Team = gsub("\\)", "", Team)) %>%
      select(Year, Player, Team, Matches, Runs)
    orange_cap_data <- final_data
    
  })
  
  ################################################################################
  
  #Table 3
  
  output$purplewinnersTable <- renderTable({
    #Data for orange cap winners
    
    bowler_data <- data.frame(
      Year = 2024:2015,
      Player_Team = rev(
        c(
          "Dwayne Bravo (CSK)",
          "Bhuvneshwar Kumar (SRH)",
          "Bhuvneshwar Kumar (SRH)",
          "Andrew Tye (KXIP)",
          "Imran Tahir (CSK)",
          "Kagiso Rabada (DC)",
          "Harshal Patel (RCB)",
          "Yuzvendra Chahal (RR)",
          "Mohammed Shami (GT)",
          "Harshal Patel (PBKS)"
        )
      ),
      Matches = rev(c(16, 17, 14, 14, 17, 17, 15, 17, 17, 14)),
      Wickets = rev(c(26, 23, 26, 24, 26, 30, 32, 27, 28, 24))
    )
    
    
    final_bowler_data <- bowler_data %>%
      separate(
        Player_Team,
        into = c("Player", "Team"),
        sep = " \\(",
        extra = "drop"
      ) %>%
      mutate(Team = gsub("\\)", "", Team)) %>%
      select(Year, Player, Team, Matches, Wickets)
    
    
  })
  
  
  
  
  ################################################################################
  #FIRST PAGE DONE################################################################
  ################################################################################
  
  
  
  
  # TOTAL RUNS PLOT
  
  output$matchRunsPlot <- renderPlot({
    if (input$matchRunsAnalysis == "Total Runs Scored") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4)) %>%
        separate(
          Team_1_Score,
          into = c("Team_1_Runs", "Team_1_Wickets"),
          sep = "/",
          convert = TRUE
        ) %>%
        separate(
          Team_2_Score,
          into = c("Team_2_Runs", "Team_2_Wickets"),
          sep = "/",
          convert = TRUE
        )
      
      
      match_data <- match_data %>%
        mutate(Team_1_Runs = as.numeric(Team_1_Runs),
               Team_2_Runs = as.numeric(Team_2_Runs)) %>%
        group_by(Season) %>%
        summarize(Total_Runs = sum(Team_1_Runs + Team_2_Runs, na.rm = TRUE))
      
      
      ggplot(match_data, aes(x = factor(Season), y = Total_Runs)) +
        geom_line(group = 1,
                  color = "#34495e",
                  size = 1.5)  +
        geom_point(aes(color = Total_Runs), size = 4) +
        scale_color_gradient(low = "#e74c3c", high = "#2ecc71") +
        geom_text(
          aes(label = Total_Runs),
          vjust = -1,
          size = 3,
          color = "#2c3e50"
        ) +
        labs(
          title = "Total Runs Scored in Each IPL Season",
          subtitle = "Analysis of the cumulative runs scored over the years",
          x = "Season",
          y = "Total Runs",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            color = "#2c3e50"
          ),
          axis.title = element_text(color = "#34495e")
        )
      
    }
    
    
    else if (input$matchRunsAnalysis == "Average Runs per Match") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4)) %>%
        separate(
          Team_1_Score,
          into = c("Team_1_Runs", "Team_1_Wickets"),
          sep = "/",
          convert = TRUE
        ) %>%
        separate(
          Team_2_Score,
          into = c("Team_2_Runs", "Team_2_Wickets"),
          sep = "/",
          convert = TRUE
        )
      
      
      match_data <- match_data %>%
        mutate(Team_1_Runs = as.numeric(Team_1_Runs),
               Team_2_Runs = as.numeric(Team_2_Runs)) %>%
        group_by(Season) %>%
        summarize(
          Average_Runs = mean(Team_1_Runs + Team_2_Runs, na.rm = TRUE),
          .groups = "drop"
        )
      
      
      ggplot(match_data, aes(x = factor(Season), y = Average_Runs)) +
        geom_line(group = 1,
                  color = "#34495e",
                  size = 1.5) +
        geom_point(aes(color = Average_Runs), size = 4) +
        scale_color_gradient(low = "#e74c3c", high = "#2ecc71") +
        geom_text(
          aes(label = round(Average_Runs, 1)),
          vjust = -1,
          size = 3,
          color = "#2c3e50"
        ) +
        labs(
          title = "Average Runs per Match in Each IPL Season",
          subtitle = "Visualizing the average scoring trend across seasons",
          x = "Season",
          y = "Average Runs per Match",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            color = "#2c3e50"
          ),
          axis.title = element_text(color = "#34495e")
        )
      
    }
  })
  
  ################################################################################
  ####ADDING SIXES PER BALL AND FOURS PER BALL AFTER FEEDBACK ####################
  ################################################################################
  
  output$matchSixesPlot <- renderPlot({
    if (input$matchSixesAnalysis == "Total Sixes") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4))
      
      
      match_summary <- match_data %>%
        group_by(Season) %>%
        summarize(Total_Sixes = sum(Sixes, na.rm = TRUE),
                  .groups = "drop")
      
      
      ggplot(match_summary, aes(x = factor(Season), y = Total_Sixes)) +
        geom_line(group = 1,
                  color = "#8e44ad",
                  size = 1.5) +
        geom_point(aes(color = Total_Sixes), size = 4) +
        scale_color_gradient(low = "#3498db", high = "#e74c3c") +
        geom_text(
          aes(label = Total_Sixes),
          vjust = -1,
          size = 3,
          color = "#2c3e50"
        ) +
        labs(
          title = "Total Sixes Hit in Each IPL Season",
          subtitle = "Visualizing the power-hitting trend across seasons",
          x = "Season",
          y = "Total Sixes",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            color = "#2c3e50"
          ),
          axis.title = element_text(color = "#34495e")
        )
    }
    
    
    else if (input$matchSixesAnalysis == "Average Sixes") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4))  
      
      match_summary <- match_data %>%
        group_by(Season) %>%
        summarize(
          Total_Sixes = sum(Sixes, na.rm = TRUE),
          Total_Matches = n(),  
          Average_Sixes = Total_Sixes / Total_Matches, 
          .groups = "drop"
        )
      
      
      ggplot(match_summary, aes(x = factor(Season), y = Average_Sixes)) +
        geom_line(group=1,color = "#8e44ad", size = 1.5) +  
        geom_point(aes(color = Average_Sixes), size = 4) +  
        scale_color_gradient(low = "#3498db", high = "#e74c3c") +  
        geom_text(aes(label = round(Average_Sixes, 2)), vjust = -1, size = 3, color = "#2c3e50") +  
        labs(
          title = "Average Sixes per Match in Each IPL Season",
          subtitle = "Visualizing the power-hitting efficiency trend across seasons",
          x = "Season",
          y = "Average Sixes per Match",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),  
          plot.background = element_blank(),  
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#2c3e50"), 
          axis.title = element_text(color = "#34495e")
        )
    }
    
    
    else if (input$matchSixesAnalysis == "Total Fours") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4))
      
      
      match_summary <- match_data %>%
        group_by(Season) %>%
        summarize(Total_Fours = sum(Fours, na.rm = TRUE),
                  .groups = "drop")
      
      
      ggplot(match_summary, aes(x = factor(Season), y = Total_Fours)) +
        geom_line(group = 1,
                  color = "#8e44ad",
                  size = 1.5) +
        geom_point(aes(color = Total_Fours), size = 4) +
        scale_color_gradient(low = "#3498db", high = "#e74c3c") +
        geom_text(
          aes(label = Total_Fours),
          vjust = -1,
          size = 3,
          color = "#2c3e50"
        ) +
        labs(
          title = "Total Fours Hit in Each IPL Season",
          subtitle = "Visualizing the batting efficiency trend across seasons",
          x = "Season",
          y = "Total Fours",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            color = "#2c3e50"
          ),
          axis.title = element_text(color = "#34495e")
        )
    }
    
    
    else if (input$matchSixesAnalysis == "Average Fours") {
      
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4))  
      

      match_summary <- match_data %>%
        group_by(Season) %>%
        summarize(
          Total_Fours = sum(Fours, na.rm = TRUE),  
          Total_Matches = n(),  
          .groups = "drop"
        ) %>%
        mutate(Average_Fours = Total_Fours / Total_Matches)  
      
    
      ggplot(match_summary, aes(x = factor(Season), y = Average_Fours)) +
        geom_line(group=1,color = "#8e44ad", size = 1.5) +  
        geom_point(aes(color = Average_Fours), size = 4) +  
        scale_color_gradient(low = "#3498db", high = "#e74c3c") +  
        geom_text(aes(label = round(Average_Fours, 2)), vjust = -1, size = 3, color = "#2c3e50") +  
        labs(
          title = "Average Fours per Match in Each IPL Season",
          subtitle = "Visualizing batting efficiency across seasons",
          x = "Season",
          y = "Average Fours per Match",
          caption = "Source: IPL Data"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          panel.grid = element_blank(),  
          plot.background = element_blank(),  
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#2c3e50"),  # Style x-axis text
          axis.title = element_text(color = "#34495e")  
        )
    } 
  })
  
  ################################################################################
  
  output$matchFoursPlot <- renderPlot({
    if (input$matchFoursAnalysis == "Toss Winner Wins") {
      toss_match_relation <- total_match_data %>%
        mutate(Win_Toss = ifelse(
          Toss_Winner == Winner,
          "Toss Winner Wins",
          "Toss Winner Loses"
        )) %>%
        group_by(Season = substr(Match_Id, 1, 4), Result = Win_Toss) %>%
        summarise(Count = n(), .groups = "drop")
      
      
      ggplot(toss_match_relation,
             aes(x = Season, y = Count, fill = Result)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Toss Winner vs Match Winner (2015-2024)", x = "Seasons", y = "Number of Matches") +
        theme_minimal() +
        scale_fill_manual(values = c(
          "Toss Winner Wins" = "#2ecc71",
          "Toss Winner Loses" = "#e74c3c"
        )) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank())
      
      
    }
    else if (input$matchFoursAnalysis == "Dots") {
      match_data <- total_match_data %>%
        mutate(Season = substr(Match_Id, 1, 4))
      
      
      match_summary <- match_data %>%
        group_by(Season) %>%
        summarize(Total_Dots = sum(Dots, na.rm = TRUE),
                  Total_Matches = n()) %>%
        mutate(Average_Dots = Total_Dots / Total_Matches)
      
      
      ggplot(match_summary,
             aes(
               x = as.factor(Season),
               y = Average_Dots,
               fill = Average_Dots
             )) +
        geom_bar(stat = "identity", size = 0.5) +
        labs(title = "Average Dots per Match in Each IPL Season", x = "Season", y = "Average Dots per Match") +
        scale_fill_gradientn(colors = c("red", "yellow", "green", "purple", "violet", "orange")) +
        theme_minimal(base_size = 15) +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            vjust = 1
          ),
          legend.position = "none"
        ) +
        geom_text(
          aes(label = round(Average_Dots, 2)),
          vjust = -0.5,
          size = 4,
          color = "#2c3e50"
        )
    }
    
  })
  
  ################################################################################
  ###Pitch Data Tab######
  ################################################################################
  
  pitch_stats <- reactive({
    stadium_data %>%
      filter(Venue == input$pitchy)
  })
  
  output$pitch_name <- renderText({
    input$pitchy
  })
  
  output$pitch_table <- renderTable({
    datas <- pitch_stats()
    datas_long <- reshape(
      datas,
      varying = colnames(datas),
      v.names = "Value",
      timevar = "Variable",
      times = colnames(datas),
      direction = 'long'
    )
    datas_long <- datas_long %>% select(-id)
    datas_long <- datas_long %>%
      filter(!apply(datas_long, 1, function(z)
        any(is.na(z))))
    
  })
  ################################################################################
  ########Batting Data Analysis Plot and Table#########
  ################################################################################
  
  player_data <- reactive({
    combined_batdata_main %>%
      rowwise() %>%
      filter(Name == input$player) %>%
      mutate(Teams = paste(unlist(Teams), collapse = ", ")) %>%
      ungroup()
  })
  
  
  output$player_name <- renderText({
    input$player
  })
  
  
  
  output$player_table <- renderTable({
    data <- player_data()
    data_long <- reshape(
      data,
      varying = colnames(data),
      v.names = "Value",
      timevar = "Variable",
      times = colnames(data),
      direction = "long"
    )
    data_long <- data_long %>% select(-id)
    
    data_long <- data_long %>%
      filter(!apply(data_long, 1, function(x)
        any(is.na(x))))
    
  })
  
  
  # Barplot for Runs scored over years for each batsman
  
  output$runs_barplot <- renderPlot({
    player <- player_data()
    runs_years <- player %>%
      select(starts_with("runs_")) %>%
      gather(key = "year", value = "runs") %>%
      mutate(year = gsub("runs_", "", year)) %>%
      arrange(year)
    
    ggplot(runs_years, aes(x = year, y = runs, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$player, "- Runs Over the Years"),
        x = "Year",
        y = "Runs"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        # Remove grid for a cleaner look
        plot.background = element_blank()
      )
  })
  
  # Barplot for Strike Rate over years for each batsman
  output$strike_rate_barplot <- renderPlot({
    player <- player_data()
    strike_rate_years <- player %>%
      select(starts_with("strike_rate_")) %>%
      gather(key = "year", value = "strike_rate") %>%
      mutate(year = gsub("strike_rate_", "", year)) %>%
      arrange(year)
    
    ggplot(strike_rate_years, aes(x = year, y = strike_rate, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$player, "- Strike Rate Over the Years"),
        x = "Year",
        y = "Strike Rate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        # Remove grid for a cleaner look
        plot.background = element_blank()
      )
  })
  
  ################################################################################
  ######Bowl Data#######
  ################################################################################
  
  
  
  playerb_data <- reactive({
    combined_bowldata_main %>%
      rowwise() %>%
      filter(Name == input$playerb) %>%
      mutate(Teams = paste(unlist(Teams), collapse = ", ")) %>%
      ungroup()
  })
  
  
  output$playerb_name <- renderText({
    input$playerb
  })
  
  
  
  output$playerb_table <- renderTable({
    data <- playerb_data()
    data_long <- reshape(
      data,
      varying = colnames(data),
      v.names = "Value",
      timevar = "Variable",
      times = colnames(data),
      direction = "long"
    )
    data_long <- data_long %>% select(-id)
    data_long <- data_long %>%
      filter(!apply(data_long, 1, function(x)
        any(is.na(x))))
    
  })
  
  # Barplot for Runs conceded over years
  
  
  output$runbs_barplot <- renderPlot({
    playerb <- playerb_data()
    runs_years <- playerb %>%
      select(starts_with("runs_")) %>%
      gather(key = "year", value = "runs") %>%
      mutate(year = gsub("runs_", "", year)) %>%
      arrange(year)
    
    ggplot(runs_years, aes(x = year, y = runs, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$playerb, "- Runs over the Years"),
        x = "Year",
        y = "Runs"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.background = element_blank()
      )
  })
  
  
  # Barplot for Wickets over years
  
  
  output$wickets_barplot <- renderPlot({
    playerb <- playerb_data()
    wickets_years <- playerb %>%
      select(starts_with("wickets_")) %>%
      gather(key = "year", value = "wickets") %>%
      mutate(year = gsub("wickets_", "", year)) %>%
      arrange(year)
    
    ggplot(wickets_years, aes(x = year, y = wickets, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$playerb, "- Wickets over the Years"),
        x = "Year",
        y = "Wickets"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.background = element_blank()
      )
  })
  
  # Lineplot for Economy Rate over years
  
  
  output$economy_rate_lineplot <- renderPlot({
    playerb <- playerb_data()
    economy_years <- playerb %>%
      select(starts_with("economy_rate_")) %>%
      gather(key = "year", value = "economy") %>%
      mutate(year = gsub("economy_rate_", "", year)) %>%
      arrange(year)
    
    ggplot(economy_years, aes(x = year, y = economy, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$playerb, "- Economy Rate over the Years"),
        x = "Year",
        y = "Economy Rate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.background = element_blank()
      )
  })
  
  # Barplot for Dots bowled over the years
  
  
  output$dots_barplot <- renderPlot({
    playerb <- playerb_data()
    dots_years <- playerb %>%
      select(starts_with("dots_")) %>%
      gather(key = "year", value = "dots") %>%
      mutate(year = gsub("dots_", "", year)) %>%
      arrange(year)
    
    ggplot(dots_years, aes(x = year, y = dots, fill = year)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste(input$playerb, "- Dots over the Years"),
        x = "Year",
        y = "Dots"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.background = element_blank()
      )
  })
  
  ################################################################################
  ####Team Comparision####
  ################################################################################
  
  # Team 1 Card
  
  output$team1Info <- renderUI({
    selected_team1 <- input$team1
    team_info <- teams_data[teams_data$Team == selected_team1, ]
    
    tagList(
      img(
        src = team_info$Image,
        alt = selected_team1,
        style = "width:100px; height:auto;"
      ),
      p(paste("Matches:", team_info$Matches)),
      p(paste("Wins:", team_info$Wins)),
      p(
        paste("Average Score:", team_info$Total_Runs / team_info$Matches)
      ),
      p(
        paste(
          "Average Wickets Taken:",
          team_info$Total_Wickets / team_info$Matches
        )
      ),
      p(paste(
        "Highest Score:", team_info$Highest_Score
      )),
      p(paste(
        "Lowest Score:", team_info$Lowest_Score
      )),
      p(paste(
        "Win Percentage:", round(team_info$Win_Percentage , 2), "%"
      ))
    )
  })
  
  # Team 2 Card
  
  output$team2Info <- renderUI({
    selected_team2 <- input$team2
    team_info <- teams_data[teams_data$Team == selected_team2, ]
    
    tagList(
      img(
        src = team_info$Image,
        alt = selected_team2,
        style = "width:100px; height:auto;"
      ),
      p(paste("Matches:", team_info$Matches)),
      p(paste("Wins:", team_info$Wins)),
      p(
        paste("Average Score:", team_info$Total_Runs / team_info$Matches)
      ),
      p(
        paste(
          "Average Wickets Taken:",
          team_info$Total_Wickets / team_info$Matches
        )
      ),
      p(paste(
        "Highest Score:", team_info$Highest_Score
      )),
      p(paste(
        "Lowest Score:", team_info$Lowest_Score
      )),
      p(paste(
        "Win Percentage:", round(team_info$Win_Percentage , 2), "%"
      ))
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
