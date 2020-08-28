#Load required packages
library(tidyverse)
library(teamcolors)
library(ggimage)

#Scrape MLB Team 2019 Relief Pitching Wins Above Average data from baseball reference & convert to data frame
bp_19 <- xml2::read_html("https://www.baseball-reference.com/leagues/team_compare.cgi?request=1&year=2019&lg=MLB&stat=WAA") %>% 
                          rvest::html_nodes("#team_output") %>% rvest::html_table()
bp_19 <- as.data.frame(bp_19)

bp_19 <- bp_19 %>% select(5) %>% 
            summarize(Team = substr(RP,1,3),
                      RP_WAA = as.numeric(substring(RP,4,nchar(RP)))) %>% 
            filter(row_number() != n()) #removes avg row 


#Download Retrosheet 2019 game log data locally and read into data frame (TXT file in repository): https://www.retrosheet.org/gamelogs/index.html
gl_19 <- read.csv("GL2019.TXT", header = FALSE, sep = ",")
gl_19 <- gl_19 %>% select(4,7,10:12) %>% 
            rename("AwayTeam" = V4, "HomeTeam" = V7, "AwayScore" = V10, "HomeScore" = V11, "NumOuts" = V12) %>% 
            filter(NumOuts > 54) %>% #filter for extra inning games
            mutate(AwayTeam = case_when(AwayTeam == "ANA"~ "LAA",
                            AwayTeam == "CHA"~"CHW",
                            AwayTeam == "CHN"~"CHC",
                            AwayTeam == "KCA"~"KCR",
                            AwayTeam == "LAN"~"LAD",
                            AwayTeam == "NYA"~"NYY",
                            AwayTeam == "NYN"~"NYM",
                            AwayTeam == "SDN"~"SDP",
                            AwayTeam == "SFN"~"SFG",
                            AwayTeam == "SLN"~"STL",
                            AwayTeam == "TBA"~"TBR",
                            AwayTeam == "WAS"~"WSN",
                            TRUE ~ AwayTeam),
                  HomeTeam = case_when(HomeTeam == "ANA"~ "LAA",
                                       HomeTeam == "CHA"~"CHW",
                                       HomeTeam == "CHN"~"CHC",
                                       HomeTeam == "KCA"~"KCR",
                                       HomeTeam == "LAN"~"LAD",
                                       HomeTeam == "NYA"~"NYY",
                                       HomeTeam == "NYN"~"NYM",
                                       HomeTeam == "SDN"~"SDP",
                                       HomeTeam == "SFN"~"SFG",
                                       HomeTeam == "SLN"~"STL",
                                       HomeTeam == "TBA"~"TBR",
                                       HomeTeam == "WAS"~"WSN",
                                       TRUE ~ HomeTeam),
                  Winner = if_else(AwayScore>HomeScore,AwayTeam,HomeTeam),
                  Loser = if_else(AwayScore>HomeScore,HomeTeam,AwayTeam)) 

#Create ei_winpct_19 data frame that calculates the 2019 extra inning win percentage for each team
ei_winpct_19 <- gl_19 %>% group_by(AwayTeam) %>% 
  summarize(ei_away_wins = sum(Winner == AwayTeam),
            ei_away_losses = sum(Loser == AwayTeam)) %>% 
left_join(gl_19 %>% group_by(HomeTeam) %>% 
  summarize(ei_home_wins = sum(Winner == HomeTeam),
            ei_home_losses = sum(Loser == HomeTeam)), by = c("AwayTeam" = "HomeTeam")) %>% 
  summarize(Team = AwayTeam,
            ei_winpct = (ei_away_wins + ei_home_wins) / (ei_away_wins + ei_home_wins + ei_away_losses + ei_home_losses),
            num_games= ei_away_wins + ei_home_wins + ei_away_losses + ei_home_losses)

#Get MLB Team IDs and Abbreviations and join with team logo data (CSV file in repository)
team_abbrev <- read_csv("mlb_id.CSV")

team_logo <- teamcolors %>%
  filter(league == "mlb") %>% 
  left_join(team_abbrev, by = c("name" = "Full Team Name"))

#Create Scatterplot to compare Bullpen WAA against Extra Inning Win Percentage
bp_19 %>% left_join(ei_winpct_19, by = "Team") %>% 
  left_join(team_logo, by = c("Team" = "Team ID")) %>% 
  ggplot(aes(x=RP_WAA, y= ei_winpct)) + 
  geom_point() + 
  #geom_smooth(method='lm', se = FALSE) +
  labs(y= "Extra Innings Win %", x = "Bullpen Wins Above Average") +
  scale_y_continuous(labels=function(x){sprintf("%.3f", x)}, breaks = seq(0.0, 1.0, by = 0.1)) +
  scale_x_continuous(labels=function(x){sprintf("%.1f", x)}, breaks = seq(-10.0, 5.0, by = 0.5)) +
  geom_vline(xintercept = mean(bp_19$RP_WAA), color="red", linetype = 2) +
  geom_text(aes(x=mean(bp_19$RP_WAA)+0.3, label="MLB Avg", y=max(ei_winpct_19$ei_winpct)), color="black", angle=0, text=element_text(size=8)) +
  geom_image(aes(image = logo)) +
  theme(plot.subtitle=element_text(size = 10, face = "italic"),plot.caption=element_text(size = 8),
        panel.background = element_blank()) +
  labs(title = "Does a Strong Bullpen Help Win Extra Inning Games?", subtitle = "Extra Innings Win Percentage vs. Bullpen WAA - 2019 MLB Season", caption = "Data: Baseball Reference & Retrosheet | Plot: @mikeyirene")

