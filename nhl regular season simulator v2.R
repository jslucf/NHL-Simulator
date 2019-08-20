library('scrapeR')
library(rvest)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(stringr)
library(RColorBrewer)
library(writexl)

#### Inputs ####
sport = 'NHL'
year = '2019'

setwd('//CCTSRV01//Tickets//3 - Data and Analytics//Simulation//NHL RS Simulation')

########### Scraping & Data Cleanup ##############

#Reads in list of NHL teams, divisions, and conferences 
teams.list = read.csv('./Reference Files/nhl teams conf list.csv')

#Reads in schedule (get from EIBO or use scraping script)
schedule = read.csv('./Reference Files/NHL 2020 schedule scrape.csv')

#Reads in list of team venues
venues = read.csv('./Reference Files/NHL Venue List.csv')

#This is for the preseason asjustment file Stock put together from the Athletic for offseason adjustment
athletic =  read.csv('./Reference Files/NHL Preseason Ratings 2019-20 MS.csv') %>%
  left_join(teams.list, by=c("Abbr" = "Abbr"))

#Removes any * indicating playoff births
athletic$Team = str_remove(athletic$Team, '\\*')

athletic$Team = as.factor(athletic$Team)

########### Pythagorean Win Expectation Calc ##############

pyth.exponents = 
  data.frame(league = c("NFL",	"NCAAF",	"NHL",	"MLB",	"NBA", "MLS"),
             exponent = c(2.37,	2.15,	1.86,	1.83,	16.5,1.7),
             stdev = c(9.82, 13, 2.31,4.3,13.47, 1.5),
             hfa = c(2.57, 4.39, 0.24, 0.21, 2.41, 0.5 ),
             ot.stdev = c(0, 0, .679,0,0,0.5)
  )

#Gets the exponent from previous table using the sport input from the top of the script
exponent = as.numeric(pyth.exponents %>% filter(league == sport) %>% select(exponent))
stdev = as.numeric(pyth.exponents %>% filter(league == sport) %>% select(stdev))
hfa = as.numeric( pyth.exponents %>% filter(league == sport) %>% select(hfa))
ot.stdev = as.numeric(pyth.exponents %>% filter(league == sport) %>% select(ot.stdev))

#Set the pythag variable to whichever ranking metric you want to use (TheAthletic, Pyth, or Composite)
athletic$pythag = athletic$Composite

#Adds the home and away pythag scores to every game on the schedule
schedule =
  schedule %>%
  left_join(athletic[, c("Team", "pythag")], by = c("home" = "Team")) %>%
  left_join(athletic[, c("Team", "pythag")], by = c("away" = "Team")) %>%
  #adds on the venues
  left_join(venues, by=c("home"="Home")) %>%
  mutate(away = as.character(away))

#log5 function that returns list with home win, away win, and OT/draw percentages
log5 = function(home_win, away_win, neutral_venue = F){
  #this function take the pythag for two teams and returns a H2H win pct for the home team adjusted for home field
  #neural_venue removes hfa for games played in neutral arenas
  
  log5_formula = (home_win * (1 - away_win)) / ((home_win * (1 - away_win)) + (away_win * (1 - home_win)))
  
  #gets the z score of the log5 number using the league stdev
  line = -qnorm(log5_formula) * stdev
  
  #Adjusts the z score for home field advanatge
  home_line = line - hfa
  
  #Uses the OT STD to adjust win percents for draws
  logH = pnorm((-home_line-ot.stdev)/stdev)
  logD = pnorm((-home_line+ot.stdev)/stdev) - pnorm((-home_line-ot.stdev)/stdev)
  logA = 1 - pnorm((-home_line + ot.stdev)/stdev)
  
  loglist = list(logH, logA, logD)
  
  return(loglist)
  
}

#this function simulates a game using log5 func from earlier. 
#4 inputs are the home and away teams and their pythag pcts
game.sim = function(home, away, h_win_pct, a_win_pct, neutral_venue = F){
  
  #randomly picks a winner using the hm_win_pct calculated above
  winner = sample(x = c(home,away), 
                  size=1, 
                  prob = c(h_win_pct, a_win_pct))
  
  return(winner)
  
}

################################################### Simulation ####################

set.seed(516)
sims = 5000

#Sets the percentage of games that go to OT
#ot.pct = .233

sim.results = c()
all.results= c()

for(sim in 1:sims){
  
  #Sets a blank schedule to use for simulation
  sim.schedule = cbind(schedule, 
                       winner = rep(NA, nrow(schedule)), 
                       isOT = rep(NA, nrow(schedule)))
  
  #This chunk does the single season simulation
  for(row in 1:nrow(sim.schedule)){
    
    #Gets the necessary game information to run the game sim function
    home = sim.schedule[row, "home"]
    away = sim.schedule[row, "away"]
    home_pythag = sim.schedule[row, "pythag.x"]
    away_pythag = sim.schedule[row, "pythag.y"]
    venue_sim = as.character(sim.schedule[row, "VenueName"])
    
    #Checks to see if the game is being played in one of the venues provided on master venues list.
    #False for NHL would probably mean outdoor games
    neutral_venue = ifelse(venue_sim %in% venues$VenueName, F, T)
    
    #uses log5 to get win pcts and ot pct
    list = log5(home_pythag, away_pythag, neutral_venue)
    
    h_win_pct = as.numeric(list[1])
    a_win_pct = as.numeric(list[2])
    ot.pct = as.numeric(list[3])
    
    #Determines if game went to OT
    ot.rand = sample(1:1000,1)/1000
    isOT = ifelse(ot.rand <= ot.pct, 1, 0)
    
    #Simulates game based on if it went to OT or not
    if(isOT == 0){
      
      #Simulates the game for that row, assuming regulation finish
      winner = game.sim(home, away, h_win_pct, a_win_pct, neutral_venue)
    
      } else{
      
      #OT Games have 50/50 odds with no HFA
      winner = game.sim(home, away, .5, .5, neutral_venue = T)
    }
    
    #Saves the game winner and if it was an OT game
    sim.schedule[row, "winner"] = winner
    sim.schedule[row, "isOT"] = isOT
    
  } #ends the season for loop
  
  #Spreads the sim.schedule to have one team on each row
  sim.schedule = sim.schedule %>%
    gather(homeaway, subject.team, -c(1,4,5,6,7,8,9)) %>%
    mutate(isWinner = ifelse(winner == subject.team, 1, 0)) %>%
    mutate(pts = ifelse(isWinner == 1, 2, 
                        ifelse(isOT == 1, 1, 0)))
  
  #Gets the team results for the season
  sim.results = 
    sim.schedule %>%
    #adds unique id for that season
    mutate(sim.id = sim) %>%
    #filter(subject.team %like% 'Kings') %>%       ########remove this line for all teams
    group_by(subject.team, sim.id) %>%
    summarize(W = sum(isWinner), 
              L = n() - sum(isWinner), 
              OT.G = sum(isOT), 
              OT.W = crossprod(isOT, isWinner),
              OT.L = OT.G - OT.W,
              pts = sum(pts) )
  
  #Binds the results of this simulated season for each team to the DF with all simulated seasons
  all.results = rbind(all.results, sim.results)
  
  print(sim)
}


################# Results #######################################

#Reads in CSV of Vegas lines
vegas = read.csv('./Reference Files/NHL Vegas Lines 8-12-19.csv')

#Set the team for histogram
team = 'Devils'
playoff.threshold = 95

filtered.team = all.results %>%
  filter(subject.team %like% team)

line= vegas %>% filter(Team %like% team) %>% select(2)

prop.table(table(filtered.team$pts < line[,1]))

#Sets the bounds for the graph
pts.min = round_any(min(filtered.team$pts), 10)
pts.max = round_any(max(filtered.team$pts), 10) +5

#Odds that team makes the playoffs
playoff.sims = round((table(filtered.team$pts >= playoff.threshold)/sims)[2] * 100,1)

#Histogram of Points
ggplot(filtered.team, 
       aes(x=pts)) + 
  geom_histogram(breaks=seq(pts.min, pts.max, by=1), 
                 col="white", 
                 fill = '#330066',
                 aes(y = (..count..)/sum(..count..)),
                 alpha=.25) + 
  scale_x_continuous(breaks = seq(pts.min, pts.max,10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste(team, "Expected Points 2019-20"),
       x = 'Points',
       y = 'Frequency') +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") + 
  
    #Line and annotation for mean pts
  geom_vline(aes(xintercept = mean(pts)), size=1.2, linetype = "dashed") +
  annotate("text", 
           label=paste("Mean/Median:", "\n", round(mean(filtered.team$pts), digits = 1), "Points"),
           x=ifelse(mean(filtered.team$pts) > playoff.threshold, mean(filtered.team$pts)+20, mean(filtered.team$pts)), 
           y=.025,
           hjust = 1.1,
           fontface = 2,
           size=4
           ) +
  
    #Line and annotation for playoffs
  geom_vline(aes(xintercept = playoff.threshold), size=1.2, col="red", linetype = "dashed") +
  annotate("text", 
           label=paste("Playoffs:", playoff.threshold, "Points"),  #Playoff pts threshhold
           x=ifelse(mean(filtered.team$pts) > playoff.threshold, playoff.threshold -20, playoff.threshold), 
           y=.025,
           hjust = -.1,
           fontface = 2,
           col = 'red',
           size=4) +
  annotate("text", 
           label = paste("Probability: ", playoff.sims, "%", sep=""),  #Odds teams makes playoffs
           x=ifelse(mean(filtered.team$pts) > playoff.threshold, playoff.threshold -20, playoff.threshold),
           y=.023,
           hjust = -.1, 
           fontface = 2, 
           col = 'red',
           size=4 ) 


#Saves the sim results to list (which allows us to name the sheet in Excel when used in the write_xlsx step)
sheetName = list(Simulation.Results = all.results)

#Names the file where sim results are saved
fileName = paste(sport, ' Simulation Results ', year, '.xlsx', sep="")

#Writes CSV of results of simulation
write_xlsx(sheetName, fileName)

#Gets the cumulative dist of points by season
#kings.pts.dist = 
  all.results %>%
    group_by(subject.team, pts) %>%
    summarize(pct = n()/sims) %>%
    mutate(cdf = cumsum(pct))

#write.csv(kings.pts.dist, "Kings Simulation Points Distribution Composite 2019-20.csv", row.names=F)

