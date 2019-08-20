library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(writexl)

setwd('//CCTSRV01//Tickets//3 - Data and Analytics//Simulation//NHL RS Simulation')

#Reads in the simulation results
all.results = read_excel('NHL Simulation Results Composite 2019-20 v2.xlsx', 
                         sheet='Sheet1')

#Reads in Excel Query of Vegas lines
vegas = read_excel('./Reference Files/NHL Vegas Lines Query.xlsx', 
                   sheet = 'NHL Vegas Lines') %>%
      #Splits the line string from BetOnline by space. Only need the middle string (line)
  separate(BetOnline, into = c("string", "line", "string2"), sep=" ") %>%
  select(-string, -string2)

vegas = vegas %>%
      #Turns the string into a number by replacing the character fraction with a .5
  mutate(line = as.numeric(str_replace_all(string = vegas$line, pattern = '½', replacement = ".5"))) %>%
    #Converts the vig into numerics
  mutate(Over = ifelse(substr(vegas$Over,0,1) == '-', 
                       -as.numeric(substr(vegas$Over,2,4)), 
                       as.numeric(substr(vegas$Over,2,4)) ),
         Under = ifelse(substr(vegas$Under,0,1) == '-', 
                        -as.numeric(substr(vegas$Under,2,4)), 
                        as.numeric(substr(vegas$Under,2,4))
         )
       ) 
 
#Gets the list of all team names for looping
team.list = unique(all.results$subject.team)

#Add new result columns for the loop
vegas[ , c("mean", "Over.Sim", "Under.Sim")] = 0

#Loops over every team in the team list to see how many sims are over/under their Vegas line
for(tm in 1:length(team.list)){
  
  filtered.team = all.results %>% 
    filter(subject.team == team.list[tm])
  
  team.line = 
    as.numeric(vegas %>% filter(Team == team.list[tm]) %>% select(line))
  
  odds = prop.table(table(filtered.team$pts < team.line))
  
  vegas[tm, 'Under.Sim'] = odds[2]
  vegas[tm, 'Over.Sim'] = odds[1]
  vegas[tm, 'mean'] = mean(filtered.team$pts)
}

#If you'd like to calc EV with a margin of error taken off
e = .1


vegas = 
  vegas %>%
  #What side is the bet on
    mutate(side = ifelse(Under.Sim < Over.Sim, 'Over', 'Under'  )) %>%
  #Calculates the vig on the bet and what the p of winning is
    mutate(vig = ifelse(side == 'Over', Over/100, Under/100), 
           p = ifelse(side == 'Over', Over.Sim - e, Under.Sim - e)) %>%
  #Uses the vig and p to calc the EV per $100 bet
    mutate(EV = round(ifelse(vig <0, 
                       (100/abs(vig))* p - (100*(1-p)),
                        (vig*100*p) - (100*(1-p)) )
                      , 2) ) %>%
  #removes redundant variables
    select(-vig,-p)



#Writes CSV of results of simulation
write_xlsx(vegas, "NHL Simulation 2019-20 Vegas EV.xlsx")
