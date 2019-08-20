#This script scrapes the NHL schedule off Hockey Reference
#Written by Jason Laso, 8/12/19
#########################################################

library('scrapeR')
library(rvest)
library(dplyr)
library(tidyr)
library(data.table)

wd = '//CCTSRV01//Tickets//3 - Data and Analytics//Simulation//NHL RS Simulation//'
setwd(wd)

#### Inputs ####
sport = 'NHL'
year = '2020'

########### Scraping & Data Cleanup ##############

#Reads in list of NHL teams, divisions, and conferences 
teams.list = read.csv('nhl teams conf list.csv')

#Set the URL of the Hockey Reference season
standings_url = paste('https://www.hockey-reference.com/leagues/', sport, "_", year, '_games.html', sep="")

#This chunk will scrape the standings and split them into east and west
df = scrape(url = standings_url, headers=F, parse=T)
x = readHTMLTable(df[[1]]) %>% 
  as.data.frame() 

x = x[, c('games.Date', 'games.Visitor', 'games.Home')]
colnames(x) = c('date', 'home', 'away')

filename = paste(sport, year, 'schedule scrape.csv')
write.csv(x, filename, row.names = F)
