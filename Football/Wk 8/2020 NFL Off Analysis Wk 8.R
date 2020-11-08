#Giancarlo Carino
#NFL Offense stats
#2020 Season Wk 8

#load libraries
library(tidyverse)

#Load data
NFLOff <- read_csv("2020 Offense Data Wk 8.csv")

#Regression to find relationship with Wins
offReg <- lm(W ~ NFLOff$`Pass Yds` + NFLOff$`Pass TD` + NFLOff$Att + 
                 NFLOff$Cmp + NFLOffWk8$`Rush Att` + NFLOff$`Rush Yds` + 
                 NFLOff$`Rush  TD` + NFLOff$`Rush Y/A` + 
                 NFLOff$`Pass NY/A`, data = NFLOff)
#Regression results
summary(offReg)
#Pass Yds and TDs have statistically significant influence

#Rebuild w/o Rush stats-> no influence in Wins
offReg2 <- lm(W ~ NFLOff$Att + NFLOff$`Pass Yds` + NFLOff$`Pass TD` + 
                  NFLOff$Cmp + NFLOff$`Pass NY/A` +
                  NFLOff$`Y/P` + NFLOff$`Sc%`, 
                  data = NFLOff)

#Results
summary(offReg2)

#Scatterplot w/ Wins vs Sc%
ggplot(NFLOff, aes(x = `Sc%`, y = W)) +
  geom_point(size = 2, shape = 23) +
  geom_smooth(method = lm, se = FALSE)

#Strong linear relationship between W (Wins) and Sc% (% of drives that end in a score)
#Quantify relationship with Sc% from other Offensive stats

#Regression
OffReg3 <- lm(`Sc%` ~ NFLOff$Att + NFLOff$`Pass Yds` + NFLOff$`Pass TD` + 
                      NFLOff$Cmp +
                      NFLOff$`Rush Att` + NFLOff$`Rush Yds` + 
                      NFLOff$`Rush  TD` + NFLOff$`Rush Y/A`, data = NFLOff)

#Results
summary(OffReg3)

#Passing stats show stronger effect and statistical signficance
OffReg4 <- lm(`Sc%` ~ NFLOff$Att + NFLOff$`Pass Yds` + NFLOff$`Pass TD` + 
                      NFLOff$Cmp, data = NFLOff)

#Results
summary(OffReg4)

#Pass Tds and Cmp show strongest relationship w/ statistical signficance w/ Sc%

#Correlation
cor(NFLOff$`Sc%`, NFLOff$`Pass TD`)
cor(NFLOff$`Sc%`, NFLOff$Cmp)
#Pass Tds has a stronger relationship with Sc%

