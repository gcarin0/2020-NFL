#Giancarlo Carino
#NFL Offense stats
#2020 Season Wk 9

#install packages
install.packages(c("tidyverse", "reshape2"))      

#load libraries
library(tidyverse)
library(reshape2)

setwd("C:/Users/Giancarlo/Desktop/R/Projects/Football")

#Load/Clean Off drive data
NFLOff <- read_csv("Wk 9/2020 Offense Data Wk 9.csv")
NFLOffDrives <- read_csv("Wk 9/2020 Offense Drives Wk 9.csv")
NFLOffDrives$Rk <- NULL
names(NFLOffDrives)[7] <- "AvgPlaysperDrive"
names(NFLOffDrives)[8] <- "AvgYdsperDrive"
names(NFLOffDrives)[9] <- "AvgStartperDrive"
names(NFLOffDrives)[10] <- "AvgTimeperDrive"
names(NFLOffDrives)[11] <- "AvgPtsperDrive"

#Create new columns on per game basis
NFLOff <- mutate(NFLOff, TotalTDs = `Rush TD` + `Pass TD`)
NFLOff <- mutate(NFLOff, TotalTDspG = TotalTDs/G)
NFLOff <- mutate(NFLOff, PtspG = PF/G)
NFLOff <- mutate(NFLOff, YdspG = `Total Yds`/G)
NFLOff <- mutate(NFLOff, PlyspG = `Total Ply`/G)
NFLOff <- mutate(NFLOff, TOspG = TO/G)
NFLOff <- mutate(NFLOff, `1stDpG` = `Total 1stD`/G)
NFLOff <- mutate(NFLOff, CmppG = Cmp/G)
NFLOff <- mutate(NFLOff, AttpG = Att/G)
NFLOff <- mutate(NFLOff, PassYdspG = `Pass Yds`/G)
NFLOff <- mutate(NFLOff, PassTDspG = `Pass TD`/G)
NFLOff <- mutate(NFLOff, IntpG = Int/G)
NFLOff <- mutate(NFLOff, `Pass1stDpG` = `Pass 1stD`/G)
NFLOff <- mutate(NFLOff, RushAttpG = `Rush Att`/G)
NFLOff <- mutate(NFLOff, RushYdspG = `Rush Yds`/G)
NFLOff <- mutate(NFLOff, RushTDspG = `Rush TD`/G)
NFLOff <- mutate(NFLOff, Rush1stDpG = `Rush 1stD`/G)

NFLOff$TotalTDs<- round(NFLOff$TotalTDs, 2)
NFLOff$TotalTDspG <- round(NFLOff$TotalTDspG, 2)
NFLOff$PtspG <- round(NFLOff$PtspG, 2)
NFLOff$YdspG <- round(NFLOff$YdspG, 2)
NFLOff$PlyspG <- round(NFLOff$PlyspG, 2)
NFLOff$TOspG <- round(NFLOff$TOspG, 2)
NFLOff$`1stDpG` <- round(NFLOff$`1stDpG`, 2)
NFLOff$CmppG <- round(NFLOff$CmppG, 2)
NFLOff$AttpG<- round(NFLOff$AttpG, 2)
NFLOff$PassYdspG <- round(NFLOff$PassYdspG, 2)
NFLOff$PassTDspG <- round(NFLOff$PassTDspG, 2)
NFLOff$IntpG <- round(NFLOff$IntpG, 2)
NFLOff$`Pass1stDpG` <- round(NFLOff$`Pass1stDpG`, 2)
NFLOff$RushAttpG <- round(NFLOff$RushAttpG, 2)
NFLOff$RushYdspG <- round(NFLOff$RushYdspG, 2)
NFLOff$RushTDspG <- round(NFLOff$RushTDspG, 2)
NFLOff$Rush1stDpG <- round(NFLOff$Rush1stDpG, 2)

NFLOffperG <- NFLOff[, c(1:4,17, 22, 31:46)]
NFLOffDrives <- NFLOffDrives[, c(1,3:6, 7:11)]
NFLOffperG <- NFLOffperG %>%
                left_join(NFLOffDrives)
NFLOff <- NFLOff%>%
  left_join(NFLOffDrives)

summary(NFLOffperG$PtspG)

#Zscore calc
NFLOffperG <- mutate(NFLOffperG, zPtspG = (PtspG - mean(PtspG))/sd(PtspG))
NFLOffperG$zPtspG <- round(NFLOffperG$zPtspG, 2)
NFLOffperG <- mutate(NFLOffperG, zYdspG = (YdspG - mean(YdspG))/sd(YdspG))
NFLOffperG$zYdspG <- round(NFLOffperG$zYdspG, 2)
NFLOffperG <- mutate(NFLOffperG, zPlyspG = (PlyspG - mean(PlyspG))/sd(PlyspG))
NFLOffperG$zPlyspG<- round(NFLOffperG$zPlyspG, 2)
NFLOffperG <- mutate(NFLOffperG, zTOspG = (TOspG - mean(TOspG))/sd(TOspG))
NFLOffperG$zTOspG <- round(NFLOffperG$zTOspG, 2)
NFLOffperG <- mutate(NFLOffperG, `z1stDpG` = (`1stDpG` - mean(`1stDpG`))/sd(`1stDpG`))
NFLOffperG$`z1stDpG` <- round(NFLOffperG$`z1stDpG`, 2)
NFLOffperG <- mutate(NFLOffperG, zCmppG = (CmppG - mean(CmppG))/sd(CmppG))
NFLOffperG$zCmppG <- round(NFLOffperG$zCmppG, 2)
NFLOffperG <- mutate(NFLOffperG, zAttpG = (AttpG - mean(AttpG))/sd(AttpG))
NFLOffperG$zAttpG <- round(NFLOffperG$zAttpG, 2)
NFLOffperG <- mutate(NFLOffperG, zPassYdspG = (PassYdspG - mean(PassYdspG))/sd(PassYdspG))
NFLOffperG$zPassYdspG <- round(NFLOffperG$zPassYdspG, 2)
NFLOffperG <- mutate(NFLOffperG, zPassTDspG = (PassTDspG - mean(PassTDspG))/sd(PassTDspG))
NFLOffperG$zPassTDspG <- round(NFLOffperG$zPassTDspG, 2)
NFLOffperG <- mutate(NFLOffperG, zIntpG = (IntpG - mean(IntpG))/sd(IntpG))
NFLOffperG$zIntpG <- round(NFLOffperG$zIntpG, 2)
NFLOffperG <- mutate(NFLOffperG, `zPass1stDpG` = (`Pass1stDpG` - mean(`Pass1stDpG`))/sd(`Pass1stDpG`))
NFLOffperG$`zPass1stDpG` <- round(NFLOffperG$`zPass1stDpG`, 2)
NFLOffperG <- mutate(NFLOffperG, zRushAttpG = (RushAttpG - mean(RushAttpG))/sd(RushAttpG))
NFLOffperG$zRushAttpG <- round(NFLOffperG$zRushAttpG, 2)
NFLOffperG <- mutate(NFLOffperG, zRushYdspG = (RushYdspG - mean(RushYdspG))/sd(RushYdspG))
NFLOffperG$zRushYdspG <- round(NFLOffperG$zRushYdspG, 2)
NFLOffperG <- mutate(NFLOffperG, zRushTDspG = (RushTDspG - mean(RushTDspG))/sd(RushTDspG))
NFLOffperG$zRushTDspG <- round(NFLOffperG$zRushTDspG, 2)
NFLOffperG <- mutate(NFLOffperG, zRush1stDpG = (Rush1stDpG - mean(Rush1stDpG))/sd(Rush1stDpG))
NFLOffperG$zRush1stDpG <- round(NFLOffperG$zRush1stDpG, 2)

#subset Z
NFLOffZ <- NFLOffperG[, c(1, 30:44)]
NFLOffperG <- NFLOffperG[, -c(1, 30:44)]

#Drives per Game
NFLOffperG <- mutate(NFLOffperG, DrivespG = `#Dr`/G)
NFLOffperG$DrivespG <- round(NFLOffperG$DrivespG, 2)
