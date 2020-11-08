#Giancarlo Carino
#NFL Offense stats
#2020 Season Wk 9

#install packages
install.packages(c("tidyverse", "reshape2"))

#load libraries
library(tidyverse)
library(reshape2)

setwd("C:/Users/Giancarlo/Desktop/R/Projects/Football")

#Load data
NFLOff <- read_csv("Wk 9/2020 Offense Data Wk 9.csv")

#Regression to find relationship with Wins
offReg <- lm(W ~ NFLOff$`Pass Yds` + NFLOff$`Pass TD` + NFLOff$Att + 
                 NFLOff$Cmp + NFLOff$`Rush Att` + NFLOff$`Rush Yds` + 
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

#Regression
OffReg3 <- lm(`Sc%` ~ NFLOff$Att + NFLOff$`Pass Yds` + NFLOff$`Pass TD` + 
                      NFLOff$Cmp +
                      NFLOff$`Rush Att` + NFLOff$`Rush Yds` + 
                      NFLOff$`Rush  TD` + NFLOff$`Rush Y/A`, data = NFLOff)

#Results
summary(OffReg3)

#Passing stats show stronger effect and statistical significance
OffReg4 <- lm(`Sc%` ~ NFLOff$Att + NFLOff$`Pass Yds` + NFLOff$`Pass TD` + 
                      NFLOff$Cmp, data = NFLOff)

#Results
summary(OffReg4)

#Correlation
cor(NFLOff$`Sc%`, NFLOff$`Pass TD`)
cor(NFLOff$`Sc%`, NFLOff$Cmp)

#Corr matrix
cordata <- NFLOff[, c(3, 5, 6, 8, 11,12,14,15,17:23,27,29)]
cormat <- round(cor(cordata), 2)
head(cormat)
meltcormat <- melt(cormat)
head(meltcormat)
cormap <- ggplot(data = meltcormat, aes(Var1, Var2, fill = value)) +
          geom_tile()
cormap()
cormap

#get lower triangle of corr matrix
getlowertri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

#get upper triangle of corr matrix
getuppertri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

#upper tri
uptri <- getuppertri(cormat)
uptri

#lower tri
lowtri <- getlowertri(cormat)
lowtri

# Melt corr matrix
meltcormat <- melt(uptri, na.rm = TRUE)
# Heatmap
ggplot(data = meltcormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))+
  coord_fixed()

#Load/Clean Off drive data
NFLOffDrives <- read_csv("Wk 9/2020 Offense Drives Wk 9.csv")
NFLOffDrives$Rk <- NULL
names(NFLOffDrives)[8] <- "AvgPlaysperDrive"
names(NFLOffDrives)[9] <- "AvgYdsperDrive"
names(NFLOffDrives)[10] <- "AvgStartperDrive"
names(NFLOffDrives)[11] <- "AvgTimeperDrive"
names(NFLOffDrives)[12] <- "AvgPtsperDrive"

NFLOffplDr <- NFLOff %>% left_join(NFLOffDrives)
NFLOffperG <- select(NFLOff, Tm)
NFLOff <- mutate(NFLOff, YdspG = `Total Yds`/G)
NFLOff <- mutate(NFLOff, PlyspG = `Total Ply`/G)

NFLOffperG <- NFLOff[, c(1,30)]
