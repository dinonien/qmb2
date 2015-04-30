#Script file for the different task

##Install Packages

if ("dplyr" %in% row.names(installed.packages())  == FALSE)
install.packages("dplyr") 

if ("ggplot2" %in% row.names(installed.packages())  == FALSE)
        install.packages("ggplot2") 

library("dplyr")
library("ggplot2")

housingrents <- read.csv("./Data/housingrents.csv",sep=";")

housingrents <- mutate(housingrents, rooms = factor(rooms), 
        nre = factor(nre,levels=c(0,1),labels=c("no","yes")))
housingrents <- mutate(housingrents,rps = rent/area)

nrehousing <- filter(housingrents,nre=="yes")
nonnrehousing <- filter(housingrents,nre=="no")

qqnorm(nrehousing$rps)
qqline(nrehousing$rps, col = 2)

qqnorm(nonnrehousing$rps)
qqline(nonnrehousing$rps, col = 2)