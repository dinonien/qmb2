#Script file for the different task

##Install Packages

if ("dplyr" %in% row.names(installed.packages())  == FALSE)
install.packages("dplyr") 

if ("ggplot2" %in% row.names(installed.packages())  == FALSE)
        install.packages("ggplot2") 

if ("ggExtra" %in% row.names(installed.packages())  == FALSE)
        install.packages("ggExtra") 

if ("gridExtra" %in% row.names(installed.packages())  == FALSE)
        install.packages("gridExtra") 

library("dplyr")
library("ggplot2")
library("ggExtra")
library("gridExtra")

housingrents <- read.csv("./Data/housingrents.csv",sep=";")

housingrents <- mutate(housingrents, rooms = factor(rooms), 
        nre = factor(nre,levels=c(0,1),labels=c("no","yes")))
housingrents <- mutate(housingrents,rps = rent/area)

nrehousing <- filter(housingrents,nre=="yes")
nonnrehousing <- filter(housingrents,nre=="no")

# Check normal distribution of rps variable for nre houses 

p1 <- qplot(x = 1, y = rps, data = nrehousing, xlab = "", geom = 'boxplot') +
        coord_flip(ylim=c(0,60))

p2 <- ggplot(nrehousing, aes(x = rps)) +
        geom_histogram(colour="black", fill="white") +
        coord_cartesian(xlim=c(0,60)) 

grid.arrange(p1, p2, widths = c(1, 2))

print(gg_qq(nrehousing$rps))

# Check normal distribution of rps variable for non - nre houses 


p1 <- qplot(x = 1, y = rps, data = nonnrehousing, xlab = "", geom = 'boxplot') +
        coord_flip(ylim=c(0,60))

p2 <- ggplot(nonnrehousing, aes(x = rps)) +
        geom_histogram(colour="black", fill="white") +
        coord_cartesian(xlim=c(0,60)) 

grid.arrange(p1, p2, widths = c(1, 2))

print(gg_qq(nonnrehousing$rps))

t.test(housingrents$rps~housingrents$nre,alternative = "two.sided", mu=0, var.equal = FALSE)
t.test(housingrents$rps~housingrents$nre,alternative = "two.sided", mu=0, var.equal = TRUE)
t.test(housingrents$rps~housingrents$nre,alternative = "greater", mu=0, var.equal = FALSE)
