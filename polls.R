library(tidyverse)
library(ggplot2)
library(lubridate)
library(xkcd)
library(ggthemes)
library(scales)

poll <- read.table(file="polls.csv",header=T,sep=",")
tail(poll)

### make the plot data to long table first
pollplot <- poll %>% select(Order, Year, Month, National, Labour, Green, NZFirst) %>% 
  mutate(GL=Labour + Green, Date=paste0(Year,Month,"15")) %>% select(Order,Date,National,GL,NZFirst) %>% gather(key=Order,Date) 

colnames(pollplot) <- c("Order","Date","Party","Poll")

pollplot$Date <- as.Date(pollplot$Date,"%Y%B%d")
head(pollplot)

pollW <- poll %>% select(Order, Year, Month, National, Labour, Green, NZFirst) %>% 
  mutate(GLW=Labour + Green + NZFirst, NW = National + NZFirst, Date=paste0(Year,Month,"15")) %>% 
  select(Order,Date,NW,GLW) %>% gather(key=Order,Date) 

colnames(pollW) <- c("Order","Date","Party","Poll")

pollW$Date <- as.Date(pollW$Date,"%Y%B%d")
head(pollW)

# GL + N + W
a <- ggplot(data=pollplot,aes(x=Date,y=Poll,colour=Party)) + 
  stat_smooth(formula = y ~ poly(x,5), method="glm", size=1,aes(weight=Date)) + 
  scale_y_continuous(name="Percent of the Vote",breaks=seq(0,60,5)) +
  scale_color_manual(values=c("brown","blue", "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
  geom_point(position = position_jitter(width=0.1, height=0.1)) +
  labs( x = "Polling Date",title ="Going to The Polls",
      subtitle = "Guessing the Election",
      caption = "Who does Winston 1st Choose")
#a + theme_xkcd()
a

## with winston
w <- ggplot(data=pollW,aes(x=Date,y=Poll,colour=Party)) + 
  stat_smooth(formula = y ~ poly(x,5), method="glm", size=1,aes(weight=Date)) + 
  scale_y_continuous(name="Percent of the Vote",breaks=seq(0,60,5)) +
  scale_color_manual(values=c("brown","blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
  geom_point(position = position_jitter(width=0.1, height=0.1)) +
  labs( x = "Polling Date",title ="Going to The Polls",
        subtitle = "Guessing the Election",
        caption = "Who does Winston 1st Choose")

ww <- ggplot(data=pollW,aes(x=Date,y=Poll,colour=Party)) + 
  stat_smooth(formula = y ~ poly(x,5), method="glm", size=1,aes(weight=Date)) + 
  scale_color_manual(values=c("brown","blue")) +
  geom_point(position = position_jitter(width=0.1, height=0.1))

ww

ww + theme(plot.subtitle = element_text(size = 12), 
    plot.caption = element_text(size = 12), 
    axis.ticks = element_line(colour = "gray15", 
        size = 0.9), axis.title = element_text(family = "serif", 
        size = 20, face = "bold.italic"), 
    axis.text = element_text(family = "serif", 
        size = 15, colour = "gray4", angle = 30), 
    plot.title = element_text(family = "serif", 
        size = 40, face = "bold", hjust = 0.5), 
    plot.background = element_rect(linetype = "dotdash"), 
    legend.direction = "horizontal") +labs(title = "The Polls", y = "Percent", 
    subtitle = "Winston will choose what Winston Chooses", 
    caption = "What could be worse? - The Trump")

