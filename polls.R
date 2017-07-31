library(tidyverse)
library(lubridate)
library(xkcd)
library(ggthemes)
library(scales)

poll <- read.table(file="polls.csv",header=T,sep=",")
tail(poll)

### make the plot data to long table first
pollplot <- poll %>% select(Order, Poll,Year, Month, National, Labour, Green, NZFirst) %>% 
  mutate(GL=Labour + Green, Date=paste0(Year,Month,"15")) %>% select(Order,Date,National,GL,NZFirst) %>% 
  gather(key=Order,Date)

colnames(pollplot) <- c("Order","Date","Party","Poll")
pollplot <- mutate(pollplot,Company=rep(poll$Poll,3))


pollplot$Date <- as.Date(pollplot$Date,"%Y%B%d")
head(pollplot)

pollW <- poll %>% select(Order, Year, Month, National, Labour, Green, NZFirst) %>% 
  mutate(GLW=Labour + Green + NZFirst, NW = National + NZFirst, Date=paste0(Year,Month,"15")) %>% 
  select(Order,Date,NW,GLW) %>% gather(key=Order,Date) 

colnames(pollW) <- c("Order","Date","Party","Poll")
pollW <- mutate(pollW,Company=rep(poll$Poll,2))
pollW$Date <- as.Date(pollW$Date,"%Y%B%d")
head(pollW)

# GL + N + W
a <- ggplot(data=pollplot,aes(x=Date,y=Poll,colour=Party)) + 
  geom_point(position = position_jitter(width=5, height=0.0),aes(shape=Company,size=1.5),alpha=0.7) +
  scale_shape_manual(values=c(18,17,15, 16, 1))+
  stat_smooth(formula = y ~ poly(x,5), method="glm", level = 0.99,size=1,aes(weight=Date)) + 
  scale_y_continuous(name="Percent of the Vote",breaks=seq(0,60,5)) +
  scale_color_manual(values=c("brown","blue", "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
  labs( x = "Polling Date",title ="Going to The Polls",
      subtitle = "Guessing the Election",
      caption = "Who does Winston 1st Choose") + 
  theme(plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15), 
  axis.title = element_text(size = 15), plot.title = element_text(size = 20))
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
w



