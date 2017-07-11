library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(xkcd)

poll <- read.table(file="polls.csv",header=T,sep=",")
tail(poll)

### make the plot data to long table first
pollplot <- poll %>% select(Order, Year, Month, National, Labour, Green, NZFirst) %>% 
  mutate(GL=Labour + Green, Date=paste0(Year,Month,"15")) %>% select(Order,Date,National,GL,NZFirst) %>% gather(key=Order,Date) 

colnames(pollplot) <- c("Order","Date","Party","Poll")

pollplot$Date <- as.Date(pollplot$Date,"%Y%B%d")
head(pollplot)

a <- ggplot(data=pollplot,aes(x=Date,y=Poll,colour=Party)) + 
  stat_smooth(formula = y ~ poly(x,3), method="glm", size=1,aes(weight=Date)) + 
  scale_y_continuous(name="Percent of the Vote",breaks=seq(0,60,5)) +
  scale_color_manual(values=c("brown","blue", "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
  geom_point(position = position_jitter(width=0.1, height=0.1)) +
  labs( x = "Polling Date",title ="Going to The Polls",
      subtitle = "Guessing the Election",
      caption = "Who does Winston 1st Choose")
a + theme_xkcd()

mm <- glm(Poll~Date,data=pollplot,family=binomial)



