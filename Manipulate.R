library(manipulate)

### use the manipulate librray to control the span in loess

manipulate({
  #define plotting function 
  pollplot %>% filter(Party %in% c('National',"GL")) %>% left_join(., pollplot %>% filter(Party == "NZFirst"), by = c('Poll',"Date")) %>% mutate(Party = paste0(Party.x,"_" ,Party.y), Value = Value.x + Value.y) %>%  
    
    ggplot(data=. ,aes(x=Date,y=Value,colour=Party)) + 
    scale_y_continuous(name="Percent of the Vote",breaks=seq(0,65,5)) +
    geom_hline(aes(yintercept=50), colour="black", linetype="dashed",size=1.5)+
    #stat_smooth(formula = y ~ poly(x,3), method="glm", size=1,aes(weight=Date),level = 0.99) + 
    geom_smooth(method="loess",span = span.val) + 
    geom_point(position = position_jitter(width=5, height=0.0),aes(shape=Poll,size=1.5),alpha=0.7) +
    scale_shape_manual(values=c(18,11,15, 16, 1,17)) +
    scale_color_manual(values=c("brown","blue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
    labs( x = "Polling Date",title ="Going to The Polls", subtitle = "With Winston") + 
    theme(plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15), 
          axis.title = element_text(size = 15), plot.title = element_text(size = 20))
},
#define variable that will be changed in plot
#span.val=slider(0.2,1)
span.val=picker(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
)

manipulate({
  #define plotting function 
pollplot %>% filter(Party %in% c("National","Green","Labour","NZFirst")) %>% 
  ggplot(.,aes(x=Date,y=Value,colour=Party)) + 
  geom_point(position = position_jitter(width=5, height=0.0),aes(shape=Poll,size=1.5),alpha=0.7) +
  scale_shape_manual(values=c(18,11,15, 16, 1,17)) +
  #stat_smooth(formula = y ~ poly(x,3), method="glm", level = 0.99,size=1,aes(weight=Date)) + 
  geom_smooth(method="loess",span = span.val, size=1,level = 0.99) + 
  scale_y_continuous(name="Percent of the Vote",breaks=seq(0,60,5)) +
  scale_color_manual(values=c("green","red","blue", "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6)) +
  labs( x = "Polling Date",title ="Going to The Polls", subtitle = "Guessing the Election",
        caption = "Who does Winston 1st Choose") + 
  theme(plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15), 
        axis.title = element_text(size = 15), plot.title = element_text(size = 20))
},
#define variable that will be changed in plot
#span.val=slider(0.2,1)
span.val=picker(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
)
