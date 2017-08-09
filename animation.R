# basic animation script

install.packages("animation")
library(animation)

xN = poll$National
xGL = poll$Labour+poll$Green
xNZF = poll$NZFirst
y = poll$Order
## set up an empty frame, then add points one by one

par(bg = "white") # ensure the background color is white
plot(y, x, type = "n",ylim=c(0,60))
ani.record(reset = TRUE) # clear history before recording
for (i in 1:nrow(poll)) {
  points(y[i], xN[i], pch = 19, cex = 2,col="blue")
  points(y[i], xGL[i], pch = 19, cex = 2,col="brown")
  points(y[i], xNZF[i], pch = 19, cex = 2,col="black")
  ani.record() # record the current frame
}
## now we can replay it, with an appropriate pause between frames
oopts = ani.options(interval = 0.5)
ani.replay()
## or export the animation to an HTML page
saveHTML(ani.replay(), img.name = "record_plot")

### tidy animation from murray
for (date in unique(pollplot$Date) ) {
  p<- pollplot %>% filter(Party %in% c("National","Green","Labour","NZFirst")) %>% filter(Date == date) %>% group_by(Party) %>% summarise(meanValue = mean(Value, na.rm=TRUE)) %>% ggplot(.,aes(x = Party, y=meanValue,fill=Party)) + geom_bar( stat = "identity") + scale_fill_manual(values=c("green","red","blue", "black")) + ylim(c(0,55)) + ggtitle(as_date(date))
  plot(p)
  ani.record() # record the current frame
}

oopts = ani.options(interval = 1.0)
saveGIF(expr = ani.replay(), movie.name = "record_plot.gif" )
