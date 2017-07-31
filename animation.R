install.packages("animation")
library(animation)

n = nrow(poll)
xN = poll$National
xGL = poll$Labour+poll$Green
xNZF = poll$NZFirst
y = poll$Order
## set up an empty frame, then add points one by one

par(bg = "white") # ensure the background color is white
plot(y, x, type = "n",ylim=c(0,60))
ani.record(reset = TRUE) # clear history before recording
for (i in 1:n) {
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
