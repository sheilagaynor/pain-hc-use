##Make 'forest plot' for the body regions, plotting the odds ratio CI
##and displaying the tabular info

odds.summary <- summary

#Get in correct format
#odds.summary[,7] <- formatC(round(as.numeric(odds.summary[,7]), 6), digits =6,format = "f")
#odds.summary <- odds.summary[order(odds.summary[,7]),]
odds.summary[,7] <- formatC(round(as.numeric(odds.summary[,7]), 3), digits =3,format = "f")
odds.summary <- odds.summary[c(2,1,3,11,8),]
odds.summary[,1] <- c("Orofacial pain", "Headache", "Body ", "Head", "Face")

#Find largest, smallest bounds of all CI
xlim <- c(-4.2, 5.5)
ylim <- c(-4.2, 13)

#Open blank plot
setwd("~/Dropbox/Sheila/OCP2018/Output/")
png("StoppingForestPlot.png", width=1550, height=800)
plot(NA, NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "",
     yaxt = "n", xaxt = "n", xaxs = "i", bty = "n")

rect(-4.15, 7.6,-2.75, 10,col = 'gray84', border=NA)
rect(-4.15,0.2,-2.75, 7.4,col = 'gray84', border=NA)

#Reference line for Odds (=1)
segments(1, 3.2, 1, 10.2, lty = "dotted", lwd=2.5)
segments(-4.15, 10.2, 4.97, 10.2, lwd=2.5)
segments(-4.15, 0, 4.97, 0, lwd=2.5)

#Text box of actual Odds and CI
odds.summary[,4:6] <- formatC(round(as.numeric(odds.summary[,4:6]), 2), digits =2,format = "f")
annotext <- cbind(odds.summary[, 4], " [ ", odds.summary[, 5], " , ", odds.summary[, 6], " ]")
annotext <- apply(annotext, 1, paste, collapse = "")
text(x = 2.65, c(9:8, 6:4), labels = annotext, adj=c(0,0), cex=2)
text(x = 2.52, 11, labels = "Odds Ratio (95% CI)", adj=c(0,0), cex=2, font=2)

#Add body regions/events/total info
text(x=-4.1, c(9), labels=c("Craniofacial pain"),cex=2, adj=c(0,0), font=2)
text(x=-4.1, c(6), labels=c("Pain for \u2265 1 day"),cex=2, adj=c(0,0), font=2)
text(x=-2.66, c(9:8, 6:4), labels=odds.summary[,1],cex=2, adj=c(0,0))
text(x=-1.15, c(9:8, 6:4), labels=odds.summary[,2],cex=2, adj=c(0,0))
text(x=-.5, c(9:8, 6:4), labels=odds.summary[,3],cex=2, adj=c(0,0))

#Add body regions/events/total labels
text(x=-2.66, 11, labels="Condition", cex=2, adj = c(0,0), font=2)
text(x=-1.15, 11, labels="Events", cex=2, adj = c(0,0), font=2)
text(x=-.5, 11, labels="Total", cex=2, adj = c(0,0), font=2)

#Add P-values and label for p-values
text(x=4.35, c(9:8, 6:4), labels=odds.summary[,7],cex=2, adj=c(0,0))
text(x=4.35, 11, labels="P-Value", cex=2, adj = c(0,0), font=2)


#Plot point estimate of Odds
points(odds.summary[,4], c(9:8, 6:4)+0.2, pch=19, cex=2)

#Plot CI of odds
rows <- c(9:8, 6:4)
for (i in seq.int(5)) {
  #segments(as.numeric(odds.summary[i,5]), rows[i], as.numeric(odds.summary[i,6]), rows[i], lwd=2.5)
  arrows(as.numeric(odds.summary[i,5]), rows[i]+0.2, as.numeric(odds.summary[i,6]), rows[i]+0.2, angle=90, code=3, length=0.15, lwd=2)}

#segments(0, 0.5, 2, 0.5)
for (i in 0:1) {
  arrows(i, 3.6, i+1, 3.6, angle=90, code=3, length=0.15, lwd=2)}
text(x=-.025, 1.8, labels="0", cex=1.5, adj = c(0,0), font=2)
text(x=.975, 1.8, labels="1", cex=1.5, adj = c(0,0), font=2)
text(x=1.975, 1.8, labels="2", cex=1.5, adj = c(0,0), font=2)

text(x = 0.35, 0.5, labels = "Odds Ratio (95% CI)", adj=c(0,0), cex=2, font=2)


graphics.off()
