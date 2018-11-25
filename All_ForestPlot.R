##Make 'forest plot' for the body regions, plotting the odds ratio CI
##and displaying the tabular info

odds.summary <- summary

#Get in correct format
odds.summary[,7] <- formatC(round(as.numeric(odds.summary[,7]), 6), digits =6,format = "f")
odds.summary <- odds.summary[order(odds.summary[,7]),]
odds.summary[1,7] <- "<0.001";odds.summary[2,7] <- "<0.001";odds.summary[3,7] <- "<0.001"
odds.summary[4:15,7] <- formatC(round(as.numeric(odds.summary[4:15,7]), 3), digits =3,format = "f")
odds.summary <- odds.summary[c(1,3,5,2,4,6:15),]
odds.summary[,1] <- c("Headache", "Orofacial pain", "Body", "Head", "Face", "Shoulders", "Legs",
                      "Arms", "Neck", "Back", "Hands", "Hips", "Feet", "Abdomen", "Chest")
 

#Find largest, smallest bounds of all CI
minLB <- as.numeric(min(odds.summary[,5]))
maxUB <- as.numeric(max(odds.summary[,6]))

#Determine bounds of plot
xlim <- c(minLB - 1.2 * (maxUB - minLB) , maxUB + 1.2 * (maxUB - minLB))
xlim <- c(-4,5); ylim <- c(-4, 24)

#Open blank plot
setwd("~/Dropbox/Sheila/OCP2018/Output/")
png("AllForestPlot.png", width=1800, height=1100)
plot(NA, NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "",
     yaxt = "n", xaxt = "n", xaxs = "i", bty = "n")

rect(-3.74,18.6,-2.64, 21,col = 'gray84', border=NA)
rect(-3.74,1.12,-2.64, 18.4,col = 'gray84', border=NA)

#Reference line for Odds (=1)
segments(1, 3.5, 1, 21.2, lty = "dotted", lwd=2.5)
segments(-3.74, 21.2, 4.4, 21.2, lwd=2.5)
segments(-3.74, 0.92, 4.4, 0.92, lwd=2.5)

#Text box of actual Odds and CI
odds.summary[,4:6] <- formatC(round(as.numeric(odds.summary[,4:6]), 2), digits =2,format = "f")
annotext <- cbind(odds.summary[, 4], " [ ", odds.summary[, 5], " , ", odds.summary[, 6], " ]")
annotext <- apply(annotext, 1, paste, collapse = "")
text(x = 2.35, c(20:19, 17:5), labels = annotext, adj=c(0,0), cex=2)
text(x = 2.28, 22, labels = "Odds Ratio (95% CI)", adj=c(0,0), cex=2, font=2)

#Add body regions/events/total info
text(x=-3.70, c(20), labels=c("Craniofacial pain"),cex=2, adj=c(0,0), font=2)
text(x=-3.70, c(17), labels=c("Pain for \u2265 1 day"),cex=2, adj=c(0,0), font=2)
text(x=-2.575, c(20:19, 17:5), labels=odds.summary[,1],cex=2, adj=c(0,0))
text(x=-1.225, c(20:19, 17:5), labels=odds.summary[,2],cex=2, adj=c(0,0))
text(x=-.6, c(20:19, 17:5), labels=odds.summary[,3],cex=2, adj=c(0,0))

#Add body regions/events/total labels
text(x=-2.575, 22, labels="Condition", cex=2, adj = c(0,0), font=2)
text(x=-1.225, 22, labels="Events", cex=2, adj = c(0,0), font=2)
text(x=-.6, 22, labels="Total", cex=2, adj = c(0,0), font=2)

#Add P-values and label for p-values
text(x=3.8, c(20:19, 17:5), labels=odds.summary[,7],cex=2, adj=c(0,0))
text(x=3.8, 22, labels="P-Value", cex=2, adj = c(0,0), font=2)

#Plot point estimate of Odds
points(odds.summary[,4], c(20:19, 17:5)+0.2, pch=19, cex=2)

#Plot CI of odds
rows <- c(20:19, 17:5)
for (i in seq.int(15)) { arrows(as.numeric(odds.summary[i,5]), rows[i]+0.2, as.numeric(odds.summary[i,6]), rows[i]+0.2, angle=90, code=3, length=0.15,lwd=2)}

#segments(0, 0.5, 2, 0.5)
for (i in 0:1) {  arrows(i, 3.9, i+1, 3.9, angle=90, code=3, length=0.15,lwd=2)}
text(x=-.025, 2.5, labels="0", cex=1.75, adj = c(0,0), font=2)
text(x=.975, 2.5, labels="1", cex=1.75, adj = c(0,0), font=2)
text(x=1.975, 2.5, labels="2", cex=1.75, adj = c(0,0), font=2)
text(x = 0.45, 1.51, labels = "Odds Ratio (95% CI)", adj=c(0,0), cex=2, font=2)
 
graphics.off()
