#####################################
############Read in data#############
#####################################
setwd("~/Dropbox/Sheila/OCP_MostRecent/Data/")
qhucomplete.case <- read.csv(file="QHU_FinalData.csv", head=TRUE,sep=",", na.strings="M")
qhu.complete <- qhucomplete.case[qhucomplete.case[,50]!="NA",]

#####################################
##Getting rid of observations after positive RDC exam
#####################################
pos <- qhu.complete[qhu.complete$rdc_outcome==2,]
qhu.complete$positiverdc <- FALSE

for (ij in 1:length(table(qhu.complete$respid))) {
  print(ij)
  id <- unique(qhu.complete$respid)[ij]
  obs <- which(qhu.complete$respid==id)
  numqhu <- dim(qhu.complete[qhu.complete$respid==id,])[1]
  if (numqhu>1) {
    for (jj in 2:numqhu) { 
      if (qhu.complete$rdc_outcome[obs[jj]]==1) {
      {qhu.complete$positiverdc[obs[jj+1:numqhu]] <- TRUE}}}}}

#Subset the data
IDtoNOTUse <- qhu.complete[qhu.complete$positiverdc==TRUE,]
qhu.usable <- qhu.complete[!qhu.complete$respid %in% IDtoNOTUse$respid, ]


qhu.usable$HCdummy =-1 * (as.numeric(qhu.usable$QHU_Hormones) - 2)
for (qq in 1:dim(qhu.usable)[1]) { 
  if (is.na(qhu.usable$HCdummy[qq])== FALSE & qhu.usable$HCdummy[qq]==-1) 
  {is.na(qhu.usable$HCdummy[qq]) <- TRUE}}


library(survival)
CoxModel<-coxph(Surv(start_time, end_time, rdc_outcome == 1)~ factor(HCdummy) + factor(siteid) + factor(race) +as.numeric(age), data=qhu.usable)
(HR <- exp(summary(CoxModel)$coeff[1,1]))
(Low <- exp(summary(CoxModel)$coeff[1,1] - 1.96*summary(CoxModel)$coeff[1,3]))
(Hi <- exp(summary(CoxModel)$coeff[1,1] + 1.96*summary(CoxModel)$coeff[1,3]))
(pvalue <- (2 * pnorm(abs( exp(summary(CoxModel)$coeff[1,1])/ exp(summary(CoxModel)$coeff[1,3])), lower.tail = FALSE)))

hr <- matrix(rep(0,4),1,4)
hr[1] <- HR
hr[2] <- Low
hr[3] <- Hi
hr[4] <- pvalue
colnames(hr) <- c("Hazard Ratio", "95% CI Low", "95% CI High", "P-Value")
write.csv(hr, file="~/Dropbox/Sheila/OCP2018/Output/hr.cox.csv")
