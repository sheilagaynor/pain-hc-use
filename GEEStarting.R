#Read in data
library(geepack)
setwd("~/Dropbox/Sheila/OCP_MostRecent/")
qhu.complete <- read.csv(file="./Data/QHU_FinalData.csv", head=TRUE,sep=",", na.strings="M", stringsAsFactors=FALSE)

##First getting cases that do use HC during study
obs <- which(qhu.complete$QHU_Hormones==1)
ids <- unique(qhu.complete$respid[obs])
qhu.complete <- qhu.complete[qhu.complete$respid %in% ids, ]
qhu.complete$dateVar <- as.Date(qhu.complete$qhudate)
qhu.complete <- qhu.complete[with(qhu.complete, order(respid, dateVar)),]

##Next getting cases that do START HC during study
qhu.complete$beginHCuse <- FALSE
for (i in 1:length(unique(qhu.complete$respid))) {
  id <- unique(qhu.complete$respid)[i]
  obs <- which(qhu.complete$respid==id)
  numqhu <- length(obs)
  if (numqhu>1) {
    for (j in 2:numqhu) { qhu.complete$beginHCuse[obs[j]] <- qhu.complete$QHU_Hormones[obs[j]]==1 & qhu.complete$QHU_Hormones[obs[j-1]]==2}}}

#Subset the data
starters <- qhu.complete[qhu.complete$beginHCuse==TRUE,]
ID_starting <- unique(starters$respid)
qhu.subset <- qhu.complete[qhu.complete$respid %in% ID_starting, ]

##Removing any time off the beginning if already using it
qhu.subset$nouse <- FALSE
for (ijk in 1:length(unique(qhu.subset$respid))) {
  id <- unique(qhu.subset$respid)[ijk]
  obs <- which(qhu.subset$respid==id)
  numqhu <- length(obs)
  if (numqhu>1) {
    for (ijkk in 1:numqhu) {
      if (qhu.subset$QHU_Hormones[obs[ijkk]]==2 & qhu.subset$nouse[obs[ijkk]]==FALSE)
      {qhu.subset$nouse[obs[ijkk:numqhu]] <- TRUE}}}} #Take obs w/any non-use at beginning, and then all obs after (so start is non use)
qhu.subset <- qhu.subset[qhu.subset$nouse==TRUE,]

##Removing any time off the end when they stop using it
qhu.subset$stop <- FALSE
for (ij in 1:length(unique(qhu.subset$respid))) {
  id <- unique(qhu.subset$respid)[ij]
  obs <- which(qhu.subset$respid==id)
  numqhu <- length(obs)
  if (numqhu<2) {qhu.subset$stop[obs] <- TRUE} #delete if only one obs per respid
  if (numqhu>1) {
    for (jj in 2:numqhu) {
      if (qhu.subset$QHU_Hormones[obs[jj]]==2 & qhu.subset$QHU_Hormones[obs[jj-1]]==1)  #meaning they started using again
      {qhu.subset$stop[obs[jj:numqhu]] <- TRUE}}}} #True if stopping using so take those obs off
qhu.usable <- qhu.subset[qhu.subset$stop==FALSE,]

##Fix coding of dummy variables & skip sequence
#If body aches missing, make the sequence all missing
for (n in 1:nrow(qhu.usable)) {
  if (is.na(as.numeric(qhu.usable[n,24])) == TRUE) {qhu.usable[n, 25:36] <- NA} #replace w/NA if gateway is NA
  if (is.na(as.numeric(qhu.usable[n,24])) == FALSE) { if (as.numeric(qhu.usable[n,24])==2) { qhu.usable[n,25:36] <- 0} } } #replace w/0 (No) if gateway is No

#Make numeric from character (not factor!)
for (p in 25:36) { qhu.usable[,p] <- as.numeric(qhu.usable[,p]) }

#Make all numeric 0,1 instead of character 1,2
qhu.usable$HCdummy <- ifelse(as.numeric(qhu.usable$QHU_Hormones) == 2, 0, as.numeric(qhu.usable$QHU_Hormones) )
qhu.usable$headache <- ifelse(as.numeric(qhu.usable$QHU_Headache) == 2, 0, as.numeric(qhu.usable$QHU_Headache) )
qhu.usable$cheek <- ifelse(as.numeric(qhu.usable$QHU_CheekJawPain) == 2, 0, as.numeric(qhu.usable$QHU_CheekJawPain) )
qhu.usable$body <- ifelse(as.numeric(qhu.usable$QHU_BodyAches) == 2, 0, as.numeric(qhu.usable$QHU_BodyAches) )

library(geepack)

#Remove those with missingness
qhucomplete.case <- qhu.usable[( is.na(qhu.usable$HCdummy)==FALSE & is.na(qhu.usable$siteid)==FALSE &
                     is.na(qhu.usable$race)==FALSE & is.na(qhu.usable$age)==FALSE) ,]


qhucomplete.case.v2 <- qhucomplete.case[qhucomplete.case[,50]!="NA",]

#Add wave variable
qhucomplete.case.v2$waveVar <- 0
ids <- unique(qhucomplete.case.v2$respid)
qhucomplete.case.v2$dateVar <- as.Date(qhucomplete.case.v2$qhudate)
for (k in 1:length(ids)) {
  obs <- which(qhucomplete.case.v2$respid==ids[k])
  qhucomplete.case.v2$waveVar[obs] <- rank(qhucomplete.case.v2$dateVar[obs])}


varlist <- names(qhucomplete.case.v2[c(165:167, 25:36)])
varnumbers <- c( 165:167, 25:36)
summary <- matrix(-10,length(varlist),7)
colnames(summary ) <- c("Variable", "Event", "Total", "Odds", "Lower", "Upper", "Pvalue")

for (ijk in 1:nrow(summary)) {
  qhucomplete <- qhucomplete.case.v2[ is.na(qhucomplete.case.v2[,varnumbers[ijk]])==FALSE,]
  varNameIndex <- which(names(qhucomplete)==varlist[ijk])
  model <- geeglm(get(varlist[ijk])~HCdummy+factor(siteid)+factor(race)+as.numeric(age),id=respid,data=qhucomplete, waves=qhucomplete$waveVar, family=binomial(), corstr="ar1", na.action=na.omit)
  Vcov <- model$geese$vbeta
  betas <- model$geese$beta
  se <- sqrt(diag(Vcov))
  summary[ijk,1] <- varlist[ijk]
  summary[ijk,2] <- table(qhucomplete[,varNameIndex])[[2]]
  summary[ijk,3] <- table(qhucomplete[,varNameIndex])[[2]] + table(qhucomplete[,varNameIndex])[[1]]
  summary[ijk,4] <- exp(betas)[[2]]
  summary[ijk,5] <- exp(betas - (qnorm(0.975)*se))[[2]]
  summary[ijk,6] <- exp(betas + (qnorm(0.975)*se))[[2]]
  summary[ijk,7] <- (2 * pnorm(abs(betas / se), lower.tail = FALSE))[[2]]
}

write.csv(summary, file="~/Dropbox/Sheila/OCP2018/Output/odds.startGEE.v2.csv")
