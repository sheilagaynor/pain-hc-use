#Read in data
library(geepack)
setwd("~/Dropbox/Sheila/OCP_MostRecent/")
qhu.usable <- read.csv(file="./Data/QHU_FinalData.csv", head=TRUE,sep=",", na.strings="M", stringsAsFactors=FALSE)

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

#Remove those with missingness in covariates
qhucomplete.case <- qhu.usable[( is.na(qhu.usable$HCdummy)==FALSE & is.na(qhu.usable$siteid)==FALSE &
                                   is.na(qhu.usable$race)==FALSE & is.na(qhu.usable$age)==FALSE) ,]

#Run associations across variables
qhucomplete.case.v2 <- qhucomplete.case[qhucomplete.case[,50]!="NA",]

#Add wave variable
qhucomplete.case.v2$waveVar <- 0
ids <- unique(qhucomplete.case.v2$respid)
qhucomplete.case.v2$dateVar <- as.Date(qhucomplete.case.v2$qhudate)
for (k in 1:length(ids)) {
  obs <- which(qhucomplete.case.v2$respid==ids[k])
  qhucomplete.case.v2$waveVar[obs] <- rank(qhucomplete.case.v2$dateVar[obs])}

varlist <- names(qhucomplete.case.v2[c(161:163, 25:36)])
varnumbers <- c( 161:163, 25:36)
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

write.csv(summary, file="~/Dropbox/Sheila/OCP2018/Output/odds.allGEE.v2.csv")
