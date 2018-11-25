#Read in data
setwd("~/Dropbox/Sheila/OCP_MostRecent/")
qhu.usable <- read.csv(file="./Data/QHU_FinalData.csv", head=TRUE,sep=",", na.strings="M", stringsAsFactors=FALSE)
qhu.usable$dateVar <- as.Date(qhu.usable$qhudate)
qhucomplete.case <- qhu.usable[with(qhu.usable, order(respid, dateVar)),]


qhucomplete.case.v2 <- qhucomplete.case[qhucomplete.case[,50]!="NA",]

#Get number of quarterly questionnaires
dim(qhucomplete.case.v2)
table(qhucomplete.case.v2$QHU_Hormones)

qhucomplete.case.v2$waveVar <- 0
ids <- unique(qhucomplete.case.v2$respid)
qhucomplete.case.v2$dateVar <- as.Date(qhucomplete.case.v2$qhudate)
for (k in 1:length(ids)) {
  obs <- which(qhucomplete.case.v2$respid==ids[k])
  qhucomplete.case.v2$waveVar[obs] <- rank(qhucomplete.case.v2$dateVar[obs])}
qhu_SingObs <- qhucomplete.case.v2[qhucomplete.case.v2$waveVar==1,]

#Get race
table(qhu_SingObs$race)
prop.table(table(qhu_SingObs$race))

#Get site
table(qhu_SingObs$siteid)
prop.table(table(qhu_SingObs$siteid))

#Get age
library(Hmisc)
qhu_SingObs$ageCat <- cut2(qhu_SingObs$age, c(20,25,30,35,40))
table(qhu_SingObs$ageCat)
prop.table(table(qhu_SingObs$ageCat))

870+465+86+159

#Get number of questionnaires completed
 min(table(qhucomplete.case.v2$respid))
 max(table(qhucomplete.case.v2$respid))
 mean(table(qhucomplete.case.v2$respid))
 
 
 
 
 
 ##Check how many use HC at some point
 #Read in data
 setwd("~/Dropbox/Sheila/OCP_MostRecent/")
 qhu.usable <- read.csv(file="./Data/QHU_FinalData.csv", head=TRUE,sep=",", na.strings="M", stringsAsFactors=FALSE)
 qhu.usable$dateVar <- as.Date(qhu.usable$qhudate)
 qhucomplete.case <- qhu.usable[with(qhu.usable, order(respid, dateVar)),]
 
 
 qhucomplete.case.v2 <- qhucomplete.case[qhucomplete.case[,50]!="NA",]
 
 HCUsers <- 0
 for (i in 1:length(unique(qhucomplete.case.v2$respid))) {
   id <- unique(qhucomplete.case.v2$respid)[i]
   HCUsers <- HCUsers+ as.numeric(sum(qhucomplete.case.v2$QHU_Hormones[qhucomplete.case.v2$respid==id]==1) > 0) }
 HCUsers
 