#####################################
############Read in data#############
##################################### 
setwd("~/Dropbox/Sheila/OCP_MostRecent/")
qhu.complete.all <- read.csv(file="./Data/QHU_FinalData.csv", head=TRUE,sep=",",
                na.strings=c("M", "NA"))
qhu.complete <- qhu.complete.all[qhu.complete.all[,50]!="NA",]
##Sorting to get correct ordering
qhu.complete$dateVar <- as.Date(qhu.complete$qhudate)
qhu <- qhu.complete[with(qhu.complete, order(respid, dateVar)),]

#Recode headache, HC variable
qhu$QHU_Headache <- 2-qhu$QHU_Headache
qhu$HCdummy <- 2-qhu$QHU_Hormones

#Eliminate those with only 1 qhu
multqhu <- which((table(qhu$respid)) == 1)
qhu.mult <- qhu[ ! qhu$respid %in% multqhu, ]

#Make variable to hold time+1 HC use
qhu.mult$HCuseTplus1 <- NA
qhu.mult$HeadacheTplus1 <- NA
qhu.mult$deleteobs <- NA

#Fill in vars
for (ijk in 1:length(table(qhu.mult$respid))) {
  print(ijk)
  id <- unique(qhu.mult$respid)[ijk]
  obs <- which(qhu.mult$respid==id)
  qhu.mult$deleteobs[obs[length(obs)]] <- TRUE
  numqhu <- dim(qhu.mult[qhu.mult$respid==id,])[1]
  for (ijkk in 1:(numqhu-1)) {
      qhu.mult$HCuseTplus1[obs[ijkk]] <- qhu.mult$HCdummy[obs[ijkk+1]]
      qhu.mult$HeadacheTplus1[obs[ijkk]] <- qhu.mult$QHU_Headache[obs[ijkk+1]]
  }}


#Delete extra obs at end of each respid
qhu.final <- qhu.mult[is.na(qhu.mult$deleteobs),]
qhu.final2 <- qhu.final[qhu.final$HCdummy==1,]
qhu.final3 <- qhu.final2[!is.na(qhu.final2$HCuseTplus1)&
                         !is.na(qhu.final2$QHU_Headache),]


#Run actual analysis
library(geepack)
model <- geeglm(HCuseTplus1~QHU_Headache+factor(siteid)+age+factor(race), data=qhu.final3, family=binomial, id=respid)

#Delete extra obs at end of each respid
qhu.finalb <- qhu.mult[is.na(qhu.mult$deleteobs),]
qhu.finalb3 <- qhu.finalb[!is.na(qhu.finalb$HeadacheTplus1)&
                          !is.na(qhu.finalb$QHU_Headache)&
                          !is.na(qhu.finalb$HCdummy),]

model2 <- geeglm(HeadacheTplus1~HCdummy+QHU_Headache+factor(siteid)+age+factor(race), data=qhu.finalb3, family=binomial, id=respid, corstr="ar1")
model3 <- geeglm(HeadacheTplus1~HCdummy+factor(siteid)+age+factor(race), data=qhu.finalb3, family=binomial, id=respid, corstr="ar1")


Vcov <- model2$geese$vbeta
betas <- model2$geese$beta
se <- sqrt(diag(Vcov))
exp(betas)[[2]]
exp(betas - (2*se))[[2]]
exp(betas + (2*se))[[2]]
(2 * pnorm(abs(betas / se), lower.tail = FALSE))[[2]]


