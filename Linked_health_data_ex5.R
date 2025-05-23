# Q1. ####
# Set working directory to the path where your R Data files are, e.g.
setwd("~/Desktop/IALHD")
load("bphsurgery.RData")
View(bphsurgery)
# ~/Desktop/PMIM302-IALHD/bphsurgery.RData

dim(bphsurgeryrgery)

head(bphsurgery)

tail(bphsurgery)

summary(bphsurgery)
# Q2. ####
library(chron)
bphsurgery$yearsep <- years(bphsurgery$sepdate)

# create empty variable
bphsurgery$bphsurg <- NA

# depending on the year of discharge assign `bphsurg`` 1 or 0
for (i in 1:nrow(bphsurgery)){
  
  if (bphsurgery$yearsep[i] < 1988){
    if (any(bphsurgery[i,12:14] == 56.01, na.rm=TRUE)) {
      bphsurgery$bphsurg[i] <- 1}
    if (any(bphsurgery[i,12:14] %in% seq(56.02,56.05,0.01))) {
      bphsurgery$bphsurg[i] <- 0}
  }
  if (bphsurgery$yearsep[i] >= 1988){
    if (any(bphsurgery[i,12:14] %in% seq(60.20,60.29,0.01))) {
      bphsurgery$bphsurg[i] <- 1}
    if (any(bphsurgery[i,12:14] %in% seq(60.30,60.69,0.01))) {
      bphsurgery$bphsurg[i] <- 0}
  }
}


# Q3. ####
# create `fileseq` by counting through the records from 1 to the last one
# you could use row.names instead, but then you have to transform the character
# vector into a number as well
bphsurgery$fileseq <- seq(1,nrow(bphsurgery))

library(plyr)
# for each `rootlpno` count the number of rows
bphsurgery <- ddply(bphsurgery,"rootlpno", mutate, morbseq=1:length(rootlpno))

tmp <- bphsurgery[!is.na(bphsurgery$bphsurg),c("rootlpno","yearsep")]
# identify rows of first occurence
first <- as.numeric(row.names(tmp[!duplicated(tmp$rootlpno, fromLast=FALSE),]))

bphsurgery$indexseq[first] <- 1
# rm(tmp, first)

# check previous row then assign `indexseq`
for (i in 2:nrow(bphsurgery)){
  if (bphsurgery$rootlpno[i]==bphsurgery$rootlpno[i-1] & !is.na(bphsurgery$indexseq[i-1])){
    bphsurgery$indexseq[i] <- bphsurgery$indexseq[i-1]+1
  }
}

# View(bphsurgery[2041:2047,])

# Q4. ####
# create dead
bphsurgery$dead <- 0
bphsurgery$dead[na.pass(bphsurgery$dthdate <= as.Date("1996-12-31"))] <- 1

# create followup
bphsurgery$followup <- as.Date("1996-12-31")
bphsurgery$followup[bphsurgery$dead == 1 & 
                      bphsurgery$dthdate <= as.Date("1996-12-31")] <- 
  bphsurgery$dthdate[bphsurgery$dead == 1 & bphsurgery$dthdate <= as.Date("1996-12-31")]

# create survival
bphsurgery$survival <- as.integer(bphsurgery$followup - bphsurgery$admdate)

# just show `fileseq` 2041 to 2047 to match screenshot in workbook
View(bphsurgery[bphsurgery$fileseq %in% c(2041:2047),])

# Q5. ####
library(survival)
ex5.5 <- bphsurgery[bphsurgery$indexseq == 1,]
surv5.5 <- survfit(Surv(ceiling(survival/365), dead)~ bphsurg, conf.type="none", data=ex5.5)
plot(surv5.5, ylim=c(0.5,1), main="Survival Time", xlab="Days", ylab="Cumulative Survival", 
     xscale=1/365, mark.time=F, col=c('blue','red'))
legend("topright",c("OP", "TURP"),lty=c(1,1),col=c('blue','red'))

cox5.5 <- coxph(Surv(survival, dead)~ bphsurg, data=ex5.5)
summary(cox5.5)

# Q6. ####
mean(bphsurgery$age[bphsurgery$bphsurg == 0], na.rm=TRUE)
mean(bphsurgery$age[bphsurgery$bphsurg == 1], na.rm=TRUE)

# remove the factor coding
bphsurgery$yearsep <- as.integer(as.character(bphsurgery$yearsep))
mean(bphsurgery$yearsep[bphsurgery$bphsurg == 0], na.rm=TRUE) 
mean(bphsurgery$yearsep[bphsurgery$bphsurg == 1], na.rm=TRUE)

# Q7. ####
bphsurgery$age2 <- bphsurgery$age^2
bphsurgery$age2ln <- bphsurgery$age^2*log(bphsurgery$age)

ex5.7 <- bphsurgery[bphsurgery$indexseq == 1,]
cox5.7 <- coxph(Surv(survival, dead)~ bphsurg + yearsep + age2 + age2ln, data=ex5.7)
summary(cox5.7)

# Q8. ####
bphsurgery$chmsqr <- 1/((0.01 + bphsurgery$charlson)^0.5)
bphsurgery$chln <- log(bphsurgery$charlson + 0.01)

bphsurgery$anycom <- 0
bphsurgery$anycom[bphsurgery$charlson > 0] <- 1

ex5.8 <-  bphsurgery[bphsurgery$indexseq == 1,]

cox5.8 <- coxph(Surv(survival, dead)~ bphsurg + yearsep + age2 + age2ln + chmsqr + 
                  chln + anycom, data=ex5.8)
summary(cox5.8)
