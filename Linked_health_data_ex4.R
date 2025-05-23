# Q1. ####
# Set working directory to the path where your R Data files are, e.g.
setwd("~/Desktop/IALHD")
load("vaslinks4.2.RData")
View("vaslinks4.2.RData")

# Q2. ####
vaslinks4.2$followup <- as.Date('1996-12-31')

tmp <- which(vaslinks4.2$dead == 1 & vaslinks4.2$dthdate < vaslinks4.2$followup)

vaslinks4.2$followup[tmp] <- vaslinks4.2$dthdate[tmp]

rm(tmp)

# reverse
tmp <- vaslinks4.2[vaslinks4.2$treat == 2,c("rootlpno","fileseq","admdate","treat")]
names(tmp)[3] <- "vasodate"
# nrow(tmp) # 188 cases

# get first fileseq per rootlpno
library(plyr)
# for each rootlpno find the min values of `fileseq` and `vasodate`
tmp2 <- ddply(tmp, .(rootlpno), summarise, fileseq=min(fileseq), vasodate=min(vasodate))
# nrow(tmp2) # 172 cases

# append vasodate to all records of the same rootlpno in vaslinks4.2
vaslinks4.2 <- merge(vaslinks4.2, tmp2[,c("rootlpno","vasodate")], by="rootlpno",all.x=TRUE)

# sort file
vaslinks4.2 <- vaslinks4.2[order(vaslinks4.2$fileseq),]

# set all records to reverse =0
vaslinks4.2$reverse <- 0

# now overwrite those that should be 1, please notice that we need to pass by the missing dates
# i.e. where admdate <= vasodate and vadodate is not null AND vasodate is <= followup and vasodate
# is not null
vaslinks4.2$reverse[vaslinks4.2$admdate <= na.pass(vaslinks4.2$vasodate) & 
                      na.pass(vaslinks4.2$vasodate) < vaslinks4.2$followup] <- 1

# timerev
vaslinks4.2$timerev <- na.pass(vaslinks4.2$vasodate) - vaslinks4.2$admdate

cond1 <- which(vaslinks4.2$revseq > 1)
vaslinks4.2$timerev[cond1] <- vaslinks4.2$followup[cond1] - vaslinks4.2$admdate[cond1] + 1

cond2 <- which(vaslinks4.2$revseq == 1)
vaslinks4.2$timerev[cond2] <- vaslinks4.2$vasodate[cond2] - vaslinks4.2$admdate[cond2]

cond3 <- which(is.na(vaslinks4.2$vasodate))
vaslinks4.2$timerev[cond3] <- vaslinks4.2$followup[cond3] - vaslinks4.2$admdate[cond3]

# time variable need to be numeric for survival analysis
vaslinks4.2$timerev <- as.integer(vaslinks4.2$timerev)

# output to match screenshot in workbook
View(vaslinks4.2[1523:1536,c(2,29:32,37,39:40)])

# Q3. ####
library(survival)
ex4.3 <- vaslinks4.2[vaslinks4.2$vasseq == 1,] 
surv4.3 <- survfit(Surv(ceiling(timerev/365), reverse)~1, conf.type="none", data=ex4.3) 
plot(surv4.3, ylim=c(0.975,1), main="Survival Time to Vasovasectomy", xlab="Days", 
     xscale=1/365, ylab="Cumulative Survival", mark.time=F)

# Q4. ####
# create variable age30
vaslinks4.2$age30 <- 0
vaslinks4.2$age30[vaslinks4.2$age < 30] <- 1

# Q5. ####
# survival plot
ex4.4 <- vaslinks4.2[vaslinks4.2$vasseq == 1,] 
surv4.4 <- survfit(Surv(ceiling(timerev/365), reverse)~ age30, conf.type="none", data=ex4.4) 
plot(surv4.4, ylim=c(0.96,1), main="Survival Time to Vasovasectomy", xlab="Days", 
     xscale=1/365, ylab="Cumulative Survival", mark.time=F, col=c('blue','red')) 
legend("topright",c("Age 30+ yr", "Age < 30 yr"),lty=c(1,1),col=c('blue','red')) 

# cox regression
cox4.4 <- coxph(Surv(timerev, reverse)~ age30, data=ex4.4) 
summary(cox4.4)

# Q6. ####
# patern
vaslinks4.2$patern <- 0
vaslinks4.2$patern[!is.na(vaslinks4.2$bthdate) & vaslinks4.2$bthdate <= vaslinks4.2$followup] <- 1

# timepat
vaslinks4.2$timepat <- NA
cond1 <- which(vaslinks4.2$patern == 1)
vaslinks4.2$timepat[cond1] <- vaslinks4.2$bthdate[cond1] - vaslinks4.2$admdate[cond1]
cond2 <- which(vaslinks4.2$patern != 1)
vaslinks4.2$timepat[cond2] <- vaslinks4.2$followup[cond2] - vaslinks4.2$admdate[cond2]

# time variable need to be numeric for survival analysis
vaslinks4.2$timepat <- as.integer(vaslinks4.2$timepat)

# output to match screenshot in workbook
View(vaslinks4.2[3363:3370,c(2,29:32,37,39:43)])

# Q7. ####
ex4.7 <- vaslinks4.2[vaslinks4.2$revseq == 1,] 
surv4.7 <- survfit(Surv(ceiling(timepat/365), patern)~ 1, conf.type="none", data=ex4.7) 
plot(surv4.7, ylim=c(0.4,1), main="Survival Time to Paternity on Birth Certificate", xlab="Days", 
     xscale=1/365, ylab="Cumulative Survival", mark.time=F)

# Q8. ####
vaslinks4.2$year1990 <- 0
vaslinks4.2$year1990[vaslinks4.2$yearsep >= 1990] <- 1

# set elapse6 to 0 to start with
vaslinks4.2$elapse6 <- 0

tmp <- vaslinks4.2[vaslinks4.2$vasseq == 1,c("rootlpno","fileseq","admdate","treat")]
names(tmp)[3] <- "vasdate"
# nrow(tmp) # 2806 cases

# append vasodate to all records of the same rootlpno in vaslinks4.2
vaslinks4.2 <- merge(vaslinks4.2, tmp[,c("rootlpno","vasdate")], by="rootlpno",all.x=TRUE)

# sort file
vaslinks4.2 <- vaslinks4.2[order(vaslinks4.2$fileseq),]

vaslinks4.2$diff <- na.pass(vaslinks4.2$vasodate - vaslinks4.2$vasdate)

vaslinks4.2$elapse6[vaslinks4.2$diff < 2190] <- 1
vaslinks4.2$elapse6[vaslinks4.2$diff >= 2190] <- 0

# check number of records
length(which(vaslinks4.2$revseq==1 & vaslinks4.2$vasseq >= 1))

# Q9. ####
ex4.9 <- vaslinks4.2[vaslinks4.2$revseq == 1 & vaslinks4.2$vasseq >= 1,] 
cox4.9 <- coxph(Surv(timepat, patern)~ age30 + year1990 + elapse6, data=ex4.9) 
summary(cox4.9)
