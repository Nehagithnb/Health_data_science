# Q1. ####
# Set working directory to the path where your R Data files are, e.g.
setwd("~/Desktop/IALHD")
load("vashmds.RData")

View(vashmds)

# Q2. ####
load("vasdeath.RData")
View(vasdeath)
load("vasbirth.RData")
View(vasbirth)
load("vascancer2.RData")

# Q3. ####
# R will join automatically on rootlpno, 
# as this is the only variable both tables have in common
# R will not throw an error message despite the duplicate
# by="rootlpno" is the common field to join on
# it's not stricly necessary to use the by, as R will automatically find
# matching rows, but it is good practise
# all.x=TRUE will make sure it is a left outer join
vaslinks <- merge(vashmds,vasdeath,by="rootlpno",all.x=TRUE)

# Q4. ####
# are there any dupicated rootlpno's?
duplicated(vasdeath$rootlpno)
# there is one duplicate TRUE
# which `rootlpno` is duplicated?
vasdeath$rootlpno[duplicated(vasdeath$rootlpno)]
# let's have a look at this `rootlpno`
vasdeath[vasdeath$rootlpno == 14171943,]
# the fully automated version would be
# vasdeath[vasdeath$rootlpno == vasdeath$rootlpno[duplicated(vasdeath$rootlpno)],]

# check against admission
vaslinks[vaslinks$rootlpno == 14171943,]
vaslinks[vaslinks$dthdate < vaslinks$admdate & vaslinks$rootlpno == 14171943,]
# this person had a hospital appointment in 1984 and could thus not have died in 1982

# delete wrong record in death data
# identify row with the probably wrong death date
vasdeath[vasdeath$dthdate == '1982-11-09',]
# save all rows apart from the identified row number 33 to `vasdeath2`
vasdeath2 <- vasdeath[rownames(vasdeath) != 33,]

# save to file
save(vasdeath2, file="vasdeath2.RData")

# link data again
vaslinks <- merge(vashmds,vasdeath2,by="rootlpno",all.x=TRUE)

# save interim table (optional)
save(vaslinks, file="vaslinks.RData")

# Q5. ####
# check for duplicates first
vascancer2$rootlpno[duplicated(vascancer2$rootlpno)]
# no matches, therefore no duplicates
vaslinks2 <- merge(vaslinks,vascancer2,by="rootlpno",all.x=TRUE)

# save interim table (optional)
save(vaslinks2, file="vaslinks2.RData")

# Q6. ####
# check for duplicates first
vasbirth$rootlpno[duplicated(vasbirth$rootlpno)]
# no matches, therefore no duplicates
vaslinks3 <- merge(vaslinks2,vasbirth,by="rootlpno",all.x=TRUE)

# Q7. ####
# Sorting data by rootlpno and sepdate and save as`vaslinks3`
# The given method used the discharge date from hospital, but
# one might equally argue that the admission date is more important
# (this will lead to slightly different results though)
vaslinks3 <- vaslinks3[order(vaslinks3$rootlpno, vaslinks3$sepdate),]
save(vaslinks3, file="vaslinks3.RData")

# show rows 1172-1190, which correspond to the screenshot in the workbooks
View(vaslinks3[1172:1190,])

# Q8. ####
library(chron)
vaslinks3$yearsep <- years(vaslinks3$sepdate)

# set treat = 0 to start with
vaslinks3$treat <- 0

# for each row in `vaslinks3` check if `yearsep` is before or in/after 1988,
# then check proc1-proc3 (columns 14:16) for codes and accordlingly assign
# treatment code 1 or 2
for (i in 1:nrow(vaslinks3)){
  
  if (vaslinks3$yearsep[i] < 1988){
    if (any(vaslinks3[i,14:16] %in% c(56.36,59.81))) {
      vaslinks3$treat[i] <- 1}
    if (any(vaslinks3[i,14:16] %in% c(56.34,56.37))) {
      vaslinks3$treat[i] <- 2}
  }
  
  if (vaslinks3$yearsep[i] >= 1988) {
    if (any(vaslinks3[i,14:16] %in% seq(63.70,63.79,0.01))){
      vaslinks3$treat[i] <- 1}
    if (any(vaslinks3[i,14:16] %in% seq(63.80,63.89,0.01))) {
      vaslinks3$treat[i] <- 2}  
  }
}

# this loop takes several seconds on my computer to finish
table(vaslinks3$treat)

# Q9. ####
# create `fileseq` by counting from 1 to the total number of rows
vaslinks3$fileseq <- seq(1,nrow(vaslinks3))


# Q10. ####
# create `morbseq` variable
# for each record in `rootlpno` count the number of rows (in the given order)
library(plyr)
vaslinks3 <- ddply(vaslinks3,"rootlpno", mutate, morbseq=1:length(rootlpno))

# Q11. ####

vaslinks3$vasseq <- 0

# subset to only treat = 1
tmp <- vaslinks3[vaslinks3$treat == 1,]
# identify rows of first occurence (not the first duplication)
tmp[!duplicated(tmp$rootlpno, fromLast=FALSE),]
# we now want to keep the row numbers for subsetting (row.names is a character vector,
# therefore we need to make a number out of it)
first <- as.numeric(row.names(tmp[!duplicated(tmp$rootlpno, fromLast=FALSE),]))

# set matching rows to 1
vaslinks3$vasseq[first] <- 1
# rm(tmp, first)

# start in second row as we cound one backwards, check previous row, if `vasseq` is > 0
# then add a 1 to it
for (i in 2:nrow(vaslinks3)){
  if (vaslinks3$rootlpno[i]==vaslinks3$rootlpno[i-1] & vaslinks3$vasseq[i-1] != 0){
    vaslinks3$vasseq[i] <- vaslinks3$vasseq[i-1]+1
  }
}

# Q12. ####
vaslinks3$revseq <- 0

# subset to only treat = 2
tmp <- vaslinks3[vaslinks3$treat == 2,]
# identify rows of first occurence (if duplicated bring back the first record, see above)
tmp[!duplicated(tmp$rootlpno, fromLast=FALSE),]
# get a list of matching row numbers
first <- as.numeric(row.names(tmp[!duplicated(tmp$rootlpno, fromLast=FALSE),]))

vaslinks3$revseq[first] <- 1
# rm(tmp, first)

# again, we are checking the previsou line, so start wirh row 2
for (i in 2:nrow(vaslinks3)){
  if (vaslinks3$rootlpno[i]==vaslinks3$rootlpno[i-1] & vaslinks3$revseq[i-1] != 0){
    vaslinks3$revseq[i] <- vaslinks3$revseq[i-1]+1
  }
}

# show the same number of rows displayed in the workbook
View(vaslinks3[1:20,])

# Q13. ####
vaslinks3$dead <- 0
# where deathdate is not null assign 1 to `dead`
vaslinks3$dead[!is.na(vaslinks3$dthdate)] <- 1

# Q14. ####
vaslinks4 <- vaslinks3
save(vaslinks4, file="vaslinks4.RData")

# results table ####

# numbers
length(vaslinks4$rootlpno[vaslinks4$vasseq == 1])
length(vaslinks4$rootlpno[vaslinks4$revseq == 1])

# mean age
describe(vaslinks4$age[vaslinks4$vasseq == 1])
describe(vaslinks4$age[vaslinks4$revseq == 1])

# number dead
length(vaslinks4$rootlpno[vaslinks4$vasseq == 1 & vaslinks4$dead == 1])
length(vaslinks4$rootlpno[vaslinks4$revseq == 1 & vaslinks4$dead == 1])

# first cancer
length(vaslinks4$rootlpno[vaslinks4$vasseq == 1 & !is.na(vaslinks4$cansite1)])
length(vaslinks4$rootlpno[vaslinks4$revseq == 1 & !is.na(vaslinks4$cansite1)])

# second cancer
length(vaslinks4$rootlpno[vaslinks4$vasseq == 1 & !is.na(vaslinks4$cansite2)])
length(vaslinks4$rootlpno[vaslinks4$revseq == 1 & !is.na(vaslinks4$cansite2)])

# number of vasovasostomies
vaslinks4$vasotab[vaslinks4$vasseq == 1] <- 0
vaslinks4$vasotab[vaslinks4$revseq >= 1 & !is.na(vaslinks4$vasotab)] <- 1
vaslinks4$vasotab[vaslinks4$vasseq > 1 & vaslinks4$revseq == 1] <- 2
table(vaslinks4$vasotab)

# number of paternities
vaslinks4$patern <- 0
vaslinks4$patern[!is.na(vaslinks4$bthdate)] <- 1
table(vaslinks4$patern[vaslinks4$revseq == 1])
