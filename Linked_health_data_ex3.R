# Q1. ####
# Set working directory to the path where your R Data files are, e.g.
setwd("~/Desktop/IALHD")
load("vaslinks4.RData")

# Q2. ####
tmp <- vaslinks4[vaslinks4$yearsep >= 1985,]

# create morbseq, for each `rootlpno` count the number of rows from 1 to maximum number of records for this `rootlpno`
library(plyr)
tmp <- ddply(tmp,"rootlpno", mutate, morbseq=1:length(rootlpno))

# Q3. ####
table(tmp$yearsep[tmp$morbseq==1])
#or
frequency(vaslinks4$yearsep)
# Q4+Q5. ####
table(vaslinks4$yearsep[vaslinks4$yearsep >= 1985 & vaslinks4$morbseq == 1])

# Q6. ####
# we already sorted our table previously
# vaslinks4 <- vaslinks4[order(vaslinks4$rootlpno, vaslinks4$sepdate),]

vaslinks4$transseq <- 0

# check previous row and assign either `transseq` 1 or 2 to it
for (i in 2:nrow(vaslinks4)){
  if ((vaslinks4$morbseq[i] >=2 && vaslinks4$sepdate[i-1] > vaslinks4$admdate[i]) |
      (vaslinks4$morbseq[i] >=2 && vaslinks4$septype[i-1] == 2 
       && vaslinks4$sepdate[i-1] == vaslinks4$admdate[i])){
    vaslinks4$transseq[i] <- 1
  }
}

# check if there are 133 tranfers
table(vaslinks4$transseq)

# then add a count to matching records after transseq = 1
for (i in 2:nrow(vaslinks4)){
  if (vaslinks4$morbseq[i] >=2 && vaslinks4$transseq[i] == 1 
      && vaslinks4$transseq[i-1] >= 1 ){
    vaslinks4$transseq[i] <- vaslinks4$transseq[i-1]+1
  }
}

table(vaslinks4$transseq)

# Q7. ####
# find records with transfer sequence or first admission of transfer and order by row number
to.replace <- sort(c(which(vaslinks4$transseq != 0),which(vaslinks4$transseq == 1)-1))
# create temporary table of these cases, using the minimum number of columns to make it easier to check
tmp <- vaslinks4[to.replace,c("rootlpno","sepdate","fileseq","transseq")]

# unfortunately some individuals have several admissions with transfers,
# this is why we need to group them up first and use a nested statement
# grp is basically a counter for each rootlpno, whic enables us to split the
# results into a ones for transseq = 1 and transseq = 0
tmp2 <- ddply(tmp, .(rootlpno,grp=c(0,cumsum(diff(tmp$transseq)-1))),
              mutate, findate=max(sepdate))

# merge the solution back onto vaslinks4
# note that fileseq will be moved to the front of the table
vaslinks4.1 <- merge(vaslinks4,tmp2[,c("rootlpno","fileseq","findate")], by=c("rootlpno","fileseq"), all.x=TRUE)

# change the format of findate to date
vaslinks4.1$findate <- as.Date(vaslinks4.1$findate)

# fill in sepdate for all findate = NA
vaslinks4.1$findate[is.na(vaslinks4.1$findate)] <- vaslinks4.1$sepdate[is.na(vaslinks4.1$findate)]

# drop the temporary variables
rm(tmp,tmp2,to.replace)

# Q8. ####
vaslinks4.1$los <- as.integer(vaslinks4.1$sepdate-vaslinks4.1$admdate)
vaslinks4.1$los[vaslinks4.1$los == 0] <- 1

vaslinks4.1$totlos <- as.integer(vaslinks4.1$findate-vaslinks4.1$admdate)
vaslinks4.1$totlos[vaslinks4.1$totlos == 0] <- 1

# check numbers, to match screentshot in workbook
View(vaslinks4.1[1839:1846,c(2,29:36)])

# Q9. ####
# the summary command would probably be sufficient, but it doesn't include sd,
# that's why I'm using the psych library
install.packages("pysch")
library(psych)
describe(vaslinks4.1[vaslinks4.1$transseq == 0,c("los","totlos")])

# save new version of vaslinks4
vaslinks4.2 <- vaslinks4.1
save(vaslinks4.2, file="vaslinks4.2.RData")

