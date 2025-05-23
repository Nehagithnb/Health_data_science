# 13th Feb 2023

#to set the working directory to this particular folder 
#where all the files and data are place
setwd("~/Desktop/IALHD")

#to list all the files in that directory
list.files()

#loading the vashmds data for exercise 1
load("vashmds.RData")

#to view or open the data in the pane
View(vashmds)

#Q1 was to open the vashmds file 
#done


#to get the total number of records in that file
nrow(vashmds)

#Q2 was to get the record number
#done


#creating a temporary table
vashmds.tmp <-  vashmds

#Q3 asks for years, so we need chron package
install.packages("chron")

#loading the package
library(chron)

#we have to display the number of separation by years
#in the sepdate column in the vashmds data file

#so here yearsep is the tmp variable created inside the vashmds
#separation by years
#in the sepdata column in the vashmds data
vashmds.tmp$yearsep <- years(vashmds.tmp$sepdate)


# frequency of yearsep - viewing how many years are there 
table(vashmds.tmp$yearsep)

#to get the number of years separated
nrow(table(vashmds.tmp$yearsep))

#Q3 was to create yearsep variable
#and to display the number of years 
#done


# we will use the plyr package
#there are big problems, and it has to be broken down into manageable pieces,
#applied the function and put back together
#plyr - Tools for Splitting, Applying and Combining Data
# install the package first (you only need to do this once)
install.packages("plyr")
# load the package (you need to do this in each new R session)
library(plyr)
# make sure the data is sorted, here in this case it is

# create morbseq using `ddply`
# morbidity sequence
# for each `rootlpno` in  `vashmds.tmp` 
# create a new function (`mutate`) which counts through
# each record for this `rootlpno` 
# and append the result as a new column `morbseq` to the data
# ddply - split dataframe, apply function and return result in dataframe
vashmds.tmp <- ddply(vashmds.tmp,"rootlpno", mutate, morbseq=1:length(rootlpno))

#Q4 was to create a morbseq variable
#Done


# 1 is for the first hospital record of that patient
# 2 is for the second record and 3 will be third and so on..
#to determine nmbr of patients in the dataset
nrow(vashmds.tmp[vashmds.tmp$morbseq == 1,])
#to determine nmbr of patients with single or multiple hospital records
nrow(vashmds.tmp[vashmds.tmp$morbseq == 2,])

#or

#create a new named object so we dont overwrite the old one
vashmds3 <- vashmds
#length of patients with atleast one record
length(unique(vashmds3$rootlpno[vashmds3$morbseq ==1]))
#for 2 records
sum(vashmds3$morbseq==2)
# Q5 is to determine the number of patients in the dataset 
# and with the single or multiple records using morbseq
#Done


# simple solution, but doesn't include std. dev.
# summary(vashmds.tmp$age[vashmds.tmp$morbseq == 1])
# install the package first (you only need to do this once)
install.packages("psych")
# load the package (you need to do this in each new R session)
library(psych)
describe(vashmds.tmp$age[vashmds.tmp$morbseq == 1])

#or

summary.stats1 <- function(x) {round(c("N"=length(x),"Min"=min(x),"Max"=max(x),"Mean"=mean(x),"Std. Dev."=sd(x)),3)}
summary.stats1(vashmds3[vashmds3$morbseq==1,"age"])
# Q6 Done



# Q7. ####
# create a new column `totrec`, which summarises the total number of `morseq`
# for each `rootlpno`
vashmds.tmp <- ddply(vashmds.tmp,"rootlpno", mutate, totrec=max(morbseq))
describe(vashmds.tmp$totrec[vashmds.tmp$morbseq == 1])

# Q8. ####
load("vascancer.RData")
View(vascancer)

# Q9. ####
# order by `rootlpno`, should already be ordered
# vascancer <- vascancer[order(vascancer$rootlpno),]

# create`morbseq` in `vascancer`
vascancer <- ddply(vascancer,"rootlpno", mutate, morbseq=1:length(rootlpno))
# just show rows 20-33, which are the ones shown in the workbook
View(vascancer[20:33,])

# Q10. ####
table(vascancer$morbseq)

# Q11. ####
# create new empty columns
vascancer$cansite1 <- NA
vascancer$cantis1 <- NA
vascancer$candate1 <- as.Date(NA)
vascancer$cansite2 <- NA
vascancer$cantis2 <- NA
vascancer$candate2 <- as.Date(NA)

# if `morbseq` = 1 copy into `cansite1`,`cantis1` and `candate1`
c1 <- which(vascancer$morbseq == 1)
vascancer$cansite1[c1] <- vascancer$cansite[c1]
vascancer$cantis1[c1] <- vascancer$cantis[c1]
vascancer$candate1[c1] <- vascancer$candate[c1]

# if `morbseq` = 2 copy into `cansite2`,`cantis2` and `candate2`
c2 <- which(vascancer$morbseq == 2)
vascancer$cansite2[c2-1] <- vascancer$cansite[c2]
vascancer$cantis2[c2-1] <- vascancer$cantis[c2]
vascancer$candate2[c2-1] <- vascancer$candate[c2]

# only keep rows, where `cansite1` is not NULL (ignoring the duplicate rows) and
# save as `vascancer2`, we also ignore the now redundant columns 2-5
c3 <- which(is.na(vascancer$cansite1) == FALSE)
vascancer2 <- vascancer[c3,c(1,6:11)]

# show rows 19-28 as displayed in the workbook
View(vascancer2[19:28,])

# remove temporary objects `c1`, `c2` and `c3`
rm(c1,c2,c3)

# save `vascancer2` as a new R Data file
save(vascancer2, file="vascancer2.RData")

