#connecting to Db2
source('login.R')


#loading libraries

library(tidyverse)
library(survival)
library(ggfortify)
library(ggplot2)
library(survminer)
library(rpart)

################## GP Data #########################
ASTHMA <- sqlQuery(channel,
        "SELECT ALF_PE, GNDR_CD AS GENDER , WOB ,
        EVENT_CD AS ASTHMA, EVENT_DT AS EVENT_DATE
        FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec 
        WHERE EVENT_CD LIKE 'H33%' AND EVENT_DT > '2020-02-01' ")

############### Covid ###################
COVID_19 <- sqlQuery(channel,
         "SELECT ALF_PE, EVENT_CD AS COVID_19
         FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec 
         WHERE EVENT_CD LIKE 'A795.'")



############### Severe Asthma / Chronic Asthma ###################
CHRONIC_ASTHMA <- sqlQuery(channel,
        "SELECT ALF_PE, EVENT_CD AS SEVERITY
        FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
        WHERE EVENT_CD LIKE 'H333.'")




# Joining Asthma and Covid-19 and naming it as Analysis Table.
ANALYSIS_TABLE <- ASTHMA %>%
  left_join(COVID_19, by = "ALF_PE", relationship = "many-to-many")


# Joining Analysis and Chronic Asthma and joining it to Analysis Table.
ANALYSIS_TABLE <- ANALYSIS_TABLE %>%
  left_join(CHRONIC_ASTHMA, by = "ALF_PE", relationship = "many-to-many")


#Asthma only after the event date
COMOR_ASTHMA <- sqlQuery(channel,
                   "SELECT ALF_PE, EVENT_CD AS ASTHMA
                   FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                   WHERE EVENT_CD LIKE 'H33%' AND EVENT_DT > '2020-02-01' ")



################ Co morbidities #################


RHINITIS <- sqlQuery(channel,
                    "SELECT ALF_PE, EVENT_CD AS RHINITIS
                    FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                    WHERE EVENT_CD LIKE 'H17%' AND EVENT_DT > '2020-02-01' ")
COMORBIDITY <- COMOR_ASTHMA %>%
  left_join(RHINITIS, by = "ALF_PE", relationship = "many-to-many")





OBESITY <- sqlQuery(channel, 
                    "SELECT ALF_PE, EVENT_CD AS OBESITY
                    FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                    WHERE EVENT_CD LIKE 'C380.' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY  %>%
  left_join(OBESITY, by = "ALF_PE", relationship = "many-to-many")




CHRONIC_SINUSITIS<-sqlQuery(channel,
                    "SELECT ALF_PE, EVENT_CD AS CHRONIC_SINUSITIS
                    FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                    WHERE EVENT_CD LIKE 'H13%' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY %>%
  left_join(CHRONIC_SINUSITIS, by = "ALF_PE", relationship = "many-to-many")




ANXIETY <- sqlQuery(channel,
                    "SELECT ALF_PE, EVENT_CD AS ANXIETY
                    FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                    WHERE EVENT_CD LIKE 'E200.' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY %>%
  left_join(ANXIETY, by = "ALF_PE", relationship = "many-to-many")




DEPRESSION <- sqlQuery(channel,
                       "SELECT ALF_PE, EVENT_CD AS DEPRESSION
                       FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                       WHERE EVENT_CD LIKE '1285.' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY %>%
  left_join(DEPRESSION, by = "ALF_PE", relationship = "many-to-many")




OBSTRUCTIVE_SLEEP_APNOEA<-sqlQuery(channel,
                            "SELECT ALF_PE, EVENT_CD AS OBSTRUCTIVE_SLEEP_APNOEA
                          FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                          WHERE EVENT_CD LIKE 'Fy03.' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY %>%
  left_join(OBSTRUCTIVE_SLEEP_APNOEA, by = "ALF_PE", relationship = "many-to-many")



NASAL_POLYPS <- sqlQuery(channel,
                      "SELECT ALF_PE, EVENT_CD AS NASAL_POLYPS
                      FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                      WHERE EVENT_CD LIKE 'H11%' AND EVENT_DT > '2020-02-01' ")
COMORBIDITY <- COMORBIDITY %>%
  left_join(NASAL_POLYPS, by = "ALF_PE", relationship = "many-to-many")




ATOPY <- sqlQuery(channel,
                  "SELECT ALF_PE, EVENT_CD AS ATOPY
                  FROM SAIL1281V.WLGP_GP_EVENT_CLEANSED_20220201 wgec
                  WHERE EVENT_CD LIKE 'SN533' AND EVENT_DT > '2020-02-01'")
COMORBIDITY <- COMORBIDITY %>%
  left_join(ATOPY, by = "ALF_PE", relationship = "many-to-many")




#Removing the Asthma column 
COMORBIDITY <- COMORBIDITY[,-2]



#Removing the repeated data from ANALYSIS_TABLE
ANALYSIS_TABLE <- ANALYSIS_TABLE %>%
  filter(!duplicated(ALF_PE))


#Joining Comorbidity table to ANALYSIS_TABLE
ANALYSIS_TABLE <- ANALYSIS_TABLE %>%
  left_join(COMORBIDITY, by = "ALF_PE", relationship = "many-to-many")


#Removing the repeated data from ANALYSIS_TABLE again
ANALYSIS_TABLE <- ANALYSIS_TABLE %>%
  filter(!duplicated(ALF_PE))


#Defining start and stop date for my study
ANALYSIS_TABLE$STUDY_START_DATE <- '2020-02-01'
ANALYSIS_TABLE$STUDY_STOP_DATE <- '2022-01-01'



ANALYSIS_TABLE$WOB <- as.Date(ANALYSIS_TABLE$WOB)
ANALYSIS_TABLE$STUDY_START_DATE <- as.Date(ANALYSIS_TABLE$STUDY_START_DATE)
ANALYSIS_TABLE$STUDY_STOP_DATE <- as.Date(ANALYSIS_TABLE$STUDY_STOP_DATE)


#Calculating age
ANALYSIS_TABLE$Age <- interval(ANALYSIS_TABLE$WOB, ANALYSIS_TABLE$STUDY_STOP_DATE) %/% years(1)



#Converting Male and Female data into binary data
ANALYSIS_TABLE$GENDER <- ifelse(ANALYSIS_TABLE$GENDER == 1,0,1)


#converting all co morbidity values to binary
to_convert <- c("COVID_19", "SEVERITY", "RHINITIS","OBESITY","CHRONIC_SINUSITIS",
                "ANXIETY","DEPRESSION","OBSTRUCTIVE_SLEEP_APNOEA","NASAL_POLYPS", "ATOPY")
for(column in to_convert){
  ANALYSIS_TABLE[[column]] <- ifelse(is.na(ANALYSIS_TABLE[[column]]),0,1)
}



#Removing the WOB column
ANALYSIS_TABLE <- ANALYSIS_TABLE[,-3]


ANALYSIS_TABLE$EVENT_DATE <- as.Date(ANALYSIS_TABLE$EVENT_DATE)
ANALYSIS_TABLE$STUDY_START_DATE <- as.Date(ANALYSIS_TABLE$STUDY_START_DATE)

#Time to Event
ANALYSIS_TABLE <- ANALYSIS_TABLE %>%
  mutate(TIME_TO_EVENT = EVENT_DATE - STUDY_START_DATE)



#removing NA values
na_rows <- ANALYSIS_TABLE[is.na(ANALYSIS_TABLE$STUDY_START_DATE)|
                            is.na(ANALYSIS_TABLE$EVENT_DATE)|
                            is.na(ANALYSIS_TABLE$STOP_DATE),]
ANALYSIS_TABLE<- ANALYSIS_TABLE[complete.cases(ANALYSIS_TABLE),]



#removing outliers
outliers<-ANALYSIS_TABLE[ANALYSIS_TABLE$EVENT_DATE > as.Date("2022-01-01")|
                           ANALYSIS_TABLE$EVENT_DATE < as.Date("2020-02-01"),]
ANALYSIS_TABLE<-ANALYSIS_TABLE[!(ANALYSIS_TABLE$EVENT_DATE > as.Date("2022-01-01")|
                           ANALYSIS_TABLE$EVENT_DATE < as.Date("2020-02-01")),]


#Age into Age groups
age_groups <- cut(ANALYSIS_TABLE$Age, breaks =  c(1,20,40,60,80,Inf),
                labels = c ("GROUP 1", "GROUP 2", "GROUP 3", "GROUP 4", "GROUP 5"))
ANALYSIS_TABLE <- mutate(ANALYSIS_TABLE, age_groups = age_groups)

#write.csv(ANALYSIS_TABLE, file = 'ANALYSIS_TABLE.csv', row.names = FALSE)



ANALYSIS_TABLE <- read_csv("ANALYSIS_TABLE.csv")
################ LOGISTIC REGRESSION ####################


#Fitting the logistic regression model
regmodel <- glm(SEVERITY ~ GENDER + RHINITIS + OBESITY + CHRONIC_SINUSITIS +
                  ANXIETY + DEPRESSION + OBSTRUCTIVE_SLEEP_APNOEA +
                  NASAL_POLYPS + ATOPY + COVID_19 + Age ,
                data = ANALYSIS_TABLE,
                family = binomial)
#summary of the model's results,
summary(regmodel)



############### DECISION TREE #####################

par(mfrow=c(1,1),xpd=NA)

#scaling the age
ANALYSIS_TABLE$Age <- scale(ANALYSIS_TABLE$Age)

Tree_Model <- rpart(SEVERITY ~ 
                   GENDER + RHINITIS + OBESITY +
                     CHRONIC_SINUSITIS + ANXIETY + DEPRESSION +
                     OBSTRUCTIVE_SLEEP_APNOEA + NASAL_POLYPS + 
                     ATOPY + COVID_19 + Age ,
                    data = ANALYSIS_TABLE,  
                  control = rpart.control(minsplit = 10, cp = 0.001))

plot(Tree_Model)
text(Tree_Model)


############## SURVIVAL ANALYSIS #################



Survobject <- with(ANALYSIS_TABLE, Surv(TIME_TO_EVENT, SEVERITY))
print(Survobject)


object_model <- survfit(Survobject ~ 1)
print(object_model)

#fit the curve
ggsurvplot(object_model, data = ANALYSIS_TABLE, censor = FALSE)


#Fit a cox proportional model
comorbidities_model <- coxph(Survobject ~ GENDER + RHINITIS + OBESITY +
                               CHRONIC_SINUSITIS + ANXIETY + DEPRESSION +
                               OBSTRUCTIVE_SLEEP_APNOEA + NASAL_POLYPS + 
                               ATOPY + COVID_19 + Age ,
                             data = ANALYSIS_TABLE)

#summary for cox proportional model
summary(comorbidities_model)





################### Survival Analysis Plots ###################

Genderplot <- survfit(Survobject ~ GENDER, 
                     data = ANALYSIS_TABLE)
print(Genderplot)
ggsurvplot(Genderplot, data = ANALYSIS_TABLE, censor = FALSE)





ChronicSinusitisplot <- survfit(Survobject ~ CHRONIC_SINUSITIS,
                       data = ANALYSIS_TABLE)
print(ChronicSinusitisplot)
ggsurvplot(ChronicSinusitisplot, data = ANALYSIS_TABLE, censor = FALSE)




ObstructiveSleepApnoeaplot <- survfit(Survobject ~ OBSTRUCTIVE_SLEEP_APNOEA,
                       data = ANALYSIS_TABLE)
print(ObstructiveSleepApnoeaplot)
ggsurvplot(ObstructiveSleepApnoeaplot, data = ANALYSIS_TABLE, censor = FALSE)




Covid19plot <- survfit(Survobject ~ COVID_19,
                       data = ANALYSIS_TABLE)
print(Covid19plot)
ggsurvplot(Covid19plot, data = ANALYSIS_TABLE, censor = FALSE)




AgeGroupsplot <- survfit(Survobject ~ age_groups,
                       data = ANALYSIS_TABLE)
print(AgeGroupsplot)
ggsurvplot(AgeGroupsplot, data = ANALYSIS_TABLE, censor = FALSE)


