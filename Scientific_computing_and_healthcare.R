#SCIENTIFIC COMPUTING AND HEALTHCARE
# MARCH 06 2023

#packages installation

#Since we are using the data from the PostgreSQL Database,
#we need to install this package for a smooth interaction
#of R package with the PostgreSQL database
#install.packages("RPostgreSQL")

#To get values or a list of objects/ names, we need GetoptLong package
#install.packages("GetoptLong")

#This packages makes it easier to access the database
#It doesn't feel like we are retrieving data from a different database or a platform
#They work quickly as if they are in the memory frames, even when the large data doesn't fit 
#install.packages("dbplyr")

#Tidyverse is mainly used to tidy up the data which we have
#For cleaning the data, removing of missing values
#for getting head or tail part of the data, just like top and least data
#install.packages("tidyverse")

#To do the statistical analysis
#for the T test, Regression
#for Chi-squared tests
#install.packages("stats")

#To do the visualisation of the outcomes
#Plotting can be done easily when this package is installed
#This ggplot has different kinds of plotting in it
#install.packages("ggplot2)


#Let us see if the library packages are present or not
library(RPostgreSQL)
library(GetoptLong)
library(tidyverse)
library(stats)
library(ggplot2)

#Looks like everything is installed
#If not go back and install them again

#Assigning the values
#to connect to the database in this machine
host <- "localhost"
#Port number is the port number which we give while using the PostgreSQL in the PGAdmin
#this is same port, which is used most of the time
port <- 5432
#we are giving the database name in order to retrieve the data
database_name <- "gp_practice_data"
#it is like the name always used when connecting to the database
#we can call it default also
user="postgres"
#Password is the password which I have given 
password="N"


#This part is connection with the postgresql database
con <- dbConnect(
  dbDriver("PostgreSQL"),
  host='localhost',
  port=port, 
  dbname <- database_name,
  user='postgres',
  password=password
)
#this creates a connection with the mentioned username, password, database name, host, port



# **************************************************************
# Part 1: Specific Questions/Tasks


#The query retrieves the top 5 drugs in the entered practice by the user
#I have taken a variable(q1a) where in,
#first I will paste the output which I get from the query
#get_data function is created by me, 
#here it is used to return the data from a particular source that is from the database
get_data <- function(idpractice) {
  q1a <- paste0(" select bnfname as PRESCRIBED_MED, sum(actcost) as MED_PRICE --, quantity as TOTAL_COST
                  from gp_data_up_to_2015 as gpdata
                  where gpdata.practiceid = '", idpractice, "'
                  -- group by 1,2,3,4
                  group by PRESCRIBED_MED
                  order by MED_PRICE desc
                  -- sum(actcost) desc
                  limit 5
                  ")
  topfivedrugs <- dbGetQuery (con, q1a)
  return(topfivedrugs)
}
#this function returns the outcome that is top 5 drugs


#first I am initializing the pattern variable by taking W followed by 5 digits
#the while statement might never stop working, so we have to be careful
# it might create a infinite loop
#inside the While statemnt,if statement is taken 
#to check if I have entered a valid PracticeId
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline("Enter a valid Practice ID: ")
  if(!grepl(pattern,idpractice)) {
    cat("Wrong Practice ID!! Please try again.\n")
  } else {
    cat("The entered Practice ID :",idpractice, "is valid!!\n")
  }
}

#repeat{
#  idpractice <- readline("Enter a valid practice ID: ")
  
#  if(grepl('^W\\d{5}$', idpractice)){
#    valid = TRUE
#    cat("The entered Practice ID :",idpractice, "is valid!!\n")
#    break
#  } else {
#    cat("Wrong Practice ID!! Please try again.\n")
#  }
#}
#it displays wrong practice id and asks to re-enter the practice Id if invalid input is entered
#if valid inputr is entered, then taht input is taken inside the above function and 
#the top five drugs table of that practice ID will be displayed
topfivedrugs <- get_data(idpractice) %>% rename("Prescribed Medication" = prescribed_med)
View(topfivedrugs)

##################
practice <- function(con, idpractice) {
  df <- dbGetQuery(con, paste("
             SELECT practiceid, bnfcode, bnfname, items, nic, 
                  actcost, quantity, period
	           FROM gp_data_up_to_2015
	           WHERE practiceid like '", idpractice, "%'                 
                              ", sep = ""))
  return(df)
}

gp <- data.frame(practice(db_con, idpractice))
###################

medication_chart <- function(gp) {
  if (!is.null(gp$bnfname) && any(!is.na(gp$bnfname))) {
    print("Medication Chart is available")
  } else {
    print("Medication Chart not available")
  }
}
medication_chart(gp)

##################
qof_chart <- function(con, idpractice) {
  df <- dbGetQuery(con, paste("
             SELECT numerator, field4, ratio, orgcode
	           FROM qof_achievement
	           WHERE orgcode like '", idpractice, "%'                 
                              ", sep = ""))
  return(df)
}

qof_data <- data.frame(qof_chart(con, idpractice))
###################

qof_info <- function(qof_data) {
  if (!is.null(qof_data$orgcode) && any(!is.na(qof_data$orgcode))) {
    print("Qof information is available")
  } else {
    print("Qof information not available")
  }
}
qof_info(qof_data)


if(medication_chart && qof_info) {
  
}


get_data <- function(idpractice) {
    number_of_patients <- paste0("
                        SELECT numerator, ratio, orgcode, qi.area, 
                               field4, qi.indicator
                        FROM qof_achievement qa
                        LEFT JOIN qof_indicator qi
                        ON qa.indicator = qi.indicator
                        WHERE qa.orgcode = '", idpractice ,"'
                        AND qa.indicator LIKE '%001' 
                        ")
    total_count <- dbGetQuery( con, number_of_patients)
    total_count$indicator <- as.character(total_count$indicator)
# Filter out rows where indicator starts with 'PC', 'CS', or 'CVD-PP'
    total_count <- total_count %>%
      filter(!grepl('^(PC|CS|CVD-PP)', indicator))
    return(total_count)
}

total_count <- get_data(idpractice)
View(total_count)

print(total_count$field4[1])
cat("The total number of patients in the practice are", total_count$field4[1], "\n")

# According to thttps://www.gpcontract.co.uk/browse/WAL/15
# Palliative Care, Cervical Screening and Primary Prevention of 
# Cardiovascular Disease are not diseases. 



# Defining a function to get the required data
get_medication_data <- function() {
  query <- "
            SELECT practiceid, period, sum_actcost, field4
            FROM gp_qofa_data
            WHERE practiceid LIKE 'W98012';
          "
  data <- dbGetQuery(db_con, query)
  
  # Convert 'period' to Date format
  data$period <- ymd(paste0(data$period, "01"))
  
  return(data)
}

# Call the function
medication_data <- get_medication_data()

# Calculate 'month' column and monthly spend
medication_data <- medication_data %>%
  mutate(month = month(period, label = TRUE),
         monthly_spend = sum_actcost / field4)

# Aggregate data to get one row per month
medication_data <- medication_data %>%
  group_by(month) %>%
  summarise(
    avg_monthly_spend = mean(monthly_spend, na.rm = TRUE)
  )

# Order the data by month
medication_data <- medication_data[order(match(medication_data$month, month.abb)), ]

# Calculate average spend per month per patient
average_spend_per_patient <- mean(medication_data$avg_monthly_spend, 
                                  na.rm = TRUE)

# Output the result
cat("The practice spent an average of", 
    round(average_spend_per_patient, 2), 
    "per month on medication per patient.\n")

# View the result
print(medication_data)

# Visualization: Bar plot of monthly spend
library(ggplot2)

ggplot(medication_data, aes(x = month, y = avg_monthly_spend)) +
  geom_bar(stat = "identity", fill = "pink", color = "brown") +
  labs(title = "Monthly Spend on Medication",
       x = "Month",
       y = "Average Monthly Spend") +
  theme_minimal()

#print(average_spend_per_month)

#################################

# Create a visualisation showing the spend on medication per patient,
# compared to other practices within the postcode area 
# (i.e. first part of postcode);



get_postcode <- function(idpractice, con) {
  query <- paste0("
                    SELECT postcode
                    FROM address
                    WHERE practiceid = '", idpractice ,"'
  ")
  
  result <- dbGetQuery(con, query)
  
  return(result$postcode)
}

postcode <- get_postcode(idpractice, db_con)

# Output the result
cat("The postcode for practice", idpractice, "is:", postcode, "\n")


###### getting all the practices in the postcode
get_practices_in_postcode_part <- function(firstpart_postcode) {
  query <- paste0("
                    SELECT practiceid
                    FROM address
                    WHERE LEFT(postcode, 5) = ' %s';
  ", firstpart_postcode)
  
  result <- dbGetQuery(db_con, query)
  
  return(result$practiceid)
}

# Extract the first part of the postcode
target_postcode_part <- substr(postcode, 1, 5)

# Call the function
practices_in_postcode_part <- get_practices_in_postcode_part(target_postcode_part)

# Output the result
cat("Practices in the postcode part", target_postcode_part, "are:", 
    toString(practices_in_postcode_part), "\n")
##############

library(PostcodesioR)

get_practices_in_postcode_part <- function(db_con, idpractice){
  df <- dbGetQuery(db_con, paste("
                              select postcode
                              from address
                              where practiceid = '", idpractice ,"'
                              ", sep = ""))
  return(df)
}
get_practices_in_postcode_part (db_con, idpractice)

post_code_df <- as.character(get_practices_in_postcode_part (db_con, idpractice))

postcode_c <- data_frame(postcode_lookup(post_code_df))

out_postcode <- postcode_c$outcode

print(out_postcode)

##
county <- function(db_con, idpractice){
  df <- dbGetQuery(db_con, paste("
                              select county
                              from address
                              where practiceid = '", idpractice ,"'
                              ", sep = ""))
  return(df)
}
county <- as.character(county(db_con, idpractice))
county[1]

##

df <- dbGetQuery(db_con, paste("
                              select practiceid, postcode
                              from address
                              where postcode ilike '", out_postcode, " %'
                              ", sep = ""))

ppp <- data_frame(df)
# Assuming 'ppp' is the dataframe with 'practiceid' and 'postcode'
print(ppp)

############

# Assuming 'ppp' is your data frame
practiceid_list <- as.list(ppp$practiceid)
practiceid_string <- paste0("'", practiceid_list, "'", collapse = ",")
get_actcost_data <- glue::glue("
  SELECT practiceid, SUM(actcost) AS total_actcost
  FROM gp_data_up_to_2015
  WHERE practiceid IN ({practiceid_string})
  GROUP BY practiceid
")
total_actcost <- dbGetQuery(db_con, get_actcost_data)

get_qof_data <- glue::glue("
  SELECT orgcode as practiceid, SUM(numerator) AS no_of_patients
  FROM qof_achievement
  WHERE orgcode IN ({practiceid_string})
  GROUP BY orgcode
")

numerator<- dbGetQuery(db_con, get_qof_data)

spend_table <- numerator %>%
  left_join(total_actcost, by = 'practiceid')

spend_table <- spend_table %>%
  mutate(spend_per_pateint = total_actcost/no_of_patients)

ggplot(spend_table, aes(x = practiceid, y = spend_per_pateint)) +
  geom_bar(stat = "identity", fill = "pink", color = "brown") +
  labs(title = paste("Spend Per Patient For practices in", county[1]),
       x = "Practices",
       y = "Spend Per Patient") +
  theme_minimal()

###########

# Execute queries and fetch data
gp_data <- dbGetQuery(db_con, get_gp_data)
qof_data <- dbGetQuery(db_con, get_qof_data)


# print(gp_data)

# Merge the dataframes based on 'practiceid'
merged_data <- merge(ppp, gp_data, by = 'practiceid', all.x = TRUE)
merged_data <- merge(merged_data, qof_data, by = 'practiceid', all.x = TRUE)

merged_data$spend_per_patient <- merged_data$total_actcost / merged_data$total_numerator

# View the merged data
View(merged_data)

##################

get_practice_ids_by_postcode <- function(con, idpractice) {
  # Get the postcode for the specified practice ID
  postcode_df <- dbGetQuery(con, paste("
                               SELECT postcode
                               FROM address
                               WHERE practiceid = '", idpractice, "'
                               ", sep = ""))
  
  # Extract the outcode using the PostcodesioR package
  postcode_info <- postcode_lookup(postcode_df$postcode)
  out_postcode <- postcode_info$outcode
  
  # Extract only the outcode from the full postcode
  outcode <- substr(out_postcode, 1, 3)  # Adjust the substring positions based on your postcode pattern
  
  # Query practice IDs based on the outcode
  practice_ids_df <- dbGetQuery(con, paste("
                               SELECT practiceid
                               FROM address
                               WHERE LEFT(postcode, 3) = '", outcode, "'
                               ", sep = ""))
  
  return(practice_ids_df$practiceid)
}

# Example: Specify the practice ID you are interested in
# target_practice_id <- 'W98012'

# Call the function
practice_ids <- get_practice_ids_by_postcode(con, idpractice)

# Output the result
cat("Practice IDs in the same postcode part as", idpractice, "are:", toString(practice_ids), "\n")















#################

diabetes_practice <- function(idpractice) {
  query2 <- paste0("
                  SELECT id, year, numerator, field4, ratio, 
                         centile, orgcode, indicator, active
                  FROM public.qof_achievement
                  WHERE orgcode = '", idpractice, "'
                  AND indicator = 'DM001'
                  ")
  practicedm <- dbGetQuery(con, query2)
  practicedm$ratio <- practicedm$ratio * 100
  return(practicedm)
}

diabetes_practice <- as.data.frame(diabetes_practice(idpractice))
# Function to retrieve QOF data for all the practices in Wales
diabetes_wales <- function() {
  query3 <- paste0("
                  SELECT id, year, numerator, field4, ratio, 
                         centile, orgcode, indicator, active
                  FROM public.qof_achievement
                  WHERE orgcode = 'WAL' and indicator = 'DM001'
                  ")
  walesdm <- dbGetQuery(con, query3)
  walesdm$ratio <- walesdm$ratio * 100
  return(walesdm)
}

diabetes_wales <- as.data.frame(diabetes_wales())

diabetes_df <- diabetes_practice %>%
  merge(diabetes_wales, all = TRUE)

###########
diabetes_comparison_inside_bars_percentage_labels <- function(diabetes_df) {
  ggplot(diabetes_df, aes(x = orgcode, y = ratio, fill = orgcode)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", ratio)), 
              position = position_dodge(width = 0.8), 
              vjust = 1.5, size = 3, color = "black") +  # Adjust vjust for label position inside the bar
    labs(title = "Rate of Diabetes: Practice vs Wales",
         x = "Practice ID",
         y = "Ratio (%)") +
    scale_fill_manual(values = c("blue", "red"),
                      name = "Dataset") +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 10)) +
    theme_minimal()
}

# Visualize the comparison using a side-by-side bar chart with percentage y-axis and labels inside the bars
diabetes_comparison_inside_bars_percentage_labels(diabetes_df)
##########








# ****************************************************************

#The query returns the top five illness along with the affected count and percentage
#in the Wales
get_data <- function(idpractice) {
  q1b <- paste0("
                  select qofi.area as TOP_FIVE_ILLNESS,
                  qofa.indicator as DISEASE_ID,
	                qofa.numerator as AFFECTED_COUNT,
	                qofa.field4 as TOTAL_COUNT,
	                ((qofa.ratio)*100) as AFFECTED_PERCENTAGE,
	                qofa.orgcode as WALES_ORGANIZATIONCODE
                  from qof_achievement as qofa
                  join qof_indicator as qofi
                  on qofa.indicator = qofi.indicator
                  where qofa.orgcode like '%WAL%' 
                  order by 3 desc
                  limit 5
                  ")
  topfiveillness <- dbGetQuery (con, q1b)
  return(topfiveillness)
}

topfiveillness <- get_data(idpractice)
View(topfiveillness)
#to display the top five illness in Whole of Wales


# ***************************************************************

#TIDYVERSE
#in case there are any 'na' or missing values in the tables 
#it can be removed using the function 
#.      na.omit()
#there are few values missing in the county column in the table
#.      address_table <- tb(con, "select * from address")
#this removes the values which are na that is not available
#.      address_table <- drop_na(address)


# *******************************************************************

#decide which region the practice is in 
#since the user inputs the practice id,
#below is the query to get the region
#(practice name, street, county, postcode) of that practice input

#in the below few codes - I am showing how to get the region 
#in different ways by entering the practiceID


# to get the region(county) of the practice id which is being input
get_data <- function(idpractice) {
  q1c <- paste0("select practiceid, street as practice_name, county, postcode
                 from address
                 where practiceid = '", idpractice, "'
                ")
  region <- dbGetQuery (con, q1c)
  return(region)
}

#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
region <- get_data(idpractice)
print(region)
View(region)               
#output gives the region where the entered practice ID is in


# to get the region(county) of the practice id entered by user 
#through postcode
get_data <- function(idpractice) {
  q1cpo <- paste0("select county
                  from address
                  where postcode in (
                                  select postcode
                                  from address
                                  where practiceid = '", idpractice, "'
                                  )
                ")
  gp_county2 <- dbGetQuery (con, q1cpo)
  return(gp_county2)
} 
#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
gp_county2 <- get_data(idpractice)
print(gp_county2)
View(gp_county2)                  
#Displays the county name by using the postcode of that practice ID


# to get the region(postcode) of the practice id entered by user through county
get_data <- function(idpractice) {
  q1cco <- paste0("select county, postcode
                   from address
                   where county in (
                                    select county
                                    from address
                                    where practiceid = '", idpractice, "'
                                   )
                 ")
  gp_postcode <- dbGetQuery (con, q1cco)
  return(gp_postcode)
}
#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
gp_postcode <- get_data(idpractice)
print(gp_postcode)
View(gp_postcode)
#displays the list of all the postcodes in that county of the entered prcatice ID



#the list of gps in the returned county output
get_data <- function(idpractice) {
  q1ca <- paste0("select practiceid, street as practice_name, county
                  from address
                  where county in (
                                  select county
                                  from address
                                  where practiceid = '", idpractice, "'
                                  )
                ")
  gp_list <- dbGetQuery (con, q1ca)
  return(gp_list)
}
#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
gp_list <- get_data(idpractice)
print(gp_list)
View(gp_list)
#displays all the Practices in that county


#-- total count of gps in the returned county output
get_data <- function(idpractice) {
  q1cb <- paste0("select count(distinct practiceid) as number_of_gp
                  from address
                  where county in (
                                  select county
                                  from address
                                  where practiceid = '", idpractice, "'
                                  )
                ")
  gp_count <- dbGetQuery (con, q1cb)
  return(gp_count)
}
#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
gp_count <- get_data(idpractice)
print(gp_count)
View(gp_count)
#Total number of GP in that county



#top 10 diseases from the input practiceid
get_data <- function(idpractice) {
  q1cc <- paste0("select qofi.area as illness, qofi.indicator as diseaseid, qofa.numerator as affected_count
                  from qof_indicator as qofi
                  inner join qof_achievement as qofa
                  on qofi.indicator = qofa.indicator
                  where qofa.orgcode = '", idpractice, "' and qofa.year = 15
                  group by 1,2,3
                  order by 3 desc
                  limit 10
                ")
  gp_illness <- dbGetQuery (con, q1cc)
  return(gp_illness)
}
#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
gpdisease <- get_data(idpractice)
print(gpdisease)
View(gpdisease)
#displays the top 10 diseases in the practice



# *************************************************************

########### PART 1 ### Q2 ### 1 point ##############


#The association of spend on medication per patient and top five illness in Wales

get_data <- function(idpractice) {
  q2a <- paste0("
  SELECT DISTINCT
qofi.area AS illness, 
qofa.indicator AS illness_id,
qofa.numerator AS affected_count,
(qofa.ratio * 100) AS percentage,
gpdata.actcost,
SUM(gpdata.actcost) / SUM(qofa.numerator) AS spend_on_med
FROM qof_achievement AS qofa
INNER JOIN qof_indicator AS qofi ON qofa.indicator = qofi.indicator
INNER JOIN gp_data_up_to_2015 AS gpdata ON qofa.orgcode = gpdata.practiceid
WHERE  gpdata.practiceid = '", idpractice, "'
--GROUP BY gpdata.actcost,qofi.area, qofa.indicator, qofa.numerator, qofa.ratio
group by 5,1,2,3,qofa.ratio
order by 5 desc
limit 5")
  medspend_illness <- dbGetQuery (con, q2a)
  return(medspend_illness)
}

#now enter the input and validate 
pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}

medspend_illness <- get_data(idpractice)
print(medspend_illness)
View(medspend_illness)
#displays the spend on medication with respect to the top five illness

#let me a plot a graph for the same
library(ggplot2)
#the package is already installed 


#ggplot is a kind of plotting in R
#medspend_illness is the output table
#x axis is the illness_id
#y axis is the spend_on_med
#in geom
#I am taking a point or a scatter plot first
#if the numbers in shape is changed, different shapes are appeared on the plot
#size is how large the shape should be and color is as name says
#theme is where anything can be given
#i have filled the background color into blue
#labs is the naming of the x and y axis
#ggtitle is the plot name

#scatter plot
ggplot(medspend_illness, aes(x = illness_id, y = spend_on_med)) +
  geom_point(shape = 1,size = 9, col='black' ) +
  theme(panel.background = element_rect(fill = "lightblue"))+
  labs(y = "SPEND ON MEDICATION",
       x = "ILLNESS") +
  ggtitle("SPEND ON MEDICATION IN ASSOCIATION WITH TOPFIVE ILLNESS")

# OR


#in the theme, panel grid can remove the grid structure in the background
#it doesn't look good though, I left it to be there
#but I did remove the minor grid lines which were amidst the major grid lines
#width is how much the width of the bar should be
#bar plot
ggplot(medspend_illness, aes(x=illness_id, y=spend_on_med))+
  geom_bar(stat="identity", fill='pink', col='black', width=0.3, lty=3)+
  theme(panel.background = element_rect(fill = "lightblue"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("SPEND ON MEDICATION IN ASSOCIATION WITH TOPFIVE ILLNESS")+
  xlab("ILLNESS")+
  ylab("SPEND ON MEDICATION")


# *************************************************************#

#STATISTICAL ANALYSIS ON med spend per patient 
#in association with illness

summary(medspend_illness$spend_on_med)
#the summary means it gives 
#the minimum, median, mean and the maximum values 


cormed <- cor(medspend_illness$affected_count,medspend_illness$spend_on_med)
cat("the analysis of correlation of people affected and medication spent:",cormed,"\n")
#correlation means finding tehe relationship among these variables


anova_model <- aov(spend_on_med ~ illness, data = medspend_illness)
summary(anova_model)
#Hypothesis Testing is to check whether they take null values
#here we are checking how the spend on medication for illness is significantly different


t_med <- t.test(medspend_illness$affected_count,medspend_illness$spend_on_med)
print(t_med)
#T Test is applied here to get the mean of x and y

reg_model <- lm(spend_on_med ~ affected_count + percentage, data = medspend_illness)
summary(reg_model)
#This is linear regression analysis




###########################   PART 2    ###########################

#the illness or conditions are classified into different Therapeutic Classes.
#Given some thought, framed a query for retrieving 
#the five most affected therapeutic class along with 
#total number of illnesses in the same. 

therapeutic_class_distribution <- dbGetQuery(con,
                                             "select bnfchapter as therapeutic_class_id,
                                              chapterdesc as therapeutic_class,
                                              count(chapterdesc) as total_no
                                              from bnf
                                              group by 1,2
                                              order by 2 desc
                                              limit 5"
)

View(therapeutic_class_distribution)

#Visual representation of the same is also plotted. 
ggplot(therapeutic_class_distribution, aes(x=therapeutic_class, y=total_no, color = therapeutic_class ))+
  geom_point(stat="identity",shape=1, size=10)+
  scale_color_manual(values =c("lightpink","deeppink","plum","magenta","purple"))+
  theme(panel.background = element_rect(fill = "black"),
        #panel.grid.major = element_blank()
        panel.grid.minor = element_blank())+
  ggtitle("therapeutic_class_distribution")+
  xlab("Therapeutic_Class")+
  ylab("Total_Number")

#Pearson’s Chi-squared test is also applied.
therapeutic_class_exp <- table(therapeutic_class_distribution$therapeutic_class, therapeutic_class_distribution$total_no)
chisq.test(therapeutic_class_exp)

####################


#Top drug in 2013
topdrug2013 <- dbGetQuery(con,"select distinct bnfcode, bnfname, quantity
from gp_data_up_to_2015 as gpdata
where cast(gpdata.period as text) like '2013%'
group by 1,2,3
order by 3 desc
limit 1")
View(topdrug2013)

#top drug in 2014
topdrug2014 <- dbGetQuery(con, "select distinct bnfcode, bnfname, quantity
from gp_data_up_to_2015 as gpdata
where cast(gpdata.period as text) like '2014%'
group by 1,2,3
order by 3 desc
limit 1")
View(topdrug2014)

#top drug in 2015
topdrug2015 <- dbGetQuery(con, "select distinct bnfcode, bnfname, quantity
from gp_data_up_to_2015 as gpdata
where cast(gpdata.period as text) like '2015%'
group by 1,2,3
order by 3 desc
limit 1")
View(topdrug2015)

topdrug <- rbind(topdrug2013, topdrug2014, topdrug2015)
topdrug$year <- c(2013, 2014, 2015)


########################

#The Pembrokeshire Herald News, on October 21, 2021 said 
#that there is a high demand of health care facilities in that region. 
#I went through the data related to the diseases and the affected regions and 
#found out that PEMBROKE DOCK is a town in Wales 
#which is one of the most dangerous places to live in. 
#The code when executed, retrieves the top 8 affected diseases and 
#number of cases or the affected rates in Pembroke Dock. 

Pembroke_Dock <- dbGetQuery(con,"
select qofi.area, qofi.indicator, addr.posttown, addr.county, qofa.numerator
from qof_indicator as qofi
inner join qof_achievement as qofa
on qofi.indicator = qofa.indicator
inner join address as addr
on qofa.orgcode = addr.practiceid
where lower(addr.posttown) ilike '%Pembroke Dock%'
order by qofa.numerator desc
limit 8"
)
View(Pembroke_Dock)



####################

#which county sells the highest number of drug and the drug name
HighestDrugCounty <- dbGetQuery(con,"select addr.county, gpdata.bnfname, gpdata.bnfcode, gpdata.actcost, gpdata.quantity
                            from public.gp_data_up_to_2015 as gpdata
                            inner join public.address as addr
                            on gpdata.practiceid = addr.practiceid
                            order by 3 desc
                            limit 1")

View(HighestDrugCounty)


#######################
#As I live in Swansea and Cardiff is the capital of Wales, 
#I am comparing the top five diseases in both and representing the same in a plot. 
#The outcome shows that Blood Pressure is the top illness in both the places. 
#The plot shows the clear understanding of this. 
#Performed Shapiro-Wilk normality test and T-test on the outcomes.

Top5DiseasesSwansea <- dbGetQuery(con,"select qofi.indicator, qofi.area, qofa.numerator, addr.postcode
from public.qof_indicator as qofi
inner join public.qof_achievement as qofa
on qofi.indicator = qofa.indicator
inner join public.address as addr
on qofa.orgcode = addr.practiceid
where addr.postcode like 'SA%'
order by qofa.numerator desc
limit 5")

View(Top5DiseasesSwansea)

########################

Top5DiseasesCardiff <- dbGetQuery(con,"select qofi.indicator, qofi.area, qofa.numerator, addr.postcode
from public.qof_indicator as qofi
inner join public.qof_achievement as qofa
on qofi.indicator = qofa.indicator
inner join public.address as addr
on qofa.orgcode = addr.practiceid
where addr.postcode like 'CF%'
order by qofa.numerator desc
limit 5")

View(Top5DiseasesCardiff)


TopFiveDiseases <- rbind(transform(Top5DiseasesSwansea, Location = "Swansea"),
                         transform(Top5DiseasesCardiff, Location = "Cardiff"))


#PLOTTING
ggplot(TopFiveDiseases, aes(x = reorder(indicator, -numerator), y = numerator, fill = Location))+
  geom_col(col="black")+
  #coord_flip()+
  labs(x = "Disease", y = "Number of cases", fill = "Location")+
  ggtitle("Top Diseases in Swansea and Cardiff")+
  theme(legend.position = "bottom")


#STATISTICAL ANALYSIS
mostdisease <- TopFiveDiseases[1]
swansea_result <- Top5DiseasesSwansea[Top5DiseasesSwansea$indicator == mostdisease, "numerator"]
cardiff_result <- Top5DiseasesCardiff[Top5DiseasesCardiff$indicator == mostdisease, "numerator"]

#SHAPIRO-WILK NORMALITY TEST
shapiro.test(swansea_result)
shapiro.test(cardiff_result)
var.test(swansea_result,cardiff_result)

#T TEST
ttestout <- t.test(swansea_result,cardiff_result, var.equal = TRUE)
ttestout
#OUTPUT FOR THE T TEST


######################

#	The last bit of code gives the data about the 
#5 most prevalent conditions in the Diabetes Mellitus.

#The same code can be used to get data of Atrial Fibrillation or Asthma as well.

get_data <- function(idpractice) {
  dm <- paste0("select qofi.area, qofi.description, qofi.indicator,(qofa.ratio*100) as perc
from qof_achievement as qofa
inner join qof_indicator as qofi
on qofa.indicator = qofi.indicator
inner join address as addr
on qofa.orgcode = addr.practiceid
where qofi.indicator like 'DM%' and addr.practiceid = '", idpractice, "' 
limit 5
                  ")
  DiabetesMellitus <- dbGetQuery (con, dm)
  return(DiabetesMellitus)
}

pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
DiabetesMellitus <- get_data(idpractice)
print(DiabetesMellitus)
View(DiabetesMellitus)

###############################

get_data <- function(idpractice) {
  af <- paste0("select qofi.area, qofi.description, qofi.indicator,(qofa.ratio*100) as perc
from qof_achievement as qofa
inner join qof_indicator as qofi
on qofa.indicator = qofi.indicator
inner join address as addr
on qofa.orgcode = addr.practiceid
where qofi.indicator like 'AF%' and addr.practiceid = '", idpractice, "' 
limit 5
                  ")
  AtrialFibrillation <- dbGetQuery (con, af)
  return(AtrialFibrillation)
}

pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
AtrialFibrillation <- get_data(idpractice)
print(AtrialFibrillation)
View(AtrialFibrillation)


###########################

get_data <- function(idpractice) {
  ast <- paste0("select qofi.area, qofi.description, qofi.indicator,(qofa.ratio*100) as perc
from qof_achievement as qofa
inner join qof_indicator as qofi
on qofa.indicator = qofi.indicator
inner join address as addr
on qofa.orgcode = addr.practiceid
where qofi.indicator like 'AST%' and addr.practiceid = '", idpractice, "' 
limit 5
                  ")
  Asthma <- dbGetQuery (con, ast)
  return(Asthma)
}

pattern <- "^W\\d{5}$"
idpractice <- ""
while(!grepl(pattern, idpractice)){
  idpractice <- readline(prompt = "Enter a valid Practice ID:")
  if(!grepl(pattern,idpractice)){
    cat("Wrong Practice ID!! Please try again.\n")
  }
  cat("The entered Practice ID :",idpractice, "is valid!!\n")
}
Asthma <- get_data(idpractice)
print(Asthma)
View(Asthma)


######################## END OF ANALYSIS ##########################
