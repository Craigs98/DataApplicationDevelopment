#Installing Packages
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
install.packages("RCurl")
install.packages("RSocrata")
install_github("geoffjentry/twitteR")
install.packages("installr") 
install.packages("data.table", dependencies=TRUE)
install.packages("stringi")
install.packages("lubridate", dependencies = TRUE)
install.packages("sqldf")
updateR()


#Importing Libraries
library(devtools)
library(twitteR)
library(installr) 
library("RSocrata")
library(ggmap)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(sqldf)

#DO NOT OPEN THE CSV IN EXCEL AS IT MESSES WITH THE DATE FORMATS

setwd("C:/Users/xcrai/Desktop") #change this to where the csv file is located
nyvirus <- read.csv("new-york-virus.csv", stringsAsFactors = T)
sapply(nyvirus,function(x) sum(is.na(x)))#shows missing data

nyaccidents <- fromJSON("https://data.cityofnewyork.us/resource/h9gi-nx95.json?$limit=200000&$offset=10000")
sapply(nyaccidents,function(x) sum(is.na(x)))#shows missing data


#The corona virus dataset has the following missing data:
# deathConfirmed -  integer
# deathProbable   -  integer
# hospitalized  -  integer
# hospitalizedCumilative  -  integer
# hospitalizedCurrently  -  integer
# inIcuCumulative  -  integer
# inIcuCurrently  -  integer
# totalTestsAntigen  -  integer
# negativeTestsAntibody   -  integer
# negativeTestsPeopleAntibody  -  integer
# inIcuCurrently  -  integer
# totalTestsPeopleViral  -  integer
# positiveTestsAntigen  -  integer
# totalTestsPeopleAntigen  -  integer
# positiveTestsViral  -  integer
# totalTestsPeopleAntibody  -  integer

#All of these have no entries at all so therefore i will just remove what is not present in the dataset, they also are not entirely important for the analysis we want to perform

nyvirus = select(nyvirus, -totalTestsViral, -totalTestsPeopleViral, -totalTestsAntigen, -totalTestsAntibody, -positiveTestsAntigen, -totalTestsPeopleAntigen, -totalTestsPeopleAntibody, -positiveTestsViral, -totalTestEncountersViral, -positiveTestsPeopleAntibody, -recovered, -positiveTestsPeopleAntigen, -hospitalizedCumulative, -positiveTestsAntibody, -inIcuCumulative, -negativeTestsPeopleAntibody, -negativeTestsViral, -deathProbable, -deathConfirmed, -hospitalized, -onVentilatorCurrently, -negativeTestsAntibody, -onVentilatorCumulative)
sapply(nyvirus,function(x) sum(is.na(x)))#shows missing data

# some other datasets have missing data that we can attempt to fix
# They are as follow:
# death - integer
# hospitalizedCurrently - integer
# inIcuCurrently - integer
# positiveCasesViral - integer

#I might end up using some of these so i will fill i the missing numbers with 0
nyvirus$death[is.na(nyvirus$death)] <- 0
nyvirus$hospitalizedCurrently[is.na(nyvirus$hospitalizedCurrently)] <- 0
nyvirus$inIcuCurrently[is.na(nyvirus$inIcuCurrently)] <- 0
nyvirus$positiveCasesViral[is.na(nyvirus$positiveCasesViral)] <- 0

#Checking to see if all missing data is gone
sapply(nyvirus,function(x) sum(is.na(x)))#shows missing data

#checking to see what data is missing to clean up
sapply(nyaccidents,function(x) sum(is.na(x)))#shows missing data

#The crashes in new york dataset has the following missing values:
# latitude -  Integer
# longitude -  Integer
# location -  Integer
# on_street_name - String
# off_street_name - String
# cross_street_name - String
# vehicle_type_code1 - String
# vehicle_type_code2 - String
# vehicle_type_code_3 - String
# vehicle_type_code_4 - String
# vehicle_type_code_5 - String
# borough  - String
# zip_code - Integer
# contributing_factor_vehicle_1 - String
# contributing_factor_vehicle_2 - String
# contributing_factor_vehicle_3 - String
# contributing_factor_vehicle_4 - String
# contributing_factor_vehicle_5 - String

# additional information at: https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95

#These are co-ordinates so we will default these to 0, i wont be using these
nyaccidents$latitude[is.na(nyaccidents$latitude)] <- 0
nyaccidents$longitude[is.na(nyaccidents$longitude)] <- 0
nyaccidents$location[is.na(nyaccidents$location)] <- 0
nyaccidents$zip_code[is.na(nyaccidents$zip_code)] <- 0
# on and off street names we will default to NA as we cannot get the information
nyaccidents$on_street_name[is.na(nyaccidents$on_street_name)] <- "NA"
nyaccidents$off_street_name[is.na(nyaccidents$off_street_name)] <- "NA"
nyaccidents$cross_street_name[is.na(nyaccidents$cross_street_name)] <- "NA"
#The following are vehicle codes, which obviously have missing information as there is not always multiple vehicles involved in each crash
nyaccidents$vehicle_type_code1[is.na(nyaccidents$vehicle_type_code1)] <- "NA"
nyaccidents$vehicle_type_code2[is.na(nyaccidents$vehicle_type_code2)] <- "NA"
nyaccidents$vehicle_type_code_3[is.na(nyaccidents$vehicle_type_code_3)] <- "NA"
nyaccidents$vehicle_type_code_4[is.na(nyaccidents$vehicle_type_code_4)] <- "NA"
nyaccidents$vehicle_type_code_5[is.na(nyaccidents$vehicle_type_code_5)] <- "NA"
#Realistically we could find the borough from the location co-ordinates but for this amount of data it would be incredibly difficult
nyaccidents$borough[is.na(nyaccidents$borough)] <- "NA"
# The last 4 follow the same as vehicle codes as multiple vehicles were not always involved in each crash
nyaccidents$contributing_factor_vehicle_1[is.na(nyaccidents$contributing_factor_vehicle_1)] <- "NA"
nyaccidents$contributing_factor_vehicle_2[is.na(nyaccidents$contributing_factor_vehicle_2)] <- "NA"
nyaccidents$contributing_factor_vehicle_3[is.na(nyaccidents$contributing_factor_vehicle_3)] <- "NA"
nyaccidents$contributing_factor_vehicle_4[is.na(nyaccidents$contributing_factor_vehicle_4)] <- "NA"
nyaccidents$contributing_factor_vehicle_5[is.na(nyaccidents$contributing_factor_vehicle_5)] <- "NA"



nyaccidents$number_of_persons_injured[is.na(nyaccidents$number_of_persons_injured)] <- 0
nyaccidents$number_of_persons_killed[is.na(nyaccidents$number_of_persons_killed)] <- 0



sapply(nyaccidents,function(x) sum(is.na(x)))#Checking for missing data

#This is the infotmation from when i ran the data, it may be different since the data updates daily



#Trying to find readable data for our study
table(nyaccidents$contributing_factor_vehicle_1)
table(nyaccidents$vehicle_type_code1)

#It seems the best is to look at number of people injured etc.
#We can even narrow it down to fatal accidents if the data seems too hectic
table(nyaccidents$number_of_persons_injured)

#Merging the data from both into one 
compiledData <- merge(nyaccidents, nyvirus, by="crash_date", sort = FALSE)
sapply(compiledData,function(x) sum(is.na(x)))#Checking for missing data

#We will divide the data into months to make it more manageable otherwise it will be pretty unreadable
Mar <- compiledData[grepl("2020-03", compiledData[["crash_date"]]), ]#Gets all data in month of March
Apr <- compiledData[grepl("2020-04", compiledData[["crash_date"]]), ]#Gets all data in month of April
May <- compiledData[grepl("2020-05", compiledData[["crash_date"]]), ]#Gets all data in month of May
Jun <- compiledData[grepl("2020-06", compiledData[["crash_date"]]), ]#Gets all data in month of June
Jul <- compiledData[grepl("2020-07", compiledData[["crash_date"]]), ]#Gets all data in month of July
Aug <- compiledData[grepl("2020-08", compiledData[["crash_date"]]), ]#Gets all data in month of August
Sep <- compiledData[grepl("2020-09", compiledData[["crash_date"]]), ]#Gets all data in month of September
Oct <- compiledData[grepl("2020-10", compiledData[["crash_date"]]), ]#Gets all data in month of October

meanCaseMar <- mean(Mar$positiveIncrease)
meanCaseApr <- mean(Apr$positiveIncrease)
meanCaseMay <- mean(May$positiveIncrease)
meanCaseJun <- mean(Jun$positiveIncrease)
meanCaseJul <- mean(Jul$positiveIncrease)
meanCaseAug <- mean(Aug$positiveIncrease)
meanCaseSep <- mean(Sep$positiveIncrease)
meanCaseOct <- mean(Oct$positiveIncrease)
#we now have the average number of cases for each month


#We will now run summary for each month and the number of crashes for each month
summary(Mar$number_of_persons_injured)#Length 5633
summary(Apr$number_of_persons_injured)#Length 4065
summary(May$number_of_persons_injured)#Length 6066
summary(Jun$number_of_persons_injured)#Length 7626
summary(Jul$number_of_persons_injured)#Length 9269
summary(Aug$number_of_persons_injured)#Length 9808
summary(Sep$number_of_persons_injured)#Length 9598
summary(Oct$number_of_persons_injured)#Length 9598

                        #Number of deaths: 1    2   3     Total
table(Mar$number_of_persons_killed)#Length 7           =  7
table(Apr$number_of_persons_killed)#Length 12   1      =  14
table(May$number_of_persons_killed)#Length 12          =  12
table(Jun$number_of_persons_killed)#Length 14   1      =  16
table(Jul$number_of_persons_killed)#Length 11   1      =  13
table(Aug$number_of_persons_killed)#Length 19          =  19
table(Sep$number_of_persons_killed)#Length 38          =  38
table(Oct$number_of_persons_killed)#Length 18   2   1  =  21
#Looking at this we can try to compare the number of cases to the number of deaths

#We will now graph out some of the data of the months with the highest and lowest cases as well as the overall amount
boxplot(positiveIncrease ~ number_of_persons_injured, data=compiledData, main="MERGED DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=compiledData, main="MERGED DATA")
boxplot(positiveIncrease ~ borough, data=compiledData, main="MERGED DATA")

boxplot(positiveIncrease ~ number_of_persons_injured, data=Apr, main="APR DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=Apr, main="APR DATA")
boxplot(positiveIncrease ~ borough, data=Apr, main="APR DATA")

boxplot(positiveIncrease ~ number_of_persons_injured, data=Aug, main="AUG DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=Aug, main="AUG DATA")
boxplot(positiveIncrease ~ borough, data=Aug, main="AUG DATA")

boxplot(positiveIncrease ~ number_of_persons_injured, data=May, main="MAY DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=May, main="MAY DATA")
boxplot(positiveIncrease ~ borough, data=May, main="MAY DATA")

boxplot(positiveIncrease ~ number_of_persons_injured, data=Jul, main="JULY DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=Jul, main="JULY DATA")
boxplot(positiveIncrease ~ borough, data=Jul, main="JULY DATA")

boxplot(positiveIncrease ~ number_of_persons_injured, data=Oct, main="OCT DATA")
boxplot(positiveIncrease ~ number_of_persons_killed, data=Oct, main="Oct DATA")
boxplot(positiveIncrease ~ borough, data=Oct, main="oct DATA")

#Writing the needed files to a csv
write.csv(compiledData, 'C:/Users/xcrai/Desktop/Merged_Data.csv')
