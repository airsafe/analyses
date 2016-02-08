# Background analysis updated 8 February 2016   
# Todd Curtis, AirSafe.com

# Boston Police Department Field Interrogation and Observation (FIO) data 2011-April 2015
# Details on this data release, plus results of an earlier study, at
# http://bpdnews.com/news/2016/1/7/commissioner-evans-continues-efforts-to-increase-transparency-and-accountability-of-policing-activities-to-the-public
        
# The information gleaned from this FIO data will complement  an
# October 2014 study of an earlier set of FIO data from 2007-2010. 
# The earlier study was by Jeffrey Fagan of Columbia Law School and 
# School of Public Health, and Anthony A. Braga of the School of 
# Criminal Justice at Rutgers University in Newark and a research
# fellow at the Kennedy School at Harvard.
# Braga:  973-353-5923, braga@andromeda.rutgers.edu; 617-495-5188, anthony_braga@harvard.edu
# Fagan: 212-854-2624, jfagan@law.columbia.edu, http://www.law.columbia.edu/fac/Jeffrey_Fagan
        
# Earlier study released in June 2015
# https://assets.documentcloud.org/documents/2158964/full-boston-police-analysis-on-race-and-ethnicity.pdf
        
# Key point in earlier report is that these FIO stops are more comprehensive than a Terry Stop
# which gives police the power to stop and detain citizens if they have 
# reasonable suspicion to believe that “crime is afoot.” In addition
# to stops based on reasonable suspicion, FIO stops can 
# also include non-contact observations and direct encounters.
        
# In 2011, a new FIO Rule adds an encounter to the list of documentable 
# interactions, to ensure that those interactions that do not rise to a 
# Terry Stop are properly documented. Also, records of a particular 
# individiual would be deleted if the person does not appear in the
# database for five years. These changes were  mentioned in Oct 2014 at:
# http://bpdnews.com/news/2014/10/8/boston-police-commissioner-announces-field-interrogation-and-observation-fio-study-results
        
# This AirSafe.com complaint database and do some basic exploratory analysis of
# the data that will focus on recreating some of the measures used 
# in the earlier study, as well as creating a set of measures that answer
# some basic questions that would not necessarily be the kinds of questions
# asked by the academics who authored the earlier study, but may be the 
# kinds of questions asked by local residents who may be concerned
# about the level of BPD FIO activity in specific areas of the city of
# Boston.  
        
# The latest FIO data was made available in a CSV file, so the 
# pre-processing step included downloading the file for further analysis.
# No redactions or editing was performed prior to the analysis below.
        
# The first step is to install new packages that will be needed for the analysis.
        
options(repos = c(CRAN = "http://cran.rstudio.com"))
if("e1071" %in% rownames(installed.packages()) == FALSE) 
{install.packages("e1071")}
library(e1071)
        
# Note that raw data was pre-processed to exclude non-English content
fio.raw = read.csv("Boston_Police_Department_FIO.csv")
fio=fio.raw
        
# Changing column names to capitalize first letter only
simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
                      sep="", collapse=" ")
        }
        
colnames(fio) = tolower(colnames(fio))
colnames(fio) = lapply(colnames(fio),simpleCap)

# ====== FUNCTIONS ===========
# The following functions are defined for this analysis
        
# whales function - Create a summary data table showing how many times a 
# particular Subject (uniquely identified entitity) is associated with a
# collection of records, and also includes the contribution to the cumulative 
# distribution function for each individual.
whales = function(df,df.vname){
        # To get the number of of times a Subject:
        # 1. Get a sorted table showing how many times each Subject shows up
        # Each row of the resulting data frame represents contribution of one Subject
        yy = which(colnames(df)==df.vname)
        uni.df = as.data.frame(sort(table(df[,yy]), descending = TRUE))
        colnames(uni.df) = "Contribution"
        uni.df$Donor = rownames(uni.df)
        uni.df$Donor = stripper(uni.df$Donor) # Removing leading and trailing spaces
        uni.df$Contribution = as.numeric(as.character(uni.df$Contribution))
        uni.df$Cum_donors = 1:nrow(uni.df) # Recall, each row is for one Subject
        uni.df$Cum_records = cumsum(uni.df$Contribution)
        uni.df$Frac_records = round(100*(uni.df$Contribution/sum(uni.df$Contribution)), digits=3)
        # # Last column added answers question of how many Occurrences represented by 
        # # the subset of Subjects with 'X' or more occurrences to their credit
                uni.df$Cdf_donors = round(100*(uni.df$Cum_donors/nrow(uni.df)), digits=3)
                uni.df$Cdf_records = round(100*(uni.df$Cum_records/sum(uni.df$Contribution)), digits=3)
                return(uni.df)
        }
        
# whale.table function: Takes the output of the whales function and creats a data table
# that categorizes Subjects by how many times they are represented in the FIO data table.
# Each record summaizes the number of Subjects who submitted X records, and also includes
# the contribution to the cumulative distribution function for each category.
whale.table = function(uni.df){
        # Given a table showing the number of times a unique ID (Subject) is in the 
        # database, create a table summarizing how many Subjects had 'X' Occurrences
        uni.df.table = as.data.frame(table(uni.df[,1]))
        colnames(uni.df.table) = c("Contribution","Donors") 
        # Pop is the number of times a unique subject had X Occurrences
        uni.df.table$Contribution = as.numeric(as.character(uni.df.table$Contribution))
        uni.df.table$Donors = as.numeric(as.character(uni.df.table$Donors))
        uni.df.table$Cum_donors = cumsum(uni.df.table$Donors)
        uni.df.table$Frac_all_donors = 100*(uni.df.table$Donors/sum(uni.df.table$Donors))
        # # Last column added answers question of how many Occurrences represented by 
        # # the subset of Subjects with 'X' or more occurrences to their credit
        uni.df.table$Records_at_level = (as.numeric(uni.df.table$Contribution))*(as.numeric(uni.df.table$Donors)) # Number of records for that bin size
        uni.df.table$Cum_records = cumsum(uni.df.table$Records_at_level) # Cumulative number of FIOs
        uni.df.table$Frac_all_records = 100*(uni.df.table$Records_at_level/sum(uni.df.table$Records_at_level))
        uni.df.table$Cdf_donors = cumsum(uni.df.table$Frac_all_donors)
        uni.df.table$Cdf_records = cumsum(uni.df.table$Frac_all_records)
        return(uni.df.table)
        }
# FUNCTION FOR REMOVING LEADING AND TRAILING SPACES AND NON-PRINTING CHARACTERS
        
# Function 'stripper' definition
# This function removes leading and trailing spaces from a vector.
# The first step is to ensure the vector 'x' is character type by using 'as.character()' function.
# The next step is to remove the leading space characters, including leading tab, 
#       newline, vertical tab, form feed, carriage return, and space:
# 
#      - x = sub("^[[:space:]]+", "", x) 
#
# Trailing spaces can be removed in a simlar fashion:
#      - str = sub("[[:space:]]+$", "", str)
#      
# Notes:
#      - The "$" character is the end of string character, "^"is beginning of string character
#      - Note that without the "+", only the first instance would be removed
        
stripper <- function(x){
        x = as.character(x)
        x = sub("[[:space:]]+$", "", x) # Remove leading space characters
        x = sub("^[[:space:]]+", "", x) # Remove trailing space characters
        return(x)
        }
        
# ==== END OF FUNCTIONS ====
# Overview of database
# str(fio)
        
# There are 44 variables (column names) for the raw data. 
# A data dictionary describing 43 of these variables is available 
# at http://airsafe.com/analyze/fio_data_dictionary.pdf
# One of these variables VEH_MODEL was not in the data dictionary,
# but from the context of the other variables, this one appears to provide
# information on the model of a motor vehicle.
        
# Note: A review of the BPD raw data revealed that the codes for the
# variable RACE_ID are as follows: 
# 0 - No Data entered; 1 - Asian or pacific islander; 2 - Black; 3 - Hispanic; 
# 4 - White; 5 - American Indian/Alaska native; 6 - Middle East/East indian
# There is also an Unknown category with no code. For purposes of this analysis,
# Unknown and No Data Entered are defined as NA, and Asian, Middle East, and 
# American Indian/Alaska native were combined. This results in five categories:
# Black (code 2), White (Code 4), Hispanic (Code 3) , Other (Codes 1,5, and 6).
# NA values (Unknown plus Code 0) were exluded from any analysis
# of possible racial descrepancies.
        
# 40. Race_desc - Mutually exclusive description of racial or ethnic group, eight possible
# values, all associated with a specific Race_id valuem,including 'NO DATA ENTERED' and 'UNKNOWN' 
# 41. Fio_date_corrected
# 42. Age_at_fio_corrected
# 43. Street_id
# 44. City: 21 neighborhood designations, plus an identifier for no data and other
        
# Unique number of values for each variable
        
# Find which variables are unique identifiers (number of unique
# values equal to the number of records)
        
# Identifying if any variables can be used as a unique identifier,
# specifically having a number of unique values equal 
# to the number of data frame rows.
        
# Identifying columns with nrow(fio) unique values
ndx = which(lapply(lapply(fio,unique),length)==nrow(fio)) 
        
if (length(ndx)==0){
        print("No variable can serve as a unique identifier.")
        }
        
if (length(ndx)==1)paste("Variable '", colnames(fio)[ndx],
        "' can serve as a unique identifier.", sep = "")
        
if (length(ndx)>1){
        print("The following variables that can serve as unique identifiers:")
        colnames(fio)[ndx]
        }
        
# The variables 'Seq_num' and 'Fio_id' are both unique identifiers, and from the 
# data dictionary, 'Seq_num' is used internally by the BPD and 'Fio_id' is the 
# identifier for a specific record, so 'Seq_num' was considered redundant and
# deleted from further analysis.
        
fio$Seq_num = NULL
        
# Ensure remaining unique identifier is of type character
fio$Fio_id = as.character(fio$Fio_id)
        
# From data dictionary and inital analysis, other redundant (one-to-one relationship)
# variables were identified by first using the following combination of functions:
# lapply(apply(fio,2,unique),length)
        
# Redundant variable groups typically had one or more description variables and an 
# ID value for each unique description, or had at least one redundant variable:
# 'Dist' and 'Dist_id'
# 'Supervisor' and 'Supervisor_id' (Almost one-to-one)
# 'Officer' and 'Officer_id' 
# 'Off_dist' and 'Off_dist_id' 
# 'fio_date' and 'fio$Fio_time' (all comparable entries identical)
# 'Description', Race_id', and 'Race_desc'
fio$Dist = NULL
fio$Officer = NULL
fio$Off_dist_id = NULL
fio$Fio_time = NULL
fio$Description = NULL
fio$Race_desc = NULL
        
# A number of other variables were eliminated from further analysis 
# if they did not add potentially useful information for this particular study
        
# 'Active_id' was identical for every entry
fio$Active_id = NULL
        
# 'Fio_date' is the preliminary date, and 'Fio_date_corrected' 
# is more authoritative, and the only one relevant to this analysis, 
# so all other date variables were eliminated.
fio$Fio_date = NULL
fio$First_inserttime = NULL
fio$Last_updatetime = NULL
fio$Sup_entrydate = NULL
        
# Who enter the data and who last updated the database was not central to the analysis, 
# so these too was eliminated
fio$Last_updateby = NULL
fio$Enteredby = NULL
        
# The inputs for the remaining variables could be either characters, numbers, or dates
date.vars = c("Fio_date_corrected")
num.vars = c("Age_at_fio_corrected", "Veh_year_num")
num.vars.ndx = which(colnames(fio) %in% num.vars) # Index of date or numerical variables
non.char.vars = union(date.vars,num.vars) # Names of non-character variables
non.char.ndx = which(colnames(fio) %in% non.char.vars) # Index of date or numerical variables
char.vars = colnames(fio)[-non.char.ndx] # Character variables
        
# The following provides the index of the character variables (non-date and non-numerical)
char.vars.ndx = which(colnames(fio) %in% char.vars) # Index of character variables
        
# Ensure character variables are of type character
        
# First, make everything as.character
fio = as.data.frame(apply(fio, 2, as.character)) 
        
# Strip out any leading or trailing spaces
fio = as.data.frame(apply(fio, 2, stripper)) 
        
# Now make sure non-numeric variables (which include datas)
# are made into character variables
fio[,-non.char.ndx] = apply(fio[,-non.char.ndx],2, as.character)
        
# Ensure each numeric variable is of type numeric
fio[,num.vars.ndx] = apply(fio[,num.vars.ndx],2, as.numeric)
# apply(fio,2,typeof)
        
# Change Timestamp to as.POSIXlt which has elements in a list
fio$Date = as.POSIXlt(fio$Fio_date_corrected, format="%m/%d/%y %H:%M")
fio$Year = fio$Date$year + 1900 # Years indexed from 1900
fio$Month = fio$Date$mon + 1 # Months indexed from zero
# Convert months from character to numeric 
fio$Month = as.numeric(as.character(fio$Month))
# Convert to month
fio$Month = month.abb[fio$Month]
fio$Day = fio$Date$mday
fio$Weekday = weekdays(as.Date(fio$Date), abbreviate = TRUE)
        
        # Make months factors and order them as they are in a calendar
        fio$Month = factor(fio$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                 "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)
        
        # Make days into factors and order them as they are in a calendar
        fio$Weekday = factor(fio$Weekday,levels=c("Sun","Mon","Tue",
                                                        "Wed","Thu","Fri","Sat"), ordered=TRUE)
        
# Converting the variable "Date" into Date format from as.POSIXlt format
# So the dates can be ranked by number of FIO reports
fio$Date = as.Date(fio$Date)


# =============
# Inserting NA values where appropriate
# =============
# Numerous variables have missing variables. From an inspetion of the raw data, the following 
# variables had the following kinds of missing data, or categories of data that will be 
# treated as NA data. The variables and the data categories for each variable, are below:

fio$Sex[which(fio$Sex=="UNKNOWN")] = NA # Sex - UNKNOWN 
fio$Sex[which(fio$Location=="")] = NA # Location - blank entries
fio$Clothing[which(fio$Clothing=="")] = NA # Clothing - blank entries
fio$Complexion[which(fio$Complexion=="NO DATA ENTERED")] = NA # Complexion - NO DATA ENTERED
fio$Priors[which(fio$Priors=="")] = NA # Priors - blank entries
fio$Search[which(fio$Search=="")] = NA # Search - blank entries
fio$Basis[which(fio$Basis=="")] = NA # Basis - blank entries
fio$Stop_reasons[which(fio$Stop_reasons=="")] = NA # Stop_reasons - blank entries
fio$Outcome[which(fio$Outcome=="")] = NA # Outcome - blank entriesc

# Veh_make - "N/A" or "NO DATA ENTERED"
fio$Veh_make[which(fio$Veh_make=="N/A" |fio$Veh_make=="NO DATA ENTERED") ] = NA

fio$Veh_year_num[which(fio$Veh_year_num==0)] = NA # Veh_year_num - 0

# Veh_color - N/A or "NO DATA ENTERED"
fio$Veh_color[which(fio$Veh_color=="N/A" |fio$Veh_color=="NO DATA ENTERED") ] = NA

fio$Veh_model[which(fio$Veh_model=="")] = NA # Veh_model - blank entries
fio$Veh_occupant[which(fio$Veh_occupant=="")] = NA # Veh_occupant -  blank entries

# Veh_state - "NO DATA ENTERED" or "OTHER"
fio$Veh_state[which(fio$Veh_state=="OTHER" |fio$Veh_state=="NO DATA ENTERED") ] = NA

# Supervisor_id - NA (no action needed)

fio$Off_dist[which(fio$Off_dist=="")] = NA # Off_dist_id - 9999

fio$Ethnicity[which(fio$Ethnicity=="")] = NA # Ethnicity - NA 
# (Also, lack of consistency in definitions)

fio$Race_id[which(fio$Race_id=="9999")] = NA # Race_id - 9999
        
# Last added variable is a modified BPD race code
# Note: A review of the BPD raw data revealed that the codes are as follows: 
# 0 - No Data entered; 1 - Asian or pacific islander; 2 - Black; 3 - Hispanic; 
# 4 - White; 5 - American Indian/Alaska native; 6 - Middle East/East indian
# There is also an Unknown category with no code. For purposes of this analysis,
# Unknown and No Data Entered are defined as NA, and Asian, Middle East, and 
# American Indian/Alaska native were combined. This results in five categories:
# Black (code 2), White (Code 4), Hispanic (Code 3) , Other (Codes 1,5, and 6).
# NA values (Unknown plus Code 0) were exluded from any analysis
# of possible racial descrepancies.

# Create racial identification codes based on those provided by the BPD
# and order them by how likely they are to occur in the FIO database.
# Categories with smaller populations may be combined in the following analysis.

# Define a race.code vector which creates four racial categories plus an NA category
race.code = fio$Race_id # Start with the codes assigned by the BPD
race.code[which(fio$Race_id =="0")] = NA
race.code[which(fio$Race_id =="2")] = "Black"
race.code[which(fio$Race_id =="3")] = "Hispanic"
race.code[which(fio$Race_id =="4")] = "White"

# Three codes which combined accounted for just over 1% of FIO reports were combined.
race.code[which(fio$Race_id =="1" | fio$Race_id =="5" | fio$Race_id =="6")] = "Other"
# Order the racial category by number of FIO reports with that category
race.code = ordered(race.code, levels=c("Black","White","Hispanic","Other"))
fio$Race_code = race.code # Add this race code to the fio data frame

# =========== PHASE 1: Exploratory data analysis =======
# On June, 15 2015, the following report: "An Analysis of Race and Ethnicity Patterns in Boston 
# Police Department (BPD) Field Interrogation, Observation, Frisk, and/or Search Reports," 
# was published by Jeffrey Fagan, Anthony A. Braga, Rod K. Brunson, and April Pattavina.
# This report, conducted at the request of the Boston Police Department and the 
# American Civil Liberties Union (ACLU) of Massachusetts, investigated possible racial disparaties
# in Boston Police Department Field Interrogation, Observation, Frisk, and/or 
# Search practices (informally known as FIO reports). The report covered the years 2007-2010
# and included 204,739 reports, and among other things, the previous study highlighted the 
# following observations:
# - 67.5% of subjects only experienced one FIO encounter
# - about 5% of the subjects accounted for more than 40% of the reports
# - Black subjects were 12% more likely to be frisked

# FIO distribution by Race identification for 2011-April 2015
race.colors = c("dodgerblue", "azure", "lightblue", "coral")
table.dat = sort(table(fio$Race_code),decreasing=TRUE)
barplot(table.dat,
main = "FIO reports by Race: 2011 - April 2015",
        xlab = "Race identifier",
        ylab = "FIO reports",
        col = race.colors)
legend(3,60000, rownames(table.dat), horiz = FALSE, 
       cex = 1, fill = race.colors)


# Distribution of FIO per day for 2011-April 2015 (summary and plot)

superdates = whales(fio,"Date") # Distribution of number of FIO reports for each day
summary(date.contrib$Contribution) # Summary of number of FIO reports for each day

barplot(table(superdates$Contribution),
        main = "Distribution of daily FIO report submissions (2011 - April 2015)",
        xlab = "Submissions in a day", cex.names = 0.7,
        ylab = "Number of days",
        col = "dodgerblue")

# FIO distribution by weekday for 2011-April 2015
barplot(table(fio$Weekday),
        main = "FIO reports by weekday (2011 - April 2015)",
        xlab = "Day of the week",
        ylab = "FIO reports",
        col = "dodgerblue")

# FIO distribution by weekday and year for 2011-2014
# 2015 was excluded because only four months of data were available
table.dat = table(fio$Year[fio$Year<2015],fio$Weekday[fio$Year<2015])
barplot(table.dat,
        main = "FIO reports by weekday and year (2011-2014)",
        xlab = "Weekday",
        ylab = "FIO reports",
        col = c("dodgerblue", "azure", "darkgrey", "lightblue"),
        cex.names = 0.7)
        
# Legend placed separately
legend(2.5,1, rownames(table.dat), horiz = TRUE, 
       cex = 0.6, fill = c("dodgerblue", "azure", "darkgrey", "lightblue"))

# FIO distribution by month for 2011-2014
# 2015 was excluded because only four months of data were available
barplot(table(fio$Month[fio$Year<2015]),
        main = "FIO reports by month (2011-2014)",
        xlab = "Month", ylab = "FIO reports",
        col = "dodgerblue",cex.names = 0.7)
        
table.dat = table(fio$Year[fio$Year<2015],fio$Month[fio$Year<2015])
barplot(table.dat,
        main = "FIO reports by month and year (2011-2014)",
        xlab = "Month",
        ylab = "FIO reports",
        col = c("dodgerblue", "azure", "darkgrey", "lightblue"),
        cex.names = 0.7)
        
# Legend placed separately
legend(2.5,1, rownames(table.dat), horiz = TRUE, 
       cex = 0.6,
       fill = c("dodgerblue", "azure", "darkgrey", "lightblue"))
        
# ============== Heat map for month and weekday ====================
# The following will depict which combination  of days of the week and months
# of the year have the greatest concentration of FIO reports
# dev.off() # Start by resetting device settings

if("stats" %in% rownames(installed.packages()) == FALSE) 
{install.packages("stats")}
library(stats)
# Heat maps will use a color pallette that goes from white for lowest to dark blue for the highest value
palette = colorRampPalette(c('#ffffff','#0000ff'))(64)
# Combination #1: Months vs. Weekday for 2011-2014

# Table of FIO reports by month of the year and day of the week
table(fio$Month[fio$Year<2015],fio$Weekday[fio$Year<2015])

# Test to see if distribution by day has equal probability (it does not)
chisq.test(table(fio$Month[fio$Year<2015],fio$Weekday[fio$Year<2015]))

# Create a heat map for three situations:
# 1. No scaling - Darkest cells represent higher numbers of FIO reports      
#       for a particular combination of month and weekday.
# 2. Scaled by month - Darker columns highlight the weekdays with consistently
#       higher numbers of FIO reports compared to other days of the week.
# 3. Scaled by weekday - Darker rows highlight the months with consistently
#       higher numbers of FIO reports compared to other months of the year.

#####Heat map 1: Showing month and day combinations with the most FIO reports #####
# There are three ways to display this heat map. In the first option, 
# the darkest cell corresponds to the cell (combination of month and
# day of the week) with the most FIO reports

heatmap(table(fio$Month[fio$Year<2015], fio$Weekday[fio$Year<2015]),
        Rowv=NA, Colv=NA, revC=TRUE, 
        scale="none", col = palette,  margins=c(7,7), 
        main="Heat map 1: FIO reports intensity")
        
#####Heat map 2: Showing weekdays with the most FIO reports #####
# Another way to illustrate the same table of values is to scale the 
# heat map by the row values (months). By doing so, in each row, the 
# darkest cell would correspond to the day of the week with the most 
# laser reports for that month. This means that a column that is 
# consistently darker blue would correspond to a day of the week that 
# is more likely to have FIO reports.
        
heatmap(table(fio$Month[fio$Year<2015], fio$Weekday[fio$Year<2015]),
        Rowv=NA, Colv=NA, revC=TRUE, 
        scale="row", col = palette,  margins=c(7,7), las = 2,
        main="Heat map 2: FIO reports scaled by month")
        
#####Heat map 3: Showing months with the most FIO reports #####
# By scaling the heat map by day of the week (column) instead of by month would serve  
# to illustrate the months of the year that have consistently higher levels of  
# FIO reports. 
        
heatmap(table(fio$Month[fio$Year<2015], fio$Weekday[fio$Year<2015]),
        Rowv=NA, Colv=NA, revC=TRUE, 
        scale="column", col = palette,  margins=c(7,7), las = 2,
        main="Heat map 3: FIO reports scaled by day of the week")

# # ============== Check for NA values ====================
# 
# xx = NULL
# for (i in 1:ncol(fio)) {
#       length(which(is.na(fio[,i])))
#         yy = c(colnames(fio)[i],as.character(length(which(is.na(fio[,i])))))
#         if (yy[2]!="0") xx = c(xx,yy)
# }
# xx
# # ========================
# 
# ==== WHALE HUNTING: Finding the top performers ====
# In the following, key top performing variables will be reviewed, where
# Top performance is defined as the values of the variables in 
# the top 1% and top 20% with respect to how many times that value is in
# a FIO record.
# Variables that have specific identifiers that may show up in multiple
# FIO records include Officer_id, and Street_id, both of which have over
# 1,000 unique vlaues.
# Cumulative distribution of number of times an officer submitted FIO reports
        
supercops = whales(fio,"Officer_id")
        
# Ranking of BPD with the most FIO reports
rankcops = whales(fio,"Officer_id")
rankcops = rankcops[order(-rankcops[,"Contribution"]),1:2]
rownames(rankcops) = NULL
colnames(rankcops) = c("Reports","Officer ID")
# print("Top 15 BPD officers ranked by FIO reports")
print.data.frame(rankcops[1:15,2:1])
        
superstreets = whales(fio,"Street_id")
        
# Ranking of street ID locations with the most FIO reports
rankstreets = whales(fio,"Street_id")
rankstreets = rankstreets[order(-rankstreets[,"Contribution"]),1:2]
rownames(rankstreets) = NULL
colnames(rankstreets) = c("Reports","Street ID")
# print("Top 15 dates ranked by FIO reports")
print.data.frame(rankstreets[1:15,2:1])
        
# Ranking of street ID locations with the most FIO reports
rankstreets = whales(fio,"Street_id")
rankstreets = rankstreets[order(-rankstreets[,"Contribution"]),1:2]
rownames(rankstreets) = NULL
colnames(rankstreets) = c("Reports","Street ID")
# print("Top 15 dates ranked by FIO reports")
print.data.frame(rankstreets[1:15,2:1])
        
# Ranking of street locations locations with the most FIO reports
rankaddress = whales(fio,"Location")
rankaddress = rankaddress[order(-rankaddress[,"Contribution"]),1:2]
rownames(rankaddress) = NULL
colnames(rankaddress) = c("Reports","Location")
# print("Top 15 dates ranked by FIO reports")
print.data.frame(rankaddress[1:15,2:1])


# Ranking of street locations with the most FIO reports
rankaddress = whales(fio,"Location")
rankaddress = rankaddress[order(-rankaddress[,"Contribution"]),1:2]
rownames(rankaddress) = NULL
colnames(rankaddress) = c("Reports","Location")
# print("Top 15 dates ranked by FIO reports")
print.data.frame(rankaddress[1:15,2:1])

superages = whales(fio,"Age_at_fio_corrected")
# Ranking of ages with the most FIO reports
rankage = whales(fio,"Age_at_fio_corrected")
rankage = rankage[order(-rankage[,"Contribution"]),1:2]
rownames(rankage) = NULL
colnames(rankage) = c("Reports","Age")
# print("Top 15 ages by FIO reports")
print.data.frame(rankage[1:15,2:1])

# Identifying reports for ages from 12 to 65
# First get ages in range
age.range = fio$Age_at_fio_corrected > 11 & fio$Age_at_fio_corrected <66
with.race.id = !is.na(race.code)
age.race.ndx = age.range & with.race.id 
# Now get records with an identified race
table(age.race.ndx)
age.test = fio$Age_at_fio_corrected[age.race.ndx]


# Histogram of reports by age from 12-65
barplot(table(age.test),
        main = "FIO reports by age (12-65): 2011 - April 2015",
        xlab = "Age",
        ylab = "FIO reports",
        col = "dodgerblue")

# Histogram of reports by age and race from 12-65
table.dat = table(fio$Race_code[age.race.ndx],fio$Age_at_fio_corrected[age.race.ndx])
barplot(table.dat,
        main = "FIO reports by age (12-65) and race: 2011 - April 2015",
        xlab = "Age (range 12-65)",
        ylab = "FIO reports",
        col = c("dodgerblue", "azure", "lightblue", "coral"),
        cex.names = 0.7)
legend(40,6000, rownames(table.dat), horiz = FALSE, 
       cex = 1, fill = c("dodgerblue", "azure", "lightblue", "coral"))

# ============
# Find population of performers with the fewest number of FIO reports who  
# were repsonsible for the first 1% of occurrences
# The goal is the find the first one that takes the cumulative number of 
# FIO reports over 1% of total fios reports.
        
# WHALE HUNTING: Top cops and lower performers

# ================= LOWER PERFORMERS ===========================
thresh1 = min(which(supercops$Cdf_records>1)) # minimum number of Subjects to account for 1%+ of FIOs
thresh1.ndx = 1:thresh1
        
paste("A total of ", thresh1, " officers, representing the bottom ", 
        format(100*length(thresh1.ndx)/nrow(supercops), digits=3),
      "% of the ",
      format(nrow(supercops),big.mark = ","), " officers who submitted FIO reports during the study period,",
      " were responsible for ", format(supercops$Cdf_records[length(thresh1.ndx)], digits=3),
        "% of the total FIO reports. These officers filed an average of ",
        format(mean(supercops$Contribution[thresh1.ndx]), digits=3), " FIO reports during this period.",sep="")
        
        
# The top quintile of performers are those officers who were in the top 20% with
# respect to the number of FIO reports submitted during the study period.
        
# If 20% of the number of officers is not a whole number, this value is
# rounded up to the next whole number
top20 = ceiling(0.20*nrow(supercops))
top20.ndx = (nrow(supercops)-(top20-1)):nrow(supercops) 
        
paste("A total of ", length(top20.ndx), " officers, representing the top ",
      format(100*length(top20.ndx)/nrow(supercops), digits=4), "% of the ",
      format(nrow(supercops),big.mark = ","), " officers who submitted FIO reports during the study period,",
      " were responsible for ", format(100- supercops$Cdf_records[top20.ndx[1]-1], digits=4),
      "% of the total FIO reports. These officers filed an average of ",
      format(mean(supercops$Contribution[top20.ndx]), big.mark = ",", digits = 4),
      " FIO reports during this period.",sep="")
        
# Now lets see how the top 1% of officers performed, where the number of officers
# is determined by taking 1% of the number of officers who filed at 
# least one FIO.
        
# If 1% of the number of officers is not a whole number, this value is
# rounded up to the next whole number.
        
top1 = ceiling(0.01*nrow(supercops))
top1.ndx = (nrow(supercops)-(top1-1)):nrow(supercops) 
        
paste("A total of ", length(top1.ndx), " officers, representing the top ",
      format(100*ceiling(nrow(supercops)*0.01)/nrow(supercops),digits=3),
      "% of the ", format(nrow(supercops),big.mark = ","), " officers who submitted FIO reports during the study period, were responsible for ",
      format(sum(supercops$Contribution[top1.ndx]), big.mark = ","), 
      " or ", format(100*sum(supercops$Contribution[top1.ndx]/nrow(fio)), digits= 3),
      "% of all FIO reports. These officers filed an average of ",
      format(mean(supercops$Contribution[top1.ndx]), big.mark = ",", digits = 4), 
      " FIO reports during this period.",sep="")
        
# If one defines baseline officers that are neither low performance (individuals 
# with the lowest numbers of FIO reports who collectively account for 1% of all 
# FIO reports), or high performance (officers in the top quintile of officers 
# ranked by number of FIO reports), then baseline performers could be defined 
# as the remaining population.
        
extremes.ndx = c(thresh1.ndx,top20.ndx)
        
paste("A total of ", format(nrow(supercops)-length(extremes.ndx), big.mark = ","), " officers, representing ", 
      format(100*length(extremes.ndx)/nrow(supercops),digits=4),
      "% of all the officers who submitted FIO reports during the study period, were responsible for ",
      format(sum(supercops$Contribution[-extremes.ndx]), big.mark = ","), 
      " or ", format(100*sum(supercops$Contribution[-extremes.ndx]/nrow(fio)), digits= 4),
      "% of all FIO reports. These officers filed an average of ",
      format(mean(supercops$Contribution[-extremes.ndx]), big.mark = ",", digits = 4), 
      " FIO reports during this period.",sep="")
        
# WHALE HUNTING: Top Streets
# The top quintile of street locations are those were in the top 20% with
# respect to the number of FIO reports associated with those locations.
        
# If 20% of the number of street locations is not a whole number, this
# value is rounded up to the next whole number
top20.streets = ceiling(0.20*nrow(superstreets))
top20.streets.ndx = (nrow(superstreets)-(top20.streets-1)):nrow(superstreets) 
        
paste("A total of ", length(top20.streets.ndx), " streets, representing the top ", 
      format(100*length(top20.streets.ndx)/nrow(superstreets), digits=4), "% of the ",
      format(nrow(superstreets),big.mark = ","), 
      " street locations included in FIO reports during the study period,",
      " were responsible for ", format(100- superstreets$Cdf_records[top20.streets.ndx[1]-1], digits=4),
      "% of the total FIO reports. These street locations were included an average of ",
      format(mean(superstreets$Contribution[top20.streets.ndx]), big.mark = ",", digits = 4), 
      " FIO reports during this period.",sep="")

# Now lets see how the top 1% of street locations performed, where the number of locations
# is determined by taking 1% of the locations included in at least one FIO.

# If 1% of the locations is not a whole number, this value is
# rounded up to the next whole number
top1.streets = ceiling(0.01*nrow(superstreets))
top1.streets.ndx = (nrow(superstreets)-(top1.streets-1)):nrow(superstreets) 

paste("A total of ", length(top1.streets.ndx), " streets, representing the top ", 
      format(100*length(top1.streets.ndx)/nrow(superstreets), digits=4), "% of the ",
      format(nrow(superstreets),big.mark = ","), 
      " street locations included in FIO reports during the study period,",
      " were responsible for ", format(100- superstreets$Cdf_records[top1.streets.ndx[1]-1], digits=4),
      "% of the total FIO reports. These street locations each accounted for an average of ",
      format(mean(superstreets$Contribution[top1.streets.ndx]), big.mark = ",", digits = 4), 
      " FIO reports during this period.",sep="")

# WHALE HUNTING: Top Dates



# Ranking of dates with the most FIO reports
rankdates = whales(fio,"Date")
rankdates = rankdates[order(-rankdates[,"Contribution"]),1:2]
rownames(rankdates) = NULL
colnames(rankdates) = c("Reports","Date")
# print("Top 15 dates ranked by FIO reports")
print.data.frame(rankdates[1:15,2:1])

# This is done in a manner similar to what was done for BPD officers
# and Street_id locations
superdates = whales(fio,"Date")
top20.dates = ceiling(0.20*nrow(superdates))
top20.dates.ndx = (nrow(superdates)-(top20.dates-1)):nrow(superdates) 

paste("A total of ", length(top20.dates.ndx), " dates, representing the top ", 
      format(100*length(top20.dates.ndx)/nrow(superdates), digits=4), "% of the ",
      format(nrow(superdates),big.mark = ","), 
      " dates included in FIO reports during the study period,",
      " were responsible for ", format(100- superdates$Cdf_records[top20.dates.ndx[1]-1], digits=4),
      "% of the total FIO reports. These dates each accounted for an average of ",
      format(mean(superdates$Contribution[top20.dates.ndx]), big.mark = ",", digits = 4), 
      " FIO reports.",sep="")

# Now lets see how the top 1% of dates performed, where the number of dates
# is determined by taking 1% of the dates during the study period.

# If 1% of the dates is not a whole number, this value is
# rounded up to the next whole number
top1.dates = ceiling(0.01*nrow(superdates))
top1.dates.ndx = (nrow(superdates)-(top1.dates-1)):nrow(superdates) 

paste("A total of ", length(top1.dates.ndx), " dates, representing the top ", 
      format(100*length(top1.dates.ndx)/nrow(superdates), digits=3), "% of the ",
      format(nrow(superdates),big.mark = ","), 
      " dates during the study period,",
      " were responsible for ", format(100- superdates$Cdf_records[top1.dates.ndx[1]-1], digits=3),
      "% of the total FIO reports. Each of these dates accounted for an average of ",
      format(mean(superdates$Contribution[top1.dates.ndx]), big.mark = ",", digits = 4), 
      " FIO reports.",sep="")

# COMPARING HIGH PERFORMING OFFICERS WITH OTHER OFFICERS
# Before comparing ordered officer data with the FIO database, insure
# that none of the Officer_id values have leading or trailing spaces.

# The first comparison is comparing the percentage of officers and output
# for each group of officers
# Each vector is percent of officers and percent performance
low.per = c(length(thresh1.ndx)/nrow(supercops),sum(supercops$Contribution[thresh1.ndx])/nrow(fio))
# base.per = c((length(extremes.ndx)/nrow(supercops)),sum(supercops$Contribution[-extremes.ndx])/nrow(fio))
top.quin = c(length(top20.ndx)/nrow(supercops),sum(supercops$Contribution[top20.ndx])/nrow(fio))
first80 = 1 - top.quin
top.one = c(length(top1.ndx)/nrow(supercops),sum(supercops$Contribution[top1.ndx])/nrow(fio))

perform.comp = 100*(cbind(low.per,first80,top.quin, top.one))
rownames(perform.comp) = c("People", "Output")
colnames(perform.comp) = c("First 1% output", "Other BPD", "Top quintile", "Top 1%")

fio$Officer_id = stripper(fio$Officer_id)

# Will make a grouped bar plot from the peroform.comp data frame, which
# will visually depict that the top performers have a disproportionate effect
# on output (defined as FIO reports)

barplot(perform.comp, main="Officer performance by group",
        xlab="Officer group", ylab="Percentage", cex.names = 0.8,
        col=c("dodgerblue","azure"), beside=TRUE)

# Legend placed separately
legend(9.5,80, rownames(perform.comp), horiz = FALSE, 
       cex = 0.9,
       fill = c("dodgerblue","azure"))

# ================= TOP QUINTILE ===========================
# The top quntile can be specified in the fio data frame as follows:
fio.top20 = fio$Officer_id %in% supercops$Donor[top20.ndx]
fio.top20.ndx = which(fio$Officer_id %in% supercops$Donor[top20.ndx])

# Reports by weekday, and weekday and year
barplot(table(fio$Weekday[fio$Year<2015]),
        main = "FIO reports by weekday (2011-2014)",
        xlab = "Day of the week",
        ylab = "FIO reports",
        col = "dodgerblue")

table.dat = table((fio.top20[fio$Year<2015]),fio$Weekday[fio$Year<2015])
barplot(table.dat,
        main = "FIO reports by weekday and year (2011-2014)",
        xlab = "Weekday",
        ylab = "FIO reports",
        col = c("dodgerblue", "azure", "darkgrey", "lightblue"),
cex.names = 0.7)

# Legend placed separately
legend(2.5,1, c("Others","Top quintile"), horiz = TRUE, 
       cex = 0.7,
       fill = c("dodgerblue", "azure", "darkgrey", "lightblue"))

# Performance by month
barplot(table(fio.top20,fio$Month),
        main = "Relative FIO performance by month (2011-2015)",
        xlab = "Month",
        ylab = "FIO reports",
        col = c("dodgerblue", "azure"),
        cex.names = 0.7)
        
# Legend placed separately
legend(8.5,15000, c("Others","Top quintile"), horiz = TRUE, 
       cex = 0.7,
       fill = c("dodgerblue", "azure"))

# Racial comparisons

# Will now compare actual and expected FIOs by race and by key officer groups
# (Top quintile and all others) to see if racial disparities can be identified.
# It is assumed that in the FIOs the assignment of a racial category to a Subject
# is consistently and objecively determined by all officer groups. 
# Top quintile performers and the other officers.
# For example, if 10% of all FIO reports are of category X, then
# both top quintile performers and other other officers should have
# about 10% of their FIOs in those categores

# FIO reports by officer category and race identifier
barplot(table(fio.top20, race.code),
        main = "FIO reports by race and officer group",
        xlab = "Race identifier",ylab = "FIO reports",
        col = c("dodgerblue", "azure"))
# Legend placed separately
legend(3,70000, c("Top 20%", "Other BPD"), horiz = TRUE, 
       cex = 0.8,fill = c("azure", "dodgerblue"))
        
# == Top qunitile ==
# Top quintile FIOs by race
        
race.id = !is.na(race.code) # Vector of FIO reports with a racial ID
        
# race.id = !is.na(fio$Race_id) # Vector of FIO reports with a racial ID
        
# All with a race ID
all.by.race = as.vector(table(race.code)) # all by race
all.by.race.prob = round(all.by.race/sum(race.id),4) # This is the P(race category X)
        
# top20.by.race = as.vector(table(fio$Race_id[fio.top20 & race.id])) 
top20.by.race = as.vector(table(race.code[fio.top20 & race.id])) 
# P(Top quintile submits FIO report)
top20.by.race.prob = round(sum(fio.top20 & race.id)/sum(race.id),4) 
top20.by.race.expected = round(top20.by.race.prob*all.by.race,2)
        
# == Bottom four quintiles (bottom 80%) ==
# Other officers FIOs by race
# Other_BPD.by.race = as.vector(table(fio$Race_id[(!fio.top20 & race.id)])) 
bot80.by.race = as.vector(table(race.code[(!fio.top20 & race.id)])) 
# 1 - P(Top quintile submits FIO report)
bot80.by.race.prob = round(sum(!fio.top20 & race.id)/sum(race.id),4) 
bot80.by.race.expected = round(all.by.race*(sum(!fio.top20 & race.id)/sum(race.id)),2)
        
        
race.table = cbind(names(table(race.code)),
                   all.by.race, all.by.race.prob,top20.by.race,
                   top20.by.race.prob,top20.by.race.expected,
                   bot80.by.race,bot80.by.race.prob,bot80.by.race.expected)
        
colnames(race.table) = c("Race","Reports","Fraction","Top20", "Top20_prob", "Top20_expected",
                         "Other_BPD", "Other_BPD_prob", "Other_BPD_expected")
        
# Ensure that race.table is a data frame
race.table = as.data.frame(race.table)
# Ensure all of the numeric rows are numeric
race.table[,-1] = apply(race.table[,-1],2,as.character)
race.table[,-1] = apply(race.table[,-1],2,as.numeric)
# This analysis found 562 with NA (blank) value for race, and BPD included them in
# their analysis
        
# Now for a chi-square test to see if the  observed values differ greatley from expected
chival.x = c(race.table$Top20,race.table$Other_BPD)
chival.p = c(race.table$Top20_expected,race.table$Other_BPD_expected)
chisq.result = chisq.test(chival.x, p=chival.p, rescale.p = TRUE)
chisq.result
print("A chi-square test was run on the data that describes the racial breakdown of FIO reports of the top 20% of performers compared with the other 80% of officers. If there was no significant there would be little difference in the racial breakdown of their reports. However, this is apparently not the case, as will be show two ways. First, with a chi-square test and second using a visual comparison.")
        
paste("Of the ",format(nrow(fio),big.mark = ","), " FIO reports, ", format(sum(is.na(race.code)),big.mark = ","),
      " or ", format(100*sum(is.na(race.code))/nrow(fio), digits=3),  
      "% did not specify a specific racial category and were excluded from the chi-square test. ",
      "There were a total of eight categories corresponding to the two officer groups and the four racial groups (Black, White, Hispanic, and Other).", sep="")
        
paste("The chi-sqare test with ",chisq.result[[2]][[1]], 
      " degreess of freedom produced a value of ", format(chisq.result[[1]][[1]],digits=5),
      ". Given the number of degrees of freedom, the null hypothesis of there being no significant differece in the racial breakdowns of the FIO reports for these two groups of officers would be rejected at the 0.05 level for chi-square values above 14.067, and in this case the value was well above this level.",
      sep="")
        
paste("The second way to show this disparity in reporting is visually. Below is a grouped bar chart showing the likelihood that an FIO report involved a combinatino of a particular combination of racial group and BPD officer category.")
        
# This compares top performers and bottom performers with respect to 
# how much more or less likely that officer group would have an FIO 
# report involving a person from a particular race.
races = as.character(race.table$Race)
Allratio = race.table$Reports/race.table$Reports
Top20_ratio = race.table$Top20/race.table$Top20_expected
Other_BPD_ratio = race.table$Other_BPD/race.table$Other_BPD_expected
        
bpd.comp = as.matrix(rbind(Allratio,Top20_ratio,Other_BPD_ratio))
colnames(bpd.comp) = races
rownames(bpd.comp) = c("All BPD", "Top quintile", "Other BPD")
        
barplot(bpd.comp, main="Ratio of reporting rate and BPD average by race",
        xlab="Race identifier", ylab="Ratio",
        col=c("coral","azure","dodgerblue"), beside=TRUE)
        
# Legend placed separately
legend(10,1.6, rownames(bpd.comp), horiz = FALSE,
       cex = 0.8,
       fill = c("coral","azure","dodgerblue" ))
abline(h=1,col="darkblue")

# Now let's look at it another way. 
# If there were no racial disparities,
# The group performing X% of all the FIO reports would have been responsible for
# X% of each racial group. 
# First, add two columns to the race table to show percentage variance 
# from expected racial distribution
race.table$Top20_variance = 100*((race.table$Top20-race.table$Top20_expected)/race.table$Top20_expected)
race.table$Top20_variance = round(race.table$Top20_variance, digits = 1)
race.table$Other_BPD_variance = 100*((race.table$Other_BPD-race.table$Other_BPD_expected)/race.table$Other_BPD_expected)
race.table$Other_BPD_variance = round(race.table$Other_BPD_variance, digits = 1)

# The following table and graph show how many percentage
# points above or below the expected percentage. 

bpd.var = as.matrix(cbind(race.table$Top20_variance,race.table$Other_BPD_variance))
colnames(bpd.var) = c("Top Quintile", "Other BPD")
rownames(bpd.var) = races

barplot(bpd.var, main="FIO reporting racial variance by BPD group",
        xlab="BPD group", ylab="Percent variance", ylim = c(-20,60),
        col= c("dodgerblue", "azure", "lightblue", "coral"), beside=TRUE)

# Legend placed separately
legend(2,50, rownames(bpd.var), horiz = FALSE,
       cex = 0.8,
       fill = c("dodgerblue", "azure", "lightblue", "coral"))

# ========        
# Rough plot of where top quintile BPD work, 16,3,1,are the big players
barplot(table(fio.top20,fio$Off_dist) , cex.names = 0.4,col=c("dodgerblue","azure"))
        
# Vectors of probability of a particular combination of category of BPD officer
# and racial category. If there were no significant descrepancies between the 
# two BPD officer categories, (top quintile and all others), then the probability for 
# each combination of officer category and racical category should be close to the probabilies
# for the population as a whole.
        
# an FIO report given that 
# the FIO report both included an identifiable racical category and a prthat given that an FIO report includes an 
# identifiable racial category, that a particular officer 
# This assumes that the racial category assigned to a Subject is applied fairly and objectively.
# involved a particular racial category
        
plot.ecdf(supercops$Contribution,
          main = "Cumulative distribution of number of officers' FIO records",
          xlab="Number of FIO records",
          ylab="Cumulative probabilities")
        
# Saving the fio data frame as a CSV file for review by other researchers
write.csv(fio, file = "fio_airsafe.csv")
        
# === Recap of claims from ACLU Jan 15, 2016 letter ===

# Black by year and percent of total formulation used by ACLU
black.by.year = table(fio$Year[fio$Race_code=="Black"]) # Black by year (numerator)
total.by.year = table(fio$Year[fio.raw$RACE_ID != "0"]) # All by year excluding no "No data entered" code

format(100*black.by.year/total.by.year, digits = 3)



# ACLU used the above two formulations. AirSafe.com used the first 
# formulation (using the BDP code to identify "Black" coded records), but used the 
# following for the denominator:
table(fio$Year[race.id]) # All except unknown plus no data entered
