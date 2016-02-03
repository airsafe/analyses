# Laser strike encounter analysis using FAA data from 2010-2014
# Background analysis
# 2 February 2016
# First, ensure we have the packages we need


options(repos = c(CRAN = "http://cran.rstudio.com"))
if("downloader" %in% rownames(installed.packages()) == FALSE) 
{install.packages("downloader")}
library(downloader) 

# Data file was located at FAA page "Laser News, Laws, & Civil
        # Penalties" page at https://www.faa.gov/about/initiatives/lasers/laws/

# Original data is at https://www.faa.gov/about/initiatives/lasers/laws/laser_incidents_2010-2014.xls

# Converted to a CSV file and can be downloaded from AirSafe.com 
# url <- "http://www.airsafe.com/analyze/faa_laser_data.csv"
# filename <- "faa_laser_data.csv" 
# download(url, destfile=filename)


# Raw data included are all 50 states, the District of Columbia, and Puerto Rico, and several US territories.

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

# DATA CLEANING: Pre-processing corrections
# The raw data file from the FAA contained numerious cases of incorrect
# data with respect to location (airport, city, and state), including misspellings and capitatlization errors,
# as well as missing data. The events were manually reviewed to correct these errors when sufficient 
# information was contained in the rest of the record.

# Also, for consistency, airports were identifiedc using the three-character IATA codes when they 
# were available for an airport. Several airports were apparantly miscoded, and were corrected if the other
# infomration in the reccord supported such a change. For example, 29 reports for the city of Baltimore, MD
# used the 'BAL' IATA code, which is for Batman, Turkey, when 'BWI' was likely intended.
# If a VOR was used as the location, the nearest IATA airport was used
# Simialar likely errors inculude:
#       - 'GUA' instead of 'GUM' for GUAM, 
#       - 'VHP' instead of 'VPZ' for Valparasio, IN
#       - 'POM' instead of 'POC' for Pomona or Ontario  , California

# Missing airport codes that could not be determined, or events en route, were coded as 'UNKN'
# States and cities that could not be determined were also coded as 'UNKN'.
# Any airport codes in lower case were capitalized, 

# Searched for airport codes using:
#       - World Airport Codes - https://www.world-airport-codes.com/
#       - Nations online - http://www.nationsonline.org/oneworld/IATA_Codes/airport_code_list.htm
#       - Locations identifier search tool - https://nfdc.faa.gov/xwiki/bin/view/NFDC/Location+Identifiers+Search+Tool
#       - AirNav.com - https://www.airnav.com/airports/

# Altitudes: SFC (Surface) is altitude 0
# FLxxx, changed to xx,x00
# Heights above 100K assumed to have at least one extra zero
# For example, 120000 is assumed to be 12000 for an airliner, but 1200 for a 
# single piston engine small aircraft


# Reported Laser colors were standardized
# by making all inputs with multiple identified colors of the form Color1/Color2,
# with the colors listed alphabetically, insuring that the first letter in 
# a single word color identifier was capitalized, and correcting misspellings. 
# Example: Blue and Green, Green/Blue, Blue or green all become Blue/Green



# ==== INPUT ====
# Input raw laser data file
# Raw data included are all 50 states, the District of Columbia, 
# Puerto Rico, and several US territories.
laserhits = NULL
laserhits.raw = NULL
laserhits.raw = read.csv("faa_laser_data.csv")
laserhits=laserhits.raw


# STEP 1: Ensure that all of the variables are of type character
# while making sure none of these are treated as factors
laserhits = data.frame(lapply(laserhits,as.character), stringsAsFactors=FALSE)

# STEP 2: Remove all leading and trailing spaces from selected variables
# Remove leading and trailing space characters from selected variables.

# DATA CLEANING: Removing unnecessary non-printing characters
# Before evaluating laser encounters by city, airport, and state, steps must be taken to ensure uniformity
# of definitions. One way to do that is to eliminate unecessary leading and trailing space characters.
# In this case, a function (sripper) was created that could be applied to multiple location-related variables.

laserhits$Injury = stripper(laserhits$Injury) # Injury variable
laserhits$State = stripper(laserhits$State)
laserhits$City = stripper(laserhits$City)
laserhits$Airport = stripper(laserhits$Airport)
laserhits$Airport = stripper(laserhits$Altitude)

# STEP 3: (Data cleaning) Removal of unknown or blank location (City, State, or Airport) and time of day (Hour) from further analysis

good.loc = laserhits$City!="UNKN"  & laserhits$State!="UNKN" &  
        laserhits$Airport!="UNKN"  & laserhits$Hour!="UNKN" & 
        laserhits$City!=""  & laserhits$State!="" &  
        laserhits$Airport!=""  & laserhits$Hour!=""
laserhits=laserhits[good.loc,]

paste("A total of",nrow(laserhits.raw)-nrow(laserhits), "of the original",format(nrow(laserhits.raw), digits=5, big.mark = ","), 
      "records were eliminated from analysis due to unknown values for location (any combination of city, state, and airport), or time of occurrence.", sep=" ")


# DATA CLEANING: Date conversion
# Dates are in form 11/05/2015, must convert to a date format of yyyy-mm-dd
laserhits$Date = as.Date(laserhits$Date, "%m/%d/%Y")

# DATA CONVERSION: create new columns for year, month, and day
laserhits$Year = format(laserhits$Date, "%Y")
laserhits$Month = months(laserhits$Date, abbreviate = TRUE)
laserhits$Day = format(laserhits$Date, "%d")
laserhits$Weekday = weekdays(laserhits$Date, abbreviate = TRUE)

# DATA CONVERSION: Ensure variable Hour is a numeric value, and converter to 24 numeric levels

# Because the hour values were originally in a four-digit convfiguration before converting
#     spreadsheet to CSV and then data.frame, the times before 1000 hours are
#     fewer than four digits. Since this analysis is concerned only in the hour interval of a laser encounter,
#     will convert the times into a number from 0 to 23, then turn that into a factor.

laserhits$Hour = as.numeric(as.character(laserhits$Hour))
laserhits$Hour = floor(laserhits$Hour/100)

# DATA CONVERSION: Ordering days of the week and months of the year 

# Make months and days of the week factors and order them as they are in a calendar
laserhits$Month = factor(laserhits$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                                                  "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)


laserhits$Weekday = factor(laserhits$Weekday,levels=c("Sun","Mon","Tue",
                                                      "Wed","Thu","Fri","Sat"), ordered=TRUE)


# Change unknown laser colors to NA
laserhits$Laser_color[laserhits$Laser_color == "UNKN"] = NA

# Change unknown altitudes to NA
laserhits$Altitude[laserhits$Altitude == "UNKN"] = NA
laserhits$Altitude = as.numeric(laserhits$Altitude)
laserhits$Flight_level = 1000*floor(laserhits$Altitude/1000)


# Cleaned up data
write.csv(laserhits,file="laser_clean.csv")


# QUICK SUMMARY

paste("The raw data contained", format(nrow(laserhits.raw), digits=5, big.mark = ","), 
      "records, but", nrow(laserhits.raw)-nrow(laserhits), 
      "of these records were eliminated from analysis due to unknown values for location or time of occurrence, and the remaining",
      format(nrow(laserhits), big.mark=","), "were analyzed", 
      sep=" ")
      
paste("In the period from ", min(laserhits$Date), " to ", max(laserhits$Date),
      ", there were ", format(nrow(laserhits), digits=5, big.mark = ","),
      " reported encounters where a laser beam affected one or more aircraft at or near ",
      format(length(unique(laserhits$Airport)), big.mark = ","), 
      " unique airports or other locations.", sep="") 

date.range = as.numeric(as.Date(max(laserhits$Date))) - as.numeric(as.Date(min(laserhits$Date))) + 1

paste("During this period, there was an average of ",format(nrow(laserhits)/date.range, digits=3, big.mark = ","),
      " laser encounters per day, with as many as ", max(sort(table(laserhits$Date))), 
      " strikes in a single day. There were only ",
      date.range-length(table(laserhits$Date)), " days during this ",
      format(date.range, big.mark=","), "-day period with no reported laser strikes on aircraft in the United States", sep="")

paste("In other words, during this period, on any given day in the United States, there was a ",
      format((length(unique(laserhits$Date))/date.range)*100, digits=3, big.mark = ","),
      "% chance that at least one aicraft reported a potentially dangerious encounter with a laser beam.", sep="")

# Create a complete table and histogram of days with x-amount of strike events
#  by adding a vector of zero values equal to the number of days with no strikes

# EXPLORATORY DATA ANALYSIS: Distribution, histogram, and summary of the number of daily laser encounters

# table(daily.strikes)
daily.strikes=c(rep(0,date.range-length(table(laserhits$Date))),as.data.frame(table(laserhits$Date))[,2])
summary(daily.strikes)

hist(daily.strikes, main="Distribution of reported laser encounters in a day",
     xlab="Number of strikes in a day", include.lowest=TRUE, col="dodgerblue")

# Note: the following version was used for data from years 2010-2014
# hist(daily.strikes, main="Distribution of reported laser encounters in a day",
#      xlab="Number of strikes in a day", breaks = seq(-1,35,by=1), include.lowest=TRUE, col="dodgerblue")

# Note: if you get a plotting error suggesting the areas is 
# too large, from RStudio console, use the command "dev.off()"


# Distribution and  histogram of laser encounters by (UTC) hour of the day
table(laserhits$Hour)

# Note that adding 0.001 to the vector of values helps to align the axes with
# the hourly range of encounter
hist(laserhits$Hour+0.001, main="Distribution of reported laser encounters by hour of the day",
     xlab="Local time (UTC)", breaks = seq(-1,24,by=1), include.lowest=TRUE, 
     xlim=c(-1,24),col="dodgerblue")


# Distributionof reported laser encounters by day of the week
table(laserhits$Weekday)

# Chi-square test to see if reports uniformly distributed throughout the week
chisq.test(table(laserhits$Weekday))

# Histogram of reported laser encounters by day of the week
plot(laserhits$Weekday, main="Distribution of number of laser encounters by day of the week",
     ylab = "Reported encounters", xlab="Day of the week", col="dodgerblue")

# Distributionof reported laser encounters by month of the year
table(laserhits$Month)

# Chi-square test to see if reports uniformly distributed througout the year
chisq.test(table(laserhits$Month))

plot(laserhits$Month, main="Reported laser encounters by month of the year: 2010-2014",
     ylab = "Reported encounters", xlab="Month of the year", col="dodgerblue")


# Distributions of laser encounters by hour of the day, day of the week, and month of the year
# Will use a combination of tables and heat maps to identify factors that are associated with
# relatively high or low numbers of strikes.

# Heat maps will use a color pallette that goes from white for lowest to dark blue for the highest value
palette = colorRampPalette(c('#ffffff','#0000ff'))(64)

# Combination #1: Months vs. Weekday

# Table of reported laser encounters by month of the year and day of the week
table(laserhits$Month,laserhits$Weekday)

# Chi-square test to see if laser encounter reports uniformly distributed 
# for every combination of weekday and month 
chisq.test(table(laserhits$Month,laserhits$Weekday))

# Create a heat map (#1.0) for combination of day of the week and month with no scaling.
#       Scaling by row (the month)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       The darkest cells correspond indicate that this combination of month and 
#       day of the week had more laser encounters than lighter colored cells.

heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="none", col = palette, margins=c(4, 4), 
        main="Daily reported laser encounters",
        xlab="Day of the week", ylab="Month of the year",
        cexRow=0.9, cexCol=0.9)

# Create a heat map (#1.1) for combination of day of the week and month. Scaling by row (the month)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       In other words, for each row (month), it will rank the day of the week from most strikes (darkest)
#       to least strikes (lightest). If a particular day of the week is consistently more likely to have strikes, 
#       that entire column (day of the week) will be in general darker for most months. If that day of the week is 
#       less likely to have strikes, it will be relatively lighter for most months.

heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="row", col = palette, margins=c(4, 4), 
        main="Reported daily laser encounters scaled by month",
        xlab="Day of the week", ylab="Month of the year",
        cexRow=0.9, cexCol=0.9)



# Create a heat map (#1.2) for combination of weekday and month. Scaling by column (the weekday)
#       will highlight the months with a relatively high or low number of spikes.
#       In other words, for each column (weekday), it will rank the months from most strikes (darkest)
#       to least strikes (lightest). If a particular month is consistently more likely to have strikes,
#       that entire row (month) will be in general darker for most weekdays. If that month is 
#       less likely to have strikes, it will be relatively lighter for most weekdays.

heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(4, 4), 
        main="Reported monthly laser encounters scaled by day",
        xlab="Day of the week", ylab="Month of the year",
        cexRow=0.9, cexCol=0.9)

# Combination #2: Hours vs. Weekday

# Table of reported laser encounters by hour of the day and day of the week
table(laserhits$Hour,laserhits$Weekday)

# Create a heat map (#2.1) for combination of weekday and hour of the day. Scaling by row (hour of the day)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       In other words, for each row (hour), it will rank the days from most strikes (darkest)
#       to least strikes (lightest). If a particular day of the week is consistently more likely to have strikes, 
#       that entire column (day of the week) will be in general darker for most hours If that weekday is 
#       less likely to have strikes, it will be relatively lighter for most hours.

heatmap(table(laserhits$Hour, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="row", col = palette, margins=c(4, 4), 
        main="Daily reported laser encounters scaled by hour",
        xlab="Day of the week", ylab="Hour of the day",
        cexRow=0.9, cexCol=0.9)


# Create a heat map (#2.2) for combination of weekday and hour of the day. Scaling by column (the day of the week)
#       will highlight the hour with a relatively high or low number of spikes.
#       In other words, for each column (day of the week), it will rank the hours from most strikes (darkest)
#       to least strikes (lightest). If a particular hour of the day is consistently more likely to have
#       strikes, that entire row (hour of the day) will be in general darker for most days of the week If that hour 
#       is less likely to have strikes, it will be relatively lighter for most of the days of the week.


heatmap(table(laserhits$Hour, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(4, 4), 
        main ="Reported laser encounters by hour scaled by day",
        xlab="Day of the week", ylab="Hour of the day",
        cexRow=0.9, cexCol=0.9)

# Combination #3: Hours vs. Month

# Table of reported laser encounters by hour of the day and month of the year
table(laserhits$Hour,laserhits$Month)

# Create a heat map (#3.1) for combination of month and hour of the day. Scaling by row (hour of the day)
#       will highlight the month with a relatively high or low number of spikes.
#       In other words, for each row (hour), it will rank the month from most strikes (darkest)
#       to least strikes (lightest). If a particular month is consistently more likely to have strikes, 
#       that entire column (month) will be in general darker for most hours If that month is 
#       less likely to have strikes, it will be relatively lighter for most hours.

heatmap(table(laserhits$Hour, laserhits$Month),Rowv=NA, Colv=NA,revC=TRUE,
        scale="row", col = palette, margins=c(4, 4),
        main="Monthly reported laser encounters scaled by hour",
        xlab="Month of the year", ylab="Hour of the day",
        cexRow=0.9, cexCol=0.9)


# Create a heat map (#3.2) for combination of month and hour of the day. Scaling by column (the day of the week)
#       will highlight the hour with a relatively high or low number of spikes.
#       In other words, for each column (day of the week), it will rank the hours from most strikes (darkest)
#       to least strikes (lightest). If a particular hour of the day is consistently more likely to have
#       strikes, that entire row (hour of the day) will be in general darker for most of the days of the week. 
#       If that hour is less likely to have strikes, it will be relatively lighter for most days of the week.

heatmap(table(laserhits$Hour, laserhits$Month),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(4, 4), 
        main="Reported laser encounters by hour scaled by month",
        xlab="Month of the year", ylab="Hour of the day",
        cexRow=0.9, cexCol=0.9)


# EXPLORATORY DATA ANALYSIS: Laser encounters by state, city, and airport

# LASER ENCOUNTERS BY STATE
state.tot = as.data.frame(table(laserhits$State)) 
# Turns table output into a two-column data frame sorted alphabetically by state

# Rename the columns for clarity
colnames(state.tot) = c("State","Events") 

# Ensure state variabe is of type character
state.tot$State = as.character(state.tot$State)

# Alphabetical list of states with laser totals
paste("Alphabetical list of states with laser encounter totals")
paste(state.tot$State,"-",state.tot$Events,sep=" ")

# States ordered by laser totals
paste("States ordered by laser encounter totals")
ordered.state.tot = state.tot[order(state.tot$Events, decreasing=TRUE),] 
paste(ordered.state.tot$State,"-",ordered.state.tot$Events,sep=" ")

# LASER STRIKES BY CITY
# Using whales function
supercities = whales(laserhits,"City")
# Now reverse order so largest contributor comes first
supercities = supercities[order(-supercities$Contribution), ]
paste("Top 50 cities ordered by reported laser encounters")
paste(supercities$Donor[1:50],"-",
      supercities$Contribution[1:50],sep=" ")


# LASER STRIKES BY AIRPORT
# Using whales function
superport = whales(laserhits,"Airport")
# Now reverse order so largest contributor comes first
superport = superport[order(-superport$Contribution), ]
paste("Top 100 airports ordered by reported laser encounters")
paste(superport$Donor[1:100],"-",
      superport$Contribution[1:100],sep=" ")


# TOP AIRPORTS BY STATE

# Summary of laser hits by selected airports in a state. 
# A maxiumum of 15 airports are listed for each state

# Creating a list of lists with all the summarized airport information by state

us.list = NULL
paste("Summary of reported laser hits by selected airports in a state with a maximum of 15 airports listed per state.")
for(i in 1:nrow(state.tot)){
        # Laser events in state i
        
        state.list = laserhits[laserhits$State==state.tot[i,1],]
        
        # Orderd list of table of airport events in state i
        state.airports.ordered = as.data.frame(sort(table(state.list$Airport), decreasing=TRUE))
        
        # Printing out states and their airports (maximum of 15)
        cat("\n")
        state.airports.unique = length(unique(state.list$Airport)) # Unique airport names in state i
        if (state.airports.unique>15) cat(state.tot[i,1]," (Top 15 airports)")
        if (state.airports.unique<=15 & state.airports.unique>1) cat(state.tot[i,1]," (All reporting airports)")
        if(state.airports.unique==1) cat(state.tot[i,1]," (All reporting airports)\n") # Ensures propor formatting for one-airport states
        # cat(state.tot[i,1],"\n")
        print(state.airports.ordered[1:min(state.airports.unique,15),]) # Prints a maximum of 10 airports
        
        # Creating a list of list where each list element is the state summary
        us.list[[i]] = list(State=state.tot[i,1], Locations=state.airports.unique, Airports=state.airports.ordered)
}



# Now for the heat maps using US states outline
# Load the necessry packages

# For map-related data displays that used as inspiration
# the example at http://docs.ggplot2.org/0.9.3.1/map_data.html

options(repos = c(CRAN = "http://cran.rstudio.com"))
if("maps" %in% rownames(installed.packages()) == FALSE) 
{install.packages("maps")}
library(maps)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) 
{install.packages("ggplot2")}
library(ggplot2)

# Input summary flight operations statistics

# Converted to a CSV file and can be downloaded from AirSafe.com 
# url <- "http://www.airsafe.com/analyze/faa_opsnet_by_state.csv"
# filename <- "faa_opsnet_by_state.csv" 
# download(url, destfile=filename)

flight.data = NULL
flight.data.raw = NULL
flight.data.raw = read.csv("faa_opsnet_by_state.csv")
flight.data=flight.data.raw
# Ensure that State variable is character
flight.data$State = as.character(flight.data$State)

# Rename column "State" to "State.abb" to be consistent with earlier usage
names(flight.data)[names(flight.data)=="State"] = "State.abb"

# flight.data$State.abb = flight.data$State

# Ensure the other columns are numeric
# for (i in 2:length(colnames(flight.data))) {
#         flight.data[,i] = as.numeric(as.character(flight.data[,i]))
# }
# apply(flight.data,2,typeof)

# Add column for percentage of total flights
usa.tot.flights = flight.data$Total.Operations[nrow(flight.data)]
flight.data$Percent.ops = 100*flight.data$Total.Operations/usa.tot.flights


# Note that in ggplot2, the map function used here has the 48 states of the 
# continental US plus the district of columbia
# For the map displays used in this study, only those 48 states plus the
# District of Columbia will be used in the rest of this analysis'
# However, the FAA air traffic data used in this analysis, inlcudes 
# four other areas in addition to the 50 states and the District of 
# Columbia. All five will be added to facilitate any subsequent analysis.

extra.abb = c("DC", "GU", "MP", "PR", "VI")
extra.name = c("District of Columbia", "Guam", "Northern Mariana Is",
               "Puerto Rico", "Virgin Islands")

# Now append them to the R-provided list of 50 state names and abbreviations
every.abb = c(state.abb,extra.abb)
every.state = c(state.name,extra.name)

# But take out Alaska, Hawaii, Guam, Northern Mariana Islands, 
# Puerto Rico, and Virgin Islands
used.abb = setdiff(every.abb, c("AK", "HI", "GU", "MP", "PR", "VI"))
used.state = setdiff(every.state, c("Alaska", "Hawaii", "Guam", 
                                    "Northern Mariana Is","Puerto Rico",
                                    "Virgin Islands"))

# Now make the combination of state abbreviation and lowercase state name 
# a data frame with column names "State.abb" and "State" and row names 
# equal to the state name
state.combo = as.data.frame(cbind(used.abb,used.state))
rownames(state.combo) = used.state
colnames(state.combo) = c("State.abb","State")

# Ensure that both columns are characters
state.combo$State = as.character(state.combo$State)
state.combo$State.abb = as.character(state.combo$State.abb)

# Will now merge do two sets of merges. 
# The first will merge the data frames "state.tot" which has total laser events
# for each state with the "state.combo" which has the states that will be used in 
# the maps. This will use the variable "State" to merge into a new "state.combo"
state.combo = merge(state.tot,state.combo, by = "State")

# will now add a new column representing the percent of all laser events by state
state.combo$Percent.events = 100*state.combo$Events/nrow(laserhits)

# The second merge will involve "state.tot" and "flight.data",
# this time merging by the common variable "State.abb" into a new "state.combo"
state.combo = merge(flight.data,state.combo, by = "State.abb")

# This combined data frame "state.combo" now has the data for the 48 states plus DC
# An additional column will have a relative risk measue that is the ratio 
# of the percentage of laser events divided by the precentage of flight operarions
# A state with a proportion of strikes greater than the proportion of flight 
# operations (a ration greater than 1) would imply that this state has more strike
# events per flight operation.
state.combo$risk.ratio = state.combo$Percent.events/state.combo$Percent.ops




# The following gets the state boundary data into a data frame
# Regions happpen to be lower case state names
states <- map_data("state") 

# map_data is from ggplot2, creates data frame from information from the 
# 'maps' package and each region of a state is one of the polygons making up that state
# the data frame "states" has a column "region" which corresponds to the "States"
# column in the "state.combo" data frame, so now will create a "regions" column to
# allow a merging of "states" and "state.combo"
state.combo$region = state.combo$State

# ggplot2 likes their full state names (regions) in lower case, so the 
# new "regions" column in state.combo will have values in lower case.
state.combo$region = tolower(state.combo$region)


# Each row corresponds to a state. 
# Create a new variable called 'region' which is the lower case
# version of each state name. 
# Note that ggplot2 related variables use lower case of state
# and the map of the states only includes 49 areas
# corresponding to the 48 states in the continental US plus
# the District of Columbia.
#
# Merging data frames "states" and "state.combo" by common variable "region")"
# 
# The merged data frame "chrono" has relevant laser data in each state region
# Note that because "states" has more rows than "state.combo", the
# number of rows of the merged data.frame will match the 
# data frame with the highest number of rows
# In other words, nrow(<merged data frame>) = nrow(states) and
# some of the data from the data frame with the smaller
# number of rows will be duplicated.

choro = merge(states, state.combo, sort = FALSE, by = "region")

# In the 'states' data frame, each region has a unique number (variable 'order'),
# from 1 to nrow(states). The line below reorders the rows by 'order' from 1 to 
# nrow(choro). Note again that nrow(choro) = nrow(states)
choro <- choro[order(choro$order), ]

# Note that there are 50 states (regions) but 62 groups. 
# ========
gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=Percent.ops, group=group))
gg <- gg + labs(title = "1. Percent US of flight operations by state", 
                x="Darker colors imply a greater percentage of US flight operations", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg

# ========
gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=Percent.events, group=group))
gg <- gg + labs(title = "2. Percent of laser encounters by state", 
                x="Darker colors imply a greater percentage of laser encounters", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg

# ========

gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=risk.ratio, group=group))
gg <- gg + labs(title = "3. Risk ratio of percent events over percent traffic", 
                x="Darker colors imply higher risk of laser encounters", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg
# For more on customizing ggplot, see http://docs.ggplot2.org/dev/vignettes/themes.html
#
# Some of the parts of the map of the continental US, particularly 
# smaller states and the  District of Columbia, can't be seen easily, 
# and areas outside the continental US are not visible at all, 
# so will develop a ranked list of the top 15 areas with the highest 
# ratios percent of laser encounters divided by the percent of traffic.


# Create risk ratio data frame using all 55 designations, including
# State abbreviations, full state name, 

# Start to build table with states, laser reports, traffic, and ratios
# Start with state names and state abbreviations
state.ratio = data.frame(cbind(every.state,every.abb), stringsAsFactors=FALSE)
colnames(state.ratio)[colnames(state.ratio) == "every.state"] = "State"
colnames(state.ratio)[colnames(state.ratio) == "every.abb"] = "State.abb"

# Get state report totals and percentages using function whales
superstates = whales(laserhits,"State")
# Change column name for state and report totals
colnames(superstates)[colnames(superstates) == "Contribution"] = "Reports"
colnames(superstates)[colnames(superstates) == "Donor"] = "State"

# Merge with the state abbreviations
state.ratio = merge(state.ratio, superstates, by = "State", suffixes = NULL)
# colnames(state.ratio)[colnames(state.ratio) == "Events"] = "Reports"

# Eliminate columns no loger needed
state.ratio$Cum_donors = NULL
state.ratio$Cum_records = NULL
state.ratio$Cdf_donors = NULL
state.ratio$Cdf_records = NULL

# Now merge with the traffic totals
state.ratio = merge(state.ratio, flight.data, by = "State.abb")

# Add the ratio figure
state.ratio$Ratio = state.ratio$Frac_records/state.ratio$Percent.ops

# List of top airports by number of associated laser reports
top.airports = whales(laserhits, "Airport")
top.airports = top.airports[order(-top.airports[,"Contribution"]),1:2]
rownames(top.airports) = NULL
colnames(top.airports) = c("Reports","Airport")
print("Top 15 airports ranked by reported laser events.")
print.data.frame(top.airports[1:15,2:1])

# List of top states and territories by number of associated laser reports
top.states = whales(laserhits, "State")
top.states = top.states[order(-top.states[,"Contribution"]),1:2]
rownames(top.states) = NULL
colnames(top.states) = c("Reports","State")
# print("Top 15 states ranked by reported laser events.")
print.data.frame(top.states[1:15,2:1])

# Create a data frame of state.combo data frame ordered by
# ratio of percentage of reports and percentage of flights
state.ratio.ordered = state.ratio[order(-state.ratio[,"Ratio"]),]
state.ratio.ordered = state.ratio.ordered[,c("State","Ratio")]
rownames(state.ratio.ordered) <- NULL
state.ratio.ordered$Ratio = round(state.ratio.ordered$Ratio,2)
# Now print the data frame
print("Top 15 states ranked by ratio of percent of US laser reports and percent of US air traffic.")
print.data.frame(state.ratio.ordered[1:15,])

# Table of reported laser colors
laser.colors = whales(laserhits, "Laser_color")
laser.colors = laser.colors[order(-laser.colors[,"Contribution"]),1:2]
rownames(laser.colors) = NULL
colnames(laser.colors) = c("Reports","Color")

paste("While there were", format(nrow(laserhits), big.mark = ","), 
      "reports with location information, only",
      format(sum(!is.na(laserhits$Laser_color)), big.mark = ","),
      "of these also contained color information.", sep=" " )

print.data.frame(laser.colors[,2:1])


# Ranking of altitude ranges number of associated laser reports
superlevels = whales(laserhits, "Flight_level")
superlevels = superlevels[order(-superlevels[,"Contribution"]),1:2]
rownames(superlevels) = NULL
colnames(superlevels) = c("Reports","Altitude_range")
superlevels$Altitude_range = paste(superlevels$Altitude_range,
                                   "-",as.numeric(superlevels$Altitude_range) + 999)
# print("Top 15 flight levels ranked by reported laser events.")
print.data.frame(superlevels[1:15,2:1])
