# Laser strike encounter analysis using FAA data from 2010-2014
# Background analysis
# 5 December 2015
# First, ensure we have the packages we need

# For downloading raw data
install.packages("downloader")  
library(downloader) 

# Data file was located at FAA page "Laser News, Laws, & Civil
# Penalties" page at https://www.faa.gov/about/initiatives/lasers/laws/

# Original data is at https://www.faa.gov/about/initiatives/lasers/laws/laser_incidents_2010-2014.xls

# Converted to a CSV file and can be downloaded from AirSafe.com 
url <- "http://www.airsafe.com/analyze/faa_laser_data.csv"
filename <- "faa_laser_data.csv" 
download(url, destfile=filename)

# DATA CLEANING: Pre-processing correcgtions
# The raw dat file from the FAA contained numerious cases of incorrect
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
# States and cities that could not be determined were also coded as 'UNKN'

# Any codes in lower case were capitalized

# Searched for airport codes using:
#       - World Airport Codes - https://www.world-airport-codes.com/
#       - Nations online - http://www.nationsonline.org/oneworld/IATA_Codes/airport_code_list.htm
#       - Locations identifier search tool - https://nfdc.faa.gov/xwiki/bin/view/NFDC/Location+Identifiers+Search+Tool
#       - AirNav.com - https://www.airnav.com/airports/

# Raw data included are all 50 states, the District of Columbia, and Puerto Rico, and several US territories.

# Input raw laser data file
laserhits = NULL
laserhits.raw = NULL
laserhits.raw = read.csv("faa_laser_data.csv")
laserhits=laserhits.raw



# DATA CLEANING: Removal of unknown location (City, State, or Airport) and time of day (Hour) from further analysis
laserhits = subset(laserhits.raw, City!="UNKN"  & State!="UNKN" &  Airport!="UNKN"  & Hour!="UNKN" )
paste("A total of",nrow(laserhits.raw)-nrow(laserhits), "of the original",format(nrow(laserhits.raw), digits=5, big.mark = ","), 
      "records were eliminated from analysis due to unknown values for location (any combination of city, state, and airport), or time of occurrence.", sep=" ")

# DATA CLEANING: Removing unnecessary non-printing characters

# Before evaluating laser encounters by city, airport, and state, steps must be taken to ensure uniformity
# of definitions. One way to do that is to eliminate unecessary leading and trailing space characters.
# In this case, a function was created that could be applied to multiple location-related variables.

# FUNCTION FOR REMOVING LEADING AND TRAILING SPACES AND NON-PRINTING CHARACTERS

# Function 'stripper' definition

# The first step is to ensure the vector 'x' is character type by using 'as.character()' function.
# The next step is to remove the leading space characters, including leading tab, 
#       newline, vertical tab, form feed, carriage return, and space:
#
#      - x = sub("^[[:space:]]+", "", x) 
#
# Less general alterative is t use sub("^\\s+", "", x)
#
# Trailing spaces can be removed in a simlar fashion:
#      - str = sub("[[:space:]]+$", "", str)
#
# Less general alterative is t use sub("\\s+$", "", x)   
# Notes:
#      - The "$" character is the end of string character, "^"is beginning of string character
#      - Note that without the "+", only the first instance would be removed
#      - [:space:] is all space characters (tab, newline, vertical tab, form feed, carriage return, and space)

stripper <- function(x){
      # This function removes leading and trailing spaces from a vector.
      # Equivalent to the str_trim() function in the strigr package   
      x = as.character(x)
      x = sub("[[:space:]]+$", "", x) # Remove leading space characters
      x = sub("^[[:space:]]+", "", x) # Remove trailing space characters
      return(x)
}

# Remove leading and trailing space characters from selected variables.
laserhits$Injury = stripper(laserhits$Injury) # Injury variable
laserhits$State = stripper(laserhits$State)
laserhits$City = stripper(laserhits$City)
laserhits$Airport = stripper(laserhits$Airport)

# DATA CLEANING: Date conversion
# Dates are in form 5-Jan-06, must convert to a date format of yyyy-mm-dd
laserhits$Date = as.Date(laserhits$Date, "%d-%b-%y")

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

# Confirm that it is of class before processing
# class(laserhits)

# Confirm type of data in each column (variable)
# sapply(laserhits,typeof)

# Confirm if variable (column) is a factor
# sapply(laserhits,is.factor)

# QUICK SUMMARY

paste("From 2010 to 2014, there were",
      format(nrow(laserhits), digits=5, big.mark = ","),
      "encounters where a laser beam affected one or more aircraft at or near at least",format(length(table(laserhits$Airport))-1, digits=4, big.mark = ","),
      "unique airports or other locations.", sep=" ") 
# Note: had to remove one unique location due to the "UNKN" location events 

paste("During this five-year period, there was an average of",format(nrow(laserhits)/1826, digits=3, big.mark = ","),
      "laser encounters per day, with as many as", max(sort(table(laserhits$Date))), "strikes in a single day.",
      "There were only",1826-length(table(laserhits$Date)),
      "days over these five years with no reported laser strikes on aircraft in the United States", sep=" ")

paste("In other words, on any given day in the United States, there is a ",format((length(unique(laserhits$Date))/1826)*100, digits=3, big.mark = ","),
      "% chance that at least one aicraft will have a potentially dangerious encounter with a laser beam.", sep="")

# Create a complete table and histogram of days with x-amount of strike events
#  by adding a vector of zero values equal to the number of days with no strikes
daily.strikes=c(rep(0,1826-length(table(laserhits$Date))),as.data.frame(table(laserhits$Date))[,2])

# EXPLORATORY DATA ANALYSIS: Distribution, histogram, and summary of the number of daily laser encounters

table(daily.strikes)

hist(daily.strikes, main="Distribution of number of laser encounters in a day",
     xlab="Number of strikes in a day",  breaks = seq(-1,35,by=1), include.lowest=TRUE, col="dodgerblue")

# Note: if you get a plotting error suggesting the areas is 
# too large, from RStudio console, use the command "dev.off()"
summary(daily.strikes)

# Distribution and  histogram of laser encounters by (UTC) hour of the day
table(laserhits$Hour)

# Note that adding 0.001 to the vector of values helps to align the axes with
# the hourly range of encounter
hist(laserhits$Hour+0.001, main="Distribution of number of laser encounters by hour of the day",
     xlab="Local time (UTC)", breaks = seq(-1,24,by=1), include.lowest=TRUE, 
     xlim=c(-1,24),col="dodgerblue")


# Distribution and  histogram of laser encounters by day of the week
table(laserhits$Weekday)

chisq.test(table(laserhits$Weekday))

plot(laserhits$Weekday, main="Distribution of number of laser encounters by day of the week",
     ylab = "Number of encounters", xlab="Day of the week", col="dodgerblue")


# Distribution and  histogram of laser encounters by month of the year
table(laserhits$Month)
chisq.test(table(laserhits$Month))

plot(laserhits$Month, main="Distribution of number of laser encounters by month of the year",
     ylab = "Number of encounters", xlab="Month of the year", col="dodgerblue")



# Distributions of laser encounters by hour of the day, day of the week, and month of the year
# Will use a combination of tables and heat maps to identify factors that are associated with
# relatively high or low numbers of strikes.

# Heat maps will use a color pallette that goes from white for lowest to dark blue for the highest value
palette = colorRampPalette(c('#ffffff','#0000ff'))(64)

# Combination #1: Months vs. Weekday

# Table of laser encounters by month of the year and day of the week
table(laserhits$Month,laserhits$Weekday)

chisq.test(table(laserhits$Month,laserhits$Weekday))

# Create a heat map (#1.0) for combination of day of the week and month with no scaling.
# Scaling by row (the month)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       The darkest cells correspond indicate that this combination of month and 
#       day of the week had more laser encounters than lighter colored cells.0

heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="none", col = palette, margins=c(15, 13), 
        main="1.0: Daily laser encounters with no scaling")

# Create a heat map (#1.1) for combination of day of the week and month. Scaling by row (the month)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       In other words, for each row (month), it will rank the day of the week from most strikes (darkest)
#       to least strikes (lightest). If a particular day of the week is consistently more likely to have strikes, 
#       that entire column (day of the week) will be in general darker for most months. If that day of the week is 
#       less likely to have strikes, it will be relatively lighter for most months.

heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="row", col = palette, margins=c(15, 13), 
        main="1.1: Daily laser encounters scaled by month")


# Create a heat map (#1.2) for combination of weekday and month. Scaling by column (the weekday)
#       will highlight the months with a relatively high or low number of spikes.
#       In other words, for each column (weekday), it will rank the months from most strikes (darkest)
#       to least strikes (lightest). If a particular month is consistently more likely to have strikes,
#       that entire row (month) will be in general darker for most weekdays. If that month is 
#       less likely to have strikes, it will be relatively lighter for most weekdays


heatmap(table(laserhits$Month, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(15,13), 
        main="1.2: Monthly laser encouners scaled by day")

# Combination #2: Hours vs. Weekday

# Table of laser encounters by hour of the day and day of the week
table(laserhits$Hour,laserhits$Weekday)

# Create a heat map (#2.1) for combination of weekday and hour of the day. Scaling by row (hour of the day)
#       will highlight the day of the week with a relatively high or low number of spikes.
#       In other words, for each row (hour), it will rank the days from most strikes (darkest)
#       to least strikes (lightest). If a particular day of the week is consistently more likely to have strikes, 
#       that entire column (day of the week) will be in general darker for most hours If that weekday is 
#       less likely to have strikes, it will be relatively lighter for most hours.

heatmap(table(laserhits$Hour, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="row", col = palette, margins=c(15,2), 
        main="2.1: Daily laser encounters scaled by hour")


# Create a heat map (#2.2) for combination of weekday and hour of the day. Scaling by column (the day of the week)
#       will highlight the hour with a relatively high or low number of spikes.
#       In other words, for each column (day of the week), it will rank the hours from most strikes (darkest)
#       to least strikes (lightest). If a particular hour of the day is consistently more likely to have
#       strikes, that entire row (hour of the day) will be in general darker for most days of the week If that hour 
#       is less likely to have strikes, it will be relatively lighter for most of the days of the week.


heatmap(table(laserhits$Hour, laserhits$Weekday),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(15,2), 
        main="2.2: By hour laser encounters scaled by day")

# Combination #3: Hours vs. Month

# Table of laser encounters by hour of the day and month of the year
table(laserhits$Hour,laserhits$Month)

# Create a heat map (#3.1) for combination of month and hour of the day. Scaling by row (hour of the day)
#       will highlight the month with a relatively high or low number of spikes.
#       In other words, for each row (hour), it will rank the month from most strikes (darkest)
#       to least strikes (lightest). If a particular month is consistently more likely to have strikes, 
#       that entire column (month) will be in general darker for most hours If that month is 
#       less likely to have strikes, it will be relatively lighter for most hours.

heatmap(table(laserhits$Hour, laserhits$Month),Rowv=NA, Colv=NA,revC=TRUE,
        scale="row", col = palette, margins=c(13,2),
        main="3.1: Monthly laser encounters scaled by hour")


# Create a heat map (#3.2) for combination of month and hour of the day. Scaling by column (the day of the week)
#       will highlight the hour with a relatively high or low number of spikes.
#       In other words, for each column (day of the week), it will rank the hours from most strikes (darkest)
#       to least strikes (lightest). If a particular hour of the day is consistently more likely to have
#       strikes, that entire row (hour of the day) will be in general darker for most of the days of the week. 
#       If that hour is less likely to have strikes, it will be relatively lighter for most days of the week.


heatmap(table(laserhits$Hour, laserhits$Month),Rowv=NA, Colv=NA,revC=TRUE, 
        scale="column", col = palette, margins=c(13,2), 
        main="3.2: By hour laser encounters scaled by month")

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

# Summary of laser hits by city 
city.tot = as.data.frame(table(laserhits$City)) 
# Turns table output into a two-column data frame sorted alphabetically by state

# Rename the columns for clarity
colnames(city.tot) = c("City","Events") 

# Cities ordered by laser totals
ordered.city.tot = city.tot[order(city.tot$Events, decreasing=TRUE),] 

# Top 50 cities by laser encounter totals
paste("Top 50 cities ordered by laser encounter totals")
paste(ordered.city.tot$City[1:50],"-",ordered.city.tot$Events[1:50],sep=" ")


# LASER STRIKES BY AIRPORT

# Summary of laser encounters by airport 
airport.tot = as.data.frame(table(laserhits$Airport)) 
# Turns table output into a two-column data frame sorted alphabetically by state

# Rename the columns for clarity
colnames(airport.tot) = c("Airport","Events") 

# Airports ordered by laser totals
ordered.airport.tot = airport.tot[order(airport.tot$Events, decreasing=TRUE),] 

# Top 100 airports
paste("Top 100 airports ordered by laser encounter totals")
paste(ordered.airport.tot$Airport[1:100],"-",ordered.airport.tot$Events[1:100],sep=" ")



# TOP AIRPORTS BY STATE

# Summary of laser hits by selected airports in a state. 
# A maxiumum of 15 airports are listed for each state

# Creating a list of lists with all the summarized airport information by state

us.list = NULL
paste("Summary of laser hits by selected airports in a state with a maximum of 15 airports listed per state.")
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
install.packages("maps", repos="http://cran.rstudio.com/") 
library(maps)

install.packages("ggplot2", repos="http://cran.rstudio.com/") 
library(ggplot2)

# Input summary flight operations statistics

# Converted to a CSV file and can be downloaded from AirSafe.com 
url <- "http://www.airsafe.com/analyze/faa_opsnet_by_state.csv"
filename <- "faa_opsnet_by_state.csv" 
download(url, destfile=filename)

flight.data = NULL
flight.data.raw = NULL
flight.data.raw = read.csv("faa_opsnet_by_state.csv")
flight.data=flight.data.raw
# Ensure that State variable is character
flight.data$State = as.character(flight.data$State)

# Rename column "State" to "State.abb" to be consistent with earlier usage
names(flight.data)[names(flight.data)=="State"] = "State.abb"

flight.data$State.abb = flight.data$State

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

extra.abb = c("DC")
extra.name = c("District of Columbia")

# Now append them to the R-provided list of 50 state names and abbreviations
used.abb = c(state.abb,extra.abb)
used.state = c(state.name,extra.name)

# But take out Alaska and Hawaii
used.abb = setdiff(used.abb, c("AK", "HI"))
used.state = setdiff(used.state, c("Alaska", "Hawaii"))

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
# so below is a list of the top ten areas with the highest values for 
# percent of laser encounters divided by the percent of traffic.

# First, create an ordered data frame of state.combo data frame
state.combo.ordered = state.combo[order(-state.combo[,"risk.ratio"]),]
state.top.10.risk = state.combo.ordered[1:10,c("State","risk.ratio")]
rownames(state.top.10.risk) <- NULL
state.top.10.risk$risk.ratio = round(state.top.10.risk$risk.ratio,2)
# Now print the data frame
print.data.frame(state.top.10.risk)
