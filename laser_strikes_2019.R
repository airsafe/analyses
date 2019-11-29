# Processing start time
sink('laser_hits_playpen_output.txt')
timeStart = Sys.time()


# Start writing to an output file (before first "paste" command)
cat(paste("Processing start date and time", date(), "",sep="\n"))



# === Created 27 August 2019 ===
# Used to test out techniques before applying to main program


# Data file was located at FAA page "Laser News, Laws, & Civil
# Penalties" page at https://www.faa.gov/about/initiatives/lasers/laws/

# Original data is at https://www.faa.gov/about/initiatives/lasers/laws/laser_incidents_2010-2014.xls

# Converted to a CSV file and can be downloaded from AirSafe.com at
#       http://www.airsafe.com/analyze/faa_laser_data_2010_to_2019.csv"

# Operational data on air carriers taken from the FAA Operations Network (OPSNET)
#       site at https://aspm.faa.gov/opsnet/sys/airport.asp

# Raw data included are all 50 states, the District of Columbia, and Puerto Rico, and several US territories.


# ===PACKAGES===
options(repos = c(CRAN = "http://cran.rstudio.com"))
if("downloader" %in% rownames(installed.packages()) == FALSE) 
{install.packages("downloader")}
library(downloader) 

if("maps" %in% rownames(installed.packages()) == FALSE) 
{install.packages("maps")}
library(maps)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) 
{install.packages("ggplot2")}
library(ggplot2)

if("stringr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("stringr")}
library(stringr)

# === END PACKAGES ===

# ===FUNCTIONS===

# Function: expanded_date
expanded_date = function(df, date_var_name){
        # Function: Insert processed date value into data frame
        #       Goal is to take the date, and add the following variables:
        #       Day_num (day of the month), Month, Year, Weekday
        #       Month and Weekday will be three-character abbreviation
        # Requirements: Date must be in the format '%d-%b-%y' and must not be NA or missing
        # df = laserhits      
        # date_var_name = "Date"
        # Create Day_num variable
        Day_num = as.numeric(format(df[,date_var_name],'%d'))
        
        # Create Month variable
        Month =  months(df[,date_var_name], abbreviate = TRUE)
        Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        
        # Create Year variable
        Year = as.numeric(format(df[,date_var_name],'%Y')) # Four-digit year format
        
        # Create Weekday variable
        Weekday = weekdays(df[,date_var_name], abbreviate = TRUE)
        Weekday = factor(Weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
        
        # Insert new variables into data frame
        
        df_split = which(colnames(df)==date_var_name)
        df = cbind(df[,1:df_split], Day_num, Month, Year, Weekday, df[,(df_split+1):ncol(df)])
} # End expanded_date

# Function: expanded_time
expanded_time = function(df, time_var_name){
        # If the time variable is in the 24-hour (HHMM) format in the 
        #       original file, converting that file to CSV format may remove leading zeros.
        #       The goal is to put the time into the 
        #       format HH:MM by doing a combination of adding leading zeros 
        #       and inserting a colon to get to HH:MM format
        
        # In addition to converting the format, an additional factor variable
        #       will be created, with 25 levels including any missing time
        
        #       Requirements: Missing time values are already coded as NA
        
        df[,time_var_name] = as.character(df[,time_var_name])
        Raw_time=df[,time_var_name]
        # Paste string "00:0 if one character long
        one.long.ndx = which(nchar(df[,time_var_name])==1)
        
        df[,time_var_name][one.long.ndx] = paste0("00:0",df[,time_var_name][one.long.ndx])
        
        # Add two leading zeros and colon if two characters long
        # Becasue 'NA' is two characters long, need to 
        #       find intersection of those not NA and those
        #       two characters long
        
        non.NA.values = which(!is.na(df[,time_var_name]))
        two.long.all = which(nchar(df[,time_var_name])==2)                     
        two.long.ndx = intersect(non.NA.values, two.long.all)
        
        df[,time_var_name][two.long.ndx] = paste0("00:",df[,time_var_name][two.long.ndx])
        
        # Add one leading zero insert colon before last two characters if three characters long
        three.long.ndx = which(nchar(df[,time_var_name])==3)
        
        df[,time_var_name][three.long.ndx] = paste0("0",substr(df[,time_var_name][three.long.ndx], 1,1),":",substr(df[,time_var_name][three.long.ndx], 2,3))
        
        
        # Insert colon after second character if time is four characters long (1000 to 2359, as well as missing time value of 9999)
        four.long.ndx = which(nchar(df[,time_var_name])==4)
        
        df[,time_var_name][four.long.ndx] = paste0(substr(df[,time_var_name][four.long.ndx], 1,2),":",substr(df[,time_var_name][four.long.ndx], 3,4))
        
        #==Create factor variable for time of day==
        # Will also create a factor variable of time by hour of the day
        # Convert hour vector so that time is put into 25 bins
        #       with bin 1 (coded as "1) including events from 0000 to 0059 hours, etc. until bin 24 
        #       Bin 25 will be coded as "25" and will include those events with an unknown time.
        #       Then create an ordered factor variable from 1-24, plus 25
        
        Hour_Category=as.integer(substr(df[,time_var_name],1,2))
        Hour_Category=Hour_Category+1
        Hour_Category[which(Hour_Category==100)]=25
        Hour_Category=as.factor(Hour_Category) 
        
        df_split = which(colnames(df) == time_var_name)
        df = cbind(df[,1:df_split], Raw_time, Hour_Category, df[,(df_split+1):ncol(df)])
        
        
} # End expanded_time

# Function: insert_variable inserts a new vector after a specific data frame column
insert_variable = function(df, insert_after_name, new_data){
        # Note: Need the data frame name, the insert location',
        #       column name, and name of the new data
        #       The new data may be one variable equal in length to the number of rows of 
        #       the data frame df, or it is a data frame with
        #       the same number of rows as df
        
        df_split = which(colnames(df) == insert_after_name)
        df = cbind(df[,1:df_split], new_data, df[,(df_split+1):ncol(df)])
} # End insert_variable function

# Function to create categorical variable from widely spaced numerical vector
make_factor = function(input.num.vec, num.limits, factor.levels){
        # This function takes a numeric vector, values that define value ranges
        #       to be associated with a new factor vector and returns a factor vector
        #       of the same length.
        
        # First step is to change numbers to specific number levels
        # Second step is to define that vector as factor and simultaneously
        #       relabel the factor levels
        num.limits= c(1999,17999,39999, 328000)
        factor.levels = c("below_FL20", "FL20_to_FL180", "FL180_to_FL400", "above_FL400")
        
        num.limits = sort(num.limits) # Ensure number limits are in order
        
        if((length(num.limits)) != length(factor.levels)){paste("Incorrect number of factor levels given the number limits")}
        
        for(i in 1:length(factor.levels)){
                
                if(i==1){
                        input.num.vec[input.num.vec <= num.limits[1]] = -1      
                }
                else if (i!=1){
                        input.num.vec[(input.num.vec > num.limits[i-1]) & (input.num.vec <= num.limits[i])]  = -i     
                }
        }
        input.num.vec = -input.num.vec
        new.factor.vec = factor(input.num.vec, labels=factor.levels, ordered = TRUE)
} # End make_factor function

na_to_factor_level = function(df, cat_cols){
        # Turns any NA value in a vector into a factor level
        for(col in cat_cols){
                df[,col] = addNA(df[,col])
        }
} # End na_to_factor_level function

# === END OF FUNCTIONS ===

# === DATA CLEANING ===
# The laser event data was reviewed and cleaned prior to uploading
# The raw data file from the FAA contained numerous cases of incorrect
# data with respect to location (airport, city, and state), including misspellings and capitalization errors,
# as well as missing data. The events were manually reviewed to correct these errors when sufficient 
# information was contained in the rest of the record.

# Any missing or incomplete data was coded as "UNK" prior to uploading

# In some cases, the airport location code was substituted 
# for a navigational aid code when they were located at or 
# near an airport. For example, several reports for the city 
# of Baltimore, MD used the 'BAL' IATA code, 
# which is for the VORTAC at the field, and the arport code 
# 'BWI' was substituted.

# Incorrect, misspelled, or missing data for any variable 
# was corrected or filled in when enough informaiton was 
# available in the record.
# Example errors inculude:
#       - 'GUA' instead of 'GUM' for GUAM, 
#       - 'VHP' instead of 'VPZ' for Valparasio, IN
#       - 'POM' instead of 'POC' for Pomona or Ontario  , California

# A variety of resources were used to identify key data for some records, including:
#       - World Airport Codes - https://www.world-airport-codes.com/
#       - FlightAware - https://flightaware.com
#       - AirFleets - https://www.airfleets.net/home/
#       - Nations online - http://www.nationsonline.org/oneworld/IATA_Codes/airport_code_list.htm
#       - Locations identifier search tool - https://nfdc.faa.gov/xwiki/bin/view/NFDC/Location+Identifiers+Search+Tool
#       - AirNav.com - https://www.airnav.com/airports/

# REPORTED LASER COLORS WERE STANDARDIZED
# In the pre-analysis cleaning process, reported laser colors were standardized
# by making all inputs with multiple identified colors of the form Color1/Color2,
# with the colors listed alphabetically, insuring that the first letter in 
# a single word color identifier was capitalized, and correcting misspellings. 
# Example: Blue and Green, became Multiple (Blue, Green), and Blue or green become Multiple (Blue or Green) 

# ===UPLOAD DATA===

# Data was converted to a CSV file and cleaned and can be downloaded from AirSafe.com 
# url <- "http://www.airsafe.com/analyze/faa_laser_data_2010_2018.csv"
# filename <- "faa_laser_data_2010_2018.csv" 
# download(url, destfile=filename)

# Input raw file
laserhits = NULL
raw.laserhits = NULL
raw.laserhits = read.csv("http://www.airsafe.com/analyze/faa_laser_data_2010_2018.csv")
laserhits=raw.laserhits
# === END OF UPLOADING ===

# === POST-UPLOAD DATA FRAME FORMATTING AND CLEANING ===
# STEP 1: Ensure that all of the variables are of type character
# while making sure none of these are treated as factors
laserhits = data.frame(lapply(laserhits,as.character), stringsAsFactors=FALSE)

# STEP 2: Remove all leading and trailing spaces from selected variables
# Using the function trimws removes leading and trailing space characters
laserhits = as.data.frame(apply(laserhits, 2, trimws)) 

# STEP 3: Change all "UNK" entries to NA
laserhits[laserhits == "UNK"] = NA

# STEP 4: Identify and remove duplicated records,
#       (Must ingnore the unique ID that was added to the database)

duplicated.ndx = which(duplicated(laserhits[,-1]))
laserhits = laserhits[-duplicated.ndx,]

cat('\n')
paste(length(duplicated.ndx), " duplicated records were identified and removed, leaving ", nrow(laserhits), " events")

# The following analyses will depend on records that have inputs for 
#       all of the following variables: 
#               Date              
#               Time (UTC)
#               Aircraft ID
#               Aircraft type 
#               Location (IATA or ICAO code)
#               City
#               State
#               Metro_area

cat('\n')
initial_records = nrow(laserhits)
paste("There are initially ",initial_records, " records")
useful.vars = c("Date", "Time","Aircraft_Type", "Airport", "Altitude", "City", "State")
useful.cases.ndx = complete.cases(laserhits[,useful.vars])
laserhits = laserhits[useful.cases.ndx,]

cat('\n')
paste(nrow(laserhits), " records have no missing data in the following key variables:")
paste(useful.vars)
cat('\n')
# === END POST-UPLOAD DATA FRAME FORMATTING AND CLEANING ===

# === REFORMAT OR EXPAND VARIALBES ===

# Ensure date variable is of type date
laserhits$Date  = as.Date(laserhits$Date, format = '%d-%b-%y')

# Ensure Altitudes are of type numeric
laserhits$Altitude = as.numeric(as.character(laserhits$Altitude))

# Check for missing date values
ifelse(!anyNA(laserhits$Date[2]),paste("No date value is missing"),paste("At least one date value is missing"))
cat('\n')

# Expand the date variable by adding variables for Year, Month, Weekday, and (numerical) Day
laserhits = expanded_date(laserhits, "Date")

# Add a variable that displays time in the format hh:mm
laserhits = expanded_time(laserhits, "Time")

# Restrict data to events prior to 2019
laserhits = laserhits[laserhits$Year < 2019,]

# Restrict data to airline aircraft only
# Note: The following aircraft types were associated with
#       the the air carrier data in the FAA data used in this analysis:
#       - 1: Large jet airliner (over 100 seats, single ailse)
#       - 2: Large jet airliner (over 100 seats, twin ailse)
#       - 3: Regional jet airliner (60 to 100 seats)
#       - 4: Prop-drive airliner (60 seats and above)

laserhits = laserhits[laserhits$Aircraft_Type %in% c("1","2","3","4"),]

# Create a new variable of altitude categories
new_variable = as.numeric(as.character(laserhits$Altitude))
# Next is to add one foot to zero altitude records
new_variable[new_variable == 0] = 1

# Now create the new variable based on the log of the transformed Altitude variable
laserhits$Altitude_log = round(log(new_variable), digits=4)


# Insert a factor variable based on altitudes
num.limits= c(1999,17999,39999, 328000)
factor.levels = c("below_FL20", "FL20_to_FL180", "FL180_to_FL400", "above_FL400")
laserhits$Altitude_cat = make_factor(laserhits$Altitude, num.limits, factor.levels)

# identify numeric and factor variables
numeric.vars = c("Day_num", "Year", "Raw_time", "Altitude", "Log_altitude")
factor.vars = c("Event_ID", "Month", "Weekday", "Time", "Hour_Category", "Flight_ID", "Model", "Aircraft_Type", "Altitude_cat", "Airport", 
                "Laser_color", "Injuries", "City", "State", "Metro_area")

numeric.vars.ndx = which(colnames(laserhits) %in% numeric.vars)
factor.vars.ndx = which(colnames(laserhits) %in% factor.vars)

# Drop unused factors for selected variabls
laserhits[,factor.vars] = droplevels(laserhits[,factor.vars])
paste("Resulting data frame has",nrow(laserhits), "rows and", ncol(laserhits), "columns:")
colnames(laserhits)

cat('\n')
paste("The data summary of these", ncol(laserhits), "columns are:")
summary(laserhits)

cat('\n')
paste("The structure of the data frame is as follows")
str(laserhits)

# === END REFORMAT OR EXPAND VARIALBES ===

# === CREATE VARIABLE OF STATE ABBREVIATIONS ===
# Will match state names to official two-letter state abbreviations

# R has built in vectors of state names and abbreviations (state.name and state.abb) 
#       used by the US Postal Service (USPS)

# Source is USPS Publication 28 - postal addressing standards 
# Located at https://pe.usps.com/text/pub28/28apb.htm#ep19305 accessed 8 September 2019

# The built-in abbrevations in R datasets only the 
#       names (state.name) and abbreviations (state.abb) for the 50 US states. 
#       Additional names and abbreviations were added to match those in the laserhits data frame.

# The analysis will only deal with geographical areas that were included in the 
#       FAA data about air carrier opertions.

# The first step would be to determine what states
#       were included in the FAA data. 

# The FAA data uses state codes, so step zero would be to determine
#       what state codes could be used in the laserhits data frame.


# Find state values in laserhits not in state.name list
missing.states = which(!(unique(laserhits$State) %in% state.name))

cat('\n')
paste("State and territory names not in the R state data")
as.character(unique(laserhits$State)[missing.states]) 

# Those missing territory  names and their abbreviations 
#       (District of Columbia, Puerto Rico, Guam, and Virgin Islands),
#       will be used to create a two-letter code for every state and territory in the laserhits database

extra.abb = c("DC", "PR","GU", "VI", "MP")
extra.name = c("District of Columbia", "Puerto Rico", "Guam", "Virgin Islands", "Northern Mariana Islands")

# Now append them to the R-provided list of 50 state names and abbreviations
usps.abb = as.character(c(state.abb,extra.abb))
usps.state = as.character(c(state.name,extra.name))
usps.state.abb = data.frame(cbind(usps.state,usps.abb))
colnames(usps.state.abb) = c("State","State_abb")


# Will create a variable with the two-letter USPS abbreviation

# Initialize the full state name abbreviation variable with a number of NA values equal to the
# number of input records to ensure that the final vector will be compatible with ntsb.data
laserhits$State_abb = rep(NA,nrow(laserhits))

# Now replace an NA value with an appropriate two-letter code.
#       If not an appropriate state or territory name, remains as NA
for (i in 1:nrow(laserhits)) {
        if(laserhits$State[i] %in% usps.state) {
                laserhits$State_abb[i] = usps.abb[which(usps.state == laserhits$State[i])]
        }
}

# Check if there are any records with a remaining NA value for the state code
cat('\n')
ifelse(anyNA(laserhits$State_abb),
       paste("There were", sum(is.na(laserhits$State_abb)), "records not associated with a US state or territory that will be excluded from further analysis"),
       paste("Every record is associated with a US state or territory"))

# Eliminate any records with a remaining NA value for the state code
laserhits = laserhits[!is.na(laserhits$State_abb),]

# === END CREATE VARIALBLE OF STATE ABBREVIATIONS ===

# === INPUT FAA FLIGHT STATISTICS ===

# Raw data taken from FAA at https://aspm.faa.gov/opsnet/sys/main.asp
# Resides at "http://www.airsafe.com/analyze/faa_opsnet_by_state_2019.csv"

# Input summary flight operations statistics

# Converted to a CSV file and can be downloaded from AirSafe.com 
# url <- "http://www.airsafe.com/analyze/faa_opsnet_by_state_2019.csv"
# filename <- "faa_opsnet_by_state_2019.csv" 
# download(url, destfile=filename)
flight.data = NULL
flight.data.raw = NULL
raw.flight.data = read.csv("http://www.airsafe.com/analyze/faa_opsnet_by_state_2019.csv")
flight.data=raw.flight.data

# STEP 1: Ensure that all of the variables are of type character
# while making sure none of these are treated as factors
flight.data = data.frame(sapply(flight.data,as.character), stringsAsFactors=FALSE)

# STEP 2: Ensure that Air_carrier_ops, Total_ops, and Percent_air_carrier are numeric
flight.data[,3:5] = apply(flight.data[,3:5],2,as.numeric)

# Each record contains data for a single airport, including:
#       - State
#       - Airport code
#       - Air carrier operations
#       - Total operations (including air carrier)
#       - Percent of air carrier operations

# STEP 3: Exclude any airport with no Air Carrier operations
flight.data = flight.data[flight.data$Air_carrier_ops>0,]

# STEP 4: Rename column "State" to "State.abb" to be consistent with earlier usage
names(flight.data)[names(flight.data)=="State"] = "State.abb"

# Raw data has flights by airport and by state, so need
#       a data frame that summarizes data by state
state.data.raw = flight.data[,c("State.abb","Air_carrier_ops")]
# Make State.abb a factor
#state.data.raw$State.abb = as.factor(state.data.raw$State.abb)

# Build a summary of traffic by state
state.total.ops = NULL
state.total.events = NULL
unique.state.full = NULL
unique.state = unique(state.data.raw$State.abb)
for(i in 1:length(unique.state)){
        # Get air carrier operations for each state
        state.total.ops = c(state.total.ops, sum(state.data.raw$Air_carrier_ops[state.data.raw$State.abb == unique.state[i]]))
        # Get total events by state
        state.total.events = c(state.total.events, sum(laserhits$State_abb == unique.state[i]))
        unique.state.full = c(unique.state.full, as.character(usps.state.abb[which(usps.state.abb$State_abb == unique.state[i]),"State"]))
}
state.data = as.data.frame(cbind(unique.state.full, unique.state, state.total.ops,state.total.events))
colnames(state.data) = c("State", "State.abb", "State_air_carrier_ops", "State_laser_events")
state.data$State_air_carrier_ops = as.numeric(as.character(state.data$State_air_carrier_ops))
state.data$State_laser_events = as.numeric(as.character(state.data$State_laser_events))

# Add column for events per 100K
state.data$Rate_per_100K = round(100000*(state.data$State_laser_events/state.data$State_air_carrier_ops), digits = 2)


# === CORRECTION FOR DELALWARE OUTLIER ===

cat('\n')
paste("Delaware is in a unique position. It has very few air carrier operations") 
paste("as most passengers fly out of airports outside of Delaware, primarily Philadelphia.")
paste("In fact, over the nine-year period of the study, the state had an average of", round(state.data$State_air_carrier_ops[state.data$State.abb=="DE"]/108, digits = 1), "air carrier operations per month,")
paste("causing the state to have a rate of laser encounters that was orders of  magnitude higher than any other state.")

de.uncorrected.rate = state.data$Rate_per_100K[state.data$State.abb == "DE"]
next.highest.uncorrected.rate = sort(state.data$Rate_per_100K, decreasing = TRUE)[2] 
cat('\n')
paste("Delaware's rate per 100K flights is", round(de.uncorrected.rate, digits = 2), 
      "which is an order of magnitude higher than the next highest rate of",
      round(next.highest.uncorrected.rate, digits = 2))

cat('\n')
paste("Looking at laser rates based on states is problematic for reasons besides the unusual Delaware situation")
paste("The rate of laser events may be affected by local conditions that may be within an area of a state, or an area covering two or more states.")
paste("One alternative is to look at rates within well-defined area of population.")
paste("One well-defined alternative is US Census defined Metropolitan Statistical areas around the largest US cities.")
# 
# de.and.pa.combo.traffic = sum(state.data$State_air_carrier_ops[state.data$State.abb %in% c("DE","PA")])
# de.and.pa.combo.events  = sum(state.data$State_laser_events[state.data$State.abb %in% c("DE","PA")])    
# de.and.pa.combo.rate = 100000*(de.and.pa.combo.events/de.and.pa.combo.traffic)
# state.data$Rate_per_100K[state.data$State.abb %in% c("DE","PA")] = de.and.pa.combo.rate

# === END CORRECTION FOR DELALWARE OUTLIER ===

# Add column for ratio events per 100K compared to all US
rate_per_100K_US = 100000*(sum(state.data$State_laser_events)/sum(state.data$State_air_carrier_ops))
state.data$Rate_per_100K_ratio_US = round(state.data$Rate_per_100K/rate_per_100K_US, digits = 2)


# Add column to state.data data frame for percentage of total air carrier flights
# usa.tot.flights = sum(state.data$State_air_carrier_ops)
# state.data$Percent.tot.usa = round(100*state.data$State_air_carrier_ops/usa.tot.flights, digits = 3)

# Add column to state.data data frame for percentage of total laser events
# usa.tot.laser.events = sum(state.data$State_laser_events)
# state.data$Percent.events.usa = round(100*state.data$State_laser_events/usa.tot.laser.events, digits = 3)

# Add column to state.data data frame for rate ratio of percentage of total laser events over percentage of flights
# state.data$Rate_ratio = round(state.data$Percent.events.usa/state.data$Percent.tot.usa, digits = 3)

# Add column for rank of rate ratios
state.data$Rate_ratio_rank = as.integer(rank(-state.data$Rate_per_100K_ratio_US))

# === END INPUT FAA FLIGHT STATISTICS ===


# === METROPOLITAN AREA ANALYSIS
cat('\n')
paste("The laser event rates within the top 20 US Metropolitan Statistical Areas (MSAs), were reviewed.")
cat('\n')
paste("This group included the largest US cities, including New York, Chicago, and Los Angeles.")
cat('\n')
paste("Two additional areas, which both contained large cities and which were also adjacent a top 20 MSA were also included.")
cat('\n')
paste("The two additional MSAs were the Santa Clara, CA Baltimore, MD MSAs, which were adjacent to  the San Francisco and Washington, DC MSAs.")

cat('\n')
paste("Each metropolitan area included any airport or other location with at least one air carrier event, or one air carrier operation")
metro.areas = as.character(sort(unique(laserhits$Metro_area)))

cat('\n')
paste("The average rate of reported laser incidents in the United States was", metro.summary$Rate_per_100K[nrow(metro.summary)],"per 100,000 air carrier flights.")
paste("There were", sum(state.combo$Rate_per_100K>=metro.summary$Rate_per_100K[nrow(metro.summary)]),"states and", sum(metro.summary$Rate_per_100K>=metro.summary$Rate_per_100K[nrow(metro.summary)])-1, "MSAs with a higher encounter rate.")

cat('\n')
paste("The following ", length(metro.areas), " metropolitan areas were analyzed:")
metro.areas

# Each metro area is analyzed to create the following data frame variables:
#       Area
#       Events
#       Traffic
#       Percent_US_traffic
#       Event_rate
#       Event_rate_ratio_to_US

# List locations used in each metro area
cat("\n")
paste("List of airports, navaids, and other location identifiers in each metro area")
cat("\n")
for(i in 1:length(metro.areas)){

        cat(metro.areas[i],"Metro area locations","\n")
        print(as.character(na.omit(sort(unique(laserhits$Airport[laserhits$Metro_area==metro.areas[i]])))))
        cat("\n")

}

# Ensure Metro_area variable is of type character

laserhits$Metro_area = as.character(laserhits$Metro_area)
all.areas = c(metro.areas,"All_US")
area.events = rep(NA,length(all.areas))
area.traffic = rep(NA,length(all.areas))
# traffic.airports.ndx = rep(NA,length(metro.areas))
# traffic.airports = rep(NA,length(metro.areas))
# list(traffic.airports)
for (i in 1:(length(all.areas) - 1)){
        area.rows = which(laserhits$Metro_area == all.areas[i])
        area.events[i] = length(area.rows)
        area.airports = as.character(unique(laserhits$Airport[area.rows]))
        traffic.airports.ndx = which(flight.data$Airport %in% area.airports)
        # print(paste(all.areas[i], which(flight.data$Airport %in% area.airports))) # Included only in testing phase
        area.traffic[i] = sum(flight.data$Air_carrier_ops[which(flight.data$Airport %in% area.airports)])
}

# The row will consist of entire US (All_US)   
area.events[length(area.events)] = nrow(laserhits)
area.traffic[length(area.events)] = sum(flight.data$Air_carrier_ops)

# Summarize the metro areas and the US as a whole
metro.summary = cbind(all.areas, area.events, area.traffic)
metro.summary = as.data.frame(metro.summary, stringsAsFactors
 = FALSE)
colnames(metro.summary) = c("Metro_area", "Events", "Traffic")
metro.summary$Events = as.numeric(as.character(metro.summary$Events))
metro.summary$Traffic = as.numeric(as.character(metro.summary$Traffic))
metro.summary$Rate_per_100K = round(100000*metro.summary$Events/metro.summary$Traffic, 2)
metro.summary$Rate_ratio = round(metro.summary$Rate_per_100K/metro.summary$Rate_per_100K[nrow(metro.summary)], digits = 2)
metro.summary$Rate_ratio_rank = c(rank(-metro.summary$Rate_ratio[1:(nrow(metro.summary)-1)]),NA)

cat('\n')
paste("List of metropolitan areas with laser encounter totals and encounter rate calculations")
# First, will reorder rows by metro area names
metro.ordered = metro.summary[order(metro.summary$Metro_area),]

# Metro areas ordered by metro area name
cat('\n')
paste("Metro areas ordered by metro area name")
metro.ordered[,c("Metro_area", "Events", "Rate_per_100K")]


# Metro areas ordered by laser encountered totals
cat('\n')
paste("Metro areas ordered by laser encounter totals")
metro.ordered = metro.ordered[order(metro.ordered$Events, decreasing=TRUE),] 
metro.ordered[,c("Metro_area", "Events", "Rate_per_100K")]


# Metro areas ordered by rate per 100K air carrier operations
cat('\n')
paste("Metro areas ordered by laser encounter rate")
metro.ordered = metro.ordered[order(metro.ordered$Rate_per_100K, decreasing=TRUE),] 
metro.ordered[,c("Metro_area", "Events", "Rate_per_100K")]

# === END METROPOLITAN AREA ANALYSIS

# === COMBINED MSA AND STATE COMPARISON ===

# States included in one or more metro areas
cat('\n')
paste("The reviewed MSAs covered territory in the following states and territories:")
included.msa.areas = unique(laserhits$State[which(!is.na(laserhits$Metro_area))])
included.msa.areas = sort(as.character(included.msa.areas))
included.msa.areas


cat('\n')
paste("The following are the abbreviations of the states and territories that did not include any of the reviewed MSAs:")
excluded.msa.areas.abb = unique(laserhits$State_abb)[!(unique(laserhits$State_abb) %in% unique(laserhits$State_abb[which(!is.na(laserhits$Metro_area))]))]

# Also exclude any state or territory with no laser events
excluded.msa.areas.abb = c(excluded.msa.areas.abb, unique(flight.data$State.abb[which(!(flight.data$State.abb %in% unique(laserhits$State_abb)))]) )

excluded.msa.areas.abb = sort(excluded.msa.areas.abb)
excluded.msa.areas.abb

cat('\n')
paste("The MSAs did not include territory in any of the following states and territories")
excluded.msa.areas = sort(as.character(state.data$State[which(state.data$State.abb %in% excluded.msa.areas.abb)]))
excluded.msa.areas


# Non-MSA states and territories 
excluded.msa.areas.abb = unique(laserhits$State_abb)[!(unique(laserhits$State_abb) %in% unique(laserhits$State_abb[which(!is.na(laserhits$Metro_area))]))]

# Number of flights in territories and states not included in any MSA
excluded.msa.areas.flights = sum(flight.data$Air_carrier_ops[which(flight.data$State.abb %in% excluded.msa.areas.abb)])
cat('\n')
paste("The states and territories that were not a part of any MSA were responsible for ",round(100*excluded.msa.areas.flights/sum(flight.data$Air_carrier_ops), digits = 1),"% of all air carrier traffic", sep="")

# Total air carrier traffic: sum(flight.data$Air_carrier_ops)

cat('\n')
# Percent air carrier traffic in MSA areas
# First get all Airport identifiers in MSA areas
msa.airports = sort(as.character(unique(laserhits$Airport[!is.na(laserhits$Metro_area)])))


# Next get index of relevant airports in flight.data
msa.flights.ndx = which(flight.data$Airport %in% msa.airports)

# Also, get the index of the airports outside of the msa areas
non.msa.flights.ndx = which(!(flight.data$Airport %in% msa.airports))

paste("The air carrier traffic in reviewed MSAs represented ", round(100*(sum(flight.data$Air_carrier_ops[msa.flights.ndx]))/sum(flight.data$Air_carrier_ops), digits = 1),"% of all US air carrier traffic.", sep="")

cat('\n')
paste("The air carrier traffic outside MSAs represented ", round(100*(sum(flight.data$Air_carrier_ops[non.msa.flights.ndx]))/sum(flight.data$Air_carrier_ops), digits = 1),"% of all US air carrier traffic.", sep="")

cat('\n')
# Percent air carrier laser encounters traffic in MSA areas
# Note that one of the records in the metro.summary data frame
#       includes the entire US, and is equal to the number of laserhits rows
paste("The laser events in the reviewed MSAs represented ", round(100*(sum(metro.summary$Events)-nrow(laserhits))/nrow(laserhits), digits = 1),"% of all US air carrier laser encounters.", sep="")

cat('\n')
paste("The laser events outside the MSAs represented ", round(100*sum(is.na(laserhits$Metro_area))/nrow(laserhits), digits = 1),"% of all US air carrier laser encounters.", sep="")

cat('\n')
# Percent air carrier traffic in areas not involving a covered MSA
paste("The air carrier traffic in territories not a part of any reviewed MSA represented ",round(100*sum(flight.data$Air_carrier_ops[which(flight.data$State.abb %in% excluded.msa.areas.abb)])/sum(flight.data$Air_carrier_ops), digits = 1),"% of all US air carrier flights.", sep="")

cat('\n')
# Percent air carrier encounters in areas not involving a covered MSA
paste("The air carrier traffic in territories not a part of any reviewed MSA represented ",round(100*sum(laserhits$State_abb %in% excluded.msa.areas.abb)/nrow(flight.data$Air_carrier_ops), digits = 1),"% of all US air carrier laser encounters.", sep="")


cat('\n')
# Percent of traffic outside of these two areas
paste("The percent of air carrier traffic not in one of these two areas represents ",round(100*(sum(flight.data$Air_carrier_ops) - (sum(metro.summary$Traffic)-metro.summary$Traffic[nrow(metro.summary)]) - excluded.msa.areas.flights )/sum(flight.data$Air_carrier_ops), digits = 1),"% of all US air carrier flights.", sep="")



cat('\n')
# Percent of laser encounters involving air carrier flights outside of these same two areas
# Includes all events from excluded areas: sum(laserhits$State_abb %in% excluded.msa.areas.abb)
# Also includes all events involving an included MSA: sum(!is.na(laserhits$Metro_area))
paste("The percent of laser encounters involving air carrier flights outside of these same two areas represents ",round(100*( nrow(laserhits) - sum(laserhits$State_abb %in% excluded.msa.areas.abb) - sum(!is.na(laserhits$Metro_area)))/nrow(laserhits), digits = 1),"% of all US air carrier laser encounters.", sep="")

# By combining the selected MSAs with the states that are not part of an MSA, 
#       one is able to better gauge laser encounter problem areas

# First, build the start of this combined database with MSA data
combined.areas = metro.summary[,1:4]
colnames(combined.areas) =c("Area","Events","Flights","Rate")

# Get the subset of state data for excluede areas
new.areas = state.data[which(state.data$State.abb %in% excluded.msa.areas.abb),c("State","State_laser_events","State_air_carrier_ops", "Rate_per_100K" )]
colnames(new.areas) = c("Area","Events","Flights","Rate")

# Now combine them and print them out
combined.areas = rbind(combined.areas,new.areas)

# Now sort them by rate
combined.areas = combined.areas[order(combined.areas$Rate, decreasing = TRUE),]

cat('\n')
# Now print them out
paste("Combination of MSAs and states with no included MSAs")
combined.areas

# === END COMBINED MSA AND STATE COMPARISON ===

# === AIRILNE FLIGHT IDENTIFIER ANALYSIS
# Goal is to get a quick survey of the top flight identifiers,
#       specifically categorizing them by the three-letter ICAO code
#       at the beginning of the flight identifier
#
# Included events are those with a known value for the following
#       Flight_ID, Model, Altitude, Airport, City, State

included.flight.ids = which(complete.cases(laserhits[,c("Flight_ID", "Model", "Altitude", "Airport", "City", "State")]))
cat('\n')
paste("Of the ", format(nrow(laserhits), digits=5, big.mark = ","), " events, all but", format(nrow(laserhits) - length(included.flight.ids), digits=5, big.mark = ","), "had their flight IDs analyzed.")

cat('\n')
# Create a data frame of airline events and airline evnets per week
airline.event.summary = as.data.frame(sort(table(substr(laserhits$Flight_ID[included.flight.ids], start = 1, stop=3)), decreasing = TRUE)[1:20])
colnames(airline.event.summary) = c("Airline","Events")

# Create column of events per week
total.weeks = (as.numeric(max(laserhits$Date)) - as.numeric(min(laserhits$Date)))/7

airline.event.summary$`Weekly rate` = round(airline.event.summary$Events/total.weeks, digits = 2)

cat('\n')
print.data.frame(airline.event.summary, row.names=FALSE)

paste("The top 10 airlines in reported events per week were:")
total.weeks = (as.numeric(max(laserhits$Date)) - as.numeric(min(laserhits$Date)))/7
round((sort(table(substr(laserhits$Flight_ID[included.flight.ids], start = 1, stop=3)), decreasing = TRUE)[1:10])/total.weeks, digits=2)
rate.summary = round((sort(table(substr(laserhits$Flight_ID[included.flight.ids], start = 1, stop=3)), decreasing = TRUE)[1:10])/total.weeks, digits=2)

cat('\n')
paste("Based on the traffic from these airlines, additional MSAs may be added.")

cat('\n')
paste("From the top ten airlines, additional MSAs include Anchorage, Louisville, Memphis, and Salt Lake City")

# === END AIRILNE FLIGHT IDENTIFIER ANALYSIS

# === MISC TESTS ===
# Find all the cities with any word listed in all caps
# Note: May have multiple hits on one city, eg. LOS ANGELES
cap_cities = unlist(str_match_all(laserhits$City, "\\b[A-Z]+\\b"))

cat('\n')
paste("Table summarizing the prevalence of capitalized location names or parts of location names.")
table(cap_cities)

cat('\n')
date.test  = nrow(laserhits) == sum(sapply(laserhits$Date, function(x) !all(is.na(as.Date(as.character(x),format="%d-%b-%y")))))
ifelse(date.test, paste("At least one Date in non-Date format"), paste("All dates properly formatted"))
# ==== END OF MISC ===

# === DATA SUMMARIES ===

cat('\n')
paste("===Summaries of key all data from factor and numeric features===")
#apply(laserhits[,c(numeric.vars.ndx,factor.vars.ndx)],2,table, useNA = "always")

# Date data
cat('\n')
paste("Range of dates")
range(laserhits$Date)

cat('\n')
paste("Date summary")
summary(laserhits$Date)

# Histogram of events by year 
hist(laserhits$Year)

cat('\n')
# Table of events by year 
table(laserhits$Year, useNA = "no")
plot(as.factor(laserhits$Year), main = "Events by Year 2010-2018",xlab = "Year", col="dodgerblue")

# Table of events by month 
cat('\n')
paste("Table of events by month")
table(laserhits$Month, useNA = "no")
# plot(laserhits$Month, main = "Events by month 2010-2018",xlab = "Months", col="dodgerblue")

# Table of events by weekday 
cat('\n')
paste("Table of events by weekday")
table(laserhits$Weekday, useNA = "no")

# Table and histogram of events by hour (GMT) 
cat('\n')
paste("Table of events by hour (GMT)")
hist(as.numeric(laserhits$Hour_Category))
table(laserhits$Hour, useNA = "no")
plot(laserhits$Hour, main = "Events by hour (GMT)",xlab = "Hour", col="dodgerblue")



# Sorted table of top 100 aircraft models
cat('\n')
paste("Sorted table of aircraft models")
sort(table(laserhits$Model, useNA = "always"), decreasing=TRUE)[1:15]

# Sorted table of Aircraft_Type category
cat('\n')
paste("Sorted table of Aircraft_Type category")
table(laserhits$Aircraft_Type, useNA = "no")


# Table of events by weekday
cat('\n')
paste("Table of events by weekday")
table(laserhits$Weekday)
plot(laserhits$Weekday, main = "Events by day of the week",xlab = "Day")

# Table of injuries
cat('\n')
paste("Table of injuries")
table(laserhits$Injuries, useNA = "always")

# Sorted table of top 100 locations
cat('\n')
paste("Sorted table of top 100 locations")
sort(table(laserhits$Airport, useNA = "always"), decreasing=TRUE)[1:100]


# Save the data that will be further processed
write.csv(laserhits, file = "laserhits_playpen_airsafe.csv")

# === QUICK SUMMARY ===

cat('\n')
paste("The raw data contained", format(nrow(raw.laserhits), digits=5, big.mark = ","), 
      "records, but", format(nrow(raw.laserhits)-nrow(laserhits) , digits=5, big.mark = ","), 
      "of these records were eliminated from analysis due for various reasons, including duplications, missing data, or being out of the date range of interest",
      format(nrow(laserhits), big.mark=","), "were analyzed", 
      sep=" ")

cat('\n')
paste("In the period from ", min(laserhits$Date), " to ", max(laserhits$Date),
      ", there were ", format(nrow(laserhits), digits=5, big.mark = ","),
      " reported encounters where a laser beam affected one or more aircraft at or near ",
      format(length(unique(laserhits$Airport)), big.mark = ","), 
      " unique airports or other locations.", sep="") 

date.range = as.numeric(as.Date(max(laserhits$Date))) - as.numeric(as.Date(min(laserhits$Date))) + 1

cat('\n')
paste("During this period, there was an average of ",format(nrow(laserhits)/date.range, digits=3, big.mark = ","),
      " laser encounters per day, with as many as ", max(sort(table(laserhits$Date))), 
      " strikes in a single day. There were only ",
      date.range-length(table(laserhits$Date)), " days during this ",
      format(date.range, big.mark=","), "-day period with no reported laser strikes on aircraft in the United States", sep="")

cat('\n')
paste("In other words, during this period, on any given day in the United States, there was a ",
      format((length(unique(laserhits$Date))/date.range)*100, digits=3, big.mark = ","),
      "% chance that at least one aircraft reported a potentially dangerous encounter with a laser beam.", sep="")

# Dates with no reports
# Look for gaps (time differences) from day 2 until the last day
unique.dates = sort(unique(laserhits$Date))
date.gaps = as.numeric(unique.dates[2:(length(unique.dates))]-unique.dates[1:(length(unique.dates)-1)])
date.gaps.ndx = which(date.gaps>1)
# Print the dates before and after the gap and the size of the gap
cat('\n')
paste("The dates before and after the gap and the size of the gap")
paste(unique.dates[date.gaps.ndx],unique.dates[date.gaps.ndx+1],date.gaps[which(date.gaps>1)]-1)

# === END OF QUICK SUMMARY ===


# === EXPLORATORY DATA ANALYSIS === 
# Distribution, histogram, and summary of the number of daily laser encounters

# Create a complete table and histogram of days with x-amount of strike events
#  by adding a vector of zero values equal to the number of days with no strikes


daily.strikes=c(rep(0,date.range-length(table(laserhits$Date))),as.data.frame(table(laserhits$Date))[,2])

cat('\n')
paste("Table of daily strike frequencies.")
table(daily.strikes)

cat('\n')
paste("Summary of daily strike frequencies.")
summary(daily.strikes)

hist(daily.strikes, main="Distribution of reported laser encounters in a day",
     xlab="Number of strikes in a day", include.lowest=TRUE, col="dodgerblue")

# Note: the following version was used for data from years 2010-2018
# hist(daily.strikes, main="Distribution of reported laser encounters in a day",
#      xlab="Number of strikes in a day", breaks = seq(-1,35,by=1), include.lowest=TRUE, col="dodgerblue")

# Note: if you get a plotting error suggesting the areas is 
# too large, from RStudio console, use the command "dev.off()"


cat('\n')
paste("Table of daily strike by hour (UTC) of the day.")
table(laserhits$Hour_Category)

# Note that adding 0.001 to the vector of values helps to align the axes with
#       the hourly range of encounter.
# First must convert the Hour_Category data to numeric

Hour_num = as.numeric(as.character(laserhits$Hour_Category))
hist(Hour_num , main="Distribution of reported laser encounters by hour of the day",
     xlab="Local time (UTC)", breaks = seq(-1,24,by=1), include.lowest=TRUE, 
     xlim=c(-1,24),col="dodgerblue")



# Chi-square test to see if reports uniformly distributed throughout the week
cat('\n')
paste("Chi-square test to see if reports uniformly distributed throughout the week")
chisq.test(table(laserhits$Weekday))


# Histogram of events by year 
plot(as.factor(laserhits$Year), main="Laser encounters by year",
     ylab = "Reported encounters", xlab="Year", col="dodgerblue")

# Table of reported laser encounters by month of the year
cat('\n')
paste("Table of reported laser encounters by month of the year")
table(laserhits$Month)


# Plot of reported laser encounters by day of the week
plot(laserhits$Weekday, main="Distribution of number of laser encounters by day of the week",
     ylab = "Reported encounters", xlab="Day of the week", col="dodgerblue")

# Table of reported laser encounters by month of the year
cat('\n')
paste("Table of reported laser encounters by month of the year")
table(laserhits$Month)

# Chi-square test to see if reports uniformly distributed througout the year
cat('\n')
paste("Chi-square test to see if reports uniformly distributed throughout the months of the year")
chisq.test(table(laserhits$Month))

plot(laserhits$Month, main="Reported events by month: 2010-2018",
     ylab = "Reported encounters", xlab="Month of the year", col="dodgerblue")


# Distributions of laser encounters by hour of the day, day of the week, and month of the year
# Will use a combination of tables and heat maps to identify factors that are associated with
# relatively high or low numbers of strikes.

# Heat maps will use a color pallette that goes from white for lowest to dark blue for the highest value
palette = colorRampPalette(c('#ffffff','#0000ff'))(64)

# Combination #1: Months vs. Weekday

# Table of reported laser encounters by month of the year and day of the week
paste("Table of reported laser encounters by month of the year and day of the week")
table(laserhits$Month,laserhits$Weekday)

# Chi-square test to see if laser encounter reports uniformly distributed 
# for every combination of weekday and month 
chisq.test(table(laserhits$Month,laserhits$Weekday))

# === END EXPLORATORY DATA ANALYSIS ===

# === LASER ENCOUNTERS BY STATE ===
# state.tot = as.data.frame(table(laserhits$State)) 
# # Turns table output into a two-column data frame sorted alphabetically by state
# 
# # Rename the columns for clarity
# colnames(state.tot) = c("State","Events") 
# 
# # Ensure state variable is of type character
# state.tot$State = as.character(state.tot$State)

# Alphabetical list of states with laser totals

cat('\n')
paste("List of states and territories ordered by name, number of air carrier operations, number of laser encounters, and rate of laser encounters")

# States and territories ordered by name
cat('\n')
paste("States ordered alphabetically by state name")
state.data.ordered = state.data[order(state.data$State),c("State","State_air_carrier_ops", "State_laser_events","Rate_per_100K" )]

# Rename the column$s for clarity
colnames(state.data.ordered) = c("State","Operations (M)","Events", "Rate per 100K")

# Scale number of flight operations
state.data.ordered$`Operations (M)` = round(state.data.ordered$`Operations (M)` /1000000, digits=2)

cat('\n')
# States and territories ordered by name
print.data.frame(state.data.ordered, row.names=FALSE)

# States and territories ordered by air carrier operations
cat('\n')
paste("States and territories ordered by air carrier operations")
state.data.ordered = state.data.ordered[order(state.data.ordered$`Operations (M)`, decreasing=TRUE),] 
print.data.frame(state.data.ordered, row.names=FALSE)


# States and territories ordered by laser encounters
cat('\n')
paste("States and territories ordered by laser encounters")
state.data.ordered = state.data.ordered[order(state.data.ordered$Events, decreasing=TRUE),] 
print.data.frame(state.data.ordered, row.names=FALSE)


# States and territories ordered by laser encounter rate
cat('\n')
paste("States and territories ordered by laser encounter rate")
state.data.ordered = state.data.ordered[order(state.data.ordered$Rate, decreasing=TRUE),] 
print.data.frame(state.data.ordered, row.names=FALSE)

# === END LASER ENCOUNTERS BY STATE ===

# === THE DELAWARE OUTLIER ===
cat('\n')
paste("The state of Delaware stands out because of its very high rate of laser encounters. This is due to its relativley low number of air carrier operations.")

cat('\n')
paste("In the nine years of the study, Delaware had only", format(state.data$State_air_carrier_ops[state.data$State.abb=="DE"], big.mark = ","),"air carrier flight operations, or", round(state.data$State_air_carrier_ops[state.data$State.abb=="DE"]/date.range, digits=2), "flights per day.")

cat('\n')
paste("The next highest state or territory", as.character(state.data$State[which(state.data$State_air_carrier_ops == sort(state.data$State_air_carrier_ops)[2])]), "had", format(state.data$State_air_carrier_ops[which(state.data$State_air_carrier_ops == sort(state.data$State_air_carrier_ops)[2])], big.mark = ",")," air carrier flight operations, or", round(sort(state.data$State_air_carrier_ops)[2]/date.range, digits=2)," flights per day.")

cat('\n')
paste("The number of reported laser encounters in Delaware, was likely not due to air carrier operations within the state, but rather to Delaware's locaiton within an area of high air carrier activity in the northeast US. ")

# === END THE DELAWARE OUTLIER ===

# === TOP LOCATIONS BY STATE === 

# Summary of laser hits by locations (including airports) in a state. 
# A maxiumum of 10 airports are listed for each state

# Creating a list of summarized location information by state

us.list = NULL
cat('\n')
paste("Summary of reported laser hits by selected location in a state with a maximum of 10 locations listed per state.")


cat('\n')
paste("Note that these locations may places other than airports, such as heliports and navigational facilities.")
for(i in 1:nrow(state.data)){
        # Laser events in state i
        
        state.list = laserhits[as.character(laserhits$State) == as.character(state.data[i,1]),]
        
        
        # Orderd list of table of airport events in state i
        state.airports.ordered = as.data.frame(sort(table(state.list$Airport), decreasing=TRUE))
        colnames(state.airports.ordered) = NULL #c("Location","Events")
        
        # Printing out states and their airports (maximum of 10)
        cat("\n")
        state.airports.unique = length(unique(state.list$Airport)) # Unique airport names in state i
        if (state.airports.unique>10) cat(state.data[i,1],c(as.character(state.data[i,1]), " (Top 10 locations)"))
        if (state.airports.unique<=10 & state.airports.unique>1) cat(state.data[i,1],c(as.character(state.data[i,1]), " (All reporting locations)"))
        if(state.airports.unique==1) cat(state.data[i,1],c(as.character(state.data[i,1]), "(All reporting locations)\n")) # Ensures propor formatting for one-airport states
        # cat(state.tot[i,1],"\n")
        #print(as.character(state.data[i,1]))
        print(state.airports.ordered[1:min(state.airports.unique,10),]) # Prints a maximum of 10 airports
        
        # Creating a list of list where each list element is the state summary
        us.list[[i]] = list(State=state.data[i,1], Locations=state.airports.unique, Airports=state.airports.ordered)
}
cat('\n')

# === END TOP LOCATIONS BY STATE === 


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
cat('\n')
paste("Table of reported laser encounters by hour of the day and day of the week")

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
cat('\n')
paste("Table of reported laser encounters by hour of the day and month of the year")
table(laserhits$Hour,laserhits$Month)
cat('\n')

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



# === INPUT STATE ABBREVIATIONS ===
# Note that in ggplot2, the map function used here has the 48 states of the 
# continental US plus the district of Columbia
# For the map displays used in this study, only those 48 states plus the
# District of Columbia will be used in the rest of this analysis'
# However, the FAA air traffic data used in this analysis, inlcudes 
# several other areas in addition to the 50 states and the District of 
# Columbia. They were previously added earlier in this program.



# extra.abb = c("DC", "GU", "MP", "PR", "VI")
# extra.name = c("District of Columbia", "Guam", "Northern Mariana Is",
#                "Puerto Rico", "Virgin Islands")

# Now append them to the R-provided list of 50 state names and abbreviations
# every.abb = c(state.abb,extra.abb)
# every.state = c(state.name,extra.name)

# For ggplot, all but the lower 48 and DC will be included.
# Full set in usps.state.abb data frame, with the following columns:
#       States: usps.state.abb$State
#       Abbreviations: usps.state.abb$State_abb

# Will take out Alaska, Hawaii, Guam, Northern Mariana Islands, 
#       Puerto Rico, and Virgin Islands
used.abb = setdiff(usps.state.abb$State_abb, c("AK", "HI", "GU", "MP", "PR", "VI"))
used.state = setdiff(usps.state.abb$State, c("Alaska", "Hawaii", "Guam", 
                                    "Northern Mariana Islands","Puerto Rico",
                                    "Virgin Islands"))

# Will only use a combination of those rows in state.data that correspond 
#       to the use.state list

state.combo = state.data[which(state.data$State %in% used.state),]

# The data frame state.combo information that will be used to build four CONUS maps:
#       1. Events by state (lower 48 plus DC)
#       2. Air carrier traffic by state
#       3. Rate of laser events per 100K air carrier flights by state
#       4. Ratio of state event rate compared with the national rate




# The following gets the state boundary data into a data frame
# Regions happpen to be lower case state names
states <- map_data("state") 

# map_data is from ggplot2, creates data frame from information from the 
# 'maps' package which has a polygon associated with each state.

# ggplot2 likes their full state names (regions) in lower case, so a 
# new "regions" column in state.data will have values in lower case.
state.combo$region = tolower(state.combo$State)

# Each row corresponds to a state. 
# Create a new variable called 'region' which is the lower case
# version of each state name. 
# Note that ggplot2 related variables use lower case of state
# and the map of the states only includes 49 areas
# corresponding to the 48 states in the continental US plus
# the District of Columbia.
#
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

# Note that there are 50 states (regions) but 63 groups. 

# ========
gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=State_air_carrier_ops, group=group))
gg <- gg + labs(title = "1. US flight operations by state", 
                x="Darker colors imply a greater percentage of US flight operations", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg

# ========
gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=State_laser_events, group=group))
gg <- gg + labs(title = "2. Laser encounters by state", 
                x="Darker colors imply a greater percentage of laser encounters", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg

# ========

gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=Rate_per_100K, group=group))
gg <- gg + labs(title = "3. Laser events per 100K air carrier operations", 
                x="Darker colors imply higher risk of laser encounters", y=NULL)
gg = gg + theme(panel.border = element_blank()) # Get rid of lat/long background lines
gg = gg + theme(panel.background = element_blank()) # Get rid of default gray background
gg = gg + theme(axis.ticks = element_blank()) # Get rid of axis tic marks
gg = gg + theme(axis.text = element_blank()) # Ge rid of axis titles (lat and long values)
gg = gg + theme(plot.title = element_text(size = 19)) # Control font size of title
gg = gg + scale_fill_gradient(low="#e6eeff", high="#022b7e") # Control the fill (state) color
gg

# ========

gg <- ggplot(choro,   aes(x=long, y=lat)) + geom_polygon(aes(fill=Rate_per_100K_ratio_US, group=group))
gg <- gg + labs(title = "4. Ratio of state event rate and national rate", 
                x="Darker colors imply higher rate compared to national average", y=NULL)
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
# so will develop a ranked list of the top 10 areas with the highest 
# ratios percent of laser encounters divided by the percent of traffic.


paste("There were a total of", format(nrow(laserhits), big.mark = ","), 
      "reports, with",
      format(sum(!is.na(laserhits$Laser_color)), big.mark = ","),
      "of these also containing color information.")
cat('\n')
paste(round(100*sum(sort(table(laserhits$Laser_color), decreasing = TRUE)[1:5])/sum(!is.na(laserhits$Laser_color)), digits = 2),"%, or ", format(sum(sort(table(laserhits$Laser_color), decreasing = TRUE)[1:5]), big.mark = ","), " of these events involved the five most frequently reported colors.", sep="" )
cat('\n')


# Processing end time
timeEnd = Sys.time()

# Processing date and total processing time
cat(paste("","Processing end date and time",date(),"","",sep="\n"))
paste("Total processing time =",round(difftime(timeEnd,timeStart), digits=2),"seconds",sep=" ")

# Stop writing to an output file
sink()

# Copyright (c) 2019 Todd Curtis, All rights reserved
################