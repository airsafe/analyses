# Basic spike program
# Import data (data files online in directory http://www.airsafe.com/analyze/)

sessions.raw = NULL
sessions = NULL
range = 28
# Offset if we want to move the end of the 21 day range to (offset + 1) day prior to the day being measured
offset = 7
# Download raw session data
sessions.raw <- read.csv("http://airsafe.com/analyze/sessions.csv", header = TRUE)

# Ensure that working data is in a data frame 
sessions = as.data.frame(sessions.raw)

# Change the column names
colnames(sessions) = c("Date","Sessions")
colnames(sessions.raw) = c("Date","Sessions")

# Convert column of session values from factor to numeric
sessions$Sessions = as.numeric(as.character(sessions.raw$Sessions))
# Dates are in form 5/1/2006, must convert to a date format of yyyy-mm-dd
sessions$Date = as.Date(sessions.raw[,1], "%m/%d/%Y")


# Add columns for the mean and standard deviation of previous defined range of days of sessions and give them a default value to aid in identifying days without a spike measurment

sessions$date_index = -1
sessions$mean_range = -1
sessions$sd_range = -1
sessions$SpikeSD = -1
sessions$Spike2mean = -1


# This loop will compute each day's mean, and standard deviation for the previous range of days, starting
#        with the 22nd day of data      
for(i in (range + offset): nrow(sessions)) 
{
        sessions$date_index[i] = i-(range + offset) + 1
        sessions$mean_range[i] = mean(sessions$Sessions[(i-(range + offset) + 1):(i-offset)])
        sessions$sd_range[i] = sd(sessions$Sessions[(i-(range + offset) + 1 ):(i-offset)])
        sessions$Spike2mean[i] = sessions$Sessions[i]/sessions$mean_range[i] 
        sessions$SpikeSD[i] = (sessions$Sessions[i] - sessions$mean_range[i] )/sessions$sd_range[i]
}


# Summary and histogram of the sessions data

summary(sessions$Sessions)
summary(log(sessions$Sessions))
hist(sessions$Sessions, main="Historgram of session values", xlab="Number of sessions")
hist(log(sessions$Sessions), main="Historgram of log(session values)", xlab="log(Number of sessions)")

sessions$Spike =  round(sessions$SpikeSD, digits=2) >= 2 # Identifies as spike any SpikeSD of 2 or more

# Transfer only measurable spike days to a new data frame
measured.days = sessions[sessions$mean_range != -1,]

# Transfer only spike days to a new data frame
spike.days = sessions[sessions$Spike==TRUE,]
spike.days$Year = format(spike.days$Date, "%Y")
spike.days$Month = months(spike.days$Date)
spike.days$Day = weekdays(spike.days$Date)

# Redundant "Spike" column eliminated since all the values in spike.days would be TRUE
spike.days$Spike = NULL

write.csv(spike.days, file = "spike_days_not_the_study.csv")
write.csv(sessions, file="sessions_not_the_study.csv")


# Spikes in each year
table(spike.days$Year)

# Sorting number of spike days by year
# sort(table(spike.days$Year), decreasing = TRUE)

# Sorting number of spike days by month
sort(table(spike.days$Month), decreasing = TRUE)

# Sorting number of spike days by day of the week
sort(table(weekdays(spike.days$Date)), decreasing = TRUE)

# Summary of session and SD values from spike days
summary(spike.days$Sessions)
summary(spike.days$SpikeSD)

# Sorting number of spike days by day of the week
sort(table(weekdays(spike.days$Date)), decreasing = TRUE)

# Summary of session and SD values from spike days
summary(spike.days$Sessions)
summary(spike.days$SpikeSD)

# The top 10 days with the spikes with the greatest magnitude, specifically those days with a number of sessions 
#       with the largest number of standard deviations above the previous 21 days of sessions, were as follows:

# Reording by spike
# head(spike.days[order(spike.days$SpikeSD, decreasing=TRUE),], n=5)
spike.reordered = spike.days[order(spike.days$SpikeSD, decreasing=TRUE),]


# Just the top 10
print("Top 10 spikes")
head(spike.reordered[,c("Date", "Sessions","mean_range","SpikeSD", "Day")], n = 10)

print("Most recent spikes")
tail(spike.days[,c("Date", "Sessions","mean_range","SpikeSD", "Day")], n = 10)

# Overshadowed spikes: collection of days in the top 5% of all measured.days that have a 
#       FALSE spike flag, chose those in the top X percent

# Overshadowed in percentage - most.sessions are top 5% session values from measured.days
toprank = 5 # in percent
toprank = toprank/100 # in fraction
most.sessions = head(measured.days[order(measured.days$Sessions, decreasing=TRUE),],n=floor(nrow(measured.days)*toprank))
head(most.sessions,n=20)

# Top 10 overshadowed
head(most.sessions[most.sessions$Spike==FALSE,],n=10)
# only ones associated with a significant event and overshadowed by a previous event were 
# Comoros 30 June and 1 July 2009, CAL pilot event 17-18 June 2009, 5 Mar 2015, Harrison Ford/Delta LGA 

# MAKING A HEAT MAP

# Need to be in a matrix rather than data frame
spike.matrix = as.matrix(spike.days)

colnames(spike.matrix)
# Basic layout of spike days vs months and years are as follows
table(spike.days$Year,spike.days$Month) # months not ordered

# First, order the year
spike.ordered = spike.days[order(spike.days$Year),]

# Now make months factors and order them like the calendar
spike.ordered$Month = factor(spike.ordered$Month,levels=c("January","February","March", "April","May","June","July","August","September", "October","November","December"), ordered=TRUE)

# The table as numbers
table(spike.ordered$Month, spike.ordered$Year)

# Creating heat map with a cyan to purple color scheme  (option is heat colors using heat.colors) 

# When scaling by column (the year), the focus is on the months with a realtively high or low 
#  number of spikes compared to other months
heatmap(table(spike.ordered$Month, spike.ordered$Year),Rowv=NA, Colv=NA,revC=TRUE, scale="column", col = cm.colors(64), margins=c(9,10))

# When scaling by row (the month of the year), the focus is on the years with a realtively high or low 
#  number of spikes compared to other years. Clearly 2014 is the winner.
heatmap(table(spike.ordered$Month, spike.ordered$Year),Rowv=NA, Colv=NA,revC=TRUE, scale="row", col = cm.colors(64), margins=c(9,10))


# Now can do the same for days of the week by ordering them with Sunday being the first day
spike.ordered$Day = factor(spike.ordered$Day,levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"), ordered=TRUE)

# Scaling by column (the year) will highlight the days with a relatively high or low number of spikes
table(spike.ordered$Year,spike.ordered$Day)
heatmap(table(spike.ordered$Day, spike.ordered$Year),Rowv=NA, Colv=NA,revC=TRUE, scale="column", col = cm.colors(64), margins=c(9,11))

# Doing the same heatmap scaled by row (the day of the week) would highlight years with a realtively high or low 
#  number of spikes compared to other years. Clearly 2014 is the winner.
heatmap(table(spike.ordered$Day,spike.ordered$Year),Rowv=NA, Colv=NA,revC=TRUE, scale="row", col = cm.colors(64), margins=c(9,11))
