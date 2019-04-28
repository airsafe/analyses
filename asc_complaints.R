# Exploration of complaint file

# ADMINISTRATIVE NOTES
# Note: To describe database at any point, use str(*name*)
# Note: To clear R workspace, use rm(list = ls())
# Note: Searching R help files - RSiteSearch("character string")
# Note: To clear console, use CTRL + L 

# PURPOSE
# The goal of this exercise was to take an edited version of the contents of
# the AirSafe.com complaint database and do some basic exploratory analysis of
# the data. The information was downloaded in early January 2016 and
# consists of all of the complaints submitted from late May 2012 to early
# January 2016. The pre-processing step included removing duplicate entries
# and consolidating information apparently submitted in one or more submissions
# that were actually referring to the same complaint.

# The first step is to install new packages that will be needed for the analysis.

options(repos = c(CRAN = "http://cran.rstudio.com"))
if("e1071" %in% rownames(installed.packages()) == FALSE) 
{install.packages("e1071")}
library(e1071)

# Note that raw data was pre-processed to exclude non-English content
complaints.raw = read.csv("asc_complaints.csv")
complaints=complaints.raw

colnames(complaints)


# There are 14 variables (column names) for the raw data:
# 1. Timestamp - Date and time of submission                            
# 2. Name                                   
# 3. Address                              
# 4. City                                  
# 5. State.Province                        
# 6. Country                               
# 7. Email                                
# 8. Phone                                  
# 9. Airline                                
# 10. Flight.Number                         
# 11. Location.Flight.Leg                    
# 12. Date - Date of occurrcene                                 
# 13. Complaint.Categories                  
# 14. Please.include.additional.details.below
# 
# Will rename several columns
colnames(complaints)[colnames(complaints)=="Please.include.additional.details.below"] = "Notes"
colnames(complaints)[colnames(complaints)=="Complaint.Categories"] = "Categories"
colnames(complaints)[colnames(complaints)=="State.Province"] = "State"
colnames(complaints)[colnames(complaints)=="Flight.Number"] = "Flight"
colnames(complaints)[colnames(complaints)=="Location.Flight.Leg"] = "Location"

# All except Timestamp and Date should be of type character. Will start by 
# making them all character

# Note that the '[]' keeps it as data frame and does not make it a list 
complaints[] = lapply(complaints, as.character)

# Change Timestamp to as.POSIXlt which has elements in a list
complaints$Time = as.POSIXlt(complaints$Timestamp, format="%m/%d/%Y %H:%M:%S")
complaints$Year = complaints$Time$year + 1900 # Years indexed from 1900
complaints$Month = complaints$Time$mon + 1 # Months indexed from zero
# Convert months from character to numeric 
complaints$Month = as.numeric(as.character(complaints$Month))
# Convert to month
complaints$Month = month.abb[complaints$Month]

# Data overview
paste("There were a total of ",format(nrow(complaints), big.mark = ","),
      " unique complaints in the database. The earliest record was from ", 
      as.Date(min(range(complaints$Time))),
      " and the latest from ", as.Date(max(range(complaints$Time))), ".", 
      sep = "")

# Make months factors and order them as they are in a calendar
complaints$Month = factor(complaints$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                                                  "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)


# Extract the day of the week from the Time variable
# complaints$Day = complaints$Time$mday
complaints$Day = weekdays(complaints$Time, abbreviate = TRUE)

# Make days into factors and order them as they are in a calendar
complaints$Day = factor(complaints$Day,levels=c("Sun","Mon","Tue",
                                                      "Wed","Thu","Fri","Sat"), ordered=TRUE)

complaints$Hour = complaints$Time$hour


barplot(table(complaints$Year),
        main = "Number of complaints by year",
        xlab = "Year",
        ylab = "Number of complaints",
        las = 1,
        col = "dodgerblue")

barplot(table(complaints$Day),
        main = "Complaints by day of the week",
        xlab = "Day of the week",
        ylab = "number of complaints",
        las = 1,
        col = "dodgerblue")

barplot(table(complaints$Month),
        main = "Complaints by month of the year",
        xlab = "Month of the year",
        ylab = "number of complaints",
        las = 1,
        cex.names=0.9,
        col = "dodgerblue")

barplot(table(complaints$Hour),
        main = "Complaints by hour when submitted",
        xlab = "Time of day of submission",
        ylab = "number of complaints",
        las = 1,
        cex.names=0.6,
        col = "dodgerblue")

# Will also add a column that has a word count for the note associated
# with each complaint.

# The following splits each notes by non-word breaks (\W in regular expressions)
# and counts them using length(), and uses vapply to make it a vector
# of length nrow(complaints), which becomes the new variable 'Note_length

complaints$Note_length = vapply(strsplit(complaints$Notes,"\\W+"),length,integer(1))

# Summary statistics

paste("Of the",format(length(complaints$Note_length), big.mark = ","), "complaints, only",
      sum(complaints$Note_length==0),"did not leave some kind of explanatory a note.", sep=" ")


# Cumulative distribution of number of words used in the notes section
plot.ecdf(complaints$Note_length, 
          main = "Cumulative distribution of number of words used in Notes section",
          xlab="Number of words", 
          ylab="Cumulative probabilities")

# Note length
print("Note length varied widely, with most being between 250 and 1000 words, roughly equivalent to one to five typewritten pages.")
paste("Of the", format(nrow(complaints), big.mark = ","), "complaints," , sep = " ")
paste(" -", format(sum(complaints$Note_length==0), big.mark = ","), "left no notes,",sep = " ")
paste(" -", format(sum(complaints$Note_length>0 & complaints$Note_length < 250), big.mark = ","), "left notes 1 to 250 words long,",sep = " ")
paste(" -", format(sum(complaints$Note_length>250 & complaints$Note_length < 1000), big.mark = ","), "left notes 251 to 1,000 words long, and",sep = " ")
paste(" -", format(sum(complaints$Note_length >1000), big.mark = ","), "left notes over 1,000 words long.",sep = " ")
# There are 17 categories in the complaint form, and one can select
# more than one:
# 1. Delays or other Flight Problems
# 2. Checked or carry on baggage
# 3. Reservations/Boarding/Ground Services
# 4. Cancellations
# 5. Fares/Refunds/Online Booking
# 6. In flight services/Meals
# 7. Safety
# 8. Security/Airport Secreening
# 9. Overbooking
# 10. Customer Service
# 11. Frequent Flyer Programs
# 12. Discrimination
# 13. Disability
# 14. Travel with children
# 15. Travel with pets
# 16. Passenger behavior
# 17. Other

# Category names used in complaint form
category_names = c("Delays or other Flight Problems",
                   "Checked or carry on baggage",
                   "Reservations/Boarding/Ground Services",
                   "Cancellations",
                   "Fares/Refunds/Online Booking",
                   "In flight services/Meals",
                   "Safety",
                   "Security/Airport Secreening",
                   "Overbooking",
                   "Customer Service",
                   "Frequent Flyer Programs",
                   "Discrimination",
                   "Disability",
                   "Travel with children",
                   "Travel with pets",
                   "Passenger behavior",
                   "Other")

# Make category names R friendly column names
category.vars = make.names(category_names)

# Insert binary variables inidcating which category is associated 
# with each complaint
for (i in 1:length(category_names)){
      xx = as.numeric(grepl(category_names[i],complaints$Categories))
      complaints = cbind(complaints,xx)
      # Add appropriate R-friendly variable name to new column
      names(complaints)[ncol(complaints)] = category.vars[i]
}

# Determine how many categories were checked in each complaint
complaints$cat_checked = apply(complaints[,(colnames(complaints) %in% category.vars)],1,sum)

# Insert binary variables inidcating which category is associated 
# with each complaint
cat_used = NULL
for (i in 1:length(category_names)){
      xx = as.numeric(grepl(category_names[i],complaints$Categories))
      cat_used[i] = sum(xx) # How many times this category used  
}
# Test data frame with number of times each category used
cat_used = cbind(category_names,as.numeric(cat_used))
rownames(cat_used) = NULL
colnames(cat_used) = c("Category","Uses")

# Distribution of  categories used only once
cat_used_once = NULL
for (i in 1:length(category_names)){
      xx = as.numeric(grepl(category_names[i],complaints$Categories[which(complaints$cat_checked==1)]))
      cat_used_once[i] = sum(xx) # How many times this category used  
}
# Append this vector to cat used
cat_used = cbind(cat_used,cat_used_once)

colnames(cat_used) = c("Category","Used","Used_once")
cat_used = as.data.frame(cat_used)
cat_used$Used = as.numeric(as.character(cat_used$Used))
cat_used$Used_once = as.numeric(as.character(cat_used$Used_once))
cat_used = cat_used[cat_used$Used>0,]
cat_used$Ratio = round((cat_used$Used_once/cat_used$Used),3)
paste(nrow(cat_used), "of", length(category_names),"categories used as the only category checked at least one time.",sep=" ")

# Category used sorted, with ratio of how often that category
# was the only one checked
cat.order = order(cat_used$Used, decreasing = TRUE)
cat_used_sorted = cat_used[cat.order,]
rownames(cat_used_sorted) = NULL
print(cat_used_sorted[,1:4])

# Correlation of checkboxes where only one category checked
# solo_check = complaints[complaints$cat_checked==1,]
# 
# cor(complaints[,which(colnames(solo_check) %in% category.vars)]) 

# Choose a dependent variable based on categories
# and run predictions based on words used

# Now let's deal with the comments
# Install new packages
options(repos = c(CRAN = "http://cran.rstudio.com"))
if("tm" %in% rownames(installed.packages()) == FALSE) 
{install.packages("tm")}
library(tm)

# SnowballC is a word stemming algorithm for collapsing 
# words to a common root to aid comparison of vocabulary. 

if("SnowballC" %in% rownames(installed.packages()) == FALSE) 
{install.packages("SnowballC")}
library(SnowballC)

# Install Twitter reading package for fun
# if("twitteR" %in% rownames(installed.packages()) == FALSE) 
# {install.packages("twitteR")}
# library(twitteR)
# setup_twitter_oauth("kCYGJInM6evrkwADSySrkTroL", "JnOkxROeVHiTsiNKfrPFc7LHwEReoDwdQk5buUdmPS98xCayTY")
# 
# 
# tweets = userTimeline("airsafe", n=100)


# Create corpus
# Build a corpus, and specify the source to be character vectors
corpus = Corpus(VectorSource(complaints$Notes))

# Look at corpus
corpus
# corpus[[55]]

# Convert all words to lower case

# Creates both lower case and meta data
corpus_trans = tm_map(corpus,content_transformer(tolower)) 

# Creates lower case content without meta data
corpus = tm_map(corpus, tolower) 
# corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing 
# (it converts corpus to a Plain Text Document). 
# This is a recent change having to do with the tolower function that 
# occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument) # Convert to plain text document
corpus = tm_map(corpus, removePunctuation) # Remove punctuation
corpus = tm_map(corpus, removeNumbers) # Remove numbers

# Remove stopwords and popular air travel words
# which will leave o corpus of words more likely to be related to 
# the subject matter of the comlaint
corpus = tm_map(corpus, removeWords, c('flight',
                                       'air',
                                       'airport',
                                       'airline',
                                       'airlines',
                                       stopwords("english")))

corpus <- tm_map(corpus, stripWhitespace) # Strip whitespace
# corpus[[1]]

# Stem document (removes variations, kees only the root of words)
# corpus = tm_map(corpus, stemDocument)
# corpus[[1]]

# Create matrix
# Will now create a matrix of all the words used in the Notes section
# where the previos steps filtered out many common words

frequencies = DocumentTermMatrix(corpus)

# Will now add column to complaints data frame that will show
# How many filtered words are in each complaint
word_counts.row =  rowSums(as.matrix(frequencies)) # Number of times each word appears?
# word_counts.col =  colSums(as.matrix(frequencies)) # Number of words with each document
# Add this to the complaints data frame
complaints$Note_length_dtm = as.numeric(as.character(word_counts.row))

# Now will look only at the most common or popular words
# Will include only those words that occur in at least 3% of the complaints
frequencies.common = removeSparseTerms(frequencies, 0.97) 

# now we have a data frame of which popular words occur
# in each document, meaning they occur in at least 3% of all the complaints
most.pop = as.data.frame(as.matrix(frequencies.common)) 

# Before we looked at words for each note, now will count both
# popular words for each note, plus number of times each word occurs
word_counts.row_pop =  rowSums(as.matrix(frequencies.common)) # Number of times each word appears?
word_counts.col_pop =  colSums(as.matrix(frequencies.common)) # Number of words with each document

# Add the popular words to the complaints data frame
complaints$Note_popular_words = as.numeric(as.character(word_counts.row_pop))

# Now create, then sort, a new data frame of the mos popular words
most.popular.words = as.data.frame(word_counts.col_pop)
names(most.popular.words) = "Wordcount"

# Row names are the words, will make that a new column, and 
# get rid of the rownames
most.popular.words$Word  = rownames(most.popular.words)
rownames(most.popular.words) = NULL

pop.order = order(most.popular.words$Wordcount, decreasing = TRUE)
most.popular.words.ordered = most.popular.words[pop.order,]
rownames(most.popular.words.ordered) = NULL

# This takes care of some odd cases where the DocumentTermLength transformation
# results in more words that the plain text. This can happen for some non-English
# text content such as arabic
pos_note = which(complaints$Note_length>0 & complaints$Note_length_dtm )


# The note ratio is the ratio of number of words after the 
# document term process divided by the original number of words.
note_ratio = complaints$Note_length_dtm/complaints$Note_length
note_ratio_pop  = complaints$Note_popular_words/complaints$Note_length

# The first histogram gives the distribution of the ratio of
# filtered words to unfiltered words for all the notes
hist(note_ratio, 
     xlim=c(0,1), ylim=c(0,1000), 
     main="Ratio of filtered words to all words in Notes",
     xlab = "Ratio", col = rgb(0.8,0.1,0.1,0.5))

print("Summary of ratio of filtered words over total words in a Note")
summary(note_ratio)

print("In this second histogram, the ratio is for a Note's words that are both filtered and used in 3% of complaints over all words.")
# The second histogram gives the distribution of the ratio of
# filtered and popular (used in at least 3% of complaints) 
# words to unfiltered words for all the notes
hist(note_ratio_pop, 
     xlim=c(0,1), ylim=c(0,1000), 
     main="Ratio of filtered and popular words to all words in Notes",
     xlab = "Ratio", col=rgb(0.1,0.1,0.8,0.5))

print("Summary of ratio of filtered and popular words over total words in a Note")
summary(note_ratio_pop)


# Combining the two distributions in an overlapping way
hist(note_ratio, col= rgb(0.8,0.1,0.1,0.5), 
     xlim=c(0,1), ylim=c(0,1000), 
     main="Overlapping Histograms of ratios of filtered words", 
     xlab="Ratio")
hist(note_ratio_pop, col=rgb(0.1,0.1,0.8,0.5), add=T)
box()


if("wordcloud" %in% rownames(installed.packages()) == FALSE) 
{install.packages("wordcloud")}
library(wordcloud)

# look at top 20 words and word cloud of top 100
print("Top 20 words used")
print(most.popular.words.ordered[1:20,2:1],row.names = FALSE)

print("Word cloud of top 100 most used filtered words")
wordcloud(corpus, scale=c(2.5,0.25), 
          max.words=100, 
          random.order=FALSE)


# Note: no analysis from this point forward. What appears below
# is the outline of steps needed to create more consistency in how
# airline names were used in the complaints.

# Function for removing muliple spaces

multispace <- function(x){  
      x = gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl=TRUE) 
      return(x)
}


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

# Remove leading and trailing space characters from selected variables,
# as well as multiple spaces


# for (i in 1:ncol(complaints)) {
#       complaints[,i] = multispace(complaints[,i])
#       complaints[,i] = stripper(complaints[,i])
# }

complaints[] = lapply(complaints,multispace)
complaints[] = lapply(complaints,stripper)

# Function capitalizes first letter of each word
simpleCap <- function(x) {
      s = tolower(x)
      s = strsplit(s, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
}

# Run the simpleCap function and create a new variable
# called "Carrier"

# complaints$Carrier = sapply(complaints$Airline,simpleCap)

# Review of raw data showed a variety of spelling options for 
# Airlines. The following will collapse the varieties into
# something more tractable by using 'grep' function to match
# key character strings (all exact matches)
# "Spirit Air" Spirit_Airlines
# "Singapor" to "Singapore_Airlines"
# "u.s. airway", complaints$Carrier, ignore.case=TRUE
# "United ex"), complaints$Carrier, ignore.case = TRUE United_Express
# "United "), complaints$Carrier, ignore.case = TRUE to United
# "Us "  complaints$Carrier, ignore.case = FALSE)
# "Air India" to Air_India
# "American Airline" to American
# "British Air" British_Airways
# "Virgin Austra" to Virgin_Australia
# "Virgin Atlantic" to Virgin_Atlantic
# "Virgin Amer" to Virgin_America
# "Virgin Airlines" to Virgin
# "Southwest Air" to Southwest_Airlines
# "Saudi" to Saudia
# Qat to Qatar_Air
# "West Je" to WestJet
# "Usa"  to US_Airways
# "ppine to Philippine_Airlines"
# "Us Air Ways" to US_Airways
# "Usair" to US_Airways
# "Usairways" to US_Airways
# "Us Airways" to American
# "Aer Lingus/" | "Aerlingus" to Aer_Lingus
# "Aero Mexico" to Aeromexico
# "Argentin" to Aerolineas_Argentinas
# "Air Canad" to Air_Canada
# "Air Franc" to Air_France
# "ish Air" to British Airways
# "Cathay" Cathay_Pacific
# "China Air" to China_Airlines
# "China Eastern" to China_Eastern
# "China Southern" to China_Southern
# "Copa Air" to Copa
# "Delta" to Delta
# "Air Franc" to Air_France
# "Alaska" to Alaska
# "Alitali" to Alitalia
# "Alle" to Allegiant
# "Egypt" to EgyptAir
# "El Al" to El_Al
# "West Je" to "WestJet"
# "Westjet" to "WestJeat"
# Malaysia to Malaysia_Airlines
# hansa to Lufthansa
# tsst=grep("Delta", complaints$Carrier, ignore.case = TRUE)) find airline ndx
