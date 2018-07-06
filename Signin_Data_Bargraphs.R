## R-Ladies Chicago Sign In Data
#  Some basic visualizations for data collected during meetup sign ins 
#  Imports data from google form spreadsheet (access to this is required for this to work)
#  Code to export these images as .tiff files is at the end of the file.


# Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tibble)

#rladies_palette <- c("#88398A","#D3D3D3","#181818","#562457","#88398A") # From R-Ladies Global Github (has black as one of the colors)
rladies_palette <- c("#88398A","#D3D3D3","#C5B4E3","#562457","#7E5475") # Slightly modified to get rid of black as sa fill color

# Get data from google form
url <- 'https://docs.google.com/spreadsheets/d/1IYW5Dp46dm8uA15K49LscJHt329OWD7n2vBMlLj5WsI/export?format=csv&id=1IYW5Dp46dm8uA15K49LscJHt329OWD7n2vBMlLj5WsI&gid=1300416886'
signin <-read.csv(url)

# Look at data
head(signin)
dim(signin)
colnames(signin) <- c("Timestamp","Name","Email","Potential_Speaker","Potential_Organizer","Mentor","Mentee","R_Level","Random_Acts_of_Coffee")

# Clean Data
rl_signin <- signin[,1:9]
head(rl_signin)

rl_signin <- rl_signin %>%
  separate(Timestamp,c("Date","Time")," ") %>% # Two separate columns for date & time
  filter(Date != '3/12/2018') %>% # Who signed in on a day that isn't a meetup???
  mutate(Date = as.Date(Date, format = '%m/%d/%Y')) # Make date column read as a date

# Double check that nothing looks funny
head(rl_signin) 
class(rl_signin$Date)
table(rl_signin$Date)

##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ##### ATTENDANCE FOR EACH MEET UP ##### #####
# Generate bar chart
rl_attendance_bar <- ggplot(rl_signin,aes(x=as.factor(Date))) +
  geom_bar(colour = "black", fill="#88398A",width=0.6) +
  geom_text(data=rl_signin, stat='count', aes(label=..count..), vjust=-1) +
  xlab("Meetup Date") +
  ylab("Number of Sign Ins") +
  ggtitle("Attendance by Meetup") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Display figure
rl_attendance_bar # exported to .tiff 1200 by 700 (code to export all figures is at the end of script)

## Generate line plot with the same data
#  Make it easier by being able to define the y axis
by_date <- rl_signin %>%
  group_by(Date) %>%
  summarise(counts = n()) 
by_date

# Generate line plot
rl_attendance_line <- ggplot(by_date,aes(x=as.factor(Date),y=counts, group=1)) +
  geom_line(linetype="dashed",colour="#612141") +
  geom_point(colour="#88398A",size=2.5) +
  geom_text(data=by_date,aes(label=counts), vjust=-1) +
  xlab("Meetup Date") +
  ylab("Number of Sign Ins") +
  ggtitle("Attendance by Meetup") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

# Display figure
rl_attendance_line

##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ##### ATTENDANCE BY R EXPERIENCE LEVEL ##### ##### 

# Ony use meetups that have this data 
# (earlier meetups did not ask this question at sign in)
rl_levels <- rl_signin[rl_signin$Date >= as.Date(as.character('2018-04-26')),]
table(rl_levels$Date)

# Reorder R-Levels 
rl_levels <- within(rl_levels,
             R_Level <- factor(R_Level,levels=c('Beginner','Intermediate Beginner','Intermediate','Intermediate Advanced','Advanced')))

# Generate Figure
rl_r_levels <- ggplot(rl_levels %>% group_by(Date) %>% 
                        count(Date, R_Level) %>%
                        mutate(pct=n/sum(n)),
  aes(as.factor(Date), n, fill=as.factor(R_Level))) +
  geom_bar(color = "black", stat="identity", width=0.7) +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),position=position_stack(vjust=0.5)) +
  xlab("Meetup Date") +
  ylab("Number of Sign Ins") +
  labs(title = "Meetup Attendance",
       subtitle = "Subdivided by self-reported experience in R",
       caption = "Note: This information is unavailable for Meetups prior to 2018-04-26") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=rladies_palette,name='Level in R')

# Display figure
rl_r_levels # exported to .tiff 900 by 700

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ##### ATTENDANCE BY NEW VS. RETURNING GUESTS ##### #####
## How many people come back? How many people are new at each meetup?

## Reformat "Name" column so that name is not case sensitive. 
#  Note that this tries to account for two entries that are likely to be the same person
#  I.E. "amy yang" and "Amy Yang" are now counted as the same person.
#  This won't account for people who don't include their last names on some sign ins
#  I.E. "Amy" and "Amy Yang" won't count as the same person even if they are. 
rl_signin <- add_column(rl_signin,tolower(rl_signin$Name)) # Make all names lowercase
rl_signin[,11] <- gsub(" ", "", rl_signin[,11], fixed = TRUE) # Get rid of all spaces
colnames(rl_signin)[11] <- "Name_code" # Rename column

rl_signin$Repeat <- rep(0,nrow(rl_signin)) # Make an empty row for a Repeat code
head(rl_signin) # Double check

## Make master lists for each event
#  The master list will have all attendees for the current event and all events preceding it
mr_July2017 <- subset(rl_signin,as.character(rl_signin$Date) == as.character('2017-07-18'))
mr_Sept2017 <- rl_signin[rl_signin$Date < as.Date(as.character('2017-09-29')),]
mr_Oct2017 <- rl_signin[rl_signin$Date <= as.Date(as.character('2017-10-26')),]
mr_Nov2017 <- rl_signin[rl_signin$Date <= as.Date(as.character('2017-11-16')),]
mr_Dec2017 <- rl_signin[rl_signin$Date <= as.Date(as.character('2017-12-13')),]
mr_Feb2018 <- rl_signin[rl_signin$Date <= as.Date(as.character('2018-02-13')),]
mr_Mar2018 <- rl_signin[rl_signin$Date <= as.Date(as.character('2018-03-08')),]
mr_Apr2018 <- rl_signin[rl_signin$Date <= as.Date(as.character('2018-04-26')),]
mr_May2018 <- rl_signin[rl_signin$Date <= as.Date(as.character('2018-05-22')),]
mr_Jul2018 <- rl_signin[rl_signin$Date <= as.Date(as.character('2018-07-02')),]

## Make lists of attendees for each event
#  This list will only have attendees for this single event
July2017 <- subset(rl_signin,as.character(rl_signin$Date) == as.character('2017-07-18'))
Sept2017 <- rl_signin[rl_signin$Date == as.Date(as.character('2017-09-28')),]
Oct2017 <- rl_signin[rl_signin$Date == as.Date(as.character('2017-10-26')),]
Nov2017 <- rl_signin[rl_signin$Date == as.Date(as.character('2017-11-16')),]
Dec2017 <- rl_signin[rl_signin$Date == as.Date(as.character('2017-12-13')),]
Feb2018 <- rl_signin[rl_signin$Date == as.Date(as.character('2018-02-13')),]
Mar2018 <- rl_signin[rl_signin$Date == as.Date(as.character('2018-03-08')),]
Apr2018 <- rl_signin[rl_signin$Date == as.Date(as.character('2018-04-26')),]
May2018 <- rl_signin[rl_signin$Date == as.Date(as.character('2018-05-22')),]
Jul2018 <- rl_signin[rl_signin$Date == as.Date(as.character('2018-07-02')),]

## Fill the Repeat Columns 
#  This looks at the name code column for each event and compares it to the correct master list
#  Each for loop is for a separate event (we skipped July 2017 in this because it's the first meeting and everyone is new)
#  This is a terrible way to do this and there is likely a better way.

# September 2017 Meetup
num = 1:nrow(Sept2017)
for (i in num) {
  Sept2017$Repeat[i] <- ifelse(match(Sept2017$Name_code[i],mr_July2017$Name_code,nomatch=0),1,0)
}

# October 2017 Meetup
num = 1:nrow(Oct2017)
for (i in num) {
  Oct2017$Repeat[i] <- ifelse(match(Oct2017$Name_code[i],mr_Sept2017$Name_code,nomatch=0),1,0)
}

# November 2017 Meetup
num = 1:nrow(Nov2017)
for (i in num) {
  Nov2017$Repeat[i] <- ifelse(match(Nov2017$Name_code[i],mr_Oct2017$Name_code,nomatch=0),1,0)
}

# December 2017 Meetup
num = 1:nrow(Dec2017)
for (i in num) {
  Dec2017$Repeat[i] <- ifelse(match(Dec2017$Name_code[i],mr_Nov2017$Name_code,nomatch=0),1,0)
}

# February 2018 Meetup
num = 1:nrow(Feb2018)
for (i in num) {
  Feb2018$Repeat[i] <- ifelse(match(Feb2018$Name_code[i],mr_Dec2017$Name_code,nomatch=0),1,0)
}

# March 2018 Meetup 
num = 1:nrow(Mar2018)
for (i in num) {
  Mar2018$Repeat[i] <- ifelse(match(Mar2018$Name_code[i],mr_Feb2018$Name_code,nomatch=0),1,0)
}

# April 2018 Meetup
num = 1:nrow(Apr2018)
for (i in num) {
  Apr2018$Repeat[i] <- ifelse(match(Apr2018$Name_code[i],mr_Mar2018$Name_code,nomatch=0),1,0)
}

# May 2018 Meetup 
num = 1:nrow(May2018)
for (i in num) {
  May2018$Repeat[i] <- ifelse(match(May2018$Name_code[i],mr_Apr2018$Name_code,nomatch=0),1,0)
}

# July 2018 Meetup
num = 1:nrow(Jul2018)
for (i in num) {
  Jul2018$Repeat[i] <- ifelse(match(Jul2018$Name_code[i],mr_May2018$Name_code,nomatch=0),1,0)
}

## Create a new dataframe that combines all the lists
rl_repeats <- rbind(July2017,Sept2017,Oct2017,Nov2017,Dec2017,Feb2018,Mar2018,Apr2018,May2018,Jul2018)
rl_repeats

## Double check that new item has all the data
dim(rl_repeats) # New data frame
dim(rl_signin) # Old data frame - do they match in dimensions? (they should have the same number of rows)

## Generate Figure 
rl_guests <- ggplot(rl_repeats %>% group_by(Date) %>% 
         count(Date, Repeat) %>%
         mutate(pct=n/sum(n)),
       aes(as.factor(Date), n, fill=as.factor(Repeat))) +
  geom_bar(color = "black", stat="identity", width=0.7) +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),position=position_stack(vjust=0.5)) +
  xlab("Meetup Date") +
  ylab("Number of Sign Ins") +
  labs(title = "Meetup Attendance",
       subtitle = "Subdivided by new and returning members") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.9, .9)) +
  scale_fill_manual(values=c("#C5B4E3","#88398A"),name='Guest',labels=c("New","Returning"))

# Display figure
rl_guests # exported to .tiff 1200 by 700


##### ##### LOOKING AT NEW VS. RETURNING BEGINNERS ##### #####
## Do beginners comeback? 

# Subset data for self-reported beginners only
beginners_only <- subset(rl_repeats,rl_repeats$R_Level=="Beginner")
beginners_only

# Generate figure
rl_beginners <- ggplot(beginners_only %>% group_by(Date) %>% 
                      count(Date, Repeat) %>%
                      mutate(pct=n/sum(n)),
                    aes(as.factor(Date), n, fill=as.factor(Repeat))) +
  geom_bar(color = "black", stat="identity", width=0.7) +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")),position=position_stack(vjust=0.5)) +
  xlab("Meetup Date") +
  ylab("Number of Sign Ins") +
  labs(title = "Meetup Attendance - Beginners Only",
       subtitle = "Beginner R-Ladies subdivided by new and returning members") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(.15, .87)) +
  scale_fill_manual(values=c("#C5B4E3","#88398A"),name='Guest',labels=c("New","Returning"))

# Display figure
rl_beginners # exported to .tiff 700 by 600


##### ##### ##### ##### ##### ##### 
##### ##### SAVE FIGURES ##### #####
tiff(file="RLChicago_Meetup_Attendance_Totals.tiff",width=3400,height=2100,unit="px",res=300)
rl_attendance_bar
dev.off()

tiff(file="RLChicago_Meetup_Attendance_Line.tiff",width=3400,height=2100,unit="px",res=300)
rl_attendance_line
dev.off()

tiff(file="RLChicago_Meetup_by_R_level.tiff",width=3000,height=2100,unit="px",res=300)
rl_r_levels
dev.off()

tiff(file="RLChicago_Meetup_new_v_returning_guest.tiff",width=3400,height=2100,unit="px",res=300)
rl_guests
dev.off()

tiff(file="RLChicago_Meetup_R_level_of_Beginners.tiff",width=2400,height=2100,unit="px",res=300)
rl_beginners
dev.off()

