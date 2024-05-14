### SWIMM 2023 Rec fishers survey
### Manuel E. Coffill-Rivera
### started on 5/10/24
library(ggplot2)

## upload dataset WITH HEADERS ON
dat <- read.csv("C:\\Users\\manue\\OneDrive\\Desktop\\SWIMM data analyses\\Recreational Anglers (Pescadores recreativos)_March 15, 2024_12.00.csv", header = TRUE)

### Deleting unnecessary columns and rows
dat$StartDate <- NULL
dat$EndDate <- NULL
dat$Status <- NULL
dat$IPAddress <- NULL
dat$Duration..in.seconds. <- NULL
dat$Finished <- NULL
dat$RecordedDate <- NULL
dat$ResponseId <- NULL
dat$RecipientLastName <- NULL
dat$RecipientFirstName <- NULL
dat$RecipientEmail <- NULL
dat$ExternalReference <- NULL
dat$LocationLatitude <- NULL
dat$LocationLongitude <- NULL
dat$DistributionChannel <- NULL
dat$UserLanguage <- NULL
dat$Q_RecaptchaScore <- NULL

dat <- dat[-c(1,2),]

### delete surveys with progress = 0
zeroes <- dat[dat$Progress == 0,]
nrow(zeroes) # number of surveys with progress = 0

dat <- dat[dat$Progress > 0,]
nrow(dat) ## survey sample size after filtering for zeroes



################################################################################
## creating country-specific datasets for USA and MX based on Q44 asking where they reside
table(dat$Q44)

usa <- dat[dat$Q44 == "United States",]
nrow(usa) ## USA sample size

mex <- dat[dat$Q44 == "Mexico",]
nrow(mex) # MEX sample size




###############################################################################
#Q32 - What year were you born? [FINAL]
dat$Q32 = as.numeric(dat$Q32)

summary(dat$Q32) #Some people entered years wrong, need to fix

# Filter data to include birth years within the range of 1900 and 2023
yeardat <- dat[dat$Q32 >= 1900 & dat$Q32 <= 2023, ] #Fixed
summary(yeardat$Q32)
nrow(yeardat)
sum(is.na(yeardat$Q32))

nrow(yeardat) - sum(is.na(yeardat$Q32)) # n after excluding NAs

yeardat <- yeardat[complete.cases(yeardat[, 2]), ] # removing NAs


usayear <- yeardat[yeardat$Q44 == "United States",]
nrow(usayear) ## USA sample size
usayear["Country"] <- "USA"


mexyear <- yeardat[yeardat$Q44 == "Mexico",]
nrow(mexyear) # MEX sample size
mexyear["Country"] <- "Mex"

yeardat <- rbind(usayear, mexyear)
table(yeardat$Country)

colors <- c("#A73030FF","#0073C2FF")

#Histogram for birth years
ggplot(data = yeardat, aes(x = Q32, fill = Country)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_fill_manual(values=colors) +
  labs(title = "USA n=144, Mex n=84",
       x = "What year were you born?",
       y = "Count") +
  theme_classic()
#ggsave("q32grouped.png", width = 10, height = 8, dpi = 1000)



###############################################################################
#Q44 - What country do you currently reside in?
countrydat <- dat[complete.cases(dat[, 4]), ] # removing NAs
nrow(countrydat)
summary(countrydat$Q44)

countrydat$Q44 = as.factor(countrydat$Q44)
# Check levels 
levels(countrydat$Q44)
#Removing unecessary levels
countrydat$Q44 <- droplevels(countrydat$Q44, exclude = "")

table(countrydat$Q44)

jco <- c("#EFC000FF","#A73030FF","#7AA6DCFF","#0073C2FF")

# Bar plot for Q44
ggplot(data = countrydat[!is.na(countrydat$Q44), ], aes(x = Q44)) +
  geom_bar(fill = jco, color = "black") +
  #geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(x = "What country do you currently reside in?",
       y = "Count") +
  theme_classic()
#ggsave("q44.png", width = 10, height = 8, dpi = 1000)


################################################################################
#Q35 - Please indicate your gender.
gendat <- dat[complete.cases(dat[, 8]), ] # removing NAs
nrow(gendat)
summary(gendat$Q35)

gendat$Q35 = as.factor(gendat$Q35)
# Check levels 
levels(gendat$Q35)
#Removing unecessary levels
gendat$Q35 <- droplevels(gendat$Q35, exclude = "")

table(gendat$Q44)


usagen <- gendat[gendat$Q44 == "United States",]
nrow(usagen) ## USA sample size
usagen["Country"] <- "USA"
sum(!is.na(usagen$Q35))

mexgen <- gendat[gendat$Q44 == "Mexico",]
nrow(mexgen) # MEX sample size
mexgen["Country"] <- "Mex"
sum(!is.na(mexgen$Q35))
table(mexgen$Q35)

gendat <- rbind(usagen, mexgen)
table(gendat$Country)

table(gendat$Q35)
table(usagen$Q35)
table(mexgen$Q35)

sum(!is.na(usagen$Q35)) # n excluding NA
sum(!is.na(mexgen$Q35)) # n excluding NA

# Bar plot for Q35
ggplot(data = gendat[!is.na(gendat$Q35),], aes(x = Q35, fill = Country)) +
  geom_bar(color = "black",position = "dodge") +
  scale_fill_manual(values=colors) +
  #geom_text(stat='count', aes(label=..count..), vjust=-0.5, position = "identity") +  # Add numbers on top of bars
  labs(title = "USA n=127, Mex n=79",
       x = "Please indicate your gender",
       y = "Count") +
  theme_classic()
#ggsave("q35separated.png", width = 10, height = 8, dpi = 1000)





###############################################################################
#Q43 - What is the highest level of education you have completed?
edudat <- dat[complete.cases(dat[, 14]), ] # removing NAs
nrow(edudat)
summary(edudat$Q43)

edudat$Q43 = as.factor(edudat$Q43)
# Check levels 
levels(edudat$Q43)
#Removing unecessary levels
edudat$Q43 <- droplevels(edudat$Q43, exclude = "")

table(edudat$Q43)


usaedu <- edudat[edudat$Q44 == "United States",]
nrow(usaedu) ## USA sample size
usaedu["Country"] <- "USA"
sum(!is.na(usaedu$Q43))

mexedu <- edudat[edudat$Q44 == "Mexico",]
nrow(mexedu) # MEX sample size
mexedu["Country"] <- "Mex"
sum(!is.na(mexedu$Q43))
table(mexedu$Q43)

edudat <- rbind(usaedu, mexedu)
table(edudat$Country)

table(edudat$Q43)
table(usaedu$Q43)
table(mexedu$Q43)

sum(!is.na(usaedu$Q43)) # n excluding NA
sum(!is.na(mexedu$Q43)) # n excluding NA



edudat$Q43 = as.factor(edudat$Q43)

# Check levels 
levels(edudat$Q43)
#Removing unecessary levels
#dat$Q43 <- droplevels(dat$Q43, exclude = "NA")

library(stringr)
# Abbreviate the X labels
abbreviated_labels <- str_wrap(levels(edudat$Q43), width = 15)  # Adjust the width as needed

# Bar plot for highest level of education completed (Q43)
ggplot(data = edudat[!is.na(edudat$Q43),], aes(x = Q43, fill = Country)) +
  geom_bar(col = "black",position = "dodge") +
  scale_fill_manual(values=colors) +
  #geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=3) +  # Add count labels
  labs(title = "USA n=127, Mex n=76",
       x = "Highest level of education completed",
       y = "Count") +
  scale_x_discrete(labels = abbreviated_labels) +  # Use abbreviated labels
  theme_classic()
#ggsave("q43separated.png", width = 10, height = 8, dpi = 1000)

#-------------------------------------------------------------------------------
#Q69 - What is the highest level of education you have completed?
dat$Q69 = as.factor(dat$Q69)
table(dat$Q69)

# Not sure what to do with this question?
# is it an additional text option for Q43??? Need to ask Hannah



################################################################################
#Q40 - What is your current occupation? (Select all that apply) - Selected Choice
#Q40_8_TEXT - What is your current occupation? (Select all that apply) - Other  - Text
table(dat$Q40)
table(dat$Q40_8_TEXT) # looks like retired is the only one w > 3 entries

ocudat <- dat
ocudat <- ocudat[!(ocudat$Q40 == ""), ]
table(ocudat$Q40)
nrow(ocudat) # number of surveys that answered Q40
table(ocudat$Q44) # surveys by country

# options:  caregiver, fte, pte, full time student, part time student,
# i prefer not to answer, not employed, other,
# options in text: chef, empresario (self employed), retired, part time fishing guide

#################### usa
ocudatusa <- ocudat[ocudat$Q44 == "United States",]
nrow(ocudatusa) # sample size for usa

### caregiver
care <- ocudatusa[grep("Care", ocudatusa$Q40), ]
table(care$Q40)
nrow(care) #sample size of surveys that include caregiver

### fte
fte <- ocudatusa[grep("Full-time", ocudatusa$Q40), ]
table(fte$Q40)
nrow(fte) #sample size of surveys that include fte

### pte
pte <- ocudatusa[grep("Part-time", ocudatusa$Q40), ]
table(pte$Q40)
nrow(pte) #sample size of surveys that include pte

### fts
fts <- ocudatusa[grep("Full time student", ocudatusa$Q40), ]
table(fts$Q40)
nrow(fts) #sample size of surveys that include fts

### pts
pts <- ocudatusa[grep("Part time student", ocudatusa$Q40), ]
table(pts$Q40)
nrow(pts) #sample size of surveys that include pts

### i prefer not to answer
not <- ocudatusa[grep("I prefer not", ocudatusa$Q40), ]
table(not$Q40)
nrow(not) #sample size of surveys that include pts

### not employed
notemp <- ocudatusa[grep("Not employed", ocudatusa$Q40), ]
table(notemp$Q40)
nrow(notemp) #sample size of surveys that include pts

### other
other <- ocudatusa[grep("Other", ocudatusa$Q40), ]
table(other$Q40)
nrow(other) #sample size of surveys that include pts

## retired in text options
ret <- c("Pensionado","retired", "Retired", "RETIRED", "Retired ", "Retired Economist",
         "Retired military ")
# these are all of the names I saw in the text column that refer to retired
library(dplyr)
retired <- filter(ocudatusa, grepl(paste(ret, collapse='|'), Q40_8_TEXT))
table(retired$Q40_8_TEXT) # only column that should have any of the results

Occupation <- c("Caregiver", "FT employed","PT employed","FT student",
            "PT student","I prefer not to answer","Not employed",
            "Other","Retired")
OccupationN <-c(nrow(care), nrow(fte), nrow(pte), nrow(fts), nrow(pts), nrow(not),
            nrow(notemp), nrow(other), nrow(retired))
usaocu <- data.frame(Occupation, OccupationN)
usaocu["Country"] <- "USA"
usaocu


######################## mex
ocudatmex <- ocudat[ocudat$Q44 == "Mexico",]
nrow(ocudatmex) # sample size for mex

### caregiver
care <- ocudatmex[grep("Care", ocudatmex$Q40), ]
table(care$Q40)
nrow(care) #sample size of surveys that include caregiver

### fte
fte <- ocudatmex[grep("Full-time", ocudatmex$Q40), ]
table(fte$Q40)
nrow(fte) #sample size of surveys that include fte

### pte
pte <- ocudatmex[grep("Part-time", ocudatmex$Q40), ]
table(pte$Q40)
nrow(pte) #sample size of surveys that include pte

### fts
fts <- ocudatmex[grep("Full time student", ocudatmex$Q40), ]
table(fts$Q40)
nrow(fts) #sample size of surveys that include fts

### pts
pts <- ocudatmex[grep("Part time student", ocudatmex$Q40), ]
table(pts$Q40)
nrow(pts) #sample size of surveys that include pts

### i prefer not to answer
not <- ocudatmex[grep("I prefer not", ocudatmex$Q40), ]
table(not$Q40)
nrow(not) #sample size of surveys that include pts

### not employed
notemp <- ocudatmex[grep("Not employed", ocudatmex$Q40), ]
table(notemp$Q40)
nrow(notemp) #sample size of surveys that include pts

### other
other <- ocudatmex[grep("Other", ocudatmex$Q40), ]
table(other$Q40)
nrow(other) #sample size of surveys that include pts

## retired in text options
ret <- c("Pensionado","retired", "Retired", "RETIRED", "Retired ", "Retired Economist",
         "Retired military ")
# these are all of the names I saw in the text column that refer to retired
retired <- filter(ocudatmex, grepl(paste(ret, collapse='|'), Q40_8_TEXT))
table(retired$Q40_8_TEXT) # only column that should have any of the results

Occupation <- c("Caregiver", "FT employed","PT employed","FT student",
            "PT student","I prefer not to answer","Not employed",
            "Other","Retired")
OccupationN <-c(nrow(care), nrow(fte), nrow(pte), nrow(fts), nrow(pts), nrow(not),
            nrow(notemp), nrow(other), nrow(retired))
mexocu <- data.frame(Occupation,OccupationN)
mexocu["Country"] <- "Mex"
mexocu

q40final <- rbind(usaocu, mexocu)
q40final

nrow(ocudatusa)
nrow(ocudatmex)

ggplot(q40final, aes(x = Occupation, y = OccupationN, fill = Country)) +
  geom_bar(stat = "identity", color= "black",
           position = "dodge") +
  scale_fill_manual(values=colors) +
  labs(x = "What is your current occupation? Select all that apply"
       , y = "Count",title = "USA n=127, Mex n=76") +
  theme_classic()
#ggsave("q40.png", width = 10, height = 8, dpi = 1000)



################################################################################
#US.Sal - What was your total household income before taxes during the past 12 months?
ussal <- dat[!(dat$US.Sal == ""),]
table(ussal$US.Sal)
ussal$US.Sal <- as.factor(ussal$US.Sal)
ussal$US.Sal <- droplevels(ussal$US.Sal, exclude = "")
levels(ussal$US.Sal)
nrow(ussal)

ussalaries <- as.data.frame(table(ussal$US.Sal))
levels(ussalaries$Var1)
ussalaries

levels(ussalaries$Var1) <- c("$100,000 - 149,999",
                             "≥ $150,000",
                             "$25,000 - 49,999",
                             "$50,000 - 74,999",
                             "$75,000 - 99,999",
                             "I prefer not to answer",
                             "< $25,000")
ussalaries

ussalaries$Var1 <- factor(ussalaries$Var1, levels=c("< $25,000",
                                                    "$25,000 - 49,999",
                                                    "$50,000 - 74,999",
                                                    "$75,000 - 99,999",
                                                    "$100,000 - 149,999",
                                                    "≥ $150,000",
                                                    "I prefer not to answer"
                                                    ))

ussalaries

usasala <- ggplot(data=ussalaries, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#0073C2FF") +
  theme_classic() +
  labs(x = "What was your total household income before taxes during the past 12 months? (USD)",
       y = "Count",title = "USA n=128") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q16mex.png", width = 10, height = 8, dpi = 1000)



#Mex.Sal - What was your total household income during the past 12 months?
table(dat$Mex.Sal)

mexsal <- dat[!(dat$Mex.Sal == ""),]
table(mexsal$Mex.Sal)
mexsal$Mex.Sal <- as.factor(mexsal$Mex.Sal)
mexsal$Mex.Sal <- droplevels(mexsal$Mex.Sal, exclude = "")
levels(mexsal$Mex.Sal)
nrow(mexsal)

mexsalaries <- as.data.frame(table(mexsal$Mex.Sal))
levels(mexsalaries$Var1)
mexsalaries

10000 / 16.68 # $ 600
19000 / 16.68 # $ 1139
48000 / 16.68 # $ 2878

levels(mexsalaries$Var1) <- c("$600 - 1,139",
                             "$1,140 - 2,878",
                             "> $2,878",
                             "I prefer not to answer",
                             "< $600")
mexsalaries

mexsalaries$Var1 <- factor(mexsalaries$Var1, levels=c("< $600",
                                                      "$600 - 1,139",
                                                      "$1,140 - 2,878",
                                                      "> $2,878",
                                                      "I prefer not to answer"))

mexsalaries

  
mexsala <- ggplot(data=mexsalaries, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#A73030FF") +
  theme_classic() +
  labs(x = "What was your total household income during the past 12 months? (USD converted)",
       y = "Count",title = "Mex n=77") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q16mex.png", width = 10, height = 8, dpi = 1000)

library(ggpubr)
library(grid)

### merged plot. first remove individual axis labels
mexsalar <- mexsala + ylab(NULL)
usasalar <- usasala + ylab(NULL)

merged <- ggarrange(mexsalar, usasalar, nrow = 2)
annotate_figure(merged, left = textGrob("Count", rot = 90, vjust = 1, gp = gpar(cex = 1)))
#ggsave("salaries.png", width = 8, height = 10, dpi = 1000)



################################################################################
### Question 71: What type of angler do you identify as? (For example, deep sea fisherman, fly fisherman, etc.)

unique(usa$Q71)
unique(mex$Q71)

#Not sure what to do here because there are 90 unique values for USA and 48 unique values for MEX. Some are combined but might make more sense to do this cleaning once final results are in



################################################################################
## Question 3: How many years have you been practicing recreational fishing? (Please enter in number of years, example 10)

#Lets look at unique answers for the whole dataset
unique(dat$Q3)

#Remove NAs
dat_Q3 <- dat %>%
  filter(!is.na(dat$Q3) & dat$Q3 != "unanswered")

#Change column to numeric
dat_Q3$Q3 <- as.numeric(dat_Q3$Q3)
table(dat_Q3$Q3)

# Maximum, minimum, and average value for USA
max_dat <- max(dat_Q3$Q3)
#ANSWER: 70
min_dat <- min(dat_Q3$Q3)
#ANSWER: 1
average_dat <- mean(dat_Q3$Q3)
#ANSWER: 26.86

ggplot(dat_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing Overall",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")


#Lets look at the USA data
unique(usa$Q3)

# Remove NA and Unanswered from this column
usa_Q3 <- usa %>%
  filter(!is.na(usa$Q3) & usa$Q3 != "unanswered")

#Change column to numeric
usa_Q3$Q3 <- as.numeric(usa_Q3$Q3)

# Maximum, minimum, and average value for USA
max_usa <- max(usa_Q3$Q3)
#ANSWER: 70
min_usa <- min(usa_Q3$Q3)
#ANSWER: 2
average_usa <- mean(usa_Q3$Q3)
#ANSWER: 32.56

ggplot(usa_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing USA",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")

summary(usa_Q3$Q3) # NAs still present
usa_Q3 <- usa_Q3[!is.na(usa_Q3$Q3),] # Nas removed
usa_Q3["Country"] <- "USA"
nrow(usa_Q3) # sample size



#Lets look at the MEX data
unique(mex$Q3)

# Remove NA and Unanswer from this column
mex_Q3 <- mex %>%
  filter(!is.na(mex$Q3) & mex$Q3 != "unanswered")

#Change column to numeric
mex_Q3$Q3 <- as.numeric(mex_Q3$Q3)

# Maximum, minimum, and average value for USA
max_mex <- max(mex_Q3$Q3)
#ANSWER: 60
min_mex <- min(mex_Q3$Q3)
#ANSWER: 1
average_mex <- mean(mex_Q3$Q3)
#ANSWER:17.20

ggplot(mex_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing Mexico",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")

summary(mex_Q3$Q3) # NAs still present
mex_Q3 <- mex_Q3[!is.na(mex_Q3$Q3),] # Nas removed
mex_Q3["Country"] <- "Mex"
nrow(mex_Q3) # sample size

allq3 <- rbind(usa_Q3, mex_Q3)

ggplot(allq3, aes(x=Country, y=Q3, fill=Country)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  scale_fill_manual(values = colors) +
  stat_summary(fun.y=mean, geom="point", size=3) +
  theme_classic() +
  labs(title = "USA n=117, Mex n=69",
       y = "How many years have you been practicing recreational fishing?") +
  theme(legend.position="none")
#ggsave("q3.png", width = 8, height = 10, dpi = 1000)



###############################################################################
### Question 4: Is your boat...? (Select all that apply)
table(dat$Q4)

boatdat <- dat[!(dat$Q4 == ""), ]
table(boatdat$Q4)
nrow(boatdat) # number of surveys that answered Q4
table(boatdat$Q44) # surveys by country

# options:  borrowed, rented, charter service, do not fish on a boat, owned, 

#################### usa
boatdatusa <- boatdat[boatdat$Q44 == "United States",]
nrow(boatdatusa) # sample size for usa

### borrowed
borr <- boatdatusa[grep("Borr", boatdatusa$Q4), ]
table(borr$Q4)
nrow(borr) #sample size of surveys that include borrowed

### rented
rent <- boatdatusa[grep("Rent", boatdatusa$Q4), ]
table(rent$Q4)
nrow(rent) #sample size of surveys that include rented

### charter
char <- boatdatusa[grep("Char", boatdatusa$Q4), ]
table(char$Q4)
nrow(char) #sample size of surveys that include charter

### do not fish on a boat
donot <- boatdatusa[grep("Do not", boatdatusa$Q4), ]
table(donot$Q4)
nrow(donot) #sample size of surveys that include do not fish on boat

### owned
own <- boatdatusa[grep("Owned", boatdatusa$Q4), ]
table(own$Q4)
nrow(own) #sample size of surveys that include borrowed

boat <- c("Borrowed", "Rented", "Charter service", "Do not fish on a boat",
          "Owned")
boatn <-c(nrow(borr), nrow(rent), nrow(char), nrow(donot), nrow(own))
usaboat <- data.frame(boat, boatn)
usaboat["Country"] <- "USA"
usaboat



#################### mex
boatdatmex <- boatdat[boatdat$Q44 == "Mexico",]
nrow(boatdatmex) # sample size for mex

### borrowed
borr <- boatdatmex[grep("Borr", boatdatmex$Q4), ]
table(borr$Q4)
nrow(borr) #sample size of surveys that include borrowed

### rented
rent <- boatdatmex[grep("Rent", boatdatmex$Q4), ]
table(rent$Q4)
nrow(rent) #sample size of surveys that include rented

### charter
char <- boatdatmex[grep("Char", boatdatmex$Q4), ]
table(char$Q4)
nrow(char) #sample size of surveys that include charter

### do not fish on a boat
donot <- boatdatmex[grep("Do not", boatdatmex$Q4), ]
table(donot$Q4)
nrow(donot) #sample size of surveys that include do not fish on boat

### owned
own <- boatdatmex[grep("Owned", boatdatmex$Q4), ]
table(own$Q4)
nrow(own) #sample size of surveys that include borrowed

boat <- c("Borrowed", "Rented", "Charter service", "Do not fish on a boat",
          "Owned")
boatn <-c(nrow(borr), nrow(rent), nrow(char), nrow(donot), nrow(own))
mexboat <- data.frame(boat, boatn)
mexboat["Country"] <- "Mex"
mexboat



q4final <- rbind(usaboat, mexboat)
q4final

nrow(boatdatusa)
nrow(boatdatmex)

ggplot(q4final, aes(x = boat, y = boatn, fill = Country)) +
  geom_bar(stat = "identity", color= "black",
           position = "dodge") +
  scale_fill_manual(values=colors) +
  labs(x = "Is your boat... Select all that apply"
       , y = "Count",title = "USA n=125, Mex n=67") +
  theme_classic()
#ggsave("q4.png", width = 10, height = 8, dpi = 1000)



################################################################################
### Question 8: On average, how many days a year do you fish for recreational purposes? - Selected Choice
table(dat$Q8)
table(dat$Q8_5_TEXT)



usrec <- dat[dat$Q44 == "United States",]

usrec <- usrec[!(usrec$Q8 == ""),]
table(usrec$Q8)
usrec$Q8 <- as.factor(usrec$Q8)
levels(usrec$Q8)
nrow(usrec)

usrecdays <- as.data.frame(table(usrec$Q8))
levels(usrecdays$Var1)
usrecdays

levels(usrecdays$Var1) <- c("10 - 25","25 - 50","50 - 100","< 10","> 100")
usrecdays

usrecdays$Var1 <- factor(usrecdays$Var1, levels=c("< 10","10 - 25","25 - 50","50 - 100","> 100"))

usrecdays

ggplot(data=usrecdays, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#0073C2FF") +
  theme_classic() +
  labs(x = "On average, how many days a year do you fish for recreational purposes? Selected choice",
       y = "Count",title = "USA n=122") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("qqq.png", width = 10, height = 8, dpi = 1000)




mexrec <- dat[dat$Q44 == "Mexico",]

mexrec <- mexrec[!(mexrec$Q8 == ""),]
table(mexrec$Q8)
mexrec$Q8 <- as.factor(mexrec$Q8)
levels(mexrec$Q8)
nrow(mexrec)

mexrecdays <- as.data.frame(table(mexrec$Q8))
levels(mexrecdays$Var1)
mexrecdays

levels(mexrecdays$Var1) <- c("10 - 25","25 - 50","50 - 100","< 10","> 100")
mexrecdays

mexrecdays$Var1 <- factor(mexrecdays$Var1, levels=c("< 10","10 - 25","25 - 50","50 - 100","> 100"))

mexrecdays

ggplot(data=mexrecdays, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#A73030FF") +
  theme_classic() +
  labs(x = "On average, how many days a year do you fish for recreational purposes? Selected choice",
       y = "Count",title = "Mex n=66") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("qqq.png", width = 10, height = 8, dpi = 1000)



usrecdays["Country"] <- "USA"
mexrecdays["Country"] <- "Mex"

finalrecdays <- rbind(usrecdays, mexrecdays)

nrow(usrec)
nrow(mexrec)

ggplot(data=finalrecdays, aes(x=Var1, y=Freq, fill = Country)) +
  geom_bar(stat = "identity", color= "black",position = "dodge") +
  scale_fill_manual(values=colors) +
  theme_classic() +
  labs(x = "On average, how many days a year do you fish for recreational purposes?",
       y = "Count",title = "USA n=122, Mex n=66")
#ggsave("q8.png", width = 10, height = 8, dpi = 1000)





################################################################################
# How many days do you fish recreationally anywhere in the Gulf of Mexico?
table(dat$Q11)



usgom <- dat[dat$Q44 == "United States",]

usgom <- usgom[!(usgom$Q11 == ""),]
table(usgom$Q11)
usgom$Q11 <- as.factor(usgom$Q11)
levels(usgom$Q11)
nrow(usgom)

usgomdays <- as.data.frame(table(usgom$Q11))
levels(usgomdays$Var1)
usgomdays

levels(usgomdays$Var1) <- c("10 - 25","25 - 50","50 - 100","< 10","> 100")
usgomdays

usgomdays$Var1 <- factor(usgomdays$Var1, levels=c("< 10","10 - 25","25 - 50","50 - 100","> 100"))

usgomdays

ggplot(data=usgomdays, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#0073C2FF") +
  theme_classic() +
  labs(x = "How many days do you fish recreationally anywhere in the Gulf of Mexico?",
       y = "Count",title = "USA n=122") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("qqq.png", width = 10, height = 8, dpi = 1000)




mexgom <- dat[dat$Q44 == "Mexico",]

mexgom <- mexgom[!(mexgom$Q11 == ""),]
table(mexgom$Q11)
mexgom$Q11 <- as.factor(mexgom$Q11)
levels(mexgom$Q11)
nrow(mexgom)

mexgomdays <- as.data.frame(table(mexgom$Q11))
levels(mexgomdays$Var1)
mexgomdays

levels(mexgomdays$Var1) <- c("10 - 25","25 - 50","50 - 100","< 10","> 100")
mexgomdays

mexgomdays$Var1 <- factor(mexgomdays$Var1, levels=c("< 10","10 - 25","25 - 50","50 - 100","> 100"))

mexgomdays

ggplot(data=mexgomdays, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#A73030FF") +
  theme_classic() +
  labs(x = "How many days do you fish recreationally anywhere in the Gulf of Mexico?",
       y = "Count",title = "Mex n=65") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("qqq.png", width = 10, height = 8, dpi = 1000)



usgomdays["Country"] <- "USA"
mexgomdays["Country"] <- "Mex"

finalgomdays <- rbind(usgomdays, mexgomdays)

nrow(usgom)
nrow(mexgom)

ggplot(data=finalgomdays, aes(x=Var1, y=Freq, fill = Country)) +
  geom_bar(stat = "identity", color= "black",position = "dodge") +
  scale_fill_manual(values=colors) +
  theme_classic() +
  labs(x = "How many days do you fish recreationally anywhere in the Gulf of Mexico?",
       y = "Count",title = "USA n=122, Mex n=65")
#ggsave("q11.png", width = 10, height = 8, dpi = 1000)





###############################################################################
### Q14	Specifically, in what type of habitats do you like to fish? (Select all
### that apply) - Selected Choice
### Q14_9_TEXT	Specifically, in what type of habitats do you like to fish?
### (Select all that apply) - Other - Text
table(dat$Q14)
table(dat$Q14_9_TEXT)

q14answered <- dat[grep("Unanswered", dat$Q14, invert = TRUE), ]
table(q14answered$Q14)
nrow(q14answered) # number of surveys that answered Q14
table(q14answered$Q44) # surveys by country

### beach/shore
beach <- dat[grep("Beach", dat$Q14), ]
beach$Q14
nrow(beach) #sample size of surveys that include beach

### sandy bottom
sand <- dat[grep("Sand", dat$Q14), ]
sand$Q14
nrow(sand) #sample size of surveys that include sandy bottom

## seagrass
seagrass <- dat[grep("Seagrass", dat$Q14), ]
seagrass$Q14
nrow(seagrass) #sample size of surveys that include seagrass

## mangroves
mang <- dat[grep("Mang", dat$Q14), ]
mang$Q14
nrow(mang) #sample size of surveys that include mangroves

## rocky/reef
rocky <- dat[grep("Rocky", dat$Q14), ]
rocky$Q14
nrow(rocky) #sample size of surveys that include rocky/reef

## pier
pier <- dat[grep("Pier", dat$Q14), ]
pier$Q14
nrow(pier) #sample size of surveys that include pier

## pelagic
pelagic <- dat[grep("Pelagic", dat$Q14), ]
pelagic$Q14
nrow(pelagic) #sample size of surveys that include pelagic

## artificial reefs
artreef <- dat[grep("Artificial", dat$Q14), ]
artreef$Q14
nrow(artreef) #sample size of surveys that include


dat$Q14
table(dat$Q14_9_TEXT) ## nothing has more than 3 answers

### bar plot 
habs <- c("Beach/shore","Sandy bottom","Rocky/reef bottom","Pier or dock",
          "Seagrass","Mangroves","Artificial reefs","Pelagic")
habsn <- c(nrow(beach), nrow(sand), nrow(rocky), nrow(pier),nrow(seagrass),
           nrow(mang),nrow(artreef),nrow(pelagic))
q14habsall <- data.frame(habs, habsn)

ggplot(data=q14habsall, aes(x=habs, y=habsn)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "In what type of habitats do you like to fish?"
       , y = "Count",title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q14all.png", width = 10, height = 8, dpi = 1000)





#### usa
q14usa <- q14answered[q14answered$Q44 == "United States",]
table(q14usa$Q44) # sample size for usa

### beach/shore
beach <- q14usa[grep("Beach", q14usa$Q14), ]
beach$Q14
nrow(beach) #sample size of surveys that include beach

### sandy bottom
sand <- q14usa[grep("Sand", q14usa$Q14), ]
sand$Q14
nrow(sand) #sample size of surveys that include sandy bottom

## seagrass
seagrass <- q14usa[grep("Seagrass", q14usa$Q14), ]
seagrass$Q14
nrow(seagrass) #sample size of surveys that include seagrass

## mangroves
mang <- q14usa[grep("Mang", q14usa$Q14), ]
mang$Q14
nrow(mang) #sample size of surveys that include mangroves

## rocky/reef
rocky <- q14usa[grep("Rocky", q14usa$Q14), ]
rocky$Q14
nrow(rocky) #sample size of surveys that include rocky/reef

## pier
pier <- q14usa[grep("Pier", q14usa$Q14), ]
pier$Q14
nrow(pier) #sample size of surveys that include pier

## pelagic
pelagic <- q14usa[grep("Pelagic", q14usa$Q14), ]
pelagic$Q14
nrow(pelagic) #sample size of surveys that include pelagic

## artificial reefs
artreef <- q14usa[grep("Artificial", q14usa$Q14), ]
artreef$Q14
nrow(artreef) #sample size of surveys that include


q14usa$Q14
table(q14usa$Q14_9_TEXT) ## nothing has more than 3 answers

### bar plot 
habs <- c("Beach/shore","Sandy bottom","Rocky/reef bottom","Pier or dock",
          "Seagrass","Mangroves","Artificial reefs","Pelagic")
habsn <- c(nrow(beach), nrow(sand), nrow(rocky), nrow(pier),nrow(seagrass),
           nrow(mang),nrow(artreef),nrow(pelagic))
q14habsusa <- data.frame(habs, habsn)

ggplot(data=q14habsusa, aes(x=habs, y=habsn)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "In what type of habitats do you like to fish?"
       , y = "Count",title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q14usa.png", width = 10, height = 8, dpi = 1000)



#### mex
q14mex <- q14answered[q14answered$Q44 == "Mexico",]
table(q14mex$Q44) # sample size for mex

### beach/shore
beach <- q14mex[grep("Beach", q14mex$Q14), ]
beach$Q14
nrow(beach) #sample size of surveys that include beach

### sandy bottom
sand <- q14mex[grep("Sand", q14mex$Q14), ]
sand$Q14
nrow(sand) #sample size of surveys that include sandy bottom

## seagrass
seagrass <- q14mex[grep("Seagrass", q14mex$Q14), ]
seagrass$Q14
nrow(seagrass) #sample size of surveys that include seagrass

## mangroves
mang <- q14mex[grep("Mang", q14mex$Q14), ]
mang$Q14
nrow(mang) #sample size of surveys that include mangroves

## rocky/reef
rocky <- q14mex[grep("Rocky", q14mex$Q14), ]
rocky$Q14
nrow(rocky) #sample size of surveys that include rocky/reef

## pier
pier <- q14mex[grep("Pier", q14mex$Q14), ]
pier$Q14
nrow(pier) #sample size of surveys that include pier

## pelagic
pelagic <- q14mex[grep("Pelagic", q14mex$Q14), ]
pelagic$Q14
nrow(pelagic) #sample size of surveys that include pelagic

## artificial reefs
artreef <- q14mex[grep("Artificial", q14mex$Q14), ]
artreef$Q14
nrow(artreef) #sample size of surveys that include


q14mex$Q14
table(q14mex$Q14_9_TEXT) ## nothing has more than 3 answers

### bar plot 
habs <- c("Beach/shore","Sandy bottom","Rocky/reef bottom","Pier or dock",
          "Seagrass","Mangroves","Artificial reefs","Pelagic")
habsn <- c(nrow(beach), nrow(sand), nrow(rocky), nrow(pier),nrow(seagrass),
           nrow(mang),nrow(artreef),nrow(pelagic))
q14habsmex <- data.frame(habs, habsn)

ggplot(data=q14habsmex, aes(x=habs, y=habsn)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "In what type of habitats do you like to fish?"
       , y = "Count",title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q14mex.png", width = 10, height = 8, dpi = 1000)


################## grouped barplot by country EXCLUDING CUBA
q14habsusa
q14habsusa["Country"] <- "USA"


q14habsmex["Country"] <- "Mex"
q14habsmex

q14final <- rbind(q14habsusa,q14habsmex)
q14final


ggplot(q14final,
       aes(x = habs,
           y = habsn,
           fill = Country)) +
  geom_bar(stat = "identity", color= "black",
           position = "dodge") +
  scale_fill_manual(values=colors) +
  labs(x = "In what type of habitats do you like to fish? Select all that apply"
       , y = "Count",title = "USA n=106, Mex n=55") +
  theme_classic()
#ggsave("q14grouped.png", width = 10, height = 8, dpi = 1000)




### END Q14
###############################################################################
### Q15	What kind of fishing technique do you use? (Select all that apply) -
### Selected Choice
### Q15_6_TEXT	What kind of fishing technique do you use? (Select all that
### apply) - Other - Text
table(dat$Q15)
table(dat$Q15_6_TEXT)

q15answered <- dat[grep("Unanswered", dat$Q15, invert = TRUE), ]
table(q15answered$Q15)
nrow(q15answered) # number of surveys that answered Q15
table(q15answered$Q44) # surveys by country



### Spinning
spin <- dat[grep("Spinning", dat$Q15), ]
spin$Q15
nrow(spin) #sample size of surveys that include spinning

### Trolling
troll <- dat[grep("Trolling", dat$Q15), ]
troll$Q15
nrow(troll) #sample size of surveys that include trolling

### Hand line
hand <- dat[grep("Hand", dat$Q15), ]
hand$Q15
nrow(hand) #sample size of surveys that include hand line

### Fly Fishing
fly <- dat[grep("Fly", dat$Q15), ]
fly$Q15
nrow(fly) #sample size of surveys that include fly

### Spear fishing
spear <- dat[grep("Spear", dat$Q15), ]
spear$Q15
nrow(spear) #sample size of surveys that include spear

dat$Q15
table(dat$Q15_6_TEXT) ## nothing has more than 3 answers

### bar plot 
techs <- c("Spinning","Trolling","Hand line","Fly","Spear")
techsn <- c(nrow(spin),nrow(troll),nrow(hand),nrow(fly),nrow(spear))
q15techsall <- data.frame(techs,techsn)

ggplot(data=q15techsall, aes(x=techs, y=techsn)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "What kind of fishing technique do you use?",
       y = "Count",title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q15all.png", width = 10, height = 8, dpi = 1000)



### usa
q15answeredusa <- q15answered[q15answered$Q44 == "United States",]
nrow(q15answeredusa)

### Spinning
spin <- q15answeredusa[grep("Spinning", q15answeredusa$Q15), ]
spin$Q15
nrow(spin) #sample size of surveys that include spinning

### Trolling
troll <- q15answeredusa[grep("Trolling", q15answeredusa$Q15), ]
troll$Q15
nrow(troll) #sample size of surveys that include trolling

### Hand line
hand <- q15answeredusa[grep("Hand", q15answeredusa$Q15), ]
hand$Q15
nrow(hand) #sample size of surveys that include hand line

### Fly Fishing
fly <- q15answeredusa[grep("Fly", q15answeredusa$Q15), ]
fly$Q15
nrow(fly) #sample size of surveys that include fly

### Spear fishing
spear <- q15answeredusa[grep("Spear", q15answeredusa$Q15), ]
spear$Q15
nrow(spear) #sample size of surveys that include spear

q15answeredusa$Q15
table(q15answeredusa$Q15_6_TEXT) ## nothing has more than 3 answers

### bar plot 
techs <- c("Spinning","Trolling","Hand line","Fly","Spear")
techsn <- c(nrow(spin),nrow(troll),nrow(hand),nrow(fly),nrow(spear))
q15techsusa <- data.frame(techs,techsn)

ggplot(data=q15techsusa, aes(x=techs, y=techsn)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "What kind of fishing technique do you use?",
       y = "Count",title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q15usa.png", width = 10, height = 8, dpi = 1000)



#### mex
q15answeredmex <- q15answered[q15answered$Q44 == "Mexico",]
nrow(q15answeredmex)

### Spinning
spin <- q15answeredmex[grep("Spinning", q15answeredmex$Q15), ]
spin$Q15
nrow(spin) #sample size of surveys that include spinning

### Trolling
troll <- q15answeredmex[grep("Trolling", q15answeredmex$Q15), ]
troll$Q15
nrow(troll) #sample size of surveys that include trolling

### Hand line
hand <- q15answeredmex[grep("Hand", q15answeredmex$Q15), ]
hand$Q15
nrow(hand) #sample size of surveys that include hand line

### Fly Fishing
fly <- q15answeredmex[grep("Fly", q15answeredmex$Q15), ]
fly$Q15
nrow(fly) #sample size of surveys that include fly

### Spear fishing
spear <- q15answeredmex[grep("Spear", q15answeredmex$Q15), ]
spear$Q15
nrow(spear) #sample size of surveys that include spear

q15answeredmex$Q15
table(q15answeredmex$Q15_6_TEXT) ## nothing has more than 3 answers

### bar plot 
techs <- c("Spinning","Trolling","Hand line","Fly","Spear")
techsn <- c(nrow(spin),nrow(troll),nrow(hand),nrow(fly),nrow(spear))
q15techsmex <- data.frame(techs,techsn)

ggplot(data=q15techsmex, aes(x=techs, y=techsn)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "What kind of fishing technique do you use?",
       y = "Count",title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q15mex.png", width = 10, height = 8, dpi = 1000)



################## grouped barplot by country EXCLUDING CUBA
q15techsusa
q15techsusa["Country"] <- "USA"


q15techsmex["Country"] <- "Mex"
q15techsmex

q15final <- rbind(q15techsusa,q15techsmex)
q15final


ggplot(q15final,
       aes(x = techs,
           y = techsn,
           fill = Country)) +
  geom_bar(stat = "identity", color = "black",
           position = "dodge") +
  scale_fill_manual(values=colors) +
  labs(x = "What kind of fishing technique do you use? Select all that apply",
       y = "Count",title = "USA n=106, Mex n=55") +
  theme_classic()
#ggsave("q15grouped.png", width = 10, height = 8, dpi = 1000)







### END q15
################################################################################
### Q16	How many sport fishing tournaments do you attend each year? - Selected Choice
### Q16_5_TEXT	How many sport fishing tournaments do you attend each year? - 
### More than 20 - Text
table(dat$Q16)
table(dat$Q16_5_TEXT) # only one answer

q16answered <- dat[!(dat$Q16 == ""), ]
table(q16answered$Q16)
nrow(q16answered) # number of surveys that answered Q16
table(q16answered$Q44) # surveys by country

q16answersall <- as.data.frame(table(q16answered$Q16))
q16answersall$Var1 <- as.character(q16answersall$Var1)
q16answersall[3,1] <- "< 5"
q16answersall[4,1] <- "> 20"

q16answersall$Var1 <- as.factor(q16answersall$Var1)
levels(q16answersall$Var1)

levels(q16answersall$Var1) <- c("< 5","> 20","10 - 20","5 - 10","None")

q16answersall$Var1 <- factor(q16answersall$Var1, levels=c('None',
                                                          '< 5',
                                                          '5 - 10',
                                                          '10 - 20',
                                                          '> 20'))
q16answersall


ggplot(data=q16answersall, aes(x=Var1, y=Freq, fill = Q44)) +
  geom_bar(stat="identity", fill = "steelblue", position = "dodge") +
  theme_classic() +
  labs(x = "How many sport fishing tournaments do you attend each year?",
       y = "Count",title = "All n=162")
#ggsave("q16all.png", width = 10, height = 8, dpi = 1000)


#### usa
q16answeredusa <- q16answered[q16answered$Q44 == "United States",]
table(q16answeredusa$Q16)
nrow(q16answeredusa) #usa sample size

q16answersusa <- as.data.frame(table(q16answeredusa$Q16))
q16answersusa$Var1 <- as.character(q16answersusa$Var1)
q16answersusa
q16answersusa[3,1] <- "< 5"
q16answersusa[4,1] <- "> 20"

q16answersusa$Var1 <- as.factor(q16answersusa$Var1)
levels(q16answersusa$Var1)

levels(q16answersusa$Var1) <- c("< 5","> 20","10 - 20","5 - 10","None")


q16answersusa$Var1 <- factor(q16answersusa$Var1, levels=c('None',
                                                          '< 5',
                                                          '5 - 10',
                                                          '10 - 20',
                                                          '> 20'))
q16answersusa


ggplot(data=q16answersusa, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "How many sport fishing tournaments do you attend each year?",
       y = "Count",title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q16usa.png", width = 10, height = 8, dpi = 1000)



#### mex
q16answeredmex <- q16answered[q16answered$Q44 == "Mexico",]
nrow(q16answeredmex) #mex sample size

q16answersmex <- as.data.frame(table(q16answeredmex$Q16))
q16answersmex$Var1 <- as.character(q16answersmex$Var1)
q16answersmex
q16answersmex[3,1] <- "< 5"
q16answersmex[5,1] <- "> 20" # adding this category since there weren't any answers for it
q16answersmex[5,2] <- 0

q16answersmex$Var1 <- as.factor(q16answersmex$Var1)
levels(q16answersmex$Var1)

levels(q16answersmex$Var1) <- c("< 5","> 20","10 - 20","5 - 10","None")


q16answersmex$Var1 <- factor(q16answersmex$Var1, levels=c('None',
                                                          '< 5',
                                                          '5 - 10',
                                                          '10 - 20',
                                                          '> 20'))
q16answersmex

ggplot(data=q16answersmex, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "How many sport fishing tournaments do you attend each year?",
       y = "Count",title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q16mex.png", width = 10, height = 8, dpi = 1000)



##########3 merged 
q16answersusa["Country"] <- "USA"
q16answersmex["Country"] <- "Mex"

q16final <- rbind(q16answersusa, q16answersmex)
q16final

nrow(q16answeredusa)
nrow(q16answeredmex)

ggplot(q16final, aes(x = Var1, y = Freq, fill = Country)) +
  geom_bar(stat = "identity", color = "black",
           position = "dodge") +
  scale_fill_manual(values=colors) +
  labs(x = "How many sport fishing tournaments do you attend each year?",
       y = "Count",title = "USA n=122, Mex n=66") +
  theme_classic()
#ggsave("q16.png", width = 10, height = 8, dpi = 1000)





#### END q16
################################################################################
### Q17	In the last year, how much have you paid individually to enter fishing
### tournaments? - Selected Choice
### Q17_6_TEXT	In the last year, how much have you paid individually to enter 
### fishing tournaments? - More than $5,000 USD (Please specify) - Text
### *** usa only
table(dat$Q17)
table(dat$Q17_6_TEXT)

q17answered <- dat[!(dat$Q17 == ""), ]
table(q17answered$Q17)
nrow(q17answered) # number of surveys that answered Q17
table(q17answered$Q44) # surveys by country. confirms that all answers are from usa

q17usa <- q17answered[q17answered$Q44 == "United States",]
table(q17usa$Q17)
nrow(q17usa) #usa sample size

q17usa <- as.data.frame(table(q17usa$Q17))
q17usa$Var1 <- as.character(q17usa$Var1)
q17usa
q17usa[4,1] <- "< $250 USD"
q17usa[5,1] <- "> $5,000 USD"

q17usa$Var1 <- as.factor(q17usa$Var1)
levels(q17usa$Var1)

levels(q17usa$Var1) <- c("$2,500 - 5,000","$250 - 500","$500 - 1,000","< $250","> $5,000")


q17usa$Var1 <- factor(q17usa$Var1, levels=c("< $250","$250 - 500","$500 - 1,000","$2,500 - 5,000","> $5,000"))
q17usa

ustour <- ggplot(data=q17usa, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#0073C2FF") +
  theme_classic() +
  labs(x = "In the last year, how much have you paid individually to enter fishing tournaments? (USD)",
       y = "Count",title = "USA n=27") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q17usa.png", width = 10, height = 8, dpi = 1000)






### END q17
################################################################################
### Q51	In the last year, how much have you paid individually to enter fishing 
### tournaments? - Selected Choice
### Q51_6_TEXT	In the last year, how much have you paid individually to enter 
### fishing tournaments? - More than $10,000 MX (Please specify) - Text
### *** for MEX only
table(dat$Q51)
table(dat$Q51_6_TEXT) # no additional text entries

q51answered <- dat[!(dat$Q51 == ""), ]
table(q51answered$Q51)
nrow(q51answered) # number of surveys that answered Q51
table(q51answered$Q44) # surveys by country. confirms all answers are from MX

q51mex <- q51answered[q51answered$Q44 == "Mexico",]
table(q51mex$Q51)
nrow(q51mex) #mex sample size

q51mex <- as.data.frame(table(q51mex$Q51))
q51mex$Var1 <- as.character(q51mex$Var1)
q51mex
q51mex[5,1] <- "< $500 MX"
q51mex[6,1] <- "> $10,000 MX"

q51mex$Var1 <- as.factor(q51mex$Var1)
levels(q51mex$Var1)

1500 / 16.68
3000 / 16.68
5000 / 16.68
10000 / 16.68
500 / 16.68

levels(q51mex$Var1) <- c("$90 - 180","$180 - 300","$300 - 600","$30 - 90","< $30","> $600")

q51mex$Var1 <- factor(q51mex$Var1, levels=c("< $30","$30 - 90","$90 - 180","$180 - 300","$300 - 600", "> $600"))
q51mex

mextour <- ggplot(data=q51mex, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "#A73030FF") +
  theme_classic() +
  labs(x = "In the last year, how much have you paid individually to enter fishing tournaments? (USD converted)",
       y = "Count",title = "Mex n=36") + ylim(0, 15) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q51mex.png", width = 10, height = 8, dpi = 1000)


### merged plot. first remove individual axis labels
mextour <- mextour + ylab(NULL)
ustour <- ustour + ylab(NULL)

mergedtour <- ggarrange(mextour, ustour, nrow = 2)
annotate_figure(mergedtour, left = textGrob("Count", rot = 90, vjust = 1, gp = gpar(cex = 1)))
#ggsave("tournamentspending.png", width = 8, height = 10, dpi = 1000)





################################################################################
# What is the approximate amount of your total investment in fishing equipment? This includes your rods, hooks, lures, reels, clothing, shoes, nets, etc. (Please type number in $USD, for example 500)
# Q 19, 52, and 53
table(dat$Q19)
table(dat$Q52)
table(dat$Q53)

q19answered <- dat[!(dat$Q19 == ""), ]
table(q19answered$Q19)
nrow(q19answered) # number of surveys that answered Q19
table(q19answered$Q44) # surveys by country. confirms that all answers are from usa
q19usa <- as.data.frame(q19answered$Q19)
q19usa
q19usa$`q19answered$Q19` <- as.numeric(q19usa$`q19answered$Q19`)
q19usa["Country"] <- "USA"
q19usa
names(q19usa)[1] <- "inv"
summary(q19usa$inv)
library(plotrix)
std.error(q19usa$inv)

q52answered <- dat[!(dat$Q52 == ""), ]
table(q52answered$Q52)
nrow(q52answered) # number of surveys that answered Q52
table(q52answered$Q44) # surveys by country. confirms that all answers are from mx
q52mex <- as.data.frame(q52answered$Q52)
q52mex$`q52answered$Q52` <- as.numeric(q52mex$`q52answered$Q52`)
q52mex$`q52answered$Q52` <- q52mex$`q52answered$Q52` / 16.68
q52mex["Country"] <- "Mex"
q52mex
names(q52mex)[1] <- "inv"
summary(q52mex$inv)
std.error(q52mex$inv)

invest <- rbind(q19usa, q52mex)
summary(invest$inv)
invest[order(-invest$inv), ] # 3 usa outliers = 640k, 100k, and 50k

investminusoutlier <- invest[invest$inv < 50000,]

table(invest$Country)
table(investminusoutlier$Country) # confirming 3 usa values were removed

ggplot(investminusoutlier, aes(x=Country, y=inv, fill=Country)) + 
  geom_boxplot(width=0.5) +
  scale_fill_manual(values = colors) +
  stat_summary(fun.y=mean, geom="point", size=3) +
  theme_classic() +
  labs(title = "USA n=99, Mex n=52; 3 USA outliers (not displayed) = 50,000, 100,000, and 640,000",
       y = "What is the approximate amount of your total investment in fishing equipment? (USD)") +
  theme(legend.position="none")
#ggsave("investment.png", width = 8, height = 10, dpi = 1000)




################################################################################
# On average, how much have you spent in the last year on these items? (Please type number in $USD, for example 25) - Rods
#1 rods
#2 fishing nets
#3 fishing hooks
#4 lures
#5 live bait
#6 fishing line

table(dat$Q20_1)
q201answered <- dat[!(dat$Q20_1 == ""), ]
table(q201answered$Q44) # confirming all from usa
q201answered$Q20_1 <- as.numeric(q201answered$Q20_1)
usarods <- mean(q201answered$Q20_1)
usarodserror <- std.error(q201answered$Q20_1)
usarodsn <- nrow(q201answered)

table(dat$Q20_2)
q202answered <- dat[!(dat$Q20_2 == ""), ]
table(q202answered$Q44) # confirming all from usa
q202answered$Q20_2 <- as.numeric(q202answered$Q20_2)
usanets <- mean(q202answered$Q20_2)
usanetserror <- std.error(q202answered$Q20_2)
usanetn <- nrow(q202answered)

table(dat$Q20_3)
q203answered <- dat[!(dat$Q20_3 == ""), ]
table(q203answered$Q44) # confirming all from usa
q203answered$Q20_3 <- as.numeric(q203answered$Q20_3)
usahooks <- mean(q203answered$Q20_3)
usahookserror <- std.error(q203answered$Q20_3)
usahooksn <- nrow(q203answered)

table(dat$Q20_4)
q204answered <- dat[!(dat$Q20_4 == ""), ]
table(q204answered$Q44) # confirming all from usa
q204answered$Q20_4 <- as.numeric(q204answered$Q20_4)
usalures <- mean(q204answered$Q20_4)
usalureserror <- std.error(q204answered$Q20_4)
usaluresn <- nrow(q204answered)

table(dat$Q20_5)
q205answered <- dat[!(dat$Q20_5 == ""), ]
table(q205answered$Q44) # confirming all from usa
q205answered$Q20_5 <- as.numeric(q205answered$Q20_5)
usalive <- mean(q205answered$Q20_5)
usaliveerror <- std.error(q205answered$Q20_5)
usaliven <- nrow(q205answered)

table(dat$Q20_6)
q206answered <- dat[!(dat$Q20_6 == ""), ]
table(q206answered$Q44) # confirming all from usa
q206answered$Q20_6 <- as.numeric(q206answered$Q20_6)
usaline <- mean(q206answered$Q20_6)
usalineerror <- std.error(q206answered$Q20_6)
usalinen <- nrow(q206answered)


item <- c("Rods", "Nets", "Hooks","Lures", "Live bait", "Line")
mean <- c(usarods, usanets, usahooks, usalures, usalive, usaline)
error <- c(usarodserror, usanetserror, usahookserror, usalureserror, usaliveerror, usalineerror)
samplesize <- c(usarodsn, usanetn, usahooksn,usaluresn, usaliven, usalinen)

asdf <- as.data.frame(cbind(item, as.numeric(mean), as.numeric(error), as.numeric(samplesize)))
asdf

names(asdf) <- c("Item", "Mean", "SE","N")
names(asdf)

class(asdf$Item)
class(asdf$Mean)
class(asdf$SE)
class(asdf$N)
asdf$Item <- as.factor(asdf$Item)
asdf$Mean <- as.numeric(asdf$Mean)
asdf$SE <- as.numeric(asdf$SE)
asdf$N <- as.numeric(asdf$N)
asdf["Country"] <- "USA"
asdf

usaitems <- ggplot(asdf, aes(x=Item, y=Mean)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), fill = "#0073C2FF") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  geom_text(aes(label=N), vjust=-1,
            position = position_dodge(0.9), size=3.5) +
  labs(y="On average, how much have you spent in the last year on these items? (USD)")








##### mex part
#1 rods
#2 fishing nets
#3 fishing hooks
#4 lures
#5 live bait
#6 fishing line

table(dat$Q54_1)
q541answered <- dat[!(dat$Q54_1 == ""), ]
table(q541answered$Q44) # confirming all from mex
q541answered$Q54_1 <- as.numeric(q541answered$Q54_1)
q541answered$Q54_1 <- q541answered$Q54_1 / 16.68
mexrods <- mean(q541answered$Q54_1)
mexrodserror <- std.error(q541answered$Q54_1)
mexrodsn <- nrow(q541answered)

table(dat$Q54_2)
q542answered <- dat[!(dat$Q54_2 == ""), ]
table(q542answered$Q44) # confirming all from mex
q542answered$Q54_2 <- as.numeric(q542answered$Q54_2)
q542answered$Q54_2 <- q542answered$Q54_2 / 16.68
mexnets <- mean(q542answered$Q54_2)
mexnetserror <- std.error(q542answered$Q54_2)
mexnetn <- nrow(q542answered)

table(dat$Q54_3)
q543answered <- dat[!(dat$Q54_3 == ""), ]
table(q543answered$Q44) # confirming all from mex
q543answered$Q54_3 <- as.numeric(q543answered$Q54_3)
q543answered$Q54_3 <- q543answered$Q54_3 / 16.68
mexhooks <- mean(q543answered$Q54_3)
mexhookserror <- std.error(q543answered$Q54_3)
mexhooksn <- nrow(q543answered)

table(dat$Q54_4)
q544answered <- dat[!(dat$Q54_4 == ""), ]
table(q544answered$Q44) # confirming all from mex
q544answered$Q54_4 <- as.numeric(q544answered$Q54_4)
q544answered$Q54_4 <- q544answered$Q54_4 / 16.68
mexlures <- mean(q544answered$Q54_4)
mexlureserror <- std.error(q544answered$Q54_4)
mexluresn <- nrow(q544answered)

table(dat$Q54_5)
q545answered <- dat[!(dat$Q54_5 == ""), ]
table(q545answered$Q44) # confirming all from mex
q545answered$Q54_5 <- as.numeric(q545answered$Q54_5)
q545answered$Q54_5 <- q545answered$Q54_5 / 16.68
mexlive <- mean(q545answered$Q54_5)
mexliveerror <- std.error(q545answered$Q54_5)
mexliven <- nrow(q545answered)

table(dat$Q54_6)
q546answered <- dat[!(dat$Q54_6 == ""), ]
table(q546answered$Q44) # confirming all from mex
q546answered$Q54_6 <- as.numeric(q546answered$Q54_6)
q546answered$Q54_6 <- q546answered$Q54_6 / 16.68
mexline <- mean(q546answered$Q54_6)
mexlineerror <- std.error(q546answered$Q54_6)
mexlinen <- nrow(q546answered)


item <- c("Rods", "Nets", "Hooks","Lures", "Live bait", "Line")
mean <- c(mexrods, mexnets, mexhooks, mexlures, mexlive, mexline)
error <- c(mexrodserror, mexnetserror, mexhookserror, mexlureserror, mexliveerror, mexlineerror)
samplesize <- c(mexrodsn, mexnetn, mexhooksn,mexluresn, mexliven, mexlinen)

asdfr <- as.data.frame(cbind(item, as.numeric(mean), as.numeric(error), as.numeric(samplesize)))
asdfr

names(asdfr) <- c("Item", "Mean", "SE","N")
names(asdfr)

class(asdfr$Item)
class(asdfr$Mean)
class(asdfr$SE)
class(asdfr$N)
asdfr$Item <- as.factor(asdfr$Item)
asdfr$Mean <- as.numeric(asdfr$Mean)
asdfr$SE <- as.numeric(asdfr$SE)
asdfr$N <- as.numeric(asdfr$N)
asdfr["Country"] <- "Mex"
asdfr

mexitems <- ggplot(asdfr, aes(x=Item, y=Mean)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), fill="#A73030FF") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  geom_text(aes(label=N), vjust=-1,
            position = position_dodge(0.9), size=3.5) +
  labs(y="On average, how much have you spent in the last year on these items? (USD)",
       title = "On average, how much have you spent in the last year on these items? (USD)")



finalitems <- rbind(asdf,asdfr)

ggplot(finalitems, aes(x=Item, y=Mean, fill=Country)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  geom_text(aes(label=N), vjust=-1,
            position = position_dodge(0.9), size=3.5) +
  labs(y="On average, how much have you spent in the last year on these items? (USD)")
# not great. mx values are hard to see



### merged plot. first remove individual axis labels
mexitems <- mexitems + ylab(NULL) + xlab(NULL)
usaitems <- usaitems + ylab(NULL)

mergeditems <- ggarrange(mexitems, usaitems, nrow = 2)
mergeditems
#annotate_figure(mergeditems, left = textGrob("", rot = 90, vjust = 1.5, gp = gpar(cex = 1)))
ggsave("itemspending.png", width = 8, height = 10, dpi = 1000)



