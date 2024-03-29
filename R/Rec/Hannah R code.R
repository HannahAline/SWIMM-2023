### SWIMM 2023 Rec fishers survey
### Hannah Henry

## upload dataset 
dat <- read.csv(file.choose())


#Add headers
names(dat) ### column headers of data file
head(dat) ### column headers of data file plus first 6 rows of data 
summary(dat) ### summary statistics for datum

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

# Remove rows 1 and 2
dat <- dat[-c(1, 2), ]

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

################################################################################
# Load necessary library for plotting
library(ggplot2)

# Generate a blue scale color palette
blue_scale_colors <- colorRampPalette(c("lightblue", "darkblue"))(5)
################################################################################

#Question by question cleaning and graphs

#-------------------------------------------------------------------------------
#Q32 - What year were you born? 
dat$Q32 = as.numeric(dat$Q32)

summary(dat$Q32) #Some people entered years wrong, need to fix

# Filter data to include birth years within the range of 1900 and 2023
dat <- dat[dat$Q32 >= 1900 & dat$Q32 <= 2023, ] #Fixed

#Histogram for birth years
ggplot(data = dat, aes(x = Q32)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Birth Years",
       x = "Birth Year",
       y = "Count") +
  theme_minimal()
#-------------------------------------------------------------------------------
#Q66 - What country are you currently in, and/or where do you do most of your fishing?
dat$Q66 = as.factor(dat$Q66)
# Check levels 
levels(dat$Q66)
#Removing unecessary levels
dat$Q66 <- droplevels(dat$Q66, exclude = "")
dat$Q66 <- droplevels(dat$Q66, exclude = "What country are you currently in, and/or where do you do most of your fishing?")

# Bar plot for the distribution of respondents across countries
ggplot(data = dat[!is.na(dat$Q66), ], aes(x = Q66, fill = Q66)) +
  geom_bar() +
  labs(title = "Distribution of Respondents by Country",
       x = "Country",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = blue_scale_colors)
#-------------------------------------------------------------------------------
#Q44 - What country do you currently reside in?
dat$Q44 = as.factor(dat$Q44)
# Check levels 
levels(dat$Q44)
#Removing unecessary levels
dat$Q44 <- droplevels(dat$Q44, exclude = "")
dat$Q44 <- droplevels(dat$Q44, exclude = "What country do you currently reside in?")

# Bar plot for the distribution of respondents across countries of residence
ggplot(data = dat[!is.na(dat$Q44), ], aes(x = Q44, fill = Q44)) +
  geom_bar() +
  labs(title = "Distribution of Respondents by Country of Residence",
       x = "Country",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = blue_scale_colors)
#-------------------------------------------------------------------------------
#Q67 - Please indicate your race. - Selected Choice
dat$Q67 = as.factor(dat$Q67)
# Check levels 
levels(dat$Q67)
#Removing unecessary levels
dat$Q67 <- droplevels(dat$Q67, exclude = "")
dat$Q67 <- droplevels(dat$Q67, exclude = "Please indicate your race. - Selected Choice")

#Bar plot for the distribution of respondents across races
ggplot(data = dat[!is.na(dat$Q67), ], aes(x = Q67, fill = Q67)) +
  geom_bar() +
  labs(title = "Distribution of Respondents by Race",
       x = "Race",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = blue_scale_colors)
#-------------------------------------------------------------------------------
#Q67_5_TEXT - Please indicate your race. - Other (fill in) - Text
dat$Q67_5_TEXT = as.factor(dat$Q67_5_TEXT)

#Q34 - Do you identify with Latino, Hispanic, or Spanish origin?
dat$Q34 = as.factor(dat$Q34)

#Q35 - Please indicate your gender.
dat$Q35 = as.factor(dat$Q35)

#state - 50 States, D.C. and Puerto Rico 
dat$state = as.numeric(dat$state)

#Q47 - In which state do you currently reside?
dat$Q47 = as.factor(dat$Q47)

#US.Zip.Code - What is your US Zip Code?
dat$US.Zip.Code = as.factor(dat$US.Zip.Code)

#Q45 - What is your Zip Code? (Mexico)
dat$Q45 = as.factor(dat$Q45)

#Q46 - What is your province? (Mexico)
dat$Q46 = as.factor(dat$Q46)

#Q43 - What is the highest level of education you have completed?
dat$Q43 = as.factor(dat$Q43)

#Q69 - What is the highest level of education you have completed?
dat$Q69 = as.factor(dat$Q69)

#Q40 - What is your current occupation? (Select all that apply) - Selected Choice
dat$Q40 = as.numeric(dat$Q40)

#Q40_8_TEXT - What is your current occupation? (Select all that apply) - Other  - Text
dat$Q40_8_TEXT = as.factor(dat$Q40_8_TEXT)

#US.Sal - What was your total household income before taxes during the past 12 months?
dat$US.Sal = as.factor(dat$US.Sal)

#Mex.Sal - What was your total household income during the past 12 months?
dat$Mex.Sal = as.factor(dat$Mex.Sal)

#Cuba.Sal - What was your total household income during the past 12 months?
dat$Cuba.Sal = as.factor(dat$Cuba.Sal)

