### SWIMM 2023 Rec fishers survey
### Hannah Henry

## upload dataset 
dat <- read.csv(file.choose())

dat$summary = as.numeric(dat$summary)
summary(dat$Progress)

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

#Remove rows 1 and 2
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

# Set all bars to the same shade of blue
blue_color <- "#3182bd"
################################################################################

#Question by question cleaning and graphs

#-------------------------------------------------------------------------------
#Q32 - What year were you born? [FINAL]
dat$Q32 = as.numeric(dat$Q32)

summary(dat$Q32) #Some people entered years wrong, need to fix

# Filter data to include birth years within the range of 1900 and 2023
dat <- dat[dat$Q32 >= 1900 & dat$Q32 <= 2023, ] #Fixed

#Histogram for birth years
ggplot(data = dat, aes(x = Q32)) +
  geom_histogram(binwidth = 1, fill = blue_color, color = "black") +
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

# Bar plot for Q66
ggplot(data = dat[!is.na(dat$Q66), ], aes(x = Q66)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(title = "What country are you currently in, and/or where do you do most of your fishing?",
       x = "Country",
       y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Q44 - What country do you currently reside in?
dat$Q44 = as.factor(dat$Q44)
# Check levels 
levels(dat$Q44)
#Removing unecessary levels
dat$Q44 <- droplevels(dat$Q44, exclude = "")

# Bar plot for Q44
ggplot(data = dat[!is.na(dat$Q44), ], aes(x = Q44)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(title = "What country do you currently reside in?",
       x = "Country",
       y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Q67 - Please indicate your race. - Selected Choice [FINAL]
dat$Q67 = as.factor(dat$Q67)
# Check levels 
levels(dat$Q67)
#Removing unecessary levels
dat$Q67 <- droplevels(dat$Q67, exclude = "")

# Bar plot for Q67
ggplot(data = dat[!is.na(dat$Q67), ], aes(x = Q67)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(title = "Please indicate your race.",
       x = "Race",
       y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Q67_5_TEXT - Please indicate your race. - Other (fill in) - Text
dat$Q67_5_TEXT = as.factor(dat$Q67_5_TEXT)

#-------------------------------------------------------------------------------
#Q34 - Do you identify with Latino, Hispanic, or Spanish origin?
dat$Q34 = as.factor(dat$Q34)

# Check levels 
levels(dat$Q34)
#Removing unecessary levels
dat$Q34 <- droplevels(dat$Q34, exclude = "")

# Bar plot for Q34
ggplot(data = dat[!is.na(dat$Q34), ], aes(x = Q34)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(title = "Do you identify with Latino, Hispanic, or Spanish origin?",
       x = "Response",
       y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Q35 - Please indicate your gender.
dat$Q35 = as.factor(dat$Q35)

# Check levels 
levels(dat$Q35)
#Removing unecessary levels
dat$Q35 <- droplevels(dat$Q35, exclude = "")

# Bar plot for Q35
ggplot(data = dat[!is.na(dat$Q35), ], aes(x = Q35)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  # Add numbers on top of bars
  labs(title = "Please indicate your gender.",
       x = "Gender",
       y = "Count") +
  theme_minimal()
#-------------------------------------------------------------------------------

#state - 50 States, D.C. and Puerto Rico 
dat$state = as.factor(dat$state)
# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")
dat$state <- droplevels(dat$state, exclude = "I do not reside in the United States") #Not needed

#Count the number of respondents from each state
state_counts <- table(dat$state)

#New data frame with state counts
state_counts_df <- data.frame(State = names(state_counts), Respondent_Count = as.numeric(state_counts))

#Reorder the levels of State to match the desired order
state_counts_df$State <- factor(state_counts_df$State, levels = c("Alabama", "California", "Florida", "Georgia", "Hawaii", "Illinois", "Kentucky", "Louisiana", "Mississippi", "New Hampshire", "North Carolina", "Ohio", "Oregon", "Tennessee", "Texas", NA))

#Still figuring out how to best display...

# New green blue colors for the pie chart
light_blue_palette <- colorRampPalette(c("lightblue", "blue"))(length(state_counts_df$State))

#Pie chart for state
ggplot(state_counts_df, aes(x = "", y = Respondent_Count, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Respondents by State") +
  scale_fill_manual(values = setNames(light_blue_palette, state_counts_df$State)) +
  theme_void() +
  theme(legend.position = "bottom")  # Move legend to bottom

# Filter out NA values from state_counts_df
state_counts_df_filtered <- state_counts_df[!is.na(state_counts_df$State), ]

# New green blue colors for the pie chart
light_blue_palette <- colorRampPalette(c("lightblue", "blue"))(length(state_counts_df_filtered$State))

# Pie chart for state without NA category
ggplot(state_counts_df_filtered, aes(x = "", y = Respondent_Count, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Respondents by State") +
  scale_fill_manual(values = setNames(light_blue_palette, state_counts_df_filtered$State)) +
  theme_void() +
  theme(legend.position = "bottom")  # Move legend to bottom
#-------------------------------------------------------------------------------
#Q47 - In which state do you currently reside? (Mexico)
dat$Q47 = as.factor(dat$Q47)

# Check levels 
levels(dat$Q47)
#Removing unecessary levels
dat$Q47 <- droplevels(dat$Q47, exclude = "")

# Filter out NA values from Q47
dat_filtered <- dat[!is.na(dat$Q47), ]

# Count the number of respondents from each state in Mexico
mexican_state_counts <- table(dat_filtered$Q47)

# Create a data frame with Mexican state counts
mexican_state_counts_df <- data.frame(State = names(mexican_state_counts), Respondent_Count = as.numeric(mexican_state_counts))

# Remove NA row from the data frame
mexican_state_counts_df <- mexican_state_counts_df[!is.na(mexican_state_counts_df$State), ]

# New blue colors for the pie chart
light_blue_palette <- colorRampPalette(c("lightblue", "blue"))(length(mexican_state_counts_df$State))

# Pie chart for Mexican states
ggplot(mexican_state_counts_df, aes(x = "", y = Respondent_Count, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Respondents by State in Mexico") +
  scale_fill_manual(values = setNames(light_blue_palette, mexican_state_counts_df$State)) +
  theme_void() +
  theme(legend.position = "bottom")  # Move legend to bottom

#-------------------------------------------------------------------------------
#US.Zip.Code - What is your US Zip Code?
dat$US.Zip.Code = as.factor(dat$US.Zip.Code)

# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")

#-------------------------------------------------------------------------------
#Q45 - What is your Zip Code? (Mexico)
dat$Q45 = as.factor(dat$Q45)

# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")

#-------------------------------------------------------------------------------
#Q46 - What is your province? (Mexico)
dat$Q46 = as.factor(dat$Q46)

# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")

#-------------------------------------------------------------------------------
#Q43 - What is the highest level of education you have completed?
dat$Q43 = as.factor(dat$Q43)

# Check levels 
levels(dat$Q43)
#Removing unecessary levels
dat$Q43 <- droplevels(dat$Q43, exclude = "NA")

library(stringr)

# Abbreviate the X labels
abbreviated_labels <- str_wrap(levels(dat$Q43), width = 15)  # Adjust the width as needed

# Bar plot for highest level of education completed (Q43)
ggplot(data = dat, aes(x = Q43, fill = Q43)) +
  geom_bar(fill = blue_color) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=3) +  # Add count labels
  labs(title = "Highest Level of Education Completed",
       x = "Education Level",
       y = "Count") +
  scale_x_discrete(labels = abbreviated_labels) +  # Use abbreviated labels
  theme_minimal()

#-------------------------------------------------------------------------------
#Q69 - What is the highest level of education you have completed?
dat$Q69 = as.factor(dat$Q69)

# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")

#-------------------------------------------------------------------------------
#Q40 - What is your current occupation? (Select all that apply) - Selected Choice
dat$Q40 = as.numeric(dat$Q40)

# Check levels 
levels(dat$state)
#Removing unecessary levels
dat$state <- droplevels(dat$state, exclude = "")
#-------------------------------------------------------------------------------
#Q40_8_TEXT - What is your current occupation? (Select all that apply) - Other  - Text
dat$Q40_8_TEXT = as.factor(dat$Q40_8_TEXT)

#-------------------------------------------------------------------------------
#US.Sal - What was your total household income before taxes during the past 12 months?
dat$US.Sal = as.factor(dat$US.Sal)

#-------------------------------------------------------------------------------
#Mex.Sal - What was your total household income during the past 12 months?
dat$Mex.Sal = as.factor(dat$Mex.Sal)

#-------------------------------------------------------------------------------
#Cuba.Sal - What was your total household income during the past 12 months?
dat$Cuba.Sal = as.factor(dat$Cuba.Sal)

#-------------------------------------------------------------------------------
