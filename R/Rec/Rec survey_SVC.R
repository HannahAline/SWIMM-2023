### SWIMM 2023 Rec fishers survey
### Sophia Costa
### Started on 3/26/2024

## upload dataset WITH HEADERS ON
getwd()
setwd("/Users/sophiecosta/Desktop/SWIMM")
dat <- read.csv("Recreational Anglers (Pescadores recreativos)_March 15, 2024_12.00.csv", header = TRUE)

######## Load Libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(forcats)

#head(dat)
#view(dat)

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

## renaming blank answers to Unanswered
dat <- dat %>%
  mutate(across(everything(), ~na_if(., ""))) %>% # Replace blanks with NA
  mutate(across(everything(), ~coalesce(., "unanswered"))) # Replace NA with "unanswered"


################################################################################
## creating country-specific datasets for USA and MX based on Q44 asking where they reside
table(dat$Q44)

usa <- dat[dat$Q44 == "United States",]
nrow(usa) ## USA sample size

mex <- dat[dat$Q44 == "Mexico",]
nrow(mex) # MEX sample size

################ SOPHIE'S SECTION #####################
##########################################################
### Question 71: What type of angler do you identify as? (For example, deep sea fisherman, fly fisherman, etc.)

unique(usa$Q71)
unique(mex$Q71)

#Not sure what to do here because there are 90 unique values for USA and 48 unique values for MEX. Some are combined but might make more sense to do this cleaning once final results are in

##########################################################
### Question 3: How many years have you been practicing recreational fishing? (Please enter in number of years, example 10)

#Lets look at unique answers for the whole dataset
unique(dat$Q3)

#Remove NAs
dat_Q3 <- dat %>%
  filter(!is.na(dat$Q3) & dat$Q3 != "unanswered")

#Change column to numeric
dat_Q3$Q3 <- as.numeric(dat_Q3$Q3)

# Maximum, minimum, and average value for USA
max_dat <- max(dat_Q3$Q3)
  #ANSWER: 70
min_dat <- min(dat_Q3$Q3)
  #ANSWER: 1
average_dat <- mean(dat_Q3$Q3)
  #ANSWER: 26.86

dat_practice <- ggplot(dat_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing Overall",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")

#### T-Test between MEX and USA 
dat$Q66 <- factor(dat$Q66, levels = c("Mexico", "United States"))
clean_dat_Q3 <- dat %>%
  filter(!is.na(dat$Q3) & dat$Q3 != "unanswered", !is.na(dat$Q66) & dat$Q66 != "unanswered")

clean_dat_Q3$Q3 <- as.numeric(clean_dat_Q3$Q3)
# T-test between Mex & USA
t_test_result <- t.test(Q3 ~ Q66, data = clean_dat_Q3,
                        subset = Q66 %in% c("Mexico", "United States"))

# Print the results
print(t_test_result)
  #Welch Two Sample t-test
    #data:  Q3 by Q66
    #t = -6.3186, df = 169.12, p-value = 2.256e-09
    #alternative hypothesis: true difference in means between group Mexico and group United States is not equal to 0
    #95 percent confidence interval: -19.92256 -10.43736
    #sample estimates: mean in group Mexico mean in group United States 
    #17.23529                    32.41525 

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

usa_practice <- ggplot(usa_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing USA",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")



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

mex_practice <-ggplot(mex_Q3, aes(x = Q3)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Years Practicing Recreational Fishing Mexico",
       x = "Years Practicing Rec. Fishing",
       y = "Number of people")



##########################################################
### Question 4: Is your boat… (Select all that apply)

###First Full Dataset
unique(dat$Q4)
#Subset the data and create a new dataframe
dat_Q4 <- subset(dat, select = Q4)
dat_Q4_separated <- dat_Q4 %>%
  separate(Q4, into = c("Q4_A", "Q4_B", "Q4_C", "Q4_D", "Q4_E"), sep = ",", remove = TRUE, extra = "merge")

#Pivot the data longer and create counts
dat_Q4_long <- dat_Q4_separated %>%
  pivot_longer(cols = matches("^Q4_[ABCDE]"),
               names_to = "Question_Part",
               values_to = "Categorical_Answers")
categorical_counts_dat <- dat_Q4_long %>%
  filter(Categorical_Answers != "", !is.na(Categorical_Answers)) %>%
  count(Categorical_Answers, name = "Count") %>%
  arrange(desc(Count))
categorical_counts_dat <- categorical_counts_dat %>%
  mutate(percent = round((Count / sum(Count)) * 100, digits = 2))


#Create a pie chart

library(RColorBrewer)
colors <- brewer.pal(6, "Blues")  # Create color palet. Replace '6' with the number of unique categories

categorical_counts_dat <- categorical_counts_dat %>%
  mutate(Categorical_Answers = fct_reorder(Categorical_Answers, percent))

boat_pie <- ggplot(categorical_counts_dat, aes(x = "", y = Categorical_Answers, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "What type of Boat Overall") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size

##### Now US
unique(usa$Q4)
#Subset the data and create a new dataframe
usa_Q4 <- subset(usa, select = Q4)
usa_Q4_separated <- usa_Q4 %>%
  separate(Q4, into = c("Q4_A", "Q4_B", "Q4_C", "Q4_D", "Q4_E"), sep = ",", remove = TRUE, extra = "merge")

#Pivot the data longer and create counts
usa_Q4_long <- usa_Q4_separated %>%
  pivot_longer(cols = matches("^Q4_[ABCDE]"),
               names_to = "Question_Part",
               values_to = "Categorical_Answers")
categorical_counts_usa <- usa_Q4_long %>%
  filter(Categorical_Answers != "", !is.na(Categorical_Answers)) %>%
  count(Categorical_Answers, name = "Count") %>%
  arrange(desc(Count))
categorical_counts_usa <- categorical_counts_usa %>%
  mutate(percent = round((Count / sum(Count)) * 100, digits = 2))


#Create a pie chart

library(RColorBrewer)
colors <- brewer.pal(6, "Blues")  # Create color palet. Replace '6' with the number of unique categories

categorical_counts_usa <- categorical_counts_usa %>%
  mutate(Categorical_Answers = fct_reorder(Categorical_Answers, percent))

boat_pie_usa <- ggplot(categorical_counts_usa, aes(x = "", y = Categorical_Answers, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "What type of Boat USA") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size

############ Now Mexico
unique(mex$Q4)

#Subset the data and create a new dataframe
mex_Q4 <- subset(mex, select = Q4)
mex_Q4_separated <- mex_Q4 %>%
  separate(Q4, into = c("Q4_A", "Q4_B", "Q4_C", "Q4_D"), sep = ",", remove = TRUE, extra = "merge")

#Pivot the data longer and create counts
mex_Q4_long <- mex_Q4_separated %>%
  pivot_longer(cols = matches("^Q4_[ABCD]"),
               names_to = "Question_Part",
               values_to = "Categorical_Answers")
categorical_counts_mex <- mex_Q4_long %>%
  filter(Categorical_Answers != "", !is.na(Categorical_Answers)) %>%
  count(Categorical_Answers, name = "Count") %>%
  arrange(desc(Count))
categorical_counts_mex <- categorical_counts_mex %>%
  mutate(percent = round((Count / sum(Count)) * 100, digits = 2))

#Create a pie chart

library(RColorBrewer)
colors <- brewer.pal(6, "Blues")  # Create color palet. Replace '6' with the number of unique categories

categorical_counts_mex <- categorical_counts_mex %>%
  mutate(Categorical_Answers = fct_reorder(Categorical_Answers, percent))

boat_pie_mex <- ggplot(categorical_counts_mex, aes(x = "", y = Categorical_Answers, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "What type of Boat Mexico") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size

##########################################################
### Question 5: Do you use charter services?
###First Full Dataset
unique(dat$Q5)

# Create the summarized dataframe
charter_dat <- dat %>%
  group_by(Q5) %>%
  summarise(value = n()) %>%
  ungroup()

charter_dat <- charter_dat %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))


#Create a pie chart

charter_dat <- charter_dat %>%
  mutate(Q5 = fct_reorder(Q5, percent))

charter_pie <- ggplot(charter_dat, aes(x = "", y = Q5, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "Do you use charter services Overall") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size

##### Now US
unique(usa$Q5)

# Create the summarized dataframe
charter_usa <- usa %>%
  group_by(Q5) %>%
  summarise(value = n()) %>%
  ungroup()

charter_usa <- charter_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))


#Create a pie chart

charter_usa <- charter_usa %>%
  mutate(Q5 = fct_reorder(Q5, percent))

charter_pie_usa <- ggplot(charter_usa, aes(x = "", y = Q5, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "Do you use charter services USA") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size


unique(usa$Q5)

# Create the summarized dataframe
charter_usa <- usa %>%
  group_by(Q5) %>%
  summarise(value = n()) %>%
  ungroup()

charter_usa <- charter_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))


#Create a pie chart

charter_usa <- charter_usa %>%
  mutate(Q5 = fct_reorder(Q5, percent))

charter_pie_usa <- ggplot(charter_usa, aes(x = "", y = Q5, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "Do you use charter services USA") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size

##### Now Mex
unique(mex$Q5)

# Create the summarized dataframe
charter_mex <- mex %>%
  group_by(Q5) %>%
  summarise(value = n()) %>%
  ungroup()

charter_mex <- charter_mex %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))


#Create a pie chart

charter_mex <- charter_mex %>%
  mutate(Q5 = fct_reorder(Q5, percent))

charter_pie_mex <- ggplot(charter_mex, aes(x = "", y = Q5, fill = percent)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Category", # Customize the legend title
       title = "Do you use charter services Mexico") + 
  theme_void() + # Removes background, gridlines, and text
  theme(legend.title = element_text(size = 12), # Customize legend title size
        legend.text = element_text(size = 12)) # Customize legend text size



##########################################################
### Question 6: How many days per year do you go on charter trips? - Selected Choice

#Start with Overall
unique(dat$Q6)

days_dat <- dat %>%
  group_by(Q6) %>%
  summarise(value = n()) %>%
  ungroup()

days_dat <- days_dat %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
days_dat <- days_dat %>%
  mutate(Q6 = if_else(Q6 == "More than 15 days (Please specify)", "More than 15 days", Q6))

# Reorder the Q6 factor to specify the desired order
days_dat$Q6 <- factor(days_dat$Q6, levels = c("1 – 5 days", "5 – 10 days", "10 – 15 days", "More than 15 days", "unanswered"))

# Now, create the plot with Q6 in the desired order
days_dat_plot <- ggplot(days_dat, aes(x = Q6, y = value, fill = Q6)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#2c7bb6","#abd9e9", "#4575b4", "#313695", "lightgrey")) +
  labs(title = "Overall",
       x = "Days on Charter Trip",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

unique(dat$Q6_4_TEXT)

#### Now USA
unique(usa$Q6)

days_usa <- usa %>%
  group_by(Q6) %>%
  summarise(value = n()) %>%
  ungroup()

days_usa <- days_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
days_usa <- days_usa %>%
  mutate(Q6 = if_else(Q6 == "More than 15 days (Please specify)", "More than 15 days", Q6))

# Reorder the Q6 factor to specify the desired order
days_usa$Q6 <- factor(days_usa$Q6, levels = c("1 – 5 days", "5 – 10 days", "10 – 15 days", "More than 15 days", "unanswered"))

# Now, create the plot with Q6 in the desired order
days_usa_plot <- ggplot(days_usa, aes(x = Q6, y = value, fill = Q6)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#2c7bb6","#abd9e9", "#4575b4", "#313695", "lightgrey")) +
  labs(title = "USA",
       x = "Days on Charter Trip",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


unique(usa$Q6_4_TEXT)

## Now Mexico
unique(mex$Q6)

days_mex <- mex %>%
  group_by(Q6) %>%
  summarise(value = n()) %>%
  ungroup()

days_mex <- days_mex %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
days_mex <- days_mex %>%
  mutate(Q6 = if_else(Q6 == "More than 15 days (Please specify)", "More than 15 days", Q6))

# Reorder the Q6 factor to specify the desired order
days_mex$Q6 <- factor(days_mex$Q6, levels = c("1 – 5 days", "5 – 10 days", "10 – 15 days", "More than 15 days", "unanswered"))

# Now, create the plot with Q6 in the desired order
days_mex_plot <- ggplot(days_mex, aes(x = Q6, y = value, fill = Q6)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#2c7bb6","#abd9e9", "#4575b4", "#313695", "lightgrey")) +
  labs(title = "MEXICO",
       x = "Days on Charter Trip",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


unique(mex$Q6_4_TEXT)

##########################################################
### Question 7: On average, how much do you spend on a charter trip?

#### I decided not to do this for overall because the currencies are not in the same unit

#Identify Responses
unique(usa$Q7)

#Summarize Responses
cost_usa <- usa %>%
  group_by(Q7) %>%
  summarise(value = n()) %>%
  ungroup()

cost_usa <- cost_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Reorder the Q6 factor to specify the desired order
cost_usa$Q7 <- factor(cost_usa$Q7, levels = c("Under $250 USD", "$250 – $500 USD", "$500 – $1,000 USD", "Over $1,000 USD", "unanswered"))

# Now, create the plot with Q6 in the desired order
cost_usa_plot <- ggplot(cost_usa, aes(x = Q7, y = value, fill = Q7)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#2c7bb6","#abd9e9", "#4575b4", "#313695", "lightgrey")) +
  labs(title = "USA",
       x = "Cost of Avg. Charter Trip",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#### Now MEXICO
unique(mex$Q73)

#Summarize Responses
cost_mex <- mex %>%
  group_by(Q73) %>%
  summarise(value = n()) %>%
  ungroup()

cost_mex <- cost_mex %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Reorder the Q6 factor to specify the desired order
cost_mex$Q73 <- factor(cost_mex$Q73, levels = c("Under $1,000 MX", "$1,000 – $2,500 MX", "$2,500 – $4,000 MX", "Over $4,000 MX", "unanswered"))

# Now, create the plot with Q6 in the desired order
cost_mex_plot <- ggplot(cost_mex, aes(x = Q73, y = value, fill = Q73)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#2c7bb6","#abd9e9", "#4575b4", "#313695", "lightgrey")) +
  labs(title = "MEXICO",
       x = "Cost of Avg. Charter Trip",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


#unique(usa$Q73) THIS WAS LEFT OUT OF THIS ANALYSIS
#unique(usa$Q74)THIS WAS LEFT OUT OF THIS ANALYSIS
#unique(mex$Q7) THIS WAS LEFT OUT OF THIS ANALYSIS 2 individuals answered this question in the $250-500 range. For simplicity I have left them out of the analyses for now
#unique(mex$Q74) THIS WAS LEFT OUT OF THIS ANALYSIS

##########################################################
### Question 8: On average, how many days a year do you fish for recreational purposes? - Selected Choice
#Identify Responses
unique(dat$Q8)

#Summarize Responses
dayfish_dat <- dat %>%
  group_by(Q8) %>%
  summarise(value = n()) %>%
  ungroup()

dayfish_dat <- dayfish_dat %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
dayfish_dat <- dayfish_dat %>%
  mutate(Q8 = if_else(Q8 == "More than 100 (Please specify)", "More than 100", Q8))

# Reorder the Q6 factor to specify the desired order
dayfish_dat$Q8 <- factor(dayfish_dat$Q8, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
dayfish_dat_plot <- ggplot(dayfish_dat, aes(x = Q8, y = value, fill = Q8)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "Overall",
       x = "Days Recreationally Fishing",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

### NOW US
#Identify Responses
unique(usa$Q8)

#Summarize Responses
dayfish_usa <- usa %>%
  group_by(Q8) %>%
  summarise(value = n()) %>%
  ungroup()

dayfish_usa <- dayfish_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
dayfish_usa <- dayfish_usa %>%
  mutate(Q8 = if_else(Q8 == "More than 100 (Please specify)", "More than 100", Q8))

# Reorder the Q6 factor to specify the desired order
dayfish_usa$Q8 <- factor(dayfish_usa$Q8, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
dayfish_usa_plot <- ggplot(dayfish_usa, aes(x = Q8, y = value, fill = Q8)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "USA",
       x = "Days Recreationally Fishing",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

unique(usa$Q8_5_TEXT)

#### Now MEXICO
unique(mex$Q8)

#Summarize Responses
dayfish_mex <- mex %>%
  group_by(Q8) %>%
  summarise(value = n()) %>%
  ungroup()

dayfish_mex <- dayfish_mex %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Update the category name
dayfish_mex <- dayfish_mex %>%
  mutate(Q8 = if_else(Q8 == "More than 100 (Please specify)", "More than 100", Q8))

# Reorder the Q6 factor to specify the desired order
dayfish_mex$Q8 <- factor(dayfish_mex$Q8, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
dayfish_mex_plot <- ggplot(dayfish_mex, aes(x = Q8, y = value, fill = Q8)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "MEXICO",
       x = "Days Recreationally Fishing",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

unique(mex$Q8_5_TEXT)

##########################################################
### Question 5: How many days do you fish recreationally anywhere in the Gulf of Mexico?
#Identify Responses
unique(dat$Q11)

#Summarize Responses
GOMdays_dat <- dat %>%
  group_by(Q11) %>%
  summarise(value = n()) %>%
  ungroup()

GOMdays_dat <- GOMdays_dat %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Reorder the Q6 factor to specify the desired order
GOMdays_dat$Q11 <- factor(GOMdays_dat$Q11, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
GOMdays_dat_plot <- ggplot(GOMdays_dat, aes(x = Q11, y = value, fill = Q11)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "Overall",
       x = "Days Fishing Anywhere in GOM",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

### NOW USA
#Identify Responses
unique(usa$Q11)

#Summarize Responses
GOMdays_usa <- usa %>%
  group_by(Q11) %>%
  summarise(value = n()) %>%
  ungroup()

GOMdays_usa <- GOMdays_usa %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Reorder the Q6 factor to specify the desired order
GOMdays_usa$Q11 <- factor(GOMdays_usa$Q11, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
GOMdays_usa_plot <- ggplot(GOMdays_usa, aes(x = Q11, y = value, fill = Q11)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "USA",
       x = "Days Fishing Anywhere in GOM",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#unique(usa$Q76) THIS WAS LEFT OUT OF THIS ANALYSIS 2 individuals not included in analyses, 1 in 10 group and 1 in 150 group. For simplicity I have left them out of the analyses for now

### NOW MEXICO
#Identify Responses
unique(mex$Q11)

#Summarize Responses
GOMdays_mex <- mex %>%
  group_by(Q11) %>%
  summarise(value = n()) %>%
  ungroup()

GOMdays_mex <- GOMdays_mex %>%
  mutate(percent = round((value / sum(value)) * 100, digits = 2))

# Reorder the Q6 factor to specify the desired order
GOMdays_mex$Q11 <- factor(GOMdays_mex$Q11, levels = c("Less than 10", "10 – 25", "25 – 50", "50 – 100", "More than 100", "unanswered"))

# Now, create the plot with Q6 in the desired order
GOMdays_mex_plot <- ggplot(GOMdays_mex, aes(x = Q11, y = value, fill = Q11)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_fill_manual(values = c("#abd9e9", "#74add1", "#4575b4", "#313695", "#2c7bb6", "lightgrey")) +
  labs(title = "MEXICO",
       x = "Days Fishing Anywhere in GOM",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#unique(mex$Q76) THIS WAS LEFT OUT OF THIS ANALYSIS. All unanswered


################################################################################# SANKEY DIAGRAMS

install.packages("devtools")
library(devtools)
devtools::install_github("davidsjoberg/ggsankey")

library(ggsankey)
library(tidyverse)


##### Sankey on days fishing GOM & cost

## USA
sankey <- usa %>%
  make_long(Q11, Q7)
sankey

reagg <- sankey%>%
  dplyr::group_by(node)%>%  # Here we are grouping the data by node and then we are taking the frequency of it 
  tally()

sankey <- merge(sankey,
                reagg, 
                by.x = 'node', 
                by.y = 'node', 
                all.x = TRUE)


str(sankey)

sankey_plot <- ggplot(sankey, aes(x = x,
                                  next_x = next_x,
                                  node = node,
                                  next_node = next_node,
                                  fill = factor(node),
                                  label = node)) + 
  geom_sankey(type="sankey", flow.alpha = 0.5, width = 0.1, flow.fill = "#74add1", node.fill = "#2c7bb6", node.color = 'black', space = 30) +
  geom_sankey_label(aes(label = paste0(node, '=', n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Study.category"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Research.type"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 0, space = 30) + 
  theme_sankey(base_size = 12) + theme(legend.position = 'none') +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = c("Days fishing anywhere in GOM", "Cost of Charter USA"))

sankey_plot

## MEXICO
sankey <- mex %>%
  make_long(Q11, Q73)
sankey

reagg <- sankey%>%
  dplyr::group_by(node)%>%  # Here we are grouping the data by node and then we are taking the frequency of it 
  tally()

sankey <- merge(sankey,
                reagg, 
                by.x = 'node', 
                by.y = 'node', 
                all.x = TRUE)


str(sankey)

sankey_plot <- ggplot(sankey, aes(x = x,
                                  next_x = next_x,
                                  node = node,
                                  next_node = next_node,
                                  fill = factor(node),
                                  label = node)) + 
  geom_sankey(type="sankey", flow.alpha = 0.5, width = 0.1, flow.fill = "#74add1", node.fill = "#2c7bb6", node.color = 'black', space = 30) +
  geom_sankey_label(aes(label = paste0(node, '=', n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Study.category"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Research.type"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 0, space = 30) + 
  theme_sankey(base_size = 12) + theme(legend.position = 'none') +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = c("Days fishing anywhere in GOM", "Cost of Charter Mexico"))

sankey_plot


##### Sankey on days fishing & cost

## USA
sankey <- usa %>%
  make_long(Q8, Q7)
sankey

reagg <- sankey%>%
  dplyr::group_by(node)%>%  # Here we are grouping the data by node and then we are taking the frequency of it 
  tally()

sankey <- merge(sankey,
                reagg, 
                by.x = 'node', 
                by.y = 'node', 
                all.x = TRUE)


str(sankey)

sankey_plot <- ggplot(sankey, aes(x = x,
                                  next_x = next_x,
                                  node = node,
                                  next_node = next_node,
                                  fill = factor(node),
                                  label = node)) + 
  geom_sankey(type="sankey", flow.alpha = 0.5, width = 0.1, flow.fill = "#74add1", node.fill = "#2c7bb6", node.color = 'black', space = 30) +
  geom_sankey_label(aes(label = paste0(node, '=', n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Study.category"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Research.type"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 0, space = 30) + 
  theme_sankey(base_size = 12) + theme(legend.position = 'none') +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = c("Days fishing", "Cost of Charter USA"))

sankey_plot

## MEXICO
sankey <- mex %>%
  make_long(Q8, Q73)
sankey

reagg <- sankey%>%
  dplyr::group_by(node)%>%  # Here we are grouping the data by node and then we are taking the frequency of it 
  tally()

sankey <- merge(sankey,
                reagg, 
                by.x = 'node', 
                by.y = 'node', 
                all.x = TRUE)


str(sankey)

sankey_plot <- ggplot(sankey, aes(x = x,
                                  next_x = next_x,
                                  node = node,
                                  next_node = next_node,
                                  fill = factor(node),
                                  label = node)) + 
  geom_sankey(type="sankey", flow.alpha = 0.5, width = 0.1, flow.fill = "#74add1", node.fill = "#2c7bb6", node.color = 'black', space = 30) +
  geom_sankey_label(aes(label = paste0(node, '=', n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Study.category"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 1, space = 30) + 
  #geom_sankey_label(data=study_sankey %>% filter(x == "Research.type"), aes(label = paste0(node, " = ", n)), size = 3, fill = "white", hjust = 0, space = 30) + 
  theme_sankey(base_size = 12) + theme(legend.position = 'none') +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = c("Days fishing", "Cost of Charter Mexico"))

sankey_plot
