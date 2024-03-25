###SWIMM 2023 Data Analysis - Recreational anglers

###QUESTIONS 28 AND 31###

# Install and load necessary packages
library(tidyverse)
library(likert)
library(readr)
library(ggplot2)
library(reshape2)

#####MANUELS FILTERING CODE####
# Read the data
dat <- read_csv("Recreational Anglers (Pescadores recreativos)_March 15^J 2024_12.00.csv")

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

## creating country-specific datasets for USA and MX based on Q44 asking where they reside
table(dat$Q44)

usa <- dat[dat$Q44 == "United States",]
nrow(usa) ## USA sample size

mex <- dat[dat$Q44 == "Mexico",]
nrow(mex) # MEX sample size


#QUESTION 28: Regarding your fishing experience satisfaction, 
#please rank the following factors from 1 to 5, 
#where 1 is not important and 5 is very important:
#Spending time in nature (28_1)
#Clean environment (28_2)
#Water quality (28_3)
#Number of fish (28_3)
#Size of fish (28_4)
#Other (28_5)
#Other - text (28_5), ignoring for now

# Subset data into table with just these ranking questions for Q28
rankingsusa <- usa[, c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5","Q28_6")]
rankingsmex <- mex[, c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5","Q28_6")]

#For some reason it said the data wasn't numeric, so here I convert to numeric values
rankingsusa <- as.data.frame(lapply(rankingsusa, as.numeric))
rankingsmex<- as.data.frame(lapply(rankingsmex, as.numeric))

# Replace cells with value 6 with NA
rankingsusa <- replace(rankingsusa, rankingsusa == 6, NA)
rankingsmex <- replace(rankingsmex, rankingsmex == 6, NA)

# Calculate the mean ranking for each factor
mean_rankingsusa<- colMeans(rankingsusa, na.rm = TRUE)
mean_rankingsmex<- colMeans(rankingsmex, na.rm = TRUE)

# Create a vector of factor names
factors <- c("Time spent in Nature", "Cleanliness of Environment", "Water Quality", "Number of Fish", "Size of Fish","Other")

# Calculate the weighted mean ranking for each factor for USA and Mexico
#weighted_mean_rankings_usa <- colMeans(rankingsusa, na.rm = TRUE) * (nrow(rankingsusa) / (nrow(rankingsusa) + nrow(rankingsmex)))
#weighted_mean_rankings_mex <- colMeans(rankingsmex, na.rm = TRUE) * (nrow(rankingsmex) / (nrow(rankingsusa) + nrow(rankingsmex)))

# Combine mean rankings into a data frame
mean_rankings <- data.frame(
  Factor = factors,
  Mean_Ranking_USA = mean_rankingsusa,
  Mean_Ranking_Mexico = mean_rankingsmex
)

# Melt the data for easier plotting
mean_rankings_melted <- reshape2::melt(mean_rankings, id.vars = "Factor")


# Plot using ggplot with reordered factors
ggplot(mean_rankings_melted, aes(x = Factor, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Q28: Regarding your fishing experience satisfaction, rank the following factors from 1 to 5",
       x = "Factor",
       y = "Mean Ranking") +
  scale_fill_manual(values = c("Mean_Ranking_USA" = "blue", "Mean_Ranking_Mexico" = "red"),
                    labels = c("Mean Ranking USA", "Mean Ranking Mexico")) +
  coord_cartesian(ylim = c(1, 5)) +  # Set y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### NOW AGAIN FOR QUESTION 31###
##Q31: Regarding your fishing experience satisfaction,
#rank the following factors from 1 to 5, 
#where 1 is not important and 5 is very important. 
#Closeness to my city, Amenities, Infrastructure, Safety of Site, Boat Quality, Other

# Subset data into table with just these ranking questions for Q28
rankingsusa31 <- usa[, c("Q31_1", "Q31_2", "Q31_3", "Q31_4", "Q31_5","Q31_6")]

rankingsmex31 <- mex[, c("Q31_1", "Q31_2", "Q31_3", "Q31_4", "Q31_5","Q31_6")]

#For some reason it said the data wasn't numeric, so here I convert to numeric values
rankingsusa31 <- as.data.frame(lapply(rankingsusa31, as.numeric))
rankingsmex31 <- as.data.frame(lapply(rankingsmex31, as.numeric))

# Replace cells with value 6 with NA
rankingsusa31 <- replace(rankingsusa31, rankingsusa31 == 6, NA)
rankingsmex31 <- replace(rankingsmex31, rankingsmex31 == 6, NA)

# Calculate the mean ranking for each factor
mean_rankingsusa31<- colMeans(rankingsusa31, na.rm = TRUE)
mean_rankingsmex31<- colMeans(rankingsmex31, na.rm = TRUE)

# Create a vector of factor names
factors31 <- c("Closeness to my city", "Amenities", "Infrastructure", "Safety of site", "Boat Quality","Other")

# Calculate the weighted mean ranking for each factor for USA and Mexico
#weighted_mean_rankings_usa31 <- colMeans(rankingsusa31, na.rm = TRUE) * (nrow(rankingsusa31) / (nrow(rankingsusa31) + nrow(rankingsmex31)))
#weighted_mean_rankings_mex31 <- colMeans(rankingsmex31, na.rm = TRUE) * (nrow(rankingsmex31) / (nrow(rankingsusa31) + nrow(rankingsmex31)))

# Combine mean rankings into a data frame
mean_rankings31 <- data.frame(
  Factor = factors31,
  Mean_Ranking_USA31 = mean_rankingsusa31,
  Mean_Ranking_Mexico31 = mean_rankingsmex31
)

# Melt the data for easier plotting
mean_rankings_melted31 <- reshape2::melt(mean_rankings31, id.vars = "Factor")

# Plot using ggplot with reordered factors

ggplot(mean_rankings_melted31, aes(x = Factor, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Q31: Regarding your fishing experience satisfaction, rank the following factors from 1 to 5",
       x = "Factor",
       y = "Mean Ranking") +
  scale_fill_manual(values = c("Mean_Ranking_USA31" = "blue", "Mean_Ranking_Mexico31" = "red"),
                    labels = c("Mean Ranking USA", "Mean Ranking Mexico")) +
  coord_cartesian(ylim = c(1, 5)) +  # Set y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#QUESTION 29

Q29usa <- usa[, c("Q29")]
Q29mex <- mex[, c("Q29")]

# Remove rows with NA values
Q29usa <- na.omit(Q29usa)
Q29mex <- na.omit(Q29mex)


# Calculate the proportions of "Yes" and "No" responses for USA
Q29usa_counts <- table(Q29usa)
Q29usa_percent <- prop.table(Q29usa_counts) * 100

# Calculate the proportions of "Yes" and "No" responses for Mexico
Q29mex_counts <- table(Q29mex)
Q29mex_percent <- prop.table(Q29mex_counts) * 100

# Combine the data into a single data frame
df <- data.frame(
  Country = c(rep("USA", length(Q29usa_percent)), rep("Mexico", length(Q29mex_percent))),
  Response = c(names(Q29usa_percent), names(Q29mex_percent)),
  Percent = c(Q29usa_percent, Q29mex_percent)
)

# Plot
ggplot(df, aes(x = Response, y = Percent, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q29: Would you be willing to pay for a conservation program that preserves your top ranked choice?",
       x = "Response", y = "Percent of Respondents") +
  scale_fill_manual(values = c("USA" = "blue", "Mexico" = "green")) +
  theme_minimal()


#QUESTION 30 (USA WTP),62 (MEX WTP) (63 is Cuba WTP so omitting)

# Calculate the mean of responses of willingness to pay for each country

# Convert to numeric if needed
WTPusa <- as.numeric(as.character(WTPusa))
WTPmex <- as.numeric(as.character(WTPmex))

# Define exchange rate
exchange_rate <- 16.68

# Convert values from pesos to USD
WTPmex_usd <- WTPmex / exchange_rate

# Remove NA values
WTPusa <- na.omit(WTPusa)
WTPmex_usd <- na.omit(WTPmex_usd)

# Calculate the mean of responses for each country
meanWTP_usa <- mean(WTPusa)
meanWTP_mex <- mean(WTPmex_usd)

# Sample sizes for each country
sample_size_usa <- length(WTPusa)
sample_size_mex <- length(WTPmex_usd)

# Range of responses
rangeWTP_usa <- paste(range(WTPusa), collapse = " - ")
rangeWTP_mex <- paste(range(WTPmex_usd), collapse = " - ")

# Create a data frame for plotting
df2 <- data.frame(
  Country = c("USA", "Mexico"),
  Mean_Value = c(meanWTP_usa, meanWTP_mex),
  Sample_Size = c(sample_size_usa, sample_size_mex),
  Range = c(rangeWTP_usa, rangeWTP_mex)
)

# Plot
ggplot(df2, aes(x = Country, y = Mean_Value, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(data = df2, aes(label = paste("n =", Sample_Size)), vjust = 1.5, size = 3, position = position_dodge(0.9)) +
  geom_text(data = df2, aes(label = paste("Range:", Range)), vjust = -0.5, size = 3, position = position_dodge(0.9)) +
  labs(title = "Average Value of Responses in USA vs Mexico",
       x = "Country", y = "Average Value") +
  scale_y_continuous(limits = c(0, 25)) +  # Setting y-axis limits
  theme_minimal()
