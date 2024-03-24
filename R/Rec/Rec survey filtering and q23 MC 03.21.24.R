### SWIMM 2023 Rec fishers survey
### Manuel E. Coffill-Rivera
### started on 3/21/24

## upload dataset WITH HEADERS ON
dat <- `Recreational.Anglers.(Pescadores.recreativos)_March.15,.2024_12.00`

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
## Demographics
## Hannah Henry



################################################################################
## creating country-specific datasets for USA and MX based on Q44 asking where they reside
table(dat$Q44)

usa <- dat[dat$Q44 == "United States",]
nrow(usa) ## USA sample size

mex <- dat[dat$Q44 == "Mexico",]
nrow(mex) # MEX sample size


################################################################################
############# most caught vs most targeted Q's 21, 47, and 23 (Manuel)

### Q23: How many fish (of any species) do you commonly catch per day?
### Selected Choice
### Q23_5_TEXT: How many fish (of any species) do you commonly catch per day?
### - More than 20 - Text

table(dat$Q23)
table(dat$Q23_5_TEXT) ### consider adding these if the answers fit the preset bins
### will ignore answers to the text option for now

## renaming blank answers to Unanswered
dat[dat$Q23 == "",] <- "Unanswered"
table(dat$Q23)
table(dat$Q44)
## none of the unanswered q23cfall into either USA or MEX!

usa <- datQ44usa <- dat[dat$Q44 == "United States",]
mex <- dat[dat$Q44 == "Mexico",]

table(usa$Q23) #these should add up to total usa sample size
table(mex$Q23) # these should add up to total mex sample size


### bar plots
library(ggplot2)
library(ggpubr)
library(grid)

### all
q23all <- ggplot(dat, aes(x=Q23)) +
  geom_bar(fill = "steelblue") + theme_classic() +
  labs(x = "How many fish do you commonly catch per day?",
       y = "Count")
q23all

### by residence
q23byresid <- ggplot(dat, aes(x=Q23, group = Q44, fill = Q44)) +
  geom_bar() + theme_classic() +
  labs(x = "How many fish do you commonly catch per day?",
       y = "Count")
q23byresid

# save to directory
#ggsave("q23byresid.png", width = 10, height = 8, dpi = 1000)


### usa
q23usa <- ggplot(usa, aes(x=Q23)) +
  geom_bar(fill = "darkred") + theme_classic() +
  labs(x = "How many fish do you commonly catch per day?",
       y = "Count", title = "USA")
q23usa
#ggsave("q23usa.png", width = 10, height = 8, dpi = 1000)


### mex
q23mex <- ggplot(mex, aes(x=Q23)) +
  geom_bar(fill = "darkgreen") + theme_classic() +
  labs(x = "How many fish do you commonly catch per day?",
       y = "Count",  title = "Mex")
q23mex
#ggsave("q23mex.png", width = 10, height = 8, dpi = 1000)


### merged plot. first remove individual axis labels
q23byresid <- q23byresid + xlab(NULL) + ylab(NULL)
q23mex <- q23mex + xlab(NULL) + ylab(NULL)
q23usa <- q23usa + xlab(NULL) +ylab(NULL)

merged <- ggarrange(ggarrange(q23usa, q23mex, ncol = 2), q23byresid, nrow = 2)
annotate_figure(merged, left = textGrob("Count", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("How many fish (of any species) do you commonly catch per day?", gp = gpar(cex = 1)))
#ggsave("q23merged.png", width = 10, height = 8, dpi = 1000)




