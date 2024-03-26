### SWIMM 2023 Rec fishers survey
### Manuel E. Coffill-Rivera
### started on 3/21/24

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






################################################################################
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
       y = "Count", title = "USA n=106")
q23usa
#ggsave("q23usa.png", width = 10, height = 8, dpi = 1000)


### mex
q23mex <- ggplot(mex, aes(x=Q23)) +
  geom_bar(fill = "darkgreen") + theme_classic() +
  labs(x = "How many fish do you commonly catch per day?",
       y = "Count",  title = "Mex n=55")
q23mex
#ggsave("q23mex.png", width = 10, height = 8, dpi = 1000)


### merged plot. first remove individual axis labels
q23byresid <- q23byresid + xlab(NULL) + ylab(NULL)
q23mex <- q23mex + xlab(NULL) + ylab(NULL) # need to remove title
q23usa <- q23usa + xlab(NULL) +ylab(NULL) #need to remove title

merged <- ggarrange(ggarrange(q23usa, q23mex, ncol = 2), q23byresid, nrow = 2)
annotate_figure(merged, left = textGrob("Count", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                bottom = textGrob("How many fish (of any species) do you commonly catch per day?", gp = gpar(cex = 1)))
#ggsave("q23merged.png", width = 10, height = 8, dpi = 1000)






# END of Q23
################################################################################
#### Q21 what are your top 3 most caught spp?
#### not in order RIGHT? So can look for frequency
dat$Q21_0_GROUP

### these tell you what position each spp was ranked when placed in the top 3
### What is the difference between "" and Unanswered????
### Maybe "" means it wasn't picked and Unanswered means the question wasn't answered????

dat$Q21_0_1_RANK # Amberjack
dat$Q21_0_2_RANK # Black drum
dat$Q21_0_3_RANK # Cobia
dat$Q21_0_4_RANK # false albacore / bonito
dat$Q21_0_5_RANK # flounder
dat$Q21_0_6_RANK # Grouper
dat$Q21_0_7_RANK # Jack crevalle
dat$Q21_0_8_RANK # Mahi
dat$Q21_0_9_RANK # Mackerels
dat$Q21_0_10_RANK # Pompano
dat$Q21_0_11_RANK # red drum
dat$Q21_0_12_RANK # sailfish
dat$Q21_0_13_RANK # sharks
dat$Q21_0_14_RANK # sheepshead
dat$Q21_0_15_RANK # snapper
dat$Q21_0_16_RANK # spotted seatrout
dat$Q21_0_17_RANK # tripletail
dat$Q21_0_18_RANK # tuna
dat$Q21_0_19_RANK # wahoo
dat$Q21_0_20_RANK # other
dat$Q21_0_21_RANK # other
dat$Q21_0_22_RANK # other

dat$Q21_20_TEXT # alot of snook and tarpon here!!!!!!
dat$Q21_21_TEXT # more snook and tarpon. also saw triggerfish and macabi (bonefish)
dat$Q21_22_TEXT # more snook
### the fact that snook/robalo was found in all 3 text options leads me to
### believe we should have included this species
### snook are captured in inshore waters in both USA and MEX

### verify that tarpon is on all 3 text columns



### we can use this to figure out how many time species were selected
q21unanswered <- dat[grep("Unanswered", dat$Q21_0_GROUP), ]
q21unanswered$Q21_0_GROUP
nrow(q21unanswered) # number of surveys that didn't answer Q21
q21unanswered$Q21_0_1_RANK
q21unanswered$Q21_0_19_RANK
### when Unanswered is in the group column, it means the question was not answered
### this means we can ignore the Unanswered after noting its sample size

q21answered <- dat[grep("Unanswered", dat$Q21_0_GROUP, invert = TRUE), ]
q21answered$Q21_0_GROUP
nrow(q21answered) # number of surveys that answered Q21





### filtering for surveys that included a specific species
amberj <- dat[grep("Amber", dat$Q21_0_GROUP), ]
amberj$Q21_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- dat[grep("Black", dat$Q21_0_GROUP), ]
black$Q21_0_GROUP
nrow(black)
table(black$Q44)

cobia <- dat[grep("Cobia", dat$Q21_0_GROUP), ]
cobia$Q21_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- dat[grep("False", dat$Q21_0_GROUP), ]
false$Q21_0_GROUP
nrow(false)
table(false$Q44)

floun <- dat[grep("Flounder", dat$Q21_0_GROUP), ]
floun$Q21_0_GROUP
nrow(floun)
table(floun$Q44)

group <- dat[grep("Grouper", dat$Q21_0_GROUP), ]
group$Q21_0_GROUP
nrow(group)
table(group$Q44)

crev <- dat[grep("Crevalle", dat$Q21_0_GROUP), ]
crev$Q21_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- dat[grep("Mahi", dat$Q21_0_GROUP), ]
mahi$Q21_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- dat[grep("Mack", dat$Q21_0_GROUP), ]
mack$Q21_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- dat[grep("Pompano", dat$Q21_0_GROUP), ]
pomp$Q21_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- dat[grep("Red Drum", dat$Q21_0_GROUP), ]
reddrum$Q21_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- dat[grep("Sailf", dat$Q21_0_GROUP), ]
sail$Q21_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- dat[grep("Shark", dat$Q21_0_GROUP), ]
shark$Q21_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- dat[grep("Sheeps", dat$Q21_0_GROUP), ]
sheep$Q21_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- dat[grep("Snapper", dat$Q21_0_GROUP), ]
snapper$Q21_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and MEX

trout <- dat[grep("Spotted", dat$Q21_0_GROUP), ]
trout$Q21_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- dat[grep("Triple", dat$Q21_0_GROUP), ]
triple$Q21_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- dat[grep("Tuna", dat$Q21_0_GROUP), ]
tuna$Q21_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- dat[grep("Wahoo", dat$Q21_0_GROUP), ]
wahoo$Q21_0_GROUP
nrow(wahoo)
table(wahoo$Q44)



#### adding other species stated in TEXT
table(dat$Q21_20_TEXT) # snook and tarpon are the only ones that have n>5
table(dat$Q21_21_TEXT) # snook and tarpon are the only ones that have n>5
table(dat$Q21_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ")
# these are all of the names I saw in the 3 tect columns that refer to snook
library(dplyr)

snook1 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q21_20_TEXT))
table(snook1$Q21_20_TEXT) # only column that should have any of the results
table(snook1$Q21_21_TEXT)
table(snook1$Q21_22_TEXT)

snook2 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q21_21_TEXT))
table(snook2$Q21_20_TEXT)
table(snook2$Q21_21_TEXT) # only column that should have any of the results
table(snook2$Q21_22_TEXT)

snook3 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q21_22_TEXT))
table(snook3$Q21_20_TEXT)
table(snook3$Q21_21_TEXT)
table(snook3$Q21_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ", "Trapon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q21_20_TEXT))
table(tarpon1$Q21_20_TEXT) # only column that should have any of the results
table(tarpon1$Q21_21_TEXT)
table(tarpon1$Q21_22_TEXT)

tarpon2 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q21_21_TEXT))
table(tarpon2$Q21_20_TEXT)
table(tarpon2$Q21_21_TEXT) # only column that should have any of the results
table(tarpon2$Q21_22_TEXT)

tarpon3 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q21_22_TEXT))
table(tarpon3$Q21_20_TEXT)
table(tarpon3$Q21_21_TEXT)
table(tarpon3$Q21_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above



### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q21speciesnall <- data.frame(spp, sppn)

## bar plot
ggplot(data=q21speciesnall, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "What are your top 3 most caught species?", y = "Count",
       title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q21all.png", width = 10, height = 8, dpi = 1000)


#### MEX only
mex <- dat[dat$Q44 == "Mexico",]

amberj <- mex[grep("Amber", mex$Q21_0_GROUP), ]
amberj$Q21_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- mex[grep("Black", mex$Q21_0_GROUP), ]
black$Q21_0_GROUP
nrow(black)
table(black$Q44)

cobia <- mex[grep("Cobia", mex$Q21_0_GROUP), ]
cobia$Q21_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- mex[grep("False", mex$Q21_0_GROUP), ]
false$Q21_0_GROUP
nrow(false)
table(false$Q44)

floun <- mex[grep("Flounder", mex$Q21_0_GROUP), ]
floun$Q21_0_GROUP
nrow(floun)
table(floun$Q44)

group <- mex[grep("Grouper", mex$Q21_0_GROUP), ]
group$Q21_0_GROUP
nrow(group)
table(group$Q44)

crev <- mex[grep("Crevalle", mex$Q21_0_GROUP), ]
crev$Q21_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- mex[grep("Mahi", mex$Q21_0_GROUP), ]
mahi$Q21_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- mex[grep("Mack", mex$Q21_0_GROUP), ]
mack$Q21_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- mex[grep("Pompano", mex$Q21_0_GROUP), ]
pomp$Q21_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- mex[grep("Red Drum", mex$Q21_0_GROUP), ]
reddrum$Q21_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- mex[grep("Sailf", mex$Q21_0_GROUP), ]
sail$Q21_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- mex[grep("Shark", mex$Q21_0_GROUP), ]
shark$Q21_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- mex[grep("Sheeps", mex$Q21_0_GROUP), ]
sheep$Q21_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- mex[grep("Snapper", mex$Q21_0_GROUP), ]
snapper$Q21_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and MEX

trout <- mex[grep("Spotted", mex$Q21_0_GROUP), ]
trout$Q21_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- mex[grep("Triple", mex$Q21_0_GROUP), ]
triple$Q21_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- mex[grep("Tuna", mex$Q21_0_GROUP), ]
tuna$Q21_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- mex[grep("Wahoo", mex$Q21_0_GROUP), ]
wahoo$Q21_0_GROUP
nrow(wahoo)
table(wahoo$Q44)

#### adding other species stated in TEXT
table(mex$Q21_20_TEXT) # snook and tarpon are the only ones that have n>5
table(mex$Q21_21_TEXT) # snook and tarpon are the only ones that have n>5
table(mex$Q21_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ")
# these are all of the names I saw in the 3 tect columns that refer to snook
library(dplyr)

snook1 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q21_20_TEXT))
table(snook1$Q21_20_TEXT) # only column that should have any of the results
table(snook1$Q21_21_TEXT)
table(snook1$Q21_22_TEXT)

snook2 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q21_21_TEXT))
table(snook2$Q21_20_TEXT)
table(snook2$Q21_21_TEXT) # only column that should have any of the results
table(snook2$Q21_22_TEXT)

snook3 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q21_22_TEXT))
table(snook3$Q21_20_TEXT)
table(snook3$Q21_21_TEXT)
table(snook3$Q21_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ", "Trapon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q21_20_TEXT))
table(tarpon1$Q21_20_TEXT) # only column that should have any of the results
table(tarpon1$Q21_21_TEXT)
table(tarpon1$Q21_22_TEXT)

tarpon2 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q21_21_TEXT))
table(tarpon2$Q21_20_TEXT)
table(tarpon2$Q21_21_TEXT) # only column that should have any of the results
table(tarpon2$Q21_22_TEXT)

tarpon3 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q21_22_TEXT))
table(tarpon3$Q21_20_TEXT)
table(tarpon3$Q21_21_TEXT)
table(tarpon3$Q21_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above


### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q21speciesnmex <- data.frame(spp, sppn)

## bar plot
ggplot(data=q21speciesnmex, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "What are your top 3 most caught species?", y = "Count",
       title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q21mex.png", width = 10, height = 8, dpi = 1000)



###### USA only
usa <- dat[dat$Q44 == "United States",]

amberj <- usa[grep("Amber", usa$Q21_0_GROUP), ]
amberj$Q21_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- usa[grep("Black", usa$Q21_0_GROUP), ]
black$Q21_0_GROUP
nrow(black)
table(black$Q44)

cobia <- usa[grep("Cobia", usa$Q21_0_GROUP), ]
cobia$Q21_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- usa[grep("False", usa$Q21_0_GROUP), ]
false$Q21_0_GROUP
nrow(false)
table(false$Q44)

floun <- usa[grep("Flounder", usa$Q21_0_GROUP), ]
floun$Q21_0_GROUP
nrow(floun)
table(floun$Q44)

group <- usa[grep("Grouper", usa$Q21_0_GROUP), ]
group$Q21_0_GROUP
nrow(group)
table(group$Q44)

crev <- usa[grep("Crevalle", usa$Q21_0_GROUP), ]
crev$Q21_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- usa[grep("Mahi", usa$Q21_0_GROUP), ]
mahi$Q21_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- usa[grep("Mack", usa$Q21_0_GROUP), ]
mack$Q21_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- usa[grep("Pompano", usa$Q21_0_GROUP), ]
pomp$Q21_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- usa[grep("Red Drum", usa$Q21_0_GROUP), ]
reddrum$Q21_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- usa[grep("Sailf", usa$Q21_0_GROUP), ]
sail$Q21_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- usa[grep("Shark", usa$Q21_0_GROUP), ]
shark$Q21_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- usa[grep("Sheeps", usa$Q21_0_GROUP), ]
sheep$Q21_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- usa[grep("Snapper", usa$Q21_0_GROUP), ]
snapper$Q21_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and usa

trout <- usa[grep("Spotted", usa$Q21_0_GROUP), ]
trout$Q21_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- usa[grep("Triple", usa$Q21_0_GROUP), ]
triple$Q21_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- usa[grep("Tuna", usa$Q21_0_GROUP), ]
tuna$Q21_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- usa[grep("Wahoo", usa$Q21_0_GROUP), ]
wahoo$Q21_0_GROUP
nrow(wahoo)
table(wahoo$Q44)



#### adding other species stated in TEXT
table(usa$Q21_20_TEXT) # snook and tarpon are the only ones that have n>5
table(usa$Q21_21_TEXT) # snook and tarpon are the only ones that have n>5
table(usa$Q21_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ")
# these are all of the names I saw in the 3 tect columns that refer to snook
library(dplyr)

snook1 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q21_20_TEXT))
table(snook1$Q21_20_TEXT) # only column that should have any of the results
table(snook1$Q21_21_TEXT)
table(snook1$Q21_22_TEXT)

snook2 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q21_21_TEXT))
table(snook2$Q21_20_TEXT)
table(snook2$Q21_21_TEXT) # only column that should have any of the results
table(snook2$Q21_22_TEXT)

snook3 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q21_22_TEXT))
table(snook3$Q21_20_TEXT)
table(snook3$Q21_21_TEXT)
table(snook3$Q21_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ", "Trapon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q21_20_TEXT))
table(tarpon1$Q21_20_TEXT) # only column that should have any of the results
table(tarpon1$Q21_21_TEXT)
table(tarpon1$Q21_22_TEXT)

tarpon2 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q21_21_TEXT))
table(tarpon2$Q21_20_TEXT)
table(tarpon2$Q21_21_TEXT) # only column that should have any of the results
table(tarpon2$Q21_22_TEXT)

tarpon3 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q21_22_TEXT))
table(tarpon3$Q21_20_TEXT)
table(tarpon3$Q21_21_TEXT)
table(tarpon3$Q21_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above



### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q21speciesnusa <- data.frame(spp, sppn)

## bar plot
ggplot(data=q21speciesnusa, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "What are your top 3 most caught species?", y = "Count",
       title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q21usa.png", width = 10, height = 8, dpi = 1000)
















# END of Q21
################################################################################
#### Q47 What are your top 3 target spp?
#### not in order RIGHT? So can look for frequency
dat$Q47_0_GROUP

### these tell you what position each spp was ranked when placed in the top 3
### What is the difference between "" and Unanswered????
### Maybe "" means it wasn't picked and Unanswered means the question wasn't answered????

dat$Q47_0_1_RANK # Amberjack
dat$Q47_0_2_RANK # Black drum
dat$Q47_0_3_RANK # Cobia
dat$Q47_0_4_RANK # false albacore / bonito
dat$Q47_0_5_RANK # flounder
dat$Q47_0_6_RANK # Grouper
dat$Q47_0_7_RANK # Jack crevalle
dat$Q47_0_8_RANK # Mahi
dat$Q47_0_9_RANK # Mackerels
dat$Q47_0_10_RANK # Pompano
dat$Q47_0_11_RANK # red drum
dat$Q47_0_12_RANK # sailfish
dat$Q47_0_13_RANK # sharks
dat$Q47_0_14_RANK # sheepshead
dat$Q47_0_15_RANK # snapper
dat$Q47_0_16_RANK # spotted seatrout
dat$Q47_0_17_RANK # tripletail
dat$Q47_0_18_RANK # tuna
dat$Q47_0_19_RANK # wahoo
dat$Q47_0_20_RANK # other
dat$Q47_0_21_RANK # other
dat$Q47_0_22_RANK # other

dat$Q47_20_TEXT # alot of snook and tarpon here!!!!!!
dat$Q47_21_TEXT # more snook and tarpon. also saw triggerfish and macabi (bonefish)
dat$Q47_22_TEXT # more snook
### the fact that snook/robalo was found in all 3 text options leads me to
### believe we should have included this species
### snook are captured in inshore waters in both USA and MEX

### verify that tarpon is on all 3 text columns



### we can use this to figure out how many time species were selected
q47unanswered <- dat[grep("Unanswered", dat$Q47_0_GROUP), ]
q47unanswered$Q47_0_GROUP
nrow(q47unanswered) # number of surveys that didn't answer Q21
q47unanswered$Q47_0_1_RANK
q21unanswered$Q47_0_19_RANK
### when Unanswered is in the group column, it means the question was not answered
### this means we can ignore the Unanswered after noting its sample size

q47answered <- dat[grep("Unanswered", dat$Q47_0_GROUP, invert = TRUE), ]
q47answered$Q47_0_GROUP
nrow(q47answered) # number of surveys that answered Q47





### filtering for surveys that included a specific species
amberj <- dat[grep("Amber", dat$Q47_0_GROUP), ]
amberj$Q47_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- dat[grep("Black", dat$Q47_0_GROUP), ]
black$Q47_0_GROUP
nrow(black)
table(black$Q44)

cobia <- dat[grep("Cobia", dat$Q47_0_GROUP), ]
cobia$Q47_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- dat[grep("False", dat$Q47_0_GROUP), ]
false$Q47_0_GROUP
nrow(false)
table(false$Q44)

floun <- dat[grep("Flounder", dat$Q47_0_GROUP), ]
floun$Q47_0_GROUP
nrow(floun)
table(floun$Q44)

group <- dat[grep("Grouper", dat$Q47_0_GROUP), ]
group$Q47_0_GROUP
nrow(group)
table(group$Q44)

crev <- dat[grep("Crevalle", dat$Q47_0_GROUP), ]
crev$Q47_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- dat[grep("Mahi", dat$Q47_0_GROUP), ]
mahi$Q47_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- dat[grep("Mack", dat$Q47_0_GROUP), ]
mack$Q47_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- dat[grep("Pompano", dat$Q47_0_GROUP), ]
pomp$Q47_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- dat[grep("Red Drum", dat$Q47_0_GROUP), ]
reddrum$Q47_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- dat[grep("Sailf", dat$Q47_0_GROUP), ]
sail$Q47_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- dat[grep("Shark", dat$Q47_0_GROUP), ]
shark$Q47_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- dat[grep("Sheeps", dat$Q47_0_GROUP), ]
sheep$Q47_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- dat[grep("Snapper", dat$Q47_0_GROUP), ]
snapper$Q47_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and MEX

trout <- dat[grep("Spotted", dat$Q47_0_GROUP), ]
trout$Q47_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- dat[grep("Triple", dat$Q47_0_GROUP), ]
triple$Q47_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- dat[grep("Tuna", dat$Q47_0_GROUP), ]
tuna$Q47_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- dat[grep("Wahoo", dat$Q47_0_GROUP), ]
wahoo$Q47_0_GROUP
nrow(wahoo)
table(wahoo$Q44)



#### adding other species stated in TEXT
table(dat$Q47_20_TEXT) # snook and tarpon are the only ones that have n>5
table(dat$Q47_21_TEXT) # snook and tarpon are the only ones that have n>5
table(dat$Q47_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ", "robalo")
# these are all of the names I saw in the 3 tect columns that refer to snook

snook1 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q47_20_TEXT))
table(snook1$Q47_20_TEXT) # only column that should have any of the results
table(snook1$Q47_21_TEXT)
table(snook1$Q47_22_TEXT)

snook2 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q47_21_TEXT))
table(snook2$Q47_20_TEXT)
table(snook2$Q47_21_TEXT) # only column that should have any of the results
table(snook2$Q47_22_TEXT)

snook3 <- filter(dat, grepl(paste(snooknames, collapse='|'), Q47_22_TEXT))
table(snook3$Q47_20_TEXT)
table(snook3$Q47_21_TEXT)
table(snook3$Q47_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ",
                 "Trapon","sabalo", "tarpon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q47_20_TEXT))
table(tarpon1$Q47_20_TEXT) # only column that should have any of the results
table(tarpon1$Q47_21_TEXT)
table(tarpon1$Q47_22_TEXT)

tarpon2 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q47_21_TEXT))
table(tarpon2$Q47_20_TEXT)
table(tarpon2$Q47_21_TEXT) # only column that should have any of the results
table(tarpon2$Q47_22_TEXT)

tarpon3 <- filter(dat, grepl(paste(tarponnames, collapse='|'), Q47_22_TEXT))
table(tarpon3$Q47_20_TEXT)
table(tarpon3$Q47_21_TEXT)
table(tarpon3$Q47_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above



### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q47speciesnall <- data.frame(spp, sppn)

## bar plot
ggplot(data=q47speciesnall, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "What are your 3 top target species?", y = "Count",
       title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q47all.png", width = 10, height = 8, dpi = 1000)



#### MEX only
mex <- dat[dat$Q44 == "Mexico",]

amberj <- mex[grep("Amber", mex$Q47_0_GROUP), ]
amberj$Q47_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- mex[grep("Black", mex$Q47_0_GROUP), ]
black$Q47_0_GROUP
nrow(black)
table(black$Q44)

cobia <- mex[grep("Cobia", mex$Q47_0_GROUP), ]
cobia$Q47_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- mex[grep("False", mex$Q47_0_GROUP), ]
false$Q47_0_GROUP
nrow(false)
table(false$Q44)

floun <- mex[grep("Flounder", mex$Q47_0_GROUP), ]
floun$Q47_0_GROUP
nrow(floun)
table(floun$Q44)

group <- mex[grep("Grouper", mex$Q47_0_GROUP), ]
group$Q47_0_GROUP
nrow(group)
table(group$Q44)

crev <- mex[grep("Crevalle", mex$Q47_0_GROUP), ]
crev$Q47_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- mex[grep("Mahi", mex$Q47_0_GROUP), ]
mahi$Q47_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- mex[grep("Mack", mex$Q47_0_GROUP), ]
mack$Q47_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- mex[grep("Pompano", mex$Q47_0_GROUP), ]
pomp$Q47_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- mex[grep("Red Drum", mex$Q47_0_GROUP), ]
reddrum$Q47_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- mex[grep("Sailf", mex$Q47_0_GROUP), ]
sail$Q47_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- mex[grep("Shark", mex$Q47_0_GROUP), ]
shark$Q47_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- mex[grep("Sheeps", mex$Q47_0_GROUP), ]
sheep$Q47_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- mex[grep("Snapper", mex$Q47_0_GROUP), ]
snapper$Q47_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and MEX

trout <- mex[grep("Spotted", mex$Q47_0_GROUP), ]
trout$Q47_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- mex[grep("Triple", mex$Q47_0_GROUP), ]
triple$Q47_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- mex[grep("Tuna", mex$Q47_0_GROUP), ]
tuna$Q47_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- mex[grep("Wahoo", mex$Q47_0_GROUP), ]
wahoo$Q47_0_GROUP
nrow(wahoo)
table(wahoo$Q44)



#### adding other species stated in TEXT
table(mex$Q47_20_TEXT) # snook and tarpon are the only ones that have n>5
table(mex$Q47_21_TEXT) # snook and tarpon are the only ones that have n>5
table(mex$Q47_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ", "robalo")
# these are all of the names I saw in the 3 tect columns that refer to snook

snook1 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q47_20_TEXT))
table(snook1$Q47_20_TEXT) # only column that should have any of the results
table(snook1$Q47_21_TEXT)
table(snook1$Q47_22_TEXT)

snook2 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q47_21_TEXT))
table(snook2$Q47_20_TEXT)
table(snook2$Q47_21_TEXT) # only column that should have any of the results
table(snook2$Q47_22_TEXT)

snook3 <- filter(mex, grepl(paste(snooknames, collapse='|'), Q47_22_TEXT))
table(snook3$Q47_20_TEXT)
table(snook3$Q47_21_TEXT)
table(snook3$Q47_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ",
                 "Trapon","sabalo", "tarpon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q47_20_TEXT))
table(tarpon1$Q47_20_TEXT) # only column that should have any of the results
table(tarpon1$Q47_21_TEXT)
table(tarpon1$Q47_22_TEXT)

tarpon2 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q47_21_TEXT))
table(tarpon2$Q47_20_TEXT)
table(tarpon2$Q47_21_TEXT) # only column that should have any of the results
table(tarpon2$Q47_22_TEXT)

tarpon3 <- filter(mex, grepl(paste(tarponnames, collapse='|'), Q47_22_TEXT))
table(tarpon3$Q47_20_TEXT)
table(tarpon3$Q47_21_TEXT)
table(tarpon3$Q47_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above



### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q47speciesnmex <- data.frame(spp, sppn)

## bar plot
ggplot(data=q47speciesnmex, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "What are your 3 top target species?", y = "Count",
       title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q47mex.png", width = 10, height = 8, dpi = 1000)



###### USA only
usa <- dat[dat$Q44 == "United States",]

amberj <- usa[grep("Amber", usa$Q47_0_GROUP), ]
amberj$Q47_0_GROUP
nrow(amberj)
table(amberj$Q44)

black <- usa[grep("Black", usa$Q47_0_GROUP), ]
black$Q47_0_GROUP
nrow(black)
table(black$Q44)

cobia <- usa[grep("Cobia", usa$Q47_0_GROUP), ]
cobia$Q47_0_GROUP
nrow(cobia)
table(cobia$Q44)

false <- usa[grep("False", usa$Q47_0_GROUP), ]
false$Q47_0_GROUP
nrow(false)
table(false$Q44)

floun <- usa[grep("Flounder", usa$Q47_0_GROUP), ]
floun$Q47_0_GROUP
nrow(floun)
table(floun$Q44)

group <- usa[grep("Grouper", usa$Q47_0_GROUP), ]
group$Q47_0_GROUP
nrow(group)
table(group$Q44)

crev <- usa[grep("Crevalle", usa$Q47_0_GROUP), ]
crev$Q47_0_GROUP
nrow(crev)
table(crev$Q44)

mahi <- usa[grep("Mahi", usa$Q47_0_GROUP), ]
mahi$Q47_0_GROUP
nrow(mahi)
table(mahi$Q44)

mack <- usa[grep("Mack", usa$Q47_0_GROUP), ]
mack$Q47_0_GROUP
nrow(mack)
table(mack$Q44)

pomp <- usa[grep("Pompano", usa$Q47_0_GROUP), ]
pomp$Q47_0_GROUP
nrow(pomp)
table(pomp$Q44)

reddrum <- usa[grep("Red Drum", usa$Q47_0_GROUP), ]
reddrum$Q47_0_GROUP
nrow(reddrum)
table(reddrum$Q44) ### all red drum answers are form USA

sail <- usa[grep("Sailf", usa$Q47_0_GROUP), ]
sail$Q47_0_GROUP
nrow(sail)
table(sail$Q44)

shark <- usa[grep("Shark", usa$Q47_0_GROUP), ]
shark$Q47_0_GROUP
nrow(shark)
table(shark$Q44)

sheep <- usa[grep("Sheeps", usa$Q47_0_GROUP), ]
sheep$Q47_0_GROUP
nrow(sheep)
table(sheep$Q44)

snapper <- usa[grep("Snapper", usa$Q47_0_GROUP), ]
snapper$Q47_0_GROUP
nrow(snapper)
table(snapper$Q44) ### selected in both USA and usa

trout <- usa[grep("Spotted", usa$Q47_0_GROUP), ]
trout$Q47_0_GROUP
nrow(trout)
table(trout$Q44)

triple <- usa[grep("Triple", usa$Q47_0_GROUP), ]
triple$Q47_0_GROUP
nrow(triple)
table(triple$Q44)

tuna <- usa[grep("Tuna", usa$Q47_0_GROUP), ]
tuna$Q47_0_GROUP
nrow(tuna)
table(tuna$Q44)

wahoo <- usa[grep("Wahoo", usa$Q47_0_GROUP), ]
wahoo$Q47_0_GROUP
nrow(wahoo)
table(wahoo$Q44)



#### adding other species stated in TEXT
table(usa$Q47_20_TEXT) # snook and tarpon are the only ones that have n>5
table(usa$Q47_21_TEXT) # snook and tarpon are the only ones that have n>5
table(usa$Q47_22_TEXT)


### Snook
snooknames <- c("Common snook", "Robalo", "Robalo ", "Robalo blanco","snook",
                "Snook", "Snook ", "robalo")
# these are all of the names I saw in the 3 tect columns that refer to snook

snook1 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q47_20_TEXT))
table(snook1$Q47_20_TEXT) # only column that should have any of the results
table(snook1$Q47_21_TEXT)
table(snook1$Q47_22_TEXT)

snook2 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q47_21_TEXT))
table(snook2$Q47_20_TEXT)
table(snook2$Q47_21_TEXT) # only column that should have any of the results
table(snook2$Q47_22_TEXT)

snook3 <- filter(usa, grepl(paste(snooknames, collapse='|'), Q47_22_TEXT))
table(snook3$Q47_20_TEXT)
table(snook3$Q47_21_TEXT)
table(snook3$Q47_22_TEXT) # only column that should have any of the results

snooklist <- list(snook1, snook2, snook3)
snooks <- Reduce(function(x, y) merge(x, y, all=TRUE), snooklist)  
nrow(snooks)
nrow(snook1) + nrow(snook2) + nrow(snook3) # should equal line above


## tarpon
tarponnames <- c("Sábalo","Sabalo ","tarpo","Tarpon", "Sabalo", "Tarpon ",
                 "Trapon","sabalo", "tarpon")
# these are all of the names I saw in the 3 tect columns that refer to tarpon

tarpon1 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q47_20_TEXT))
table(tarpon1$Q47_20_TEXT) # only column that should have any of the results
table(tarpon1$Q47_21_TEXT)
table(tarpon1$Q47_22_TEXT)

tarpon2 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q47_21_TEXT))
table(tarpon2$Q47_20_TEXT)
table(tarpon2$Q47_21_TEXT) # only column that should have any of the results
table(tarpon2$Q47_22_TEXT)

tarpon3 <- filter(usa, grepl(paste(tarponnames, collapse='|'), Q47_22_TEXT))
table(tarpon3$Q47_20_TEXT)
table(tarpon3$Q47_21_TEXT)
table(tarpon3$Q47_22_TEXT) # only column that should have any of the results

tarponlist <- list(tarpon1, tarpon2, tarpon3)
tarpons <- Reduce(function(x, y) merge(x, y, all=TRUE), tarponlist)  
nrow(tarpons)
nrow(tarpon1) + nrow(tarpon2) + nrow(tarpon3) # should equal line above



### bar plot with spp frequencies
spp <- c("Amberjack","Black Drum","Cobia","False Albacore","Flounder","Grouper",
         "Jack Crevalle","Mahi Mahi","Mackerels","Pompano","Red Drum","Sailfish",
         "Sharks","Sheepshead","Snappers","Snooks","Spotted Seatrout","Tarpon",
         "Tripletail","Tuna","Wahoo")
sppn <- c(nrow(amberj),nrow(black),nrow(cobia),nrow(false),nrow(floun),
          nrow(group),nrow(crev),nrow(mahi),nrow(mack),nrow(pomp),nrow(reddrum),
          nrow(sail),nrow(shark),nrow(sheep),nrow(snapper),nrow(snooks),
          nrow(trout),nrow(tarpons),nrow(triple),nrow(tuna),nrow(wahoo))
q47speciesnusa <- data.frame(spp, sppn)

## bar plot
ggplot(data=q47speciesnusa, aes(x=spp, y=sppn)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "What are your 3 top target species?", y = "Count",
       title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q47usa.png", width = 10, height = 8, dpi = 1000)









#### end Q47
################################################################################
######## Q12 How do you travel to where you go fishing?
table(dat$Q12)
table(dat$Q12_5_TEXT)
nrow(dat) - nrow(dat[dat$Q12 == "Unanswered",]) # people that answered
nrow(dat) - nrow(dat[dat$Q12_5_TEXT == "Unanswered",]) # people that answered

## will do new dataframe manually 
Method <- c("Bus", "Personal Vehicle", "Plane", "Rented Vehicle", "Bike", "Boat",
            "Walking", "Personal Dock","Motorcycle",
            "Personal Vehicle + Airplane","Boat then Wade","Borrowed Vehicle")
All <- c(1,143,3,4,1,1,2,1,1,2,1,1) 
1+143+3+4+1+1+2+1+1+2+1+1 #sample size

table(usa$Q12)
table(usa$Q12_5_TEXT)

USA <-c(0,97,2,0,1,1,0,1,0,2,1,0)

table(mex$Q12)
table(mex$Q12_5_TEXT)

MEX <- c(1,46,1,3,0,0,2,0,1,0,0,1)


q12answersall <- cbind(Method, All, USA, MEX)
q12answersall <- as.data.frame(q12answersall)
q12answersall


library(gridExtra)
grid.table(q12answersall)



pdf("q12answersall.pdf", height=11, width=8.5)

library(gtable)
g <- tableGrob(q12answersall, rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
grid.draw(g)
dev.off()









# END q12
################################################################################
### Q13 Q13	How many miles do you travel from your home to the place where you 
### leave to fish? - Selected Choice
### Q13_5_TEXT	How many miles do you travel from your home to the place where
### you leave to fish? - More than 100 miles - Text
### *** this question only applies to USA: I'll show that below
table(dat$Q13)
table(dat$Q13_5_TEXT)

q13answered <- dat[grep("Unanswered", dat$Q13, invert = TRUE), ]
table(q13answered$Q13)
nrow(q13answered) # number of surveys that answered Q13

q13answers <- table(q13answered$Q13)
q13answers <- as.data.frame(q13answers)
q13answers$Var1 <- as.character(q13answers$Var1)
q13answers[1,1] <- "Left blank"
q13answers[6,1] <- "> 100 miles"
q13answers$Var1 <- as.factor(q13answers$Var1)
levels(q13answers$Var1)
q13answers$Var1 <- factor(q13answers$Var1, levels=c('0 - 10 miles',
                                                    '10 - 25 miles',
                                                    '25 - 50 miles',
                                                    '50 - 100 miles',
                                                    '> 100 miles',
                                                    'Left blank'))
q13answers


ggplot(data=q13answers, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "How many miles do you travel from your home to the place where you leave to fish?",
       y = "Count", title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q13all.png", width = 10, height = 8, dpi = 1000)





######### usa
q13usa <- q13answered[q13answered$Q44 == "United States",]
nrow(q13usa) # sample size
table(q13usa$Q13)


q13answersusa <- table(q13usa$Q13)
q13answersusa <- as.data.frame(q13answersusa)
q13answersusa
q13answersusa$Var1 <- as.character(q13answersusa$Var1)
q13answersusa[5,1] <- "> 100 miles"
q13answersusa

q13answersusa$Var1 <- as.factor(q13answersusa$Var1)
levels(q13answersusa$Var1)
q13answersusa$Var1 <- factor(q13answersusa$Var1, levels=c('0 - 10 miles',
                                                    '10 - 25 miles',
                                                    '25 - 50 miles',
                                                    '50 - 100 miles',
                                                    '> 100 miles'))
q13answersusa


ggplot(data=q13answersusa, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "How many miles do you travel from your home to the place where you leave to fish?",
       y = "Count", title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q13usa.png", width = 10, height = 8, dpi = 1000)




#### mex does not answer this question
q13mex <- q13answered[q13answered$Q44 == "Mexico",]
nrow(q13mex) # sample size
table(q13mex$Q13)














###END Q13
################################################################################
### Q50	How many kilometers do you travel from your home to the place where you
### leave to fish? - Selected Choice
### Q50_5_TEXT	How many kilometers do you travel from your home to the place
### where you leave to fish? - More than 150 km - Text
### *** question for MEX and CUB only
table(dat$Q50)


q50answered <- dat[grep("Unanswered", dat$Q50, invert = TRUE), ]
table(q13answered$Q50)
nrow(q50answered) # number of surveys that answered Q50
table(q50answered$Q44) # sample sizes by country

## confirming that there ar eno answers coming from USA
q50usa <- q50answered[q50answered$Q44 == "United States",]
table(q50usa$Q50) # all answers blank




######### mex
q50mex <- q50answered[q50answered$Q44 == "Mexico",]
nrow(q50mex) # sample size
table(q50mex$Q50)
table(q50mex$Q50_5_TEXT) # no additional text answers!

q50answersmex <- table(q50mex$Q50)
q50answersmex <- as.data.frame(q50answersmex)
q50answersmex
q50answersmex$Var1 <- as.character(q50answersmex$Var1)
q50answersmex[5,1] <- "> 150 km"
q50answersmex

q50answersmex$Var1 <- as.factor(q50answersmex$Var1)
levels(q50answersmex$Var1)
q50answersmex$Var1 <- factor(q50answersmex$Var1, levels=c('0 - 15 km',
                                                          '15 - 50 km',
                                                          '50 - 100 km',
                                                          '100 - 150 km',
                                                          '> 150 km'))
q50answersmex


ggplot(data=q50answersmex, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "How many kilometers do you travel from your home to the place where you leave to fish?",
       y = "Count", title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q50mex.png", width = 10, height = 8, dpi = 1000)



















##### END Q50
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

















### END q15
################################################################################
### Q16	How many sport fishing tournaments do you attend each year? - Selected Choice
### Q16_5_TEXT	How many sport fishing tournaments do you attend each year? - 
### More than 20 - Text
table(dat$Q16)
table(dat$Q16_5_TEXT) # only one answer

q16answered <- dat[grep("Unanswered", dat$Q16, invert = TRUE), ]
table(q16answered$Q16)
nrow(q16answered) # number of surveys that answered Q16
table(q16answered$Q44) # surveys by country

q16answersall <- as.data.frame(table(q16answered$Q16))
q16answersall$Var1 <- as.character(q16answersall$Var1)
q16answersall[3,1] <- "< 5"
q16answersall[4,1] <- "> 20"

q16answersall$Var1 <- as.factor(q16answersall$Var1)
levels(q16answersall$Var1)
q16answersall$Var1 <- factor(q16answersall$Var1, levels=c('None',
                                                          '< 5',
                                                          '5 - 10',
                                                          '10 - 20',
                                                          '> 20'))
q16answersall


ggplot(data=q16answersall, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "How many sport fishing tournaments do you attend each year?",
       y = "Count",title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q16all.png", width = 10, height = 8, dpi = 1000)


#### usa
q16answeredusa <- q16answered[q16answered$Q44 == "United States",]
nrow(q16answeredusa) #usa sample size

q16answersusa <- as.data.frame(table(q16answeredusa$Q16))
q16answersusa$Var1 <- as.character(q16answersusa$Var1)
q16answersusa
q16answersusa[3,1] <- "< 5"
q16answersusa[4,1] <- "> 20"

q16answersusa$Var1 <- as.factor(q16answersusa$Var1)
levels(q16answersusa$Var1)
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







#### END q16
################################################################################
### Q17	In the last year, how much have you paid individually to enter fishing
### tournaments? - Selected Choice
### Q17_6_TEXT	In the last year, how much have you paid individually to enter 
### fishing tournaments? - More than $5,000 USD (Please specify) - Text
### *** usa only
table(dat$Q17)
table(dat$Q17_6_TEXT)

q17answered <- dat[grep("Unanswered", dat$Q17, invert = TRUE), ]
table(q17answered$Q17)
nrow(q17answered) # number of surveys that answered Q16
table(q17answered$Q44) # surveys by country

q17usa <- q17answered[q17answered$Q44 == "United States",]
table(q17usa$Q17)

## confirming Mexico and Cuba were all blank
q17mex <- q17answered[q17answered$Q44 == "Mexico",]
table(q17mex$Q17)

q17cub <- q17answered[q17answered$Q44 == "Cuba",]
table(q17cub$Q17)



#### usa
nrow(q17usa) #usa sample size

q17usa <- as.data.frame(table(q17usa$Q17))
q17usa$Var1 <- as.character(q17usa$Var1)
q17usa
q17usa[1,1] <- "Left blank"
q17usa[5,1] <- "< $250 USD"
q17usa[6,1] <- "> $5,000 USD"

q17usa$Var1 <- as.factor(q17usa$Var1)
levels(q17usa$Var1)
q17usa$Var1 <- factor(q17usa$Var1, levels=c('< $250 USD','$250 - $500 USD',
                                                          '$500 - $1,000 USD',
                                                          '$2,500 - $5,000 USD',
                                                          '> $5,000 USD',
                                            'Left blank'))
q17usa

ggplot(data=q17usa, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "In the last year, how much have you paid individually to enter fishing tournaments?",
       y = "Count",title = "USA n=106") +
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

q51answered <- dat[grep("Unanswered", dat$Q51, invert = TRUE), ]
table(q51answered$Q51)
nrow(q51answered) # number of surveys that answered Q51
table(q51answered$Q44) # surveys by country

q51mex <- q51answered[q51answered$Q44 == "Mexico",]
table(q51mex$Q51)

## confirming USA and Cuba were all blank
q51usa <- q51answered[q51answered$Q44 == "United States",]
table(q51usa$Q51)

q51cub <- q51answered[q51answered$Q44 == "Cuba",]
table(q51cub$Q51)


#### usa
nrow(q51mex) #mex sample size

q51mex <- as.data.frame(table(q51mex$Q51))
q51mex$Var1 <- as.character(q51mex$Var1)
q51mex
q51mex[1,1] <- "Left blank"
q51mex[6,1] <- "< $500 MX"
q51mex[7,1] <- "> $10,000 MX"

q51mex$Var1 <- as.factor(q51mex$Var1)
levels(q51mex$Var1)
q51mex$Var1 <- factor(q51mex$Var1, levels=c('< $500 MX','$500 - $1,500 MX',
                                            '$1,500 - $3,000 MX',
                                            '$3,000 - $5,000 MX',
                                            '$5,000 - $10,000 MX',
                                            '> $10,000 MX',
                                            'Left blank'))
q51mex

ggplot(data=q51mex, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "In the last year, how much have you paid individually to enter fishing tournaments?",
       y = "Count",title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q51mex.png", width = 10, height = 8, dpi = 1000)







### end q51
################################################################################
### Q72	In the last year, how much have you paid individually to enter fishing
### tournaments? - Selected Choice
### Q72_6_TEXT	In the last year, how much have you paid individually to enter 
### fishing tournaments? - More than $10,000 CUP (Please specify) - Text
### *** for cuba only???
table(dat$Q72)
table(dat$Q72_6_TEXT) # no additional text entries

q72answered <- dat[grep("Unanswered", dat$Q72, invert = TRUE), ]
table(q72answered$Q72)
nrow(q72answered) # number of surveys that answered Q72
table(q72answered$Q44) # surveys by country

#### all the answers are coming from Mexico. Look into this
q72mex <- q72answered[q72answered$Q44 == "Mexico",]
table(q72mex$Q72)

## confirming USA and Cuba were all blank
q72usa <- q72answered[q72answered$Q44 == "United States",]
table(q72usa$Q72)

q72cub <- q72answered[q72answered$Q44 == "Cuba",]
table(q72cub$Q72)









#### end Q72
################################################################################
### Q18	Where do you usually buy your fishing gear/items? (Select all that apply)
table(dat$Q18)

q18answered <- dat[grep("Unanswered", dat$Q18, invert = TRUE), ]
table(q18answered$Q18)
nrow(q18answered) # number of surveys that answered Q18
table(q18answered$Q44) # surveys by country

### city
city <- q18answered[grep("city", q18answered$Q18), ]
city$Q18
nrow(city) #sample size of surveys that include city

### Traveling
traveling <- q18answered[grep("traveling", q18answered$Q18), ]
traveling$Q18
nrow(traveling) #sample size of surveys that include traveling

##internet
internet <- q18answered[grep("Internet", q18answered$Q18), ]
internet$Q18
nrow(internet) #sample size of surveys that include Internet

## do not buy gear
gear <- q18answered[grep("gear", q18answered$Q18), ]
gear$Q18
nrow(gear) #sample size of surveys that include gear

### bar plot 
where <- c("Fishing shop in my city","Fishing shop where I'm traveling",
           "Internet","I do not buy my gear")
wheren <- c(nrow(city),nrow(traveling),nrow(internet),nrow(gear))
q18all <- data.frame(where,wheren)
q18all

ggplot(data=q18all, aes(x=where, y=wheren)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "Where do you usually buy your fishing gear/items?",
       y = "Count",title = "All n=162") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q18all.png", width = 10, height = 8, dpi = 1000)


## usa
q18answeredusa <- q18answered[q18answered$Q44 == "United States",]
nrow(q18answeredusa)

### city
city <- q18answeredusa[grep("city", q18answeredusa$Q18), ]
city$Q18
nrow(city) #sample size of surveys that include city

### Traveling
traveling <- q18answeredusa[grep("traveling", q18answeredusa$Q18), ]
traveling$Q18
nrow(traveling) #sample size of surveys that include traveling

##internet
internet <- q18answeredusa[grep("Internet", q18answeredusa$Q18), ]
internet$Q18
nrow(internet) #sample size of surveys that include Internet

## do not buy gear
gear <- q18answeredusa[grep("gear", q18answeredusa$Q18), ]
gear$Q18
nrow(gear) #sample size of surveys that include gear

### bar plot 
where <- c("Fishing shop in my city","Fishing shop where I'm traveling",
           "Internet","I do not buy my gear")
wheren <- c(nrow(city),nrow(traveling),nrow(internet),nrow(gear))
q18usa <- data.frame(where,wheren)
q18usa

ggplot(data=q18usa, aes(x=where, y=wheren)) +
  geom_bar(stat="identity", fill = "darkred") +
  theme_classic() +
  labs(x = "Where do you usually buy your fishing gear/items?",
       y = "Count",title = "USA n=106") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q18usa.png", width = 10, height = 8, dpi = 1000)


### mex
q18answeredmex <- q18answered[q18answered$Q44 == "Mexico",]
nrow(q18answeredmex)

### city
city <- q18answeredmex[grep("city", q18answeredmex$Q18), ]
city$Q18
nrow(city) #sample size of surveys that include city

### Traveling
traveling <- q18answeredmex[grep("traveling", q18answeredmex$Q18), ]
traveling$Q18
nrow(traveling) #sample size of surveys that include traveling

##internet
internet <- q18answeredmex[grep("Internet", q18answeredmex$Q18), ]
internet$Q18
nrow(internet) #sample size of surveys that include Internet

## do not buy gear
gear <- q18answeredmex[grep("gear", q18answeredmex$Q18), ]
gear$Q18
nrow(gear) #sample size of surveys that include gear

### bar plot 
where <- c("Fishing shop in my city","Fishing shop where I'm traveling",
           "Internet","I do not buy my gear")
wheren <- c(nrow(city),nrow(traveling),nrow(internet),nrow(gear))
q18mex <- data.frame(where,wheren)
q18mex

ggplot(data=q18mex, aes(x=where, y=wheren)) +
  geom_bar(stat="identity", fill = "darkgreen") +
  theme_classic() +
  labs(x = "Where do you usually buy your fishing gear/items?",
       y = "Count",title = "Mex n=55") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q18mex.png", width = 10, height = 8, dpi = 1000)






### end q18
###############################################################################