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
  labs(x = "What are your top 3 most caught species?", y = "Count") +
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
  labs(x = "What are your top 3 most caught species?", y = "Count", title = "Mex") +
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
  labs(x = "What are your top 3 most caught species?", y = "Count", title = "USA") +
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
  labs(x = "What are your 3 top target species?", y = "Count") +
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
  labs(x = "What are your 3 top target species?", y = "Count", title = "Mex") +
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
  labs(x = "What are your 3 top target species?", y = "Count", title = "USA") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#ggsave("q47usa.png", width = 10, height = 8, dpi = 1000)




#### end Q47
################################################################################




