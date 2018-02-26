
#---------------------------------------IDS570 - Statistics for Management---------------------------------

#----------------------------------------------Airbnb - Melbourne------------------------------------------

# Library Files
library(corrplot)
library(gplots)
library(ggplot2)
library(psych)
library(dplyr)

# Set working directory
# setwd("C:/Users/yogi8/Documents/Course Works/IDS 570 - Stats/submit/")

# Select CSV File
airbnbdata <- read.csv(file.choose(), header = T)

# Omitting NAs and removing Latitude, Longitude, X, Collected, neighborhood columns
airbnbdata <- na.omit(airbnbdata) #removing NA
is.na.data.frame(airbnbdata) #Check for Nulls
airbnbdata$latitude <- NULL
airbnbdata$longitude <- NULL
airbnbdata$collected <- NULL
airbnbdata$neighborhood <- NULL
airbnbdata$X <- NULL

airbnbdata$cityZone <- "test" #initialize cityZone to test

# Removing null values and question marks
airbnbdata <- airbnbdata[airbnbdata$city != "",]
airbnbdata <- airbnbdata[airbnbdata$city != "???",]

airbnbdata$cityCheck <- "test" #initialize cityCheck to test

airbnbdata$city[airbnbdata$city == "Windsor / Prahran"] <- "Windsor"
airbnbdata$city[airbnbdata$city == "Brunswick / Melbourne"] <-
  "Brunswick"
airbnbdata$city[airbnbdata$city == "Prahran / Toorak"] <- "Prahran"
airbnbdata$city[airbnbdata$city == "Coburg (Melbourne)"] <- "Coburg"
airbnbdata$city[airbnbdata$city == "Brunswick East, Melbourne"] <-
  "Brunswick East"
airbnbdata$city[airbnbdata$city == "Abbotsford, Melbourne"] <-
  "Abbotsford"
airbnbdata$city[airbnbdata$city == "Abbotsford, Melbourne"] <-
  "Abbotsford"
airbnbdata$city[airbnbdata$city == "Fitzroy, Melbourne"] <-
  "Fitzroy"
airbnbdata$city[airbnbdata$city == "Northcote, Melbourne"] <-
  "Northcote"
airbnbdata$city[airbnbdata$city == "Collingwood, Melbourne"] <-
  "Collingwood"
airbnbdata$city[airbnbdata$city == "Elwood, Melbourne"] <- "Elwood"
airbnbdata$city[airbnbdata$city == "Richmond, Melbourne"] <-
  "Richmond"
airbnbdata$city[airbnbdata$city == "Hawthorn, Melbourne"] <-
  "Hawthorn"
airbnbdata$city[airbnbdata$city == "St Kilda West, Melbourne"] <-
  "St Kilda West"
airbnbdata$city[airbnbdata$city == "Prahran, Melbourne"] <-
  "Prahran"
airbnbdata$city[airbnbdata$city == "Toorak, Melbourne"] <- "Toorak"
airbnbdata$city[airbnbdata$city == "South Melbourne, Melbourne"] <-
  "South Melbourne"
airbnbdata$city[airbnbdata$city == "Carlton, Melbourne"] <-
  "Carlton"
airbnbdata$city[airbnbdata$city == "Malvern, Melbourne"] <-
  "Malvern"
airbnbdata$city[airbnbdata$city == "Burnley, Melbourne"] <-
  "Burnley"
airbnbdata$city[airbnbdata$city == "Fitzroy North, Melbourne"] <-
  "Fitzroy North"
airbnbdata$city[airbnbdata$city == "Toorak, Melbourne."] <- "Toorak"
airbnbdata$city[airbnbdata$city == "Kew, Melbourne"] <- "Kew"
airbnbdata$city[airbnbdata$city == "Southbank, Melbourne"] <-
  "Southbank"
airbnbdata$city[airbnbdata$city == "Brunswick, Melbourne"] <-
  "Brunswick"
airbnbdata$city[airbnbdata$city == "Melbourne, Docklands"] <-
  "Melbourne"
airbnbdata <-
  airbnbdata[airbnbdata$city != "Melbourne; Fitzroy; Carlton",]

airbnbdata <- airbnbdata[airbnbdata$city != "Windsor / Prahran",]
airbnbdata <-
  airbnbdata[airbnbdata$city != "Brunswick / Melbourne",]
airbnbdata <- airbnbdata[airbnbdata$city != "Prahran / Toorak",]

airbnbdata$city[airbnbdata$city == "Saint Kilda"] <- "St Kilda"
airbnbdata$city[airbnbdata$city == "Saint Kilda East"] <-
  "St Kilda East"
airbnbdata$city[airbnbdata$city == "Saint Kilda West"] <-
  "St Kilda West"
airbnbdata$city[airbnbdata$city == "East St. Kilda"] <-
  "St Kilda East"
airbnbdata$city[airbnbdata$city == "St. Kilda"] <- "St Kilda"
airbnbdata$city[airbnbdata$city == "St. Kilda West"] <-
  "St Kilda West"
airbnbdata$city[airbnbdata$city == "Melbourne St Kilda Road"] <-
  "St Kilda"



airbnbdata$city[airbnbdata$city == "Albert Park Melbourne"] <-
  "Albert Park"
airbnbdata$city[airbnbdata$city == "melbouren"] <- "Melbourne"
airbnbdata$city[airbnbdata$city == "Praharan"] <- "Prahran"
airbnbdata$city[airbnbdata$city == "Prahran Melbourne"] <- "Prahran"
airbnbdata$city[airbnbdata$city == "Southbank, Melbourne"] <-
  "Southbank"
airbnbdata$city[airbnbdata$city == "Southbank Melbourne"] <-
  "Southbank"





abc <-
  c(
    "Carlton South",
    "Carlton, Melbourne",
    "Caulfield East",
    "Clifton Hill",
    "Coburg",
    "Coburg (Melbourne)",
    "Coburg North",
    "Collingwood",
    "Collingwood, Melbourne",
    "Cremorne",
    "Docklands",
    "Fitzroy" ,
    "Fitzroy North" ,
    "Fitzroy, Melbourne" ,
    "Flemington" ,
    "footscray" ,
    "Footscray" ,
    "Glen Iris" ,
    "Hawthorn" ,
    "Hawthorn East" ,
    "Kensington",
    "Parkville",
    "Pascoe Vale South",
    "Port Melbourne",
    "Praharan",
    "Prahran",
    "Prahran / Toorak",
    "Prahran Melbourne",
    "Prahran, Melbourne",
    "Preston",
    "Princes Hill",
    "Richmond",
    "RICHMOND",
    "Ripponlea",
    "Saint Kilda",
    "Saint Kilda East",
    "Saint Kilda West",
    "Brunswick East" ,
    "Brunswick East, Melbourne",
    "Brunswick West",
    "Burnley" ,
    "Camberwell",
    "Carlton",
    "Carlton North",
    "Abbotsford",
    "Albert Park",
    "Albert Park Melbourne",
    "Alphington",
    "Ascot Vale",
    "Balaclava",
    "Balwyn",
    "Balwyn North",
    "brunswick",
    "Seddon",
    "South Kingsville",
    "South Melbourne",
    "South Yarra",
    "Southbank",
    "Spotswood",
    "St Kilda",
    "St Kilda East",
    "St Kilda West",
    "St Kilda West, Melbourne",
    "St. Kilda",
    "Thornbury",
    "Toorak",
    "Travancore",
    "West Footscray",
    "West Melbourne",
    "Williamstown",
    "Yarraville",
    "Melbourne",
    "Kew",
    "Northcote",
    "Newport",
    "Middle Park",
    "North Melbourne",
    "Kooyong",
    "Kingsville",
    "Malvern",
    "North Fitzroy",
    "Melbourne CBD",
    "Melbourne St Kilda Road"
  )

for (s in abc) {
  for (a in 1:nrow(airbnbdata)) {
    if (s == as.character(airbnbdata$city[a])) {
      airbnbdata$cityCheck[a] <- "Y"
    }
  }
}

# All cities less than 5 Km

onlyFive <- c(
  "Abbotsford",
  "Albert Park",
  "Albert Park Melbourne",
  "Ascot Vale",
  "Burnley",
  "Carlton",
  "Carlton North",
  "Carlton South",
  "Carlton, Melbourne",
  "Clifton Hill",
  "Collingwood",
  "Collingwood, Melbourne",
  "Cremorne",
  "Docklands",
  "Fitzroy" ,
  "Fitzroy North" ,
  "Fitzroy, Melbourne" ,
  "Flemington" ,
  "footscray" ,
  "Footscray" ,
  "Glen Iris" ,
  "Kensington",
  "Kew",
  "Melbourne",
  "Melbourne CBD",
  "Melbourne St Kilda Road",
  "Middle Park",
  "Malvern",
  "North Fitzroy",
  "North Melbourne",
  "Parkville",
  "Port Melbourne",
  "Praharan",
  "Prahran",
  "Prahran / Toorak",
  "Prahran Melbourne",
  "Prahran, Melbourne",
  "Princes Hill",
  "Richmond",
  "RICHMOND",
  "Saint Kilda West",
  "South Melbourne",
  "South Yarra",
  "Southbank",
  "St Kilda West",
  "St Kilda West, Melbourne",
  "Toorak",
  "Travancore",
  "West Melbourne"
)

# All the cities between 6 Km and 10 Km
sixtoten <- c(
  "Alphington",
  "Armadale",
  "Balaclava",
  "Balwyn",
  "Balwyn North",
  "Bentleigh",
  "Bentleigh East",
  "Brighton",
  "Brighton East",
  "brunswick",
  "Brunswick",
  "Brunswick East",
  "Brunswick East, Melbourne",
  "Brunswick West",
  "Brunswick, Melbourne",
  "Camberwell",
  "Canterbury",
  "Carnegie",
  "Caulfield",
  "Caulfield East",
  "Caulfield north",
  "Caulfield North",
  "Caulfield South",
  "Coburg",
  "Coburg North",
  "East Melbourne",
  "Elsternwick",
  "Elwood",
  "Essendon",
  "Fairfield",
  "FAWK",
  "Gardenvale",
  "Glen Huntly",
  "Hawthorn",
  "Hawthorn East",
  "Hughesdale",
  "Kew East",
  "Kew, Melbourne",
  "Kingsville",
  "Kooyong",
  "Malvern East",
  "McKinnon",
  "Murrumbeena",
  "Newport",
  "Northcote",
  "Oakleigh East",
  "Ormond",
  "Pascoe Vale South",
  "Preston",
  "Ripponlea",
  "Saint Kilda",
  "Saint Kilda East",
  "Seddon",
  "South Kingsville",
  "Spotswood",
  "St Kilda",
  "St Kilda East",
  "St. Kilda",
  "Thornbury",
  "West Footscray",
  "Williamstown",
  "Windsor",
  "Windsor / Prahran",
  "Yarraville"
)

# Creating cityZone column

for (s in onlyFive) {
  for (a in 1:nrow(airbnbdata)) {
    if (s == as.character(airbnbdata$city[a])) {
      airbnbdata$cityZone[a] <- "Within 5Km"
    }
  }
}

for (s in sixtoten) {
  for (a in 1:nrow(airbnbdata)) {
    if (s == as.character(airbnbdata$city[a])) {
      airbnbdata$cityZone[a] <- "Btw 6to10Km"
    }
  }
}


airbnb.5km <- airbnbdata[airbnbdata$cityZone == "Within 5Km", ]
airbnb.10km <- airbnbdata[airbnbdata$cityZone == "Btw 6to10Km", ]


hostid5km <- as.data.frame.table(table(airbnb.5km$host_id))
airbnb.5km$hostCount <- 1
i <- 0
s <- 0
for (i in 1:nrow(airbnb.5km)) {
  for (s in 1:nrow(hostid5km)) {
    if (airbnb.5km$host_id[i] == hostid5km$Var1[s]) {
      airbnb.5km$hostCount[i] <- hostid5km$Freq[s]
    }
  }
}

hostid10km <- as.data.frame.table(table(airbnb.10km$host_id))

airbnb.10km$hostCount <- 1

i <- 0
s <- 0
for (i in 1:nrow(airbnb.10km)) {
  for (s in 1:nrow(hostid10km)) {
    if (airbnb.10km$host_id[i] == hostid10km$Var1[s]) {
      airbnb.10km$hostCount[i] <- hostid10km$Freq[s]
    }
  }
}

airbnb <- rbind(airbnb.5km, airbnb.10km)



# Removing minstay > 10
# airbnb <- airbnbdata
airbnb <- airbnb[airbnb$minstay < 11,]
table(airbnb$minstay)

# Creating hostCategory column
airbnb$hostCategory <- "NULL"
i <- 0
for (i in 1:nrow(airbnb)) {
  if (airbnb$hostCount[i] < 6) {
    airbnb$hostCategory[i]  <- "low"
  } else if (airbnb$hostCount[i] < 12) {
    airbnb$hostCategory[i]  <- "medium"
  } else{
    airbnb$hostCategory[i]  <- "high"
  }
  
}
airbnb$hostCategory <- as.factor(airbnb$hostCategory)

# Creating Price Category Column

airbnb$price_cat[airbnb$price > 10000] <- "high"
airbnb$price_cat[airbnb$price <= 10000] <- "low"

Apt <- airbnb[airbnb$room_type == "Entire home/apt",]
Apt$price_cat[Apt$price > mean(Apt$price)] <- "high"
Apt$price_cat[Apt$price <= mean(Apt$price)] <- "low"

PrivateRoom <- airbnb[airbnb$room_type == "Private room",]
PrivateRoom$price_cat[PrivateRoom$price > mean(PrivateRoom$price)] <-
  "high"
PrivateRoom$price_cat[PrivateRoom$price <= mean(PrivateRoom$price)] <-
  "low"

Sharedroom <-  airbnb[airbnb$room_type == "Shared room",]
Sharedroom$price_cat[Sharedroom$price > mean(Sharedroom$price)] <-
  "high"
Sharedroom$price_cat[Sharedroom$price <= mean(Sharedroom$price)] <-
  "low"

airbnb$price_cat <- as.factor(airbnb$price_cat)
airbnb <- rbind(Sharedroom, PrivateRoom, Apt)

# Manually modified city names

# Removing Price above 350
airbnb <- airbnb[airbnb$price < 350,]

backup <- airbnb
backup <- airbnb[airbnb$city != "",]

airbnb$X <- NULL


# Creating minstay category
airbnb$minstayCategory <- "test"
for (g in 1:nrow(airbnb)) {
  if (airbnb$minstay[g] < 2) {
    airbnb$minstayCategory[g] <- "one"
  } else if (airbnb$minstay[g] < 3) {
    airbnb$minstayCategory[g] <- "two"
  } else {
    airbnb$minstayCategory[g] <- "three or above"
  }
}
airbnb$minstayCategory <- as.factor(airbnb$minstayCategory)

# Creating overall satisfaction category

airbnb$overallSatisfactionCategory <- "NULL"
i <- 0
for (i in 1:nrow(airbnb)) {
  if (airbnb$overall_satisfaction[i] == 4 ||
      airbnb$overall_satisfaction[i] == 1 ||
      airbnb$overall_satisfaction[i] == 2 ||
      airbnb$overall_satisfaction[i] == 2.5 ||
      airbnb$overall_satisfaction[i] == 3 ||
      airbnb$overall_satisfaction[i] == 3.5) {
    airbnb$overallSatisfactionCategory[i]  <- "low"
  } else if (airbnb$overall_satisfaction[i] == 4.5) {
    airbnb$overallSatisfactionCategory[i]  <- "medium"
  } else{
    airbnb$overallSatisfactionCategory[i]  <- "high"
  }
}

airbnb$overallSatisfactionCategory <-
  as.factor(airbnb$overallSatisfactionCategory)


# Join Postal Code dataset

airbnb$postal_code[airbnb$city == "Abbotsford"] <- 3067
airbnb$postal_code[airbnb$city == "Albert Park"] <-	3206
airbnb$postal_code[airbnb$city == "Alphington"] <-	3078
airbnb$postal_code[airbnb$city == "Ascot Vale"] <-	3032
airbnb$postal_code[airbnb$city == "Balaclava"] <- 3183
airbnb$postal_code[airbnb$city == "Balwyn"] <-	3103
airbnb$postal_code[airbnb$city == "Balwyn North"] <- 3104
airbnb$postal_code[airbnb$city == "Brunswick"] <-	3056
airbnb$postal_code[airbnb$city == "Brunswick East"] <- 3057
airbnb$postal_code[airbnb$city == "Brunswick West"] <- 3055
airbnb$postal_code[airbnb$city == "Burnley"] <-	3121
airbnb$postal_code[airbnb$city == "Camberwell"] <- 3124
airbnb$postal_code[airbnb$city == "Carlton"] <- 3053
airbnb$postal_code[airbnb$city == "Carlton North"] <-	3054
airbnb$postal_code[airbnb$city == "Carlton South"] <-	3053
airbnb$postal_code[airbnb$city == "Caulfield East"] <- 3145
airbnb$postal_code[airbnb$city == "Clifton Hill"] <-	3068
airbnb$postal_code[airbnb$city == "Coburg"] <-	3058
airbnb$postal_code[airbnb$city == "Coburg North"] <-	3058
airbnb$postal_code[airbnb$city == "Collingwood"] <- 3066
airbnb$postal_code[airbnb$city == "Cremorne"] <-	3121
airbnb$postal_code[airbnb$city == "Docklands"] <-	3008
airbnb$postal_code[airbnb$city == "Fitzroy"] <- 3065
airbnb$postal_code[airbnb$city == "Fitzroy North"] <-	3068
airbnb$postal_code[airbnb$city == "Flemington"] <- 3031
airbnb$postal_code[airbnb$city == "Footscray"] <- 3011
airbnb$postal_code[airbnb$city == "Glen Iris"] <-	3146
airbnb$postal_code[airbnb$city == "Hawthorn"] <-	3122
airbnb$postal_code[airbnb$city == "Hawthorn East"] <-	3123
airbnb$postal_code[airbnb$city == "Kensington"] <- 3031
airbnb$postal_code[airbnb$city == "Kew"] <- 3101
airbnb$postal_code[airbnb$city == "Kingsville"] <-	3012
airbnb$postal_code[airbnb$city == "Kooyong"] <-	3144
airbnb$postal_code[airbnb$city == "Malvern"] <-	3144
airbnb$postal_code[airbnb$city == "Melbourne"] <-	3000
airbnb$postal_code[airbnb$city == "Melbourne CBD	"] <-	3000
airbnb$postal_code[airbnb$city == "Middle Park"] <-	3206
airbnb$postal_code[airbnb$city == "Newport"] <- 3015
airbnb$postal_code[airbnb$city == "North Fitzroy"] <-		3068
airbnb$postal_code[airbnb$city == "North Melbourne"] <-		3051
airbnb$postal_code[airbnb$city == "Northcote"] <-	3070
airbnb$postal_code[airbnb$city == "Parkville"] <-	3052
airbnb$postal_code[airbnb$city == "Pascoe Vale South"] <-	3044
airbnb$postal_code[airbnb$city == "Port Melbourne"] <- 3207
airbnb$postal_code[airbnb$city == "Prahran"] <-	3181
airbnb$postal_code[airbnb$city == "Preston"] <-	3072
airbnb$postal_code[airbnb$city == "Princes Hill"] <-		3054
airbnb$postal_code[airbnb$city == "Richmond"] <-	3121
airbnb$postal_code[airbnb$city == "Ripponlea"] <-	3185
airbnb$postal_code[airbnb$city == "Seddon"] <- 3011
airbnb$postal_code[airbnb$city == "South Kingsville"] <- 3015
airbnb$postal_code[airbnb$city == "South Melbourne"] <- 3205
airbnb$postal_code[airbnb$city == "South Yarra"] <- 3141
airbnb$postal_code[airbnb$city == "Southbank"] <-	3006
airbnb$postal_code[airbnb$city == "Spotswood"] <-	3015
airbnb$postal_code[airbnb$city == "St Kilda"] <-	3182
airbnb$postal_code[airbnb$city == "St Kilda East"] <- 3183
airbnb$postal_code[airbnb$city == "St Kilda West"] <- 3041
airbnb$postal_code[airbnb$city == "Thornbury"] <-	3071
airbnb$postal_code[airbnb$city == "Toorak"] <- 3142
airbnb$postal_code[airbnb$city == "Travancore"] <- 3032
airbnb$postal_code[airbnb$city == "West Footscray"] <- 3012
airbnb$postal_code[airbnb$city == "West Melbourne"] <- 3003
airbnb$postal_code[airbnb$city == "West Melbourne"] <- 3003
airbnb$postal_code[airbnb$city == "Williamstown"] <-	3016
airbnb$postal_code[airbnb$city == "Yarraville"] <- 3013



# postalcodeunique <- read.csv(file.choose(), header = T)
# #airbnbCitiesCrime <- left_join(airbnb,postalcodeunique)
#
# airbnbCitiesCrimeGrant <- left_join(airbnbCrime,postalcodeunique)




# simply read the file. it has postal code and crime data
# postalcodemelbunique is already loaded in RData file. So we are commenting out the below line


# postalcodemelbunique <- read.csv(file.choose(), header = T)

# STEP 1 : have your current data set as the first parameter in the join and run this
airbnbCitiesCrime <- left_join(airbnb, postalcodemelbunique)

# crime data is merged

# now read the grant file. it has postal code and grant data
# postalcodemelbuniquegrant is already loaded in RData file. So we are commenting out the below line

# postalcodemelbuniquegrant <- read.csv(file.choose(), header = T)

# now merge this with our airbnb and crime dataframe that u did in STEP 1

airbnb <- left_join(airbnbCitiesCrime, postalcodemelbuniquegrant)


# Coverting numerical values to factors
airbnb$accommodates <- as.factor(airbnb$accommodates)
airbnb$bedrooms <- as.factor(airbnb$bedrooms)
airbnb$bathrooms <- as.factor(airbnb$bathrooms)
airbnb$minstay <- as.factor(airbnb$minstay)
airbnb$postal_code <- as.factor(airbnb$postal_code)
airbnb$overall_satisfaction <-
  as.factor(airbnb$overall_satisfaction)

# Adding Total_crime variable
airbnb$total_crimes <- airbnb$violet_crimes + airbnb$property_crimes
airbnb <- na.omit(airbnb)
table(is.na(airbnb$total_crimes))
# Creating total crimes category
fivenum(airbnb$total_crimes)
quantile(airbnb$total_crimes)
mean(airbnb$total_crimes)
median(airbnb$total_crimes)
range(airbnb$total_crimes)


airbnb$totalCrimesCategory <- "NULL"
i <- 0
for (i in 1:nrow(airbnb)) {
  if (airbnb$total_crimes[i] < 4000) {
    airbnb$totalCrimesCategory[i]  <- "low"
  } else if (airbnb$total_crimes[i] < 9001) {
    airbnb$totalCrimesCategory[i]  <- "medium"
  } else{
    airbnb$totalCrimesCategory[i]  <- "high"
  }
}
airbnb$totalCrimesCategory <- as.factor(airbnb$totalCrimesCategory)

# ---------------------------------------------Univariate Analysis---------------------------------------------

dev.off()
par(mfrow = c(3, 3))
hist(log(airbnb$reviews))
hist(log(airbnb$price))
hist(log(airbnb$violet_crimes))
hist(log(airbnb$property_crimes))
hist(log(airbnb$total_crimes))
hist(log(airbnb$no.of.grants))
hist(log(airbnb$grant.value))
hist(log(airbnb$hostCount))

backuppp <- airbnb

airbnb <- airbnb[airbnb$total_crimes < 12000, ]

dev.off()
par(mfrow = c(4, 4))
# Bedrooms
tab <- table(airbnb$bedrooms)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Bedrooms",
  ylim = c(0, 1)
)
box()

# Bathrooms
tab <- table(airbnb$bathrooms)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Bathrooms",
  ylim = c(0, 1)
)
box()

# Room Type
tab <- table(airbnb$room_type)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Room Type",
  ylim = c(0, 1)
)
box()

# Accommodates
tab <- table(airbnb$accommodates)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Accommodates",
  ylim = c(0, 1)
)
box()

# minstay
tab <- table(airbnb$minstay)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Min Stay",
  ylim = c(0, 1)
)
box()

# cityZone
tab <- table(airbnb$cityZone)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "City Zone",
  ylim = c(0, 1)
)
box()

# hostCategory
tab <- table(airbnb$hostCategory)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "City Zone",
  ylim = c(0, 1)
)
box()

# price_cat
tab <- table(airbnb$price_cat)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Price Category",
  ylim = c(0, 1)
)
box()

# minStay category
tab <- table(airbnb$minstayCategory)
ptab <- prop.table(tab)
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Minimum Stay Category",
  ylim = c(0, 1)
)
box()

# Overall Satisfaction category
tab <- table(airbnb$overallSatisfactionCategory)
ptab <- prop.table(tab)
ptab
barplot(
  ptab,
  col = c("coral", "greenyellow"),
  main = "Overall Satisfaction Category",
  ylim = c(0, 1)
)
box()



# ---------------------------------------------Bi-ivariate Analysis---------------------------------------------

# Corplot for numerical variables
#air <- read.csv(file.choose(), header = T)
#levels(as.factor(air$total_crimes))
airbnb <- na.omit(airbnb)
dev.off()
airnum <- airbnb[, c(10, 14, 20:24)]
cormat <-
  cor(airnum) # Select only numeric variables. Otherwise, you'd get an error message.
round(cormat, 2) # Rounded to 2 decimals
corrplot(cormat, method = "circle", addCoef.col = "black")
corrplot(
  cormat,
  method = "circle",
  addCoef.col = "black",
  diag = F,
  type = "upper"
)

# Price with other factors
dev.off()
par(mfrow = c(3, 3))
plotmeans(
  airbnb$price ~ airbnb$room_type,
  xlab = "Room Type",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
)
plotmeans(
  airbnb$price ~ airbnb$overall_satisfaction,
  xlab = "overall_satisfaction",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
) #Need to ignore satisfaction where n is less
plotmeans(
  airbnb$price ~ airbnb$accommodates,
  xlab = "Accommodates",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
)
plotmeans(
  airbnb$price ~ airbnb$bedrooms,
  xlab = "Bedrooms",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
) #no relation
plotmeans(
  airbnb$price ~ airbnb$bathrooms,
  xlab = "Bathrooms",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
)
plotmeans(
  airbnb$price ~ airbnb$minstayCategory,
  xlab = "Minimum stay Category",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
)
plotmeans(
  airbnb$price ~ airbnb$totalCrimesCategory,
  xlab = "Total Crimes Category",
  ylab = "Price",
  lwd = 3,
  col = "red",
  p = 0.99
)

# Creating copy of airbnb
airbnb_backup <-
  airbnb #After initializing overallSatisfactionCategory to null


#--------------------------------------------------Linear Model----------------------------------------------------
options(scipen = 99)
dev.off()
five <- airbnb[airbnb$cityZone == "Within 5Km",]
more <- airbnb[airbnb$cityZone == "Btw 6to10Km",]

mod2five <- lm(log(price) ~ log(total_crimes), data = airbnb)
mod2five
plot(mod2five)

summary(mod2five)
confint(mod2five) #1.06%

mod3five <- lm(price ~ grant.value, data = five)
mod3five
plot(mod3five)


summary(mod3five)
confint(mod3five) #0.05%



mod4five <- lm((price) ~ (accommodates), data = five)
mod4five
plot(mod4five)

summary(mod4five)
confint(mod4five) #40.79%

mod9five <- lm((price) ~ (cityZone), data = airbnb)
mod9five
plot(mod9five)

summary(mod9five)
confint(mod9five) #2.8%

mod5five <- lm((price) ~ (accommodates) + (bedrooms), data = five)
mod5five
plot(mod5five)

summary(mod5five)
confint(mod5five) #46.78%

mod6five <-
  lm((price) ~ (accommodates) + (bedrooms) + (bathrooms), data = five)
mod6five
plot(mod6five)

summary(mod6five)
confint(mod6five) #47.59%

mod7five <-
  lm((price) ~ (accommodates) + (bedrooms) + (bathrooms) + (minstay),
     data = five)
mod7five
plot(mod7five)

summary(mod7five)
confint(mod7five) #49.53%

mod8five <-
  lm((price) ~ (accommodates) + (bedrooms) + (bathrooms) + (minstay) + (hostCategory),
     data = five)
mod8five
plot(mod8five)

summary(mod8five) #49.71
confint(mod8five)

####### Final Models that are decided for Analysis############

# Model 1: Price with Total crimes for both zones
mod2five <- lm(log(price) ~ log(total_crimes), data = airbnb)
mod2five
par(mfrow = c(2, 2))
plot(mod2five)
summary(mod1five)

# Model 2: Price with Accommodates and Minstay in Zone1 (Within 5 Kms)
mod1five <- lm((price) ~ (accommodates) + (minstay), data = five)
par(mfrow = c(2, 2))
plot(mod1five)
summary(mod1five) #43.65

# Model 3: Price with Accommodates and Minstay category in Zone1 (Within 5 Kms)
mod2five <-
  lm((price) ~ (accommodates) + (minstayCategory), data = five)
par(mfrow = c(2, 2))
plot(mod2five)
summary(mod2five) #43.63

# Model 4: Price with Accommodates, Minstay & GrantValue in Zone1 (Within 5 Kms)
mod3five <-
  lm((price) ~ (accommodates) + (minstay) + (grant.value), data = five)
par(mfrow = c(2, 2))
plot(mod3five)
summary(mod3five) #43.67

# Model 5: Price with Accommodates in Zone2 (Between 6-10 Kms)
mod1more <- lm((price) ~ (accommodates), data = more)
par(mfrow = c(2, 2))
plot(mod1more)
summary(mod1more) #40.3


#-------------------------------------
mod4five <- lm((price) ~ (totalCrimesCategory), data = more)
mod4five
plot(mod4five)

summary(mod4five) #8.4%
confint(mod4five)


######

mean(five$price)
mean(more$price)

mean(five$total_crimes)
mean(more$total_crimes)

five <- airbnb[airbnb$cityZone == "Within 5Km",]
more <- airbnb[airbnb$cityZone == "Btw 6to10Km",]


# Hypothesis Testing--------------------------------------------------------------------------

price_null <-
  t.test(
    more$price,
    alternative = "two.sided",
    mu = mean(five$price),
    conf.level = 0.99
  )
price_null

price_null <-
  t.test(
    more$total_crimes,
    alternative = "two.sided",
    mu = mean(five$total_crimes),
    conf.level = 0.99
  )
price_null


#ggplots for price vs crimes in the two zones

classtheme <-
  theme(
    plot.title = element_text(face = "bold.italic", size = "16", color = "steelblue"),
    axis.title = element_text(face = "bold.italic", size =
                                10, color = "gray"),
    axis.text = element_text(face = "bold", size = 8, color =
                               "darkblue"),
    panel.background = element_rect(fill = "white", color =
                                      "lightgreen"),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "brown", linetype =
                                        2),
    legend.position = "top"
  )

ggplot(data = five,
       aes(x = total_crimes, y = price, color = totalCrimesCategory)) +
  scale_color_manual(values = c("orange", "steelblue", "grey")) + # this is how we control color
  geom_point() +
  stat_smooth(method = "lm", se = F, lwd = 1.5) +
  labs(title = "Listing Price by total crimes in Zone 1", x = "Total Crimes", y =
         "Price") + # this is how we control titles
  classtheme

ggplot(data = more,
       aes(x = total_crimes, y = price, color = totalCrimesCategory)) +
  scale_color_manual(values = c("orange", "steelblue", "grey")) + # this is how we control color
  geom_point() +
  stat_smooth(method = "lm", se = F, lwd = 1.5) +
  labs(title = "Listing Price by total crimes in Zone 2", x = "Total Crimes", y =
         "Price") + # this is how we control titles
  classtheme

qplot(
  x = total_crimes,
  y = price,
  data = airbnb,
  ylab = "Price",
  xlab = "Total crimes",
  main = "Relationship between crimes,hostCount by cityZone",
  facets =  ~ cityZone,
  geom = c("point", "smooth")
) # not ideal when we have so few observations


qplot(
  x = total_crimes,
  y = hostCount,
  data = airbnb,
  ylab = "%hosts",
  xlab = "",
  main = "Relationship between crimes,hostCount by cityZone",
  facets =  ~ cityZone,
  geom = c("point", "smooth")
) # not ideal when we have so few observations
