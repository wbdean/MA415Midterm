# William Dean
# wdean@bu.edu
# Midterm

require(foreign)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggmap)
require(lubridate)
require(zipcode)

### READ IN ALL THE FILES ###
a <- read.dbf("accid.DBF")
A <- read.dbf("lookups/acc.dbf")
O <- read.dbf("lookups/occ.DBF")
H <- read.dbf("lookups/hzs.dbf")

o <- read.dbf("osha.DBF")
N <- read.dbf("lookups/naics.dbf")
Si <- read.dbf("lookups/sic.dbf")

data("zipcode")


### CLEAN a, Accident DATAFRAME ###
a <- select(a, -c(NAME,RELINSP, SITESTATE)) # SELECT ATTRIBUTES

a$AGE[a$AGE == 0] <- NA                     #Clear all NA
a$OCC_CODE[a$OCC_CODE == "000"] <- NA
a$HAZSUB[a$HAZSUB == "0000"] <- NA
a$TASK[a$TASK == "0"] <- NA
a$DEGREE[a$DEGREE == "0"] <- NA

                                            # Relabel Values
levels(a$DEGREE) <- c(NA, "Fatal", "Hospitalized", "Non-Hospitalized")
levels(a$TASK) <- c(NA, "Regular", "Irregular")

names(O)[1] <- "OCC_CODE" # Prep for left join
a <- left_join(a, O, by = "OCC_CODE") 
a <- select(a, -c(OCC_CODE))
names(H)[1] <- "HAZSUB" # Prep for left join
a <- left_join(a, H, by = "HAZSUB")
a <- select(a, -c(HAZSUB))
names(a)[ncol(a)] <- "HAZSUB"

Hum <- filter(A, CATEGORY == "HUMAN-FAC")
names(Hum)[2] <- "HUMAN"
Hum <- select(Hum, -c(CATEGORY))
a <- left_join(a, Hum, by = "HUMAN")
a <- select(a, -c(HUMAN))
names(a)[ncol(a)] <- "HUMFACT"

Bod <- filter(A, CATEGORY == "PART-BODY")
names(Bod)[2] <- "BODYPART"
Bod <- select(Bod, -c(CATEGORY))
a <- left_join(a, Bod, by = "BODYPART")
a <- select(a, -c(BODYPART))
names(a)[ncol(a)] <- "BODYPART"

Nat <- filter(A, CATEGORY == "NATUR-INJ")
names(Nat)[2] <- "NATURE"
Nat <- select(Nat, -c(CATEGORY))
a <- left_join(a, Nat, by = "NATURE")
a <- select(a, -c(NATURE))
names(a)[ncol(a)] <- "NATURE"

Source <- filter(A, CATEGORY == "SOURC-INJ")
names(Source)[2] <- "SOURCE"
Source <- select(Source, -c(CATEGORY))
a <- left_join(a, Source, by = "SOURCE")
a <- select(a, -c(SOURCE))
names(a)[ncol(a)] <- "SOURCE"

Envir <- filter(A, CATEGORY == "ENVIR-FAC")
names(Envir)[2] <- "ENVIRON"
Envir <- select(Envir, -c(CATEGORY))
a <- left_join(a, Envir, by = "ENVIRON")
a <- select(a, -c(ENVIRON))
names(a)[ncol(a)] <- "ENVIRON"

Event <- filter(A, CATEGORY == "EVENT-TYP")
names(Event)[2] <- "EVENT"
Event <- select(Event, -c(CATEGORY))
a <- left_join(a, Event, by = "EVENT")
a <- select(a, -c(EVENT))
names(a)[ncol(a)] <- "EVENT"
a <- distinct(a)

### CLEAN o, Osha ###
                                            # SELECT ATTRIBUTE
o <- select(o, c(ACTIVITYNO,ESTABNAME,SITEADD, OWNERTYPE,OPENDATE, CLOSEDATE,NAICS,SIC,SITEZIP))

o$OPENDATE[o$OPENDATE == 0] <- NA           # ORGANIZE AND GET RID OF NA
o$CLOSEDATE[o$CLOSEDATE == 0] <- NA
o$OPENDATE <- ymd(o$OPENDATE)
o$CLOSEDATE <- ymd(o$CLOSEDATE)
o$NAICS[o$NAICS == "000000"] <- NA
o$SITEZIP[o$SITEZIP == "00000"] <- NA

                                            # RELABEL AND DO JOIN
levels(o$OWNERTYPE) <- c("Private", "Local Gov't", "State Gov't", "Fed Gov't")
o <- left_join(o, N, by="NAICS")
o <- select(o, -c(NAICS))
o <- left_join(o, Si, by="SIC")
o <- select(o, -c(SIC))
# zipcode
zipcode <- filter(zipcode, state =="MA")
zipcode <- select(zipcode, -c(state))
names(zipcode)[1] <- "SITEZIP"
o <- left_join(o, zipcode, by="SITEZIP")
o <- select(o, -c(SITEZIP))
o <- distinct(o)
o <- mutate(o, Decade= year(OPENDATE) %% 100) # Group by Decade
o <- mutate(o, Decade=Decade - (Decade %% 10))
o$Decade <- as.factor(o$Decade)
levels(o$Decade) <- c("00s","70s", "80s", "90s")
o$Decade <- factor(o$Decade, levels = c("70s", "80s", "90s", "00s"))

### COMBINE TO MAKE ONE TABLE FOR Accidents ###
Accidents <- left_join(a, o, by = "ACTIVITYNO")
# Any Last minute fixes
Accidents$ESTABNAME[Accidents$ESTABNAME == "BOSTON EDISON CO"] <- "BOSTON EDISON COMPANY"
Accidents$OCCUPATION[Accidents$OCCUPATION == "CONSTRUCTION TRADES, N.E.C."] <- "CONSTRUCTION LABORERS"
Accidents <- distinct(Accidents)

save(Accidents, file="Accidents.Rda" )
rm(list = ls())
load("Accidents.Rda")

### GET Map of MA ###
map <- get_map(location='massachusetts', zoom=8)
m <- ggmap(map)


### VISUALISE Accidents  ###

# Age
Age <- filter(Accidents, !is.na(AGE))
g <- ggplot(Age, aes(AGE))
g + geom_histogram(aes(fill = DEGREE), binwidth = 7) + facet_wrap(~DEGREE) + 
  ggtitle("Age Distribution over all Degrees of Accidents")
g + geom_histogram(aes(fill = TASK), binwidth = 7) + facet_wrap(~TASK) +
  ggtitle("Age Distribution for Accidents with Different Task Levels")
m + geom_point(data=Age, aes(longitude, latitude,color=AGE),size=3,na.rm=T) +
  scale_color_gradient(low="dark blue", high="white") +
  ggtitle("Ages of People in Accidents throughout MA")

# Occupation
Occ <- filter(Accidents, Accidents$OCCUPATION != "OCCUPATION NOT REPORTED")
Occ <- Occ[ Occ$OCCUPATION %in%  names(tail(sort(table(Occ$OCCUPATION)),10)), ]
f <- ggplot(Occ, aes(OCCUPATION, fill = OCCUPATION))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5)) +
  ggtitle("Top 10 Most Accident-Prone Occupations in MA")
f + geom_bar() +  
  facet_wrap(~DEGREE) + coord_flip() +
  ggtitle("Results from Top 10 Most Accident-Prone Occupations in MA")
ggplot(data=Occ, aes(OPENDATE, color=OCCUPATION)) + geom_density(alpha=.25, size = 1.5) +
  ggtitle("Occurences of Top 10 Occupations -- 1980s to 2005")
m + geom_point(data=Occ, na.rm = T, aes(longitude, latitude, color=OCCUPATION),size = 3) + 
  ggtitle("Locations of Top 10 Most Frequent Occupations with Accidents")

# Body Part
Body <- Accidents[Accidents$BODYPART %in% names(tail(sort(table(Accidents$BODYPART)),10)), ]
f <- ggplot(Body, aes(BODYPART, fill=BODYPART))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5)) + 
  ggtitle("Frequency of Top 10 Most Injuried Body Parts")
f + geom_bar() + facet_wrap(~DEGREE) + coord_flip() + 
  ggtitle("Frequency of Top 10 Most Injuried Body Parts by Degree of Injury")

# Nature
Nat <- filter(Accidents, Accidents$NATURE != "OTHER")
Nat <- Nat[Nat$NATURE %in% names(tail(sort(table(Nat$NATURE)),10)), ]
f <- ggplot(Nat, aes(NATURE,fill=NATURE))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5)) + 
  ggtitle("Top 10 Most Frequent Cause of Accidents")

# HAZ SUB
Hz <- Accidents[Accidents$HAZSUB %in% names(tail(sort(table(Accidents$HAZSUB)),10)), ]
f <- ggplot(Hz, aes(HAZSUB,fill=HAZSUB))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5)) + 
  ggtitle("Top 10 Most Frequent Hazardous Substances")

# City
City <- Accidents[Accidents$city %in% names(tail(sort(table(Accidents$city)),10)),]
ggplot(data=City, aes(OPENDATE, color=city)) + geom_density(size=3) + 
  ggtitle("Time Occurance of Cities with Top 10 Most Accidents")
m + geom_point(data=City, aes(longitude, latitude, color = city), size = 4) +
  ggtitle("Location of Cities with Top 10 Most Accidents")
City <- as.data.frame(table(Accidents$city))
ggplot(City,aes(Freq)) + geom_histogram(bins=45) + ggtitle("Distribution of Number of Accidents per City")

# Company
Comp <- Accidents[Accidents$ESTABNAME %in% names(tail(sort(table(Accidents$ESTABNAME)),10)),]
ggplot(data=Comp, aes(ESTABNAME,fill=ESTABNAME)) + geom_bar()+ theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5)) +
  ggtitle("Top 10 Companies with the Most Accidents")

m + geom_point(data=Comp,aes(longitude,latitude,color=ESTABNAME),size=4,na.rm = T) +
  ggtitle("Location(s) of Companies with Most Accidents")

ggplot(data=Comp,aes(OPENDATE,color=ESTABNAME)) + geom_density() + facet_wrap(~ESTABNAME,scales="free_y") +
  ggtitle("Time Occurances of Accidents by Top 10 Most Accident-Prone Companies")

Comp <- filter(Accidents, Decade=="90s") %>% select(ESTABNAME)
Comp <- as.data.frame(table(Comp))
ggplot(Comp, aes(Freq)) + geom_histogram(bins=25) + ggtitle("Distribution of Number of Accidents per Company in the 1990s")

Comp <- filter(Comp, Freq > 2)
ggplot(Comp,aes(Freq)) + geom_histogram(bins=10) + ggtitle("Distribution of Number of Accident over 2 per Company in the 1990s")

# Site
Add <- Accidents[Accidents$SITEADD %in% names(tail(sort(table(Accidents$SITEADD)),10)),]
ggplot(Add,aes(OPENDATE,color=SITEADD)) + geom_density() + facet_wrap(~SITEADD,scales="free_y")+
  ggtitle("Distribution of 10 Most Frequent Addresses -- 1970s to 2000s")

# Industry
Indust <- Accidents[Accidents$INDUSTRY %in% names(tail(sort(table(Accidents$INDUSTRY)),10)),]
ggplot(Indust,aes(INDUSTRY,fill=INDUSTRY)) + geom_bar()+ coord_flip() +
  facet_wrap(~Decade) + ggtitle("Top 10 Frequent Industries with Accidents over the Decades")

# Decade
m + geom_point(data=Accidents,aes(longitude,latitude,color=Decade),na.rm=T, size =3) 


