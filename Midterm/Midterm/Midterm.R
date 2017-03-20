# William Dean
# wdean@bu.edu
# Midterm

library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(lubridate)
library(zipcode)

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
a <- select(a, -c(NAME,RELINSP, SITESTATE))
a$AGE[a$AGE == 0] <- NA
a$OCC_CODE[a$OCC_CODE == "000"] <- NA
a$HAZSUB[a$HAZSUB == "0000"] <- NA
a$TASK[a$TASK == "0"] <- NA
a$DEGREE[a$DEGREE == "0"] <- NA
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
o <- select(o, c(ACTIVITYNO,ESTABNAME,SITEADD, OWNERTYPE,OPENDATE, CLOSEDATE,NAICS,SIC,SITEZIP))
levels(o$OWNERTYPE) <- c("Private", "Local Gov't", "State Gov't", "Fed Gov't")
o$OPENDATE[o$OPENDATE == 0] <- NA
o$CLOSEDATE[o$CLOSEDATE == 0] <- NA
o$OPENDATE <- ymd(o$OPENDATE)
o$CLOSEDATE <- ymd(o$CLOSEDATE)
o$NAICS[o$NAICS == "000000"] <- NA
o$SITEZIP[o$SITEZIP == "00000"] <- NA
o <- left_join(o, N, by="NAICS")
o <- select(o, -c(NAICS))
o <- left_join(o, Si, by="SIC")
o <- select(o, -c(SIC))
# zipcode
zipcode <- filter(zipcode, state =="MA")
zipcode <- select(zipcode, -c(state))
names(zipcode)[1] <- "SITEZIP"
o <- left_join(o, zipcode, by="SITEZIP")
o <- select(o, -c(SITEZIP,SITECITY,SITECNTY))
o <- distinct(o)

### COMBINE TO MAKE ONE TABLE FOR Accidents ###
Accidents <- full_join(a, o, by = "ACTIVITYNO")
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
g + geom_histogram(aes(fill = DEGREE), binwidth = 7) + facet_wrap(~DEGREE)
g + geom_histogram(aes(fill = TASK), binwidth = 7) + facet_wrap(~TASK)
m + geom_point(data=Age, aes(longitude, latitude,color=AGE),size=3,na.rm=T) +
  scale_color_gradient(low="dark blue", high="white") 

# Occupation
Occ <- Accidents[ Accidents$OCCUPATION %in%  names(tail(sort(table(Accidents$OCCUPATION)),10)), ]
Occ <- filter(Occ, Occ$OCCUPATION != "OCCUPATION NOT REPORTED")
f <- ggplot(Occ, aes(OCCUPATION, fill = OCCUPATION))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
ggplot(data=Occ, aes(OPENDATE, color=OCCUPATION)) + geom_density(alpha=.25, size = 1.5)
m + geom_point(data=Occ, na.rm = T, aes(longitude, latitude, color=OCCUPATION),size = 3)

# Body Part
Body <- Accidents[Accidents$BODYPART %in% names(tail(sort(table(Accidents$BODYPART)),10)), ]
f <- ggplot(Body, aes(BODYPART))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~TASK) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# Nature
Nat <- Accidents[Accidents$NATURE %in% names(tail(sort(table(Accidents$NATURE)),10)), ]
f <- ggplot(Nat, aes(NATURE))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# HAZ SUB
Hz <- Accidents[Accidents$HAZSUB %in% names(tail(sort(table(Accidents$HAZSUB)),10)), ]
f <- ggplot(Hz, aes(HAZSUB))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# City
City <- Accidents[Accidents$city %in% names(tail(sort(table(Accidents$city)),10)),]
ggplot(data=City, aes(OPENDATE, color=city)) + geom_density()
m + geom_point(data=City, aes(longitude, latitude, color = city), size = 4)

# Company
Comp <- Accidents[Accidents$ESTABNAME %in% names(tail(sort(table(Accidents$ESTABNAME)),10)),]
ggplot(data=Comp, aes(ESTABNAME)) + geom_bar()+ theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
m + geom_point(data=Comp,aes(longitude,latitude,color=ESTABNAME),size=3,na.rm = T)
ggplot(data=Comp,aes(OPENDATE,color=ESTABNAME)) + geom_density()

# Site
Add <- Accidents[Accidents$SITEADD %in% names(tail(sort(table(Accidents$SITEADD)),10)),]
ggplot(Add,aes(OPENDATE,color=SITEADD)) + geom_density()
