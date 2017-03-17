# William Dean
# wdean@bu.edu
# Midterm

library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

### READ IN ALL THE FILES ###
a <- read.dbf("accid.DBF")
A <- read.dbf("lookups/acc.dbf")
O <- read.dbf("lookups/occ.DBF")
H <- read.dbf("lookups/hzs.dbf")
o <- read.dbf("osha.DBF")
S <- read.dbf("lookups/scc.dbf")
N <- read.dbf("lookups/naics.dbf")
v <- read.dbf("viol.DBF")


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

### COMBINE TO MAKE ONE TABLE FOR Accidents ###
Accidents <- NULL






### VISUALISE a ###
# Age
Age <- filter(a, !is.na(AGE))
g <- ggplot(Age, aes(AGE))
g + geom_histogram(aes(color = DEGREE), binwidth = 7) + facet_wrap(~DEGREE)
g + geom_histogram(aes(color = TASK), binwidth = 7) + facet_wrap(~TASK)

# Occupation
Occ <- a[ a$OCCUPATION %in%  names(table(a$OCCUPATION))[table(a$OCCUPATION) >10] , ]
Occ <- filter(Occ, Occ$OCCUPATION != "OCCUPATION NOT REPORTED")
f <- ggplot(Occ, aes(OCCUPATION, color = OCCUPATION))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# Body Part
Body <- a[a$BODYPART %in% names(table(a$BODYPART))[table(a$BODYPART) > 25], ]
f <- ggplot(Body, aes(BODYPART))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~TASK) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# Nature
Nat <- a[a$NATURE %in% names(table(a$NATURE))[table(a$NATURE) > 25], ]
f <- ggplot(Nat, aes(NATURE))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# HAZ SUB
Hz <- a[a$HAZSUB %in% names(table(a$HAZSUB))[table(a$HAZSUB) > 5], ]
f <- ggplot(Hz, aes(HAZSUB))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
