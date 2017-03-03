# William Dean
# wdean@bu.edu
# Midterm

library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)

### READ IN ALL THE FILES ###
a <- read.dbf("accid.DBF")
A <- read.dbf("acc.dbf")
O <- read.dbf("occ.DBF")
H <- read.dbf("hzs.dbf")
o <- read.dbf("osha.DBF")
v <- read.dbf("viol.DBF")
r <- read.dbf("relact.DBF")
p <- read.dbf("prog.DBF")
opt <- read.dbf("optinfo.DBF")
his <- read.dbf("history.DBF")
haz <- read.dbf("hazsub.DBF")
d <- read.dbf("debt.DBF")
ad <- read.dbf("admpay.DBF")

### CLEAN a, Accident DATAFRAME ###
a <- select(a, -c(NAME,RELINSP, SITESTATE))
a$AGE[a$AGE == 0] <- NA
a$OCC_CODE[a$OCC_CODE == "000"] <- NA
a$HAZSUB[a$HAZSUB == "0000"] <- NA
a$TASK[a$TASK == "0"] <- NA
a$DEGREE[a$DEGREE == "0"] <- NA
levels(a$DEGREE) <- c(NA, "Fatal", "Hospitalized", "Non-Hospitalized")
levels(a$TASK) <- c(NA, "Regular", "Irregular")

### COMBINE TO MAKE ONE TABLE FOR ACCIDENTS ###
Accidents <- NULL
names(O)[1] <- "OCC_CODE" # Prep for left join
Accidents <- left_join(a, O, by = "OCC_CODE") 
Accidents <- select(Accidents, -c(OCC_CODE))

names(H)[1] <- "HAZSUB" # Prep for left join
Accidents <- left_join(Accidents, H, by = "HAZSUB")
Accidents <- select(Accidents, -c(HAZSUB))
names(Accidents)[ncol(Accidents)] <- "HAZSUB"

Hum <- filter(A, CATEGORY == "HUMAN-FAC")
names(Hum)[2] <- "HUMAN"
Hum <- select(Hum, -c(CATEGORY))
Accidents <- left_join(Accidents, Hum, by = "HUMAN")
Accidents <- select(Accidents, -c(HUMAN))
names(Accidents)[ncol(Accidents)] <- "HUMFACT"

Bod <- filter(A, CATEGORY == "PART-BODY")
names(Bod)[2] <- "BODYPART"
Bod <- select(Bod, -c(CATEGORY))
Accidents <- left_join(Accidents, Bod, by = "BODYPART")
Accidents <- select(Accidents, -c(BODYPART))
names(Accidents)[ncol(Accidents)] <- "BODYPART"

Nat <- filter(A, CATEGORY == "NATUR-INJ")
names(Nat)[2] <- "NATURE"
Nat <- select(Nat, -c(CATEGORY))
Accidents <- left_join(Accidents, Nat, by = "NATURE")
Accidents <- select(Accidents, -c(NATURE))
names(Accidents)[ncol(Accidents)] <- "NATURE"

Source <- filter(A, CATEGORY == "SOURC-INJ")
names(Source)[2] <- "SOURCE"
Source <- select(Source, -c(CATEGORY))
Accidents <- left_join(Accidents, Source, by = "SOURCE")
Accidents <- select(Accidents, -c(SOURCE))
names(Accidents)[ncol(Accidents)] <- "SOURCE"

Envir <- filter(A, CATEGORY == "ENVIR-FAC")
names(Envir)[2] <- "ENVIRON"
Envir <- select(Envir, -c(CATEGORY))
Accidents <- left_join(Accidents, Envir, by = "ENVIRON")
Accidents <- select(Accidents, -c(ENVIRON))
names(Accidents)[ncol(Accidents)] <- "ENVIRON"

Event <- filter(A, CATEGORY == "EVENT-TYP")
names(Event)[2] <- "EVENT"
Event <- select(Event, -c(CATEGORY))
Accidents <- left_join(Accidents, Event, by = "EVENT")
Accidents <- select(Accidents, -c(EVENT))
names(Accidents)[ncol(Accidents)] <- "EVENT"

### VISUALISE Accidents ###
# Age
Age <- filter(Accidents, !is.na(AGE))
g <- ggplot(Age, aes(AGE))
g + geom_histogram(aes(color = DEGREE), binwidth = 7) + facet_wrap(~DEGREE)
g + geom_histogram(aes(color = TASK), binwidth = 7) + facet_wrap(~TASK)

# Occupation
Occ <- Accidents[ Accidents$OCCUPATION %in%  names(table(Accidents$OCCUPATION))[table(Accidents$OCCUPATION) >10] , ]
Occ <- filter(Occ, Occ$OCCUPATION != "OCCUPATION NOT REPORTED")
f <- ggplot(Occ, aes(OCCUPATION, color = OCCUPATION))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# Body Part
Body <- Accidents[Accidents$BODYPART %in% names(table(Accidents$BODYPART))[table(Accidents$BODYPART) > 25], ]
f <- ggplot(Body, aes(BODYPART))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~TASK) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# Nature
Nat <- Accidents[Accidents$NATURE %in% names(table(Accidents$NATURE))[table(Accidents$NATURE) > 25], ]
f <- ggplot(Nat, aes(NATURE))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))

# HAZ SUB
Hz <- Accidents[Accidents$HAZSUB %in% names(table(Accidents$HAZSUB))[table(Accidents$HAZSUB) > 5], ]
f <- ggplot(Hz, aes(HAZSUB))
f + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
f + geom_bar() + facet_wrap(~DEGREE) + theme(axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
