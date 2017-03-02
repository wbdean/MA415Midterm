# William Dean
# wdean@bu.edu
# Midterm

library(foreign)
library(dplyr)
library(tidyr)

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
a <- select(a, -c(NAME, RELINSP, SITESTATE))
a$AGE[a$AGE == 0] <- NA
a$OCC_CODE[a$OCC_CODE == "000"] <- NA
a$HAZSUB[a$HAZSUB == "0000"] <- NA
a$TASK[a$TASK == "0"] <- NA
a$DEGREE[a$DEGREE == "0"] <- NA

### COMBINE TO MAKE ONE TABLE FOR ACCIDENTS ###
Accidents <- NULL
names(O)[1] <- "OCC_CODE" # Prep for left join
Accidents <- left_join(a, O, by = "OCC_CODE") 
Accidents <- select(Accidents, -c(OCC_CODE))

names(H)[1] <- "HAZSUB" # Prep for left join
Accidents <- left_join(Accidents, H, by = "HAZSUB")
Accidents <- select(Accidents, -c(HAZSUB))

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

Source <- filter(A, CATEGORY == "SOURCE-INJ")
names(Source)[2] <- "SOURCE"
Source <- select(Source, -c(CATEGORY))
Accidents <- left_join(Accidents, Source, by = "SOURCE")
Accidents <- select(Accidents, -c(SOURCE))
names(Accidents)[ncol(Accidents)] <- "SOURCE"
