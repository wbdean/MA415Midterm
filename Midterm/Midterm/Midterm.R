# William Dean
# wdean@bu.edu
# Midterm

library(foreign)
library(dplyr)

a <- read.dbf("accid.DBF")
o <- read.dbf("osha.DBF")
v <- read.dbf("viol.DBF")
r <- read.dbf("relact.DBF")
p <- read.dbf("prog.DBF")
opt <- read.dbf("optinfo.DBF")
