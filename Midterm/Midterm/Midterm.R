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
his <- read.dbf("history.DBF")
haz <- read.dbf("hazsub.DBF")
d <- read.dbf("debt.DBF")
ad <- read.dbf("admpay.DBF")
