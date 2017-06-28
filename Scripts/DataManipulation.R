##Use this file to update datafiles to avoid updating them in the markdown file
library(readr)
library(plyr)

#SimulatorProc Changes
SimulatorProc <- read_csv("~/Piers's Homework/Harvard/Work/Doctoral/UTC/UTAS/Data/Analysis/Github/utas/Data/SimulatorProc.csv")
SimulatorProc2 <- SimulatorProc

  #Write SimulatorProc2 to csv
write.csv(SimulatorProc2, "Data/SimulatorProc2.csv")
