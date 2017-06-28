##Use this file to update datafiles to avoid updating them in the markdown file
library(readr)
library(plyr)

##SimulatorProc Changes
#SimulatorProc <- read_csv("~/Piers's Homework/Harvard/Work/Doctoral/UTC/UTAS/Data/Analysis/Github/utas/Data/SimulatorProc.csv")
SimulatorProc2 <- SimulatorProc
SimulatorProc2$BankAngle <- as.numeric(SimulatorProc2$BankAngle)
SimulatorProc2$Altitude <- as.numeric(SimulatorProc2$Altitude)  

  #Function to compute absolute deviations from constants
DeviationsProc <- function(x){
  HoldingMean <- mean(subset(x,SubManeuver=="Holding")$BankAngle, na.rm=T)
  #Altitude
  x$AltitudeDev <- ifelse(x$SubManeuver=="Waypoint" & x$Maneuver!="Glide Slope Inoperative",abs(x$Altitude-1700),NA)
  x$AltitudeDev <- ifelse(x$SubManeuver=="Waypoint" & x$Maneuver=="Glide Slope Inoperative",abs(x$Altitude-1500),x$AltitudeDev)
  x$AltitudeDev <- ifelse(x$SubManeuver=="Circle to Land",abs(x$Altitude-700),x$AltitudeDev)
  x$AltitudeDev <- ifelse(x$SubManeuver=="Holding",abs(x$Altitude-1500),x$AltitudeDev)
  #PitchAttitude
  x$PitchAttitudeDev <- ifelse(x$SubManeuver=="Area Departure" & x$Maneuver!="Engine Fire", abs(x$PitchAttitude-15), NA)
  x$PitchAttitudeDev <- ifelse(x$SubManeuver=="Area Departure" & x$Maneuver=="Engine Fire", abs(x$PitchAttitude-12.5), x$PitchAttitudeDev)
  #BankAngle
  x$BankAngleDev <- ifelse(x$SubManeuver=="Takeoff" | x$SubManeuver=="Approach" | x$SubManeuver=="MissedApproach" | x$SubManeuver=="Landing", abs(x$BankAngle), NA)
  x$BankAngleDev <- ifelse(x$SubManeuver=="Holding", abs(x$BankAngle-HoldingMean), x$BankAngleDev)  
  x$BankAngleDev <- ifelse(x$SubManeuver=="Steep Turns", abs(x$BankAngle-45), x$BankAngleDev)  
  return(x)
}

SimulatorProc2 <- ddply(SimulatorProc2, .(ManeuverNum), .fun=DeviationsProc)

  #Write SimulatorProc2 to csv
write.csv(SimulatorProc2, gsub(" ","",paste(getwd(),"/utas/Data/SimulatorProc2.csv")))

Landing <- subset(SimulatorProc2,SubManeuver=="Landing")
ManeuverTime = function(x){
  x$systime3 = x$SysTime-x$SysTime[1]
  return(x)
}
Landing <- ddply(Landing,.(ManeuverNum), .fun=ManeuverTime)