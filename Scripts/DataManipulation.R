##Use this file to update datafiles to avoid updating them in the markdown file
library(readr)
library(plyr)
library(gamm4)

##SimulatorProc Changes
SimulatorProc <- read_csv("~/PiersHomework/Harvard/Work/Doctoral/UTC/UTAS/Data/Analysis/Github/utas/Data/SimulatorProc.csv")
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
write.csv(SimulatorProc2, gsub(" ","",paste(getwd(),"/Data/SimulatorProc2.csv")))

##Create column in ManeuverLevel that has summarized metric for each maneuver
  #These lines combine all the Deviation metrics into a single column
ManeuverLevel$PerfMetrics <- ifelse(is.na(ManeuverLevel$BankAngleDev)==T, ManeuverLevel$AltitudeDev, ManeuverLevel$BankAngleDev)
ManeuverLevel$PerfMetrics <- ifelse(is.na(ManeuverLevel$PerfMetrics)==T, ManeuverLevel$PitchAttitudeDev, ManeuverLevel$PerfMetrics)
ManeuverLevel$PerfMetrics <- as.double(ManeuverLevel$PerfMetrics)
#ManeuverLevel$Condition <- factor(ManeuverLevel$Condition,levels(as.factor(ManeuverLevel$Condition))[c(2,3,1)]) #only run once

  #Model of CO2 and Pilot Performance that uses all Deviation Metrics together
mod1 <- gamm(PerfMetrics ~ Condition + Instructor + as.factor(Profile) + Difficulty, na.action=na.omit, random=list(PilotID=~1), family=gaussian(), data=ManeuverLevel)
summary(mod1$gam)

  #Memo, add in your metrics here:

##Create a loop that runs the model comparing CO2 and each performance metric
##with fixed effects = profile + instructor and random effects = pilot id
##and places coefficients in a table

  #Create Table with Maneuvers to Model
PerfManeuvers <- ManeuverLevel[c(1:2,7:9,11,13:14,17,19,21:24),c("Maneuver","SubManeuver")]
PerfManeuvers$EstimateMed <- NA
PerfManeuvers$PvalueMed <- NA
PerfManeuvers$EstimateHigh <- NA
PerfManeuvers$PvalueHigh <- NA
PerfManeuvers$n <- NA

for(i in 1:nrow(PerfManeuvers)){
  if(length(subset(ManeuverLevel, Maneuver==PerfManeuvers$Maneuver[i] & SubManeuver==PerfManeuvers$SubManeuver[i] & is.na(PerfMetrics)==F)$PerfMetrics) > 50){
    mod <- gamm4(PerfMetrics ~ Condition + Profile + Instructor, na.action=na.omit, random=~(1|PilotID), family=gaussian(), 
                 data=subset(ManeuverLevel, Maneuver==PerfManeuvers$Maneuver[i] & SubManeuver==PerfManeuvers$SubManeuver[i]))
    PerfManeuvers$EstimateMed[i] <- mod$gam$coef[2]
    PerfManeuvers$PvalueMed[i] <- summary(mod$gam)$p.pv[2]
    PerfManeuvers$EstimateHigh[i] <- mod$gam$coef[3]
    PerfManeuvers$PvalueHigh[i] <- summary(mod$gam)$p.pv[3]
    PerfManeuvers$n[i] <- summary(mod$gam)[13]
  }
}
