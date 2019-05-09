# a-rule for all

# Loading packages
library('ggplot2')
library('arules')
library('arulesViz')

# Loading data
setwd('/Users/a111/Documents/data/')
df <- read.csv("Satisfaction Survey.csv")

# Data cleaning

# Filling in some missing values
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes) & (df$Flight.cancelled == 'Yes'))] <- 0

# Numericalize satisfaction
df$Satisfaction <- as.numeric(as.character(df$Satisfaction))

# Dumping all the rows with missing values
ndf <- na.omit(df)

# Bucketing 

# Satisfaction bucketing
createBucketSat <- function(vec){ 
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= 4] <- "Happy"
  vBuckets[vec < 3] <- "Unhappy"
  return(vBuckets)
}

# Bucketing numeric numbers
createBuckets <- function(vec){
  q <- quantile(vec, c(0.4, 0.6), na.rm = "TRUE")
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

ndf$Satisfaction <- createBucketSat(ndf$Satisfaction)
ndf$Age <- createBuckets(ndf$Age)
ndf$Price.Sensitivity <- as.factor(ndf$Price.Sensitivity)
ndf$No.of.Flights.p.a. <- createBuckets(ndf$No.of.Flights.p.a.)
ndf$X..of.Flight.with.other.Airlines <- createBuckets(ndf$X..of.Flight.with.other.Airlines)
ndf$No..of.other.Loyalty.Cards <- createBuckets(ndf$No..of.other.Loyalty.Cards)
ndf$Shopping.Amount.at.Airport <- createBuckets(ndf$Shopping.Amount.at.Airport)
ndf$Eating.and.Drinking.at.Airport <- createBuckets(ndf$Eating.and.Drinking.at.Airport)
ndf$Departure.Delay.in.Minutes <- createBuckets(ndf$Departure.Delay.in.Minutes)
ndf$Arrival.Delay.in.Minutes <- createBuckets(ndf$Arrival.Delay.in.Minutes)
ndf$Flight.time.in.minutes <- createBuckets(ndf$Flight.time.in.minutes)
ndf$Flight.Distance <- createBuckets(ndf$Flight.Distance)
ndf$Scheduled.Departure.Hour <- as.factor(ndf$Scheduled.Departure.Hour)
ndf$Day.of.Month <- as.factor(ndf$Day.of.Month)
ndf$Year.of.First.Flight <- as.factor(ndf$Year.of.First.Flight)

# Build dataframe for a-rule
aprioriDF <- data.frame(ndf$Satisfaction,
                        ndf$Airline.Name,
                        ndf$Age,
                        ndf$Price.Sensitivity,
                        ndf$No.of.Flights.p.a.,
                        ndf$X..of.Flight.with.other.Airlines,
                        ndf$No..of.other.Loyalty.Cards,
                        ndf$Shopping.Amount.at.Airport,
                        ndf$Eating.and.Drinking.at.Airport,
                        ndf$Departure.Delay.in.Minutes,
                        ndf$Arrival.Delay.in.Minutes,
                        ndf$Flight.time.in.minutes,
                        ndf$Airline.Status,
                        ndf$Gender,
                        ndf$Type.of.Travel,
                        ndf$Class,
                        ndf$Flight.cancelled,
                        ndf$Arrival.Delay.greater.5.Mins,
                        ndf$Flight.time.in.minutes,
                        ndf$Flight.Distance,
                        ndf$Orgin.City,
                        ndf$Origin.State,
                        ndf$Destination.City,
                        ndf$Destination.State,
                        ndf$Scheduled.Departure.Hour,
                        ndf$Day.of.Month,
                        ndf$Year.of.First.Flight)

# Analyze for the whole dataset

ruleset <- apriori(aprioriDF, parameter = list(support = 0.2, confidence = 0.2), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
plot(ruleset)

ruleset <- apriori(aprioriDF, parameter = list(support = 0.3, confidence = 0.3), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

ruleset <- apriori(aprioriDF, parameter = list(support = 0.1, confidence = 0.1), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Unhappy"))
plot(ruleset)

ruleset <- apriori(aprioriDF, parameter = list(support = 0.15, confidence = 0.15), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Unhappy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)





# Seperating the dataset into different airline
# With analysis

# Happy one
#"West Airways Inc. "
West <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "West Airways Inc. ", select = -c(ndf.Airline.Name))

ruleset <- apriori(West, parameter = list(support = 0.2, confidence = 0.2), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
plot(ruleset)

ruleset <- apriori(West, parameter = list(support = 0.4, confidence = 0.7), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)



# Unhappy one
#"GoingNorth Airlines Inc. "
GoingNorth <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "GoingNorth Airlines Inc. ", select = -c(ndf.Airline.Name))

ruleset <- apriori(GoingNorth, parameter = list(support = 0.1, confidence = 0.1), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Unhappy"))
plot(ruleset)

ruleset <- apriori(GoingNorth, parameter = list(support = 0.15, confidence = 0.2), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Unhappy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)


#Cheapseats Airlines Inc. 
Cheapseats <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Cheapseats Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(Cheapseats, parameter = list(support = 0.3, confidence = 0.3), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#Cool&Young Airlines Inc. 
CoolYoung <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Cool&Young Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(CoolYoung, parameter = list(support = 0.4, confidence = 0.5), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#EnjoyFlying Air Services 
EnjoyFlying <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "EnjoyFlying Air Services", select = -c(ndf.Airline.Name))
ruleset <- apriori(EnjoyFlying, parameter = list(support = 0.3, confidence = 0.3), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"FlyFast Airways Inc. " 
FlyFast <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "FlyFast Airways Inc. ", select = -c(ndf.Airline.Name)) 
ruleset <- apriori(FlyFast, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"FlyHere Airways"
FlyHere <- subset(aprioriDF, aprioriDF$ndf.Airline.Name =="FlyHere Airways", select = -c(ndf.Airline.Name))
ruleset <- apriori(FlyHere, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)


#"FlyToSun Airlines Inc. "
FlyToSun <- subset(aprioriDF, aprioriDF$ndf.Airline.Name =="FlyToSun Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(FlyToSun, parameter = list(support = 0.35, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"Northwest Business Airlines Inc. "
Northwest <- subset(aprioriDF, aprioriDF$ndf.Airline.Name =="Northwest Business Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(Northwest, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"OnlyJets Airlines Inc. "
OnlyJets <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "OnlyJets Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(OnlyJets, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"Oursin Airlines Inc. "
Oursin <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Oursin Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(Oursin, parameter = list(support = 0.35, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"Paul Smith Airlines Inc. "
PaulSmith <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Paul Smith Airlines Inc. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(PaulSmith, parameter = list(support = 0.4, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"Sigma Airlines Inc. "
Sigma <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Sigma Airlines Inc. " , select = -c(ndf.Airline.Name))
ruleset <- apriori(Sigma, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)

#"Southeast Airlines Co. "
Southeast <- subset(aprioriDF, aprioriDF$ndf.Airline.Name == "Southeast Airlines Co. ", select = -c(ndf.Airline.Name))
ruleset <- apriori(Southeast, parameter = list(support = 0.3, confidence = 0.6), appearance = list(default = 'lhs', rhs = "ndf.Satisfaction=Happy"))
inspect(ruleset)
summary(ruleset)
plot(ruleset)



