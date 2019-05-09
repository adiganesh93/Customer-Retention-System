#After analyzing the plot, we want to what makes some airline companies having more high satisfaction than others
#So we will build linear models for each airline companies
#subset the data by company
which(colnames(df) == "Airline.Code")
nndf <- df[,-16]
nndf <- na.omit(nndf)

Cheapseats <- data.frame(subset(nndf, Airline.Name == "Cheapseats Airlines Inc. ")) #Cheapseats Airlines Inc. 
CoolYoung <- subset(nndf, Airline.Name == "Cool&Young Airlines Inc. ") #Cool&Young Airlines Inc.
EnjoyFlying <- subset(nndf, Airline.Name == "EnjoyFlying Air Services") #EnjoyFlying Air Services 
FlyFast <- subset(nndf, Airline.Name == "FlyFast Airways Inc. ") #"FlyFast Airways Inc. " 
FlyHere <- subset(nndf, Airline.Name =="FlyHere Airways") #"FlyHere Airways"
FlyToSun <- subset(nndf, Airline.Name =="FlyToSun Airlines Inc. ") #"FlyToSun Airlines Inc. "
GoingNorth <- subset(nndf, Airline.Name == "GoingNorth Airlines Inc. ") #"GoingNorth Airlines Inc. "
Northwest <- subset(nndf, Airline.Name =="Northwest Business Airlines Inc. ") #"Northwest Business Airlines Inc. "
OnlyJets <- subset(nndf, Airline.Name == "OnlyJets Airlines Inc. ") #"OnlyJets Airlines Inc. "
Oursin <- subset(nndf, Airline.Name == "Oursin Airlines Inc. ") #"Oursin Airlines Inc. "
PaulSmith <- subset(nndf, Airline.Name == "Paul Smith Airlines Inc. ") #"Paul Smith Airlines Inc. "
Sigma <- subset(nndf, Airline.Name == "Sigma Airlines Inc. " ) #"Sigma Airlines Inc. "
Southeast <- subset(nndf, Airline.Name == "Southeast Airlines Co. ") #"Southeast Airlines Co. "
West <- subset(nndf, Airline.Name == "West Airways Inc. ") #"West Airways Inc. "

#use stepwise theory to find the important variables for each company and then build the final model

#build function for stepwise theory
which(colnames(nndf) == "Airline.Name") #16
stepwise <- function(myVec){
  DeleteName <- myVec[,-16]
  FullModel <- lm(Satisfaction~.,data = DeleteName)
  StepModel <- stepAIC(FullModel,direction = "forward", trace = F)
  Result <- which(summary(StepModel)$coef[,4] <= 0.001)
  return(Result)
}

#linear model for each company
#Cheapseats Airlines Inc.
stepwise(Cheapseats)
lm_Cheapseats <- lm(Satisfaction ~ Airline.Status+Age+Gender+Price.Sensitivity+
                      No.of.Flights.p.a.+Type.of.Travel+Class+
                      Flight.date+Scheduled.Departure.Hour+Arrival.Delay.greater.5.Mins,
                    data = Cheapseats)
try <- plot_summs(lm_Cheapseats, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

#Cool&Young Airlines Inc.
stepwise(CoolYoung)
lm_CoolYoung <- lm(Satisfaction ~ Airline.Status+Type.of.Travel+
                     Arrival.Delay.greater.5.Mins, data = CoolYoung)


#EnjoyFlying Air Services 
stepwise(EnjoyFlying)
lm_EnjoyFlying <- lm(Satisfaction ~ Airline.Status+Age+Gender+
                       No.of.Flights.p.a.+Type.of.Travel+Class+
                       Arrival.Delay.greater.5.Mins, data = EnjoyFlying)

#"FlyFast Airways Inc. " 
stepwise(FlyFast)
lm_FlyFast <- lm(Satisfaction ~ Airline.Status+Age+Gender+No.of.Flights.p.a.+
                   Type.of.Travel+Class+Flight.cancelled+Arrival.Delay.greater.5.Mins,
                 data = FlyFast)

#"FlyHere Airways"
stepwise(FlyHere)
lm_FlyHere <- lm(Satisfaction ~ Airline.Status+Age+No.of.Flights.p.a.+Type.of.Travel
                 +Arrival.Delay.greater.5.Mins, data = FlyHere)

#"FlyToSun Airlines Inc. "
stepwise(FlyToSun) 
lm_FlyToSun <- lm(Satisfaction ~ Airline.Status+Gender+Type.of.Travel+
                    Arrival.Delay.greater.5.Mins, data = FlyToSun)

#"GoingNorth Airlines Inc. "
stepwise(GoingNorth)
lm_GoingNorth <- lm(Satisfaction ~ Airline.Status+Type.of.Travel+
                      Arrival.Delay.greater.5.Mins, data = GoingNorth)
#"Northwest Business Airlines Inc. "
stepwise(Northwest)
lm_Northwest <- lm(Satisfaction ~ Airline.Status+Age+Gender+Price.Sensitivity+
                     No.of.Flights.p.a.+Type.of.Travel+Class+Arrival.Delay.greater.5.Mins,
                   data = Northwest)

#"OnlyJets Airlines Inc. "
stepwise(OnlyJets)
lm_OnlyJets <- lm(Satisfaction ~ Airline.Status+Age+Gender+No.of.Flights.p.a.+
                    Type.of.Travel+Arrival.Delay.greater.5.Mins, data = OnlyJets)

#"Oursin Airlines Inc. "
stepwise(Oursin)
lm_Oursin <- lm(Satisfaction ~ Airline.Status+Age+Gender+No.of.Flights.p.a.+
                  Type.of.Travel+Class+Arrival.Delay.greater.5.Mins,
                data = Oursin)

#"Paul Smith Airlines Inc. "
stepwise(PaulSmith) 
lm_PaulSmith <- lm(Satisfaction ~ Airline.Status+Gender+Year.of.First.Flight+
                     No.of.Flights.p.a.+Type.of.Travel+Scheduled.Departure.Hour+
                     Arrival.Delay.greater.5.Mins, data = PaulSmith)

#"Sigma Airlines Inc. "
stepwise(Sigma)
lm_Sigma <- lm(Satisfaction ~ Airline.Status+Age+Gender+Price.Sensitivity+
                 No.of.Flights.p.a.+Type.of.Travel+Arrival.Delay.greater.5.Mins,
                 data = Sigma)

#"Southeast Airlines Co. "
stepwise(Southeast) 
lm_Southeast <- lm(Satisfaction ~ Airline.Status+Age+Gender+No.of.Flights.p.a.+
                     Type.of.Travel+Flight.cancelled+Arrival.Delay.greater.5.Mins,
                   data = Southeast)

#"West Airways Inc. --- no flight cancelled
which(colnames(West)=="Flight.cancelled") #24
WestDeleted <- West[,-24]
stepwise(WestDeleted)
lm_West <- lm(Satisfaction ~ Airline.Status+Gender+Type.of.Travel+
                Arrival.Delay.greater.5.Mins, data = WestDeleted)

#All the models have 'Airline.Status', 'Type.of.Travel', 'Arrival.Delay.greater.5.Mins'
#Compare those three attributes of the most satisfacted and the least satisfacted company

#Relation between 'Airline.Status' and 'Satisfaction
prop.status <- function(vec){
  co.status <- prop.table(table(vec$Airline.Status))
  m <- matrix(co.status,ncol = 1,nrow = 4)
  return(m)
}
w <- prop.status(West)
n <- prop.status(GoingNorth)
wn <- c(w,n)
s.wn <- rep(c("Blue","Gold","Platinum","Silver"),times = 2)
co.wn <- rep(c("West Airways Inc.","GoingNorth Airlines Inc."),each = 4)
co.wn
status <- data.frame(co.wn,s.wn,wn)
colnames(status) <- c("AirlineName","AirlineStatus","StatusPorpotionByAirline")
status

bar.status <- ggplot(status, aes(x=AirlineName, y=StatusPorpotionByAirline,fill=AirlineStatus)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("The distribution of airline status")+
  theme_minimal()
bar.status

#Relation between 'Type.of.Travel' and 'Satisfaction'
prop.type <- function(vec){
  co.type <- prop.table(table(vec$Type.of.Travel))
  m <- matrix(co.type,ncol = 1,nrow = 3)
  return(m)
}
wt <- prop.type(West)
nt <- prop.type(GoingNorth)
wnt <- c(wt,nt)
t.wn <- rep(c("BusinessTravel", "MileageTickets", "PersonalTravel"),times = 2 )
co.t <- rep(c("West Airways Inc.","GoingNorth Airlines Inc."),each = 3)
type <- data.frame(co.t,t.wn,wnt)
bar.type <- ggplot(type, aes(x=co.t, y=wnt,fill=t.wn)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("The distribution of travel type for West and Goingnorth")+
  labs(x="Airline Name", y="Porpotion",fill="Travel Type")+
  theme_minimal()
bar.type

#Relation between  'Arrival.Delay.greater.5.Mins' and 'Satisfaction'
prop.delay <- function(vec){
  co.type <- prop.table(table(vec$Arrival.Delay.greater.5.Mins))
  m <- matrix(co.type,ncol = 1,nrow = 2)
  return(m)
}
delay.w <- prop.delay(West)
delay.n <- prop.delay(GoingNorth)
prop.table(table(GoingNorth$Arrival.Delay.greater.5.Mins))
delay.wn <- c(delay.w,delay.n)
DelayGreater5Mins <- rep(c("NO","YES"),times=2)
co.d <- rep(c("West Airways Inc.","GoingNorth Airlines Inc."),each = 2)
delay5min <- data.frame(co.d,DelayGreater5Mins,delay.wn)
bar.delay5min <- ggplot(delay5min, aes(x=co.d, y=delay.wn,fill=DelayGreater5Mins)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  ggtitle("The distribution of arrival delay greater than 5 mins")+
  labs(x="Airline Name", y="Porpotion",fill="Is delayed")+
  theme_minimal()
bar.delay5min
