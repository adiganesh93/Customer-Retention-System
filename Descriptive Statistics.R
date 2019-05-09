#package
library(ggplot2)
library(MASS)

#read data
df <- read.csv("C:/Users/Aditya/Desktop/IST-687/Satisfaction Survey.csv")

#clean missing value
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes) & (df$Flight.cancelled == 'Yes'))] <- 0

#change the data type of 'Satisfaction' to numeric
df$Satisfaction <- as.numeric(as.character(df$Satisfaction))

#omit the missing value
ndf <- na.omit(df)

#get the names of airlines
airline.name <- c(levels(ndf$Airline.Name))

#Insert a new column descriping the degree of satisfaction
ndf$degree <- NA
ndf$Satisfaction <- as.numeric(as.character(ndf$Satisfaction))
ndf$degree[which(ndf$Satisfaction >=4)]<- "High"
ndf$degree[which(ndf$Satisfaction ==3)] <- "Average"
ndf$degree[which(ndf$Satisfaction ==3.5)] <- "Average"
ndf$degree[which(ndf$Satisfaction <3)] <- "Low"

ndf <- na.omit(ndf)

#Build a table counting the number of each degree of satisfaction grouped by airline names
Freq <- as.data.frame(table(ndf$Airline.Name,ndf$degree))
colnames(Freq) <- c("Name","Satis.Degree","Count")
Freq

#Draw a bar chart for all airline companies showing the distribution of satisfaction
Satis_Bar <- ggplot(Freq,aes(x=Name,y=Count,fill=Satis.Degree)) + 
  geom_bar(stat="identity",color="black") + 
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  ggtitle("Distribution of Satisfaction by Airline names")+
  labs(x = "Airline Names", y = "Count")
Satis_Bar

#Build a propotion table of satisfaction grouped by company
P1 <- prop.table(table(ndf$Airline.Name,ndf$degree),1)
P1
Prop <- data.frame(matrix(data = P1, nrow= length(airline.name), ncol = 3, byrow = F))
Prop
colnames(Prop) <- c('Average','High','Low')
row.names(Prop) <- c(airline.name)
Prop

#perfect group
HighBar <- ggplot(Prop,aes(x=reorder(airline.name,High),y=High)) + geom_col(color="black",fill=" dark green") + 
  theme(axis.text.x = element_text(angle=90,hjust = 1))+ 
  ggtitle("Distribution of High Satisfaction by Airline names")+
  labs(x = "Airline Names", y = "The porpotion of high satisfaction")+
  coord_cartesian(ylim = c(0.4,0.6))
HighBar
#The west Ailrlines has the highest porpotion of high satisfaction
#This company is the most satisfacted company

#soso group
AverageBar <- ggplot(Prop,aes(x=reorder(airline.name,Average),y=Average)) + geom_col(color="black",fill="dark red") + 
  theme(axis.text.x = element_text(angle=90,hjust = 1))+ 
  ggtitle("Distribution of Average Satisfaction by Airline names")+
  labs(x = "Airline Names", y = "The porpotion of average satisfaction")+
  coord_cartesian(ylim = c(0.25,0.3))
AverageBar

#unhappy group
LowBar <- ggplot(Prop,aes(x=reorder(airline.name,Low),y=Low)) + geom_col(color="black",fill="dark blue") + 
  theme(axis.text.x = element_text(angle=90,hjust = 1))+ 
  ggtitle("Distribution of Low Satisfaction by Airline names")+
  labs(x = "Airline Names", y = "The porpotion of low satisfaction")+
  coord_cartesian(ylim = c(0.1,0.26))
LowBar
#The GoingNorth Airlines Inc. has the highest porpotion of low satisfaction.
#So this company is the least satisfacted company.