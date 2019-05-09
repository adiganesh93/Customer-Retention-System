#package
library(ggplot2)
library(MASS)
library(kernlab)
library(RJSONIO)

#read data
setwd("D:/Syracuse University/Fall'18/IST 687/Project")
df <- read.csv("Satisfaction Survey.csv")
View(df)
#clean missing value
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes) & (df$Flight.cancelled == 'Yes'))] <- 0

#change the data type of 'Satisfaction' to numeric
df$Satisfaction <- as.numeric(as.character(df$Satisfaction))

#omit the missing value
ndf <- na.omit(df)
View(df)
#get the names of airlines
airline.name <- c(levels(ndf$Airline.Name))
airline.name
#Insert a new column describing the degree of satisfaction
ndf$degree <- NA
ndf$Satisfaction <- as.numeric(as.character(ndf$Satisfaction))
ndf$degree[which(ndf$Satisfaction >=4)]<- "High"
ndf$degree[which(ndf$Satisfaction <4)] <- "Low"

ndf <- na.omit(ndf)
View(ndf)

GoingNorth <- subset(ndf, Airline.Name == "GoingNorth Airlines Inc. ") #"GoingNorth Airlines Inc. "
West <- subset(ndf, Airline.Name == "West Airways Inc. ") #"West Airways Inc. "

randIndex <- sample(1:dim(GoingNorth)[1]) 
summary(randIndex)

# Creating a breakpoint of 2/3rd and 1/3rd part for GoingNorth
cutPoint2_3_gn <- floor(2 * dim(GoingNorth)[1]/3) 
cutPoint2_3_gn
# Creating traindata with 2/3rd of GoingNorth data
trainData_gn <- GoingNorth[randIndex[1:cutPoint2_3_gn],]
# Creating testdata with 1/3rd of GoingNorth data 
testData_gn <- GoingNorth[randIndex[(cutPoint2_3_gn+1):dim(GoingNorth)[1]],] 
testData_gn
#check dimensions of the data frame GoingNorth, trainData_gn and testData_gn
dim(GoingNorth)
dim(trainData_gn)
View(trainData_gn)
dim(testData_gn)

#GoingNorth Airlines

svmOutput_gn <- ksvm(degree ~ Airline.Status+Type.of.Travel+Arrival.Delay.greater.5.Mins, data = trainData_gn,kernel ="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
print(svmOutput_gn)
svmPred_gn <- predict(svmOutput_gn, testData_gn, type ="votes") 
svmPred_gn
str(svmPred_gn)
head(svmPred_gn)
# Creating a composite table based on CompTable_gn and svmPred_gn
compTable_gn<-data.frame(testData_gn$degree,svmPred_gn[1,])
View(compTable_gn)
# Creating a confusion matrix
conMatrix_gn<-table(compTable_gn) 
conMatrix_gn

ctable <- matrix(c(68, 185, 202, 68), nrow = 2, byrow = TRUE)
ctable

colnames(ctable) <- c("Prediction:0","Prediction:1")
row.names(ctable) <- c("Degree: High", "Degree: Low")

fourfoldplot(ctable, color = c("#ff0000", "#00b300"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


errorSum_gn<-conMatrix_gn[1,1]+conMatrix_gn[2,2]
errorSum_gn
# Creating percentage of error rate
errorRate_gn<-errorSum_gn/sum(conMatrix_gn)*100 
errorRate_gn

#West Airlines:

svmOutput_gn1 <- ksvm(degree ~ Airline.Status+Gender+Type.of.Travel+Arrival.Delay.greater.5.Mins, data = trainData_gn,kernel ="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE) 
                      
print(svmOutput_gn1)
svmPred_gn1 <- predict(svmOutput_gn1, testData_gn, type ="votes") 
svmPred_gn1
str(svmPred_gn1)
head(svmPred_gn1)
# Creating a composite table based on CompTable_gn and svmPred_gn
compTable_gn1<-data.frame(testData_gn$degree,svmPred_gn1[1,])
str(compTable_gn1)
# Creating a confusion matrix
conMatrix_gn1<-table(compTable_gn1) 
conMatrix_gn1

ctable <- matrix(c(37, 208, 196, 82), nrow = 2, byrow = TRUE)
ctable

colnames(ctable) <- c("Prediction:0","Prediction:1")
row.names(ctable) <- c("Degree: High", "Degree: Low")

fourfoldplot(ctable, color = c("#ff0000", "#00b300"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

# Creating a dataframe containing sum of errors
errorSum_gn1<-conMatrix_gn1[1,1]+conMatrix_gn1[2,2]
errorSum_gn1
# Creating percentage of error rate
errorRate_gn1<-errorSum_gn1/sum(conMatrix_gn1)*100 
errorRate_gn1

