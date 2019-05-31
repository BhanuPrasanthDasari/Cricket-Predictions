install.packages("RODBC")
 Rankings[1] <- "Position"
> View(Rankings)
> Rankings <- read.csv("F:\\Datasets\\Cricket\\Final_Data\\ODI_Rankings.csv",header=TRUE)
> View(Rankings)
> names(Rankings)[1] <- "Position"
> View(Rankings)
----------------------------------------------------------------------

# Pie Chart with Percentages for Cricket Countries Win/Loss Ratio:

slices <- Rankings$Ratings
lbls <- Rankings$Country
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
   main="Pie Chart of Countries Win Capacity")


pie3D(slices,labels=lbls, col=rainbow(length(lbls)),explode=0.05,
   main="Pie Chart of Countries ")



--------------------------------------------------------------------------------------------------------------------
In progress histogram:
hist(slices,main="Ratings",xlab="Points")library
--------------------------------------------------------------------------------------------------------------------
R Command to connect to SQL DB:

channel <- odbcConnect("ORCL", uid="SYSTEM", pwd="xXx143007")

---------------------------------------------------------------------------------------------------------------------
R Command to set SQL TABLES into R DataFrames:


dataframe <- sqlQuery(channel, "
 SELECT *
 FROM
 SYS.CRICKET_PLAYERS",as.is=FALSE, errors=FALSE, max=(no.of.rows in table), buffsize=1,
nullstring=NA, na.strings="NA", believeNRows=TRUE, dec=getOption("dec"))
---------------> Its Working phase:
odbcQuery (channel, "select sysdate from dual")
 sqlGetResults(channel, as.is=FALSE, errors=FALSE, max=10, buffsize=1,
nullstring=NA, na.strings="NA", believeNRows=TRUE, dec=getOption("dec"))

----------------------------------------------------------------------------------------------------------------------

Today's Test Phase:

FINALTEAMS_BATSMEN <- read.csv("F:\\Datasets\\Cricket\\Final_Data\\FINALBATSMEN.csv",header=TRUE)

head(FINALTEAMS_BATSMEN)
library(tree)
m <- average(FINALTEAMS_BATSMEN$BATTING_STRIKE_RATE,FINALTEAMS_BATSMEN$BATTING_AVG)
f <- (FINALTEAMS_BATSMEN$COUNTRY, m)
f <- tree(COUNTRY~., FINALTEAMS_BATSMEN)


data=read.csv("D:/advDatabase/ContinousDataset.csv")
fix(data)
library(lubridate)
data$Match.Date=mdy(data$Match.Date)
new_data=data[as.Date(data$Match.Date,"%m%d%y")>"2013-01-01"&as.Date(data$Match.Date,"%m%d%y")<"2016-01-01",]
new_dataa<- new_data[which(new_data$Host_Country=='Australia'),]
fix(new_dataa)
new_dataa$Winner
set.seed(123)
datatrain=sample(1:nrow(new_dataa),60)
train=new_dataa[datatrain,]
test=new_dataa[-datatrain,]
library(tree)
library(e1071)
Naive_Bayes_Model=naiveBayes(Winner~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
test.table = table(Predict = NB_Predictions, Truth =test$Winner)
print(test.table)
mean(NB_Predictions!=test$Winner)


#Misclassification error= 0.3181818
#confusion matrix and misclassification error for India
data$Match.Date=mdy(data$Match.Date)
new_data=data[as.Date(data$Match.Date,"%m%d%y")>"2009-01-01"&as.Date(data$Match.Date,"%m%d%y")<"2012-01-01",]
new_dataa<- new_data[which(new_data$Host_Country=='India'),]
new_dataa$Winner
set.seed(123)
datatrain=sample(1:nrow(new_dataa),60)
train=new_dataa[datatrain,]
test=new_dataa[-datatrain,]
library(tree)
library(e1071)
Naive_Bayes_Model=naiveBayes(Winner~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
test.table = table(Predict = NB_Predictions, Truth =test$Winner)
print(test.table)
mean(NB_Predictions!=test$Winner) 


#Misclassification error=0.25
# misclassification error for England
data=read.csv("D:/advDatabase/ContinousDataset.csv")
fix(data)

data$Match.Date=mdy(data$Match.Date)
new_data=data[as.Date(data$Match.Date,"%m%d%y")>"2016-01-01"&as.Date(data$Match.Date,"%m%d%y")<"2019-01-01",]
fix(new_data)
new_dataa<- new_data[which(new_data$Host_Country=='England'),]
fix(new_dataa)
new_dataa$Winner
set.seed(123)
datatrain=sample(1:nrow(new_dataa),40)
train=new_dataa[datatrain,]
test=new_dataa[-datatrain,]

Naive_Bayes_Model=naiveBayes(Winner~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
test.table = table(Predict = NB_Predictions, Truth =test$Winner)
print(test.table)
mean(NB_Predictions!=test$Winner)
#misclassification error=0.2


