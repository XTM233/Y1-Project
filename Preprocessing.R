#Pre-processing of the dataset

#Load the file
churn.20 <- read.csv("churn-bigml-20.csv")
churn.80 <- read.csv("churn-bigml-80.csv")
#Combine test and training set since we are going to do our own split later
churn.whole <- rbind(churn.20,churn.80)

#Understand the dataset
dim(churn.whole)
#Inspect what are the nominal, ordinal, numeric variables
names(churn.whole)
for(i in 1:19){print(unique(churn.whole[,i]))}
#Nomnial variable, "State"[1], "Area.code"[3]
#Ordinal variable/true or false, "International.plan"[4],"Voice.mail.plan"[5]
#Numeric variable, the rest

#Analyse the correlations between numeric variable
library("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
churn.numeric <- cbind(churn.whole[,2],churn.whole[,6:19])
cor(churn.numeric)
rquery.cormat(churn.numeric, graphType="heatmap")
#Find the highly correlated vairbales through inspection
#"Total.night.minutes", "Total.night.charge"
#"Total.intl.minutes", "Total.intl.charge"
#"Total.eve.minutes", "Total.eve.charge"
#"Total.day.minutes", "Total.day.charge"

#Remove one variable for each correlated pair
names(churn.numeric)
colnames(churn.numeric)[1] <- "Account.length" #somehow the colname is dropped
churn.numeric <- churn.numeric[,-c(5,8,11,14)]
head(churn.numeric)

#Standard scale the numeric variables
churn.numeric <- scale(churn.numeric)

#Convert ordinal variable and the label
churn.whole[churn.whole == "Yes"] <- 1
churn.whole[churn.whole == "No"] <- 0
churn.whole[churn.whole == "True"] <- 1
churn.whole[churn.whole == "False"] <- 0
head(churn.whole)

#One-hot encoding for nominal variable
#Decide to drop the "State" variable because it will add too many dimensions to the feature space
churn.whole <- churn.whole[,-1]
library("caret")
churn.whole[churn.whole==408] <- "408"
churn.whole[churn.whole==415] <- "415"
churn.whole[churn.whole==510] <- "510"
dummy <- dummyVars(" ~ .", data=churn.whole)
churn.whole <- data.frame(predict(dummy, newdata = churn.whole)) 
head(churn.whole)

#Combine all the variables

churn.whole <- cbind(churn.whole[,c(2,3,6,8)],churn.numeric,churn.whole[,24]) 
colnames(churn.whole)[c(3,4,16)] <- c("International.plan","Voice.mail.plan","Churn")
head(churn.whole)

#Write the processed dataset into file
write.csv(churn.whole,"churn-whole.csv",)
                                   
