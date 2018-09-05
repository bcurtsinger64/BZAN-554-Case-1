#Exploratory

#Load packages
library(pacman)
p_load(tidyverse)
p_load(dplyr)
p_load(stringr)
p_load(randomForest)
p_load(AUC)
p_load(lift)

#Read in data
customers <- read_csv("customers.csv")
purchases <- read_csv("purchases.csv")
registrations <- read_csv("registrations.csv")

#Customers not included in registrations
customers <- customers[customers$CompanyName %in% registrations$CompanyName,] #make sure all customers are registered
purchases <- purchases[purchases$CustomerID %in% customers$CustomerID,] #Make sure all purchases are part of customers

#Does purchases = registered users
regsmall <- 
  registrations %>% 
  select(CompanyName, CompanyAddress, RegistrationDate) %>% 
  group_by(CompanyName, CompanyAddress) %>%
  summarise(NumRegistered = n(), FirstRegDate = min(RegistrationDate), LastRegDate = max(RegistrationDate)) #Find #, first and last date for each co.


#Feature ID/Engineering

#Create a "lifetime" variable for each customer that takes sysdate - date of first registration for that customer
regsmall$Lifetime <- Sys.Date() - regsmall$FirstRegDate #Difference in first registration date & current date
regsmall$LastRegLength <- Sys.Date() - regsmall$LastRegDate #Difference in last registration date & current date
regsmall$zip <- str_extract(regsmall$CompanyAddress, "\\d{5}") #Extract Zip code

#did customer purchase or not?
regsmall$Converted <- as.numeric(regsmall$CompanyName %in% customers$CompanyName) 
regsmall$Converted <- as.factor(regsmall$Converted) #create binary conversion variable


#Build Model?
trows <- sample(1:nrow(regsmall), .2*nrow(regsmall))
ctrain <- regsmall[-trows,]

ctest <- regsmall[trows,]



rf <- randomForest(Converted ~ NumRegistered + Lifetime + LastRegLength + zip, ctrain)
pred <- predict(rf, ctest)
auc(roc(pred, ctest$Converted))
TopDecileLift(pred, ctest$Converted)






