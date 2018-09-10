#Group_9
#Bryce Curtsinger & Derek Shambo

fitModel <- function(customers, purchases, registrations){
  #Load packages
  if (!require("pacman")) install.packages("pacman")  
  p_load(tidyverse)
  p_load(dplyr)
  p_load(stringr)
  p_load(randomForest)
  p_load(AUC)
  p_load(lift)
  p_load(lubridate)
  p_load(zipcode)
  
  data("zipcode") #To match state to zip
  
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
  regsmall$LastRegLength <- as.integer(as_date("2016-01-01") - regsmall$LastRegDate) #Difference in last registration date & first day of 2016
  regsmall$FirstRegLength <- as.integer(as_date("2016-01-01") - regsmall$FirstRegDate) #Difference in first registration date & first day of 2016
  regsmall$zip <- str_extract(regsmall$CompanyAddress, "\\d{5}")#Extract Zip code
  regsmall <- regsmall %>% left_join(zipcode %>% select(zip, state)) #Get state from zip
  regsmall$state <- as.factor(regsmall$state) #Convert to factor
  regsmall <- regsmall[complete.cases(regsmall$state),] #Remove NAs
  regsmall$FirstMonth <- as.factor(month(regsmall$FirstRegDate))
  regsmall$LastMonth <- as.factor(month(regsmall$LastRegDate))
  
  #did customer purchase or not?
  regsmall$Converted <- as.factor(as.numeric(regsmall$CompanyName %in% customers$CompanyName)) #Create response
  
  
  #Fit Model
  glmodel <- glm(Converted ~  LastRegLength + FirstRegLength + FirstMonth + LastMonth + NumRegistered + state, regsmall, family = "binomial")
  return(glmodel)
}

predictCust <- function(glmodel, newregistrations){
  
  newdata <- 
  newregistrations %>% 
  select(CompanyName, CompanyAddress, RegistrationDate) %>% 
  group_by(CompanyName, CompanyAddress) %>%
  summarise(NumRegistered = n(), FirstRegDate = min(RegistrationDate), LastRegDate = max(RegistrationDate)) #Find #, first and last date for each co.
  
  #Feature ID/Engineering
  newdata$LastRegLength <- as.integer(as_date("2016-01-01") - newdata$LastRegDate) #Difference in last registration date & first day of 2016
  newdata$FirstRegLength <- as.integer(as_date("2016-01-01") - newdata$FirstRegDate) #Difference in first registration date & first day of 2016
  newdata$zip <- str_extract(newdata$CompanyAddress, "\\d{5}")#Extract Zip code
  newdata <- newdata %>% left_join(zipcode %>% select(zip, state)) #Get state from zip
  newdata$state <- as.factor(newdata$state) #Convert to factor
  newdata <- newdata[complete.cases(newdata$state),] #Remove NAs
  newdata$FirstMonth <- as.factor(month(newdata$FirstRegDate))
  newdata$LastMonth <- as.factor(month(newdata$LastRegDate))
  
  gpred <- predict(glmodel, newdata, type = "response")
  gpred <- data.frame(ctest$CompanyName, gpred)
  return(gpred)
}


