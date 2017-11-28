#------------------------------------------------
#---------------  R Case Study-1  ---------------
#------------------------------------------------

getwd()
setwd("F:/R")

#----------------- Question-1 -------------------

customers=read.csv('customers.csv')
View(customers)

#----------------- Question-2 -------------------
names(customers)       # gives the names of the column
head(customers)        # first few observations
tail(customers)        # last few observations
summary(customers)     # gives min, max,mean, median, 1st quartile, 3rd quartile
str(customers)         # general info of all variables
nrow(customers)        # number of rows
ncol(customers)        # number of columns
require(dplyr)         # package required for the duplicated function
duplicated(customers)  # returns boolean values for all duplicated enteries
install.packages('psych')
require('psych')       # package required for the describe function
describe(customers)    # returns number of obs, mean, standard dev, median, min, max etc for all variabes

#----------------- Question-3 -------------------

a<-(sum(is.na(customers$Customer.Value) / nrow(customers))*100)  # percentage of missing values in "CustomerValue" var

#----------------- Question-4 -------------------

u<-unique(customers)                     # unique values in the dataframe
dup<-customers[duplicated(customers),]   # duplicated values in the dataframe


#----------------- Question-5 -------------------

q5<-customers$Customer.ID[customers$Customer.Value>1000] # customer ids of customers where value<1000

#----------------- Question-6 -------------------

customers["Customer Value Segment"]<-ifelse(customers$Customer.Value <= 10000, (st['Customer Value Segment']="Low Value Segment"), ifelse(customers$Customer.Value > 10000 & customers$Customer.Value <= 25000,(st['Customer Value Segment']="Medium Value Segment"),ifelse(customers$Customer.Value>25000,(st['Customer Value Segment']="High Value Segment"),NA)))

#----------------- Question-7 -------------------
customers$Balancepoints=customers$Points.earned-customers$Points.redeemed
customers$AverageRevenueperTrip=customers$Customer.Value/customers$buy.times

#----------------- Question-8 -------------------
require(lubridate)
customers$first.Date<-ymd(customers$first.Date)
customers$recent.date<-ymd(customers$recent.date)

class(customers$first.Date)
class(customers$recent.date)
Sys.Date()
customers["q8"]=Sys.Date()-customers$recent.date



#----------------- Question-9 -------------------
sum(customers$Customer.Value, na.rm=TRUE)  #124997325


q9<-aggregate(customers$Customer.Value, by=list(Category=customers$Last_city), FUN=sum, na.rm=TRUE)
q9$percent=((q9$x)/124997325)*100                              # groupby last city


q9a<-aggregate(customers$Customer.Value, by=list(Category=customers$Last_state), FUN=sum, na.rm=TRUE)
q9a$percent=((q9a$x)/124997325)*100                            # by last state

q9b<-aggregate(customers$Customer.Value, by=list(Category=customers$Last_region), FUN=sum, na.rm=TRUE)
q9b$percent=((q9b$x)/124997325)*100                            # by last region

#----------------- Question-10 -------------------


install.packages('sqldf')
require(sqldf)

# count of customers by Last State and Last city
sqldf(" select Last_State, count('Customer.ID') as count from customers group by Last_state;")
sqldf(" select Last_State, count('Customer.ID') as count from customers group by Last_city;")


# average number of purchases by last state and last city
aggregate(customers$buy.times, by=list(customers$Last_state), FUN=mean)
aggregate(customers$buy.times, by=list(customers$Last_city), FUN=mean)

# average purchase transaction value by last state and last city
aggregate(customers$Customer.Value, by=list(customers$Last_state), FUN=mean, na.rm=TRUE)
aggregate(customers$Customer.Value, by=list(customers$Last_city), FUN=mean, na.rm=TRUE)














