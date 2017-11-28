#---------------------------------------------------------------------------
#--------------------- SAS Assignment 1 in R -------------------------------
#---------------------------------------------------------------------------

setwd('F:/SASUniversityEdition/myfolders/file_data/Assignments/Datasets/SAS Assignment 1')
#--------------- q1 ---------------
carsales=read.csv('Car_sales.csv')

#--------------- q2 ---------------
carsales<-carsales[(carsales$X4.year.resale.value!="." &  carsales$Price.in.thousands!="."),]

#--------------- q3 ---------------

class(carsales$Price.in.thousands)
carsales$Price.in.thousands<-as.character(carsales$Price.in.thousands)
carsales$Price.in.thousands<-as.numeric(carsales$Price.in.thousands)
q2<-carsales[carsales$Price.in.thousands<=15,]
q2a<-carsales[(carsales$Price.in.thousands>15 & carsales$Price.in.thousands<=20),]


#--------------- q4 ---------------
names(carsales)
q4<-carsales[c("Manufacturer", "Model","Sales.in.thousands","Price.in.thousands")]

#--------------- q5 ---------------
class(carsales$Latest.Launch)
carsales$Latest.Launch<-as.character(carsales$Latest.Launch)
require(lubridate)

carsales$Latest.Launch<-dmy(carsales$Latest.Launch)
q5<-carsales[(carsales$Latest.Launch>as.Date("2014-10-01") & carsales$Vehicle.type=="Passenger"),]
