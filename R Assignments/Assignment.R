#------------------------------------------------
#---------------  R-Assignment 1  ---------------
#------------------------------------------------

getwd()
setwd("F:/R")

#----------------- Question-1 -------------------
sessionInfo()

#----------------- Question-2 -------------------

abc<-3


#----------------- Question-3 -------------------

a<-c(1,2,3,4,5)
b<-c("C","h","a","c","t","e","r")
c<-c(TRUE,FALSE)


#----------------- Question-4 -------------------
ls()



#----------------- Question-5 -------------------
x<-c(4,4,5,6,7,2,9)

summary(x)    # gives min, max,mean, median, 1st quartile, 3rd quartile
describe(x)   # returns number of obs, mean, standard dev, median, min, max, standard error etc
sd(x)
var(x)
sum(x)
length(x)


x[3]
x[2:6]
x[c(1,3,5,7)]


#----------------- Question-6 -------------------
m<-matrix(1:24, nrow=6, ncol=4)


#----------------- Question-7 -------------------

storeId<-c(111,208,113,408)
tenure<-c(25,34,28,52)
storetype<-c("Type1","Type2","Type1","Type1")
status<-c("poor","improved","excellent","poor")

stores<-data.frame(storeId,tenure,storetype,status)
View (stores)


#----------------- Question-8(a) -------------------

stores[c("storeId", "tenure")]


#----------------- Question-8(b) -------------------
stores[c("storetype", "status")]


#----------------- Question-8(c) -------------------
stores["tenure"]


#----------------- Question-9 -------------------

ethinicity<-c("white","african american","white","asian")
outcome<-c(1, 3, 2, 4, 3, 1, 1) 


fethinicity<-factor(ethinicity)
fstatus<-factor(status)
foutcome<-factor(outcome, labels=c("Poor","Average", "Good", "Excellent" ))


#----------------- Question-10 -------------------
h<-c(25,26,18,39)
j<-matrix(1:10, nrow=5, ncol=2)
k<-c("One","Two","Three")

mylist=list(ages=h,j,k,"My First List")


#----------------- Question-11 -------------------
st<-read.csv("stores.csv")
summary(st)
install.packages('dplyr')
require('dplyr')
summarize_each(iris,funs(mean))

#----------------- Question-12 -------------------
q12<-with(st["OperatingCost"], summary )

#----------------- Question-13 -------------------
apply(st,MARGIN=2,FUN=class)
apply(st,MARGIN=2,FUN=names)
apply(st,MARGIN=2,FUN=length)
apply(st,MARGIN=c(1,2),FUN=dim)
apply(st,MARGIN=c(1,2),FUN=str)
apply(st,MARGIN=2,FUN=head)
apply(st,MARGIN=2,FUN=tail)
apply(st,MARGIN=1,FUN=fix)



#----------------- Question-14 -------------------
st['q14']=st['OperatingCost']+st['AcqCostPercust']
transform(st, q14a=st['OperatingCost']+st['AcqCostPercust'])

#----------------- Question-15 -------------------

st["category"]<-ifelse(st$TotalSales < 120, (st['category']="Low Perform Store"), ifelse(st$TotalSales >= 120 & st$TotalSales < 240,(st['category']="Medium Perform store"),ifelse(st$TotalSales>240,(st['category']="High Perform Store"),NA)))


  
#----------------- Question-16 -------------------
install.packages('plyr')
require('plyr')

st<-rename(st, c("AcqCostPercust"="AcqCost"))
View(st)


#----------------- Question-17 -------------------
table(is.na(st)) #counting the number of missing values
st1<-na.omit(st) #creating a new dataframe with no missing values
st[is.na(st)]<-0 #repplacing missing values with a 0


#----------------- Question-18 -------------------
newstore<-sort(st1$StoreType) #part-a

newstore<-st1[order(st1$Location,-st1$TotalSales),] #part-b
View(newstore)


#----------------- Question-19 -------------------


date1<-as.Date("2014-06-22")
class(date2)

date2<-as.Date("2014-02-13")


date3<-dmy("01/05/1965")
date4<-mdy("08/16/1975")

#----------------- Question-20 -------------------
st[,c(5,7,8,9)] #a-part
st[,c(-5,-7,-8,-9)] #b-part
st[1:10,] #c-part
st[c(st$StoreType=="Apparel" & st$TotalSales>100),] #d-part


subset(st[c("StoreCode" , "StoreName", "Location","TotalSales")], TotalSales>=100 & TotalSales<300) #e-part

subset(st[,1:10],StoreType=="Electronincs" & TotalSales>100) #f-part
               