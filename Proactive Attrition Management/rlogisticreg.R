#-----------------------------------------------------------------------------------
#--------------------- LOGISTIC REGRESSION CASE STUDY ------------------------------
#-----------------------------------------------------------------------------------

#------------------------------------------------------------------
#--------- Setting the directory and reading the csv file ---------
#------------------------------------------------------------------

setwd("F:\\Business Analytics\\Case studies\\Proactive-Attrition-Management-Logistic Regression")
mydata<-read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")

str(mydata)

#------ Removing illogical variables -----

mydata$CUSTOMER<-NULL
mydata$RETCALLS<-NULL
mydata$RETACCPT<-NULL
mydata$INCMISS<-NULL
mydata$RETCALL<-NULL
mydata$SETPRCM<-NULL
mydata$CHURNDEP<-NULL
mydata$CSA<-NULL



#--- Treating missing values(DELETING OBSERVATIONS THAT HAVE MISSING VALUES) ----

sapply(mydata, function(x) sum(is.na(x)))


mydata <- mydata[complete.cases(mydata), ]


#-----------------------------------------------------------------
#---- Creating user defined function for descriptive analysis ----
#-----------------------------------------------------------------

var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}



#Vector of numaerical and categorical variables respectively
num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)

#Applying above defined function on numerical variables
my_num_data<-t(data.frame(apply(mydata[num_var], 2, var_Summ)))
getwd()
write.csv(my_num_data,"F:/Business Analytics/Case studies/Final case study 3/outlier1.csv")
my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)





#--- Outlier Treatment -----

mydata$REVENUE[mydata$REVENUE>224.3076]<-224.3076
mydata$MOU[mydata$MOU>2441.42]<-2441.42
mydata$RECCHRGE[mydata$RECCHRGE>119.99]<-119.99
mydata$DIRECTAS[mydata$DIRECTAS>9.65]<-9.65
mydata$OVERAGE[mydata$OVERAGE>424.98]<-424.98
mydata$ROAM[mydata$ROAM>21.3892]<-21.3892
mydata$CHANGEM[mydata$CHANGEM>735.959999999999]<-735.959999999999
mydata$CHANGER[mydata$CHANGER>117.739599999999]<-117.739599999999
mydata$DROPVCE[mydata$DROPVCE>42]<-42
mydata$BLCKVCE[mydata$BLCKVCE>46.67]<-46.67
mydata$UNANSVCE[mydata$UNANSVCE>178.67]<-178.67
mydata$CUSTCARE[mydata$CUSTCARE>21]<-21
mydata$THREEWAY[mydata$THREEWAY>4]<-4
mydata$MOUREC[mydata$MOUREC>771.060799999999]<-771.060799999999
mydata$OUTCALLS[mydata$OUTCALLS>164]<-164
mydata$INCALLS[mydata$INCALLS>77]<-77
mydata$PEAKVCE[mydata$PEAKVCE>499.67]<-499.67
mydata$OPEAKVCE[mydata$OPEAKVCE>436.973599999999]<-436.973599999999
mydata$DROPBLK[mydata$DROPBLK>71]<-71
mydata$CALLFWDV[mydata$CALLFWDV>0]<-0
mydata$CALLWAIT[mydata$CALLWAIT>23.33]<-23.33
mydata$MONTHS[mydata$MONTHS>49]<-49
mydata$UNIQSUBS[mydata$UNIQSUBS>5]<-5
mydata$ACTVSUBS[mydata$ACTVSUBS>4]<-4
mydata$PHONES[mydata$PHONES>7]<-7
mydata$MODELS[mydata$MODELS>5]<-5
mydata$EQPDAYS[mydata$EQPDAYS>1143]<-1143
mydata$AGE1[mydata$AGE1>74]<-74
mydata$AGE2[mydata$AGE2>76]<-76
mydata$REFER[mydata$REFER>1]<-1
mydata$CREDITAD[mydata$CREDITAD>1]<-1
mydata$SETPRC[mydata$SETPRC>199.99]<-199.99



#--------------------------------------------------------------
#------ Declaring categorical variables as factors ------------
#--------------------------------------------------------------

names(mydata)
str(mydata,list.len=ncol(mydata))


mydata$CHURN<-as.factor(mydata$CHURN)
mydata$CHILDREN<-as.factor(mydata$CHILDREN)
mydata$CREDITA<-as.factor(mydata$CREDITA)
mydata$CREDITAA<-as.factor(mydata$CREDITAA)
mydata$CREDITB<-as.factor(mydata$CREDITB)
mydata$CREDITC<-as.factor(mydata$CREDITC)
mydata$CREDITDE<-as.factor(mydata$CREDITDE)
mydata$CREDITGY<-as.factor(mydata$CREDITGY)
mydata$CREDITZ<-as.factor(mydata$CREDITZ)
mydata$PRIZMRUR<-as.factor(mydata$PRIZMRUR)
mydata$PRIZMUB<-as.factor(mydata$PRIZMUB)
mydata$PRIZMTWN<-as.factor(mydata$PRIZMTWN)
mydata$REFURB<-as.factor(mydata$REFURB)
mydata$WEBCAP<-as.factor(mydata$WEBCAP)
mydata$TRUCK<-as.factor(mydata$TRUCK)
mydata$RV<-as.factor(mydata$RV)
mydata$OCCPROF<-as.factor(mydata$OCCPROF)
mydata$OCCCLER<-as.factor(mydata$OCCCLER)
mydata$OCCCRFT<-as.factor(mydata$OCCCRFT)
mydata$OCCSTUD<-as.factor(mydata$OCCSTUD)
mydata$OCCHMKR<-as.factor(mydata$OCCHMKR)
mydata$OCCRET<-as.factor(mydata$OCCRET)
mydata$OCCSELF<-as.factor(mydata$OCCSELF)
mydata$OWNRENT<-as.factor(mydata$OWNRENT)
mydata$MARRYUN<-as.factor(mydata$MARRYUN)
mydata$MARRYYES<-as.factor(mydata$MARRYYES)
mydata$MARRYNO<-as.factor(mydata$MARRYNO)
mydata$MAILORD<-as.factor(mydata$MAILORD)
mydata$MAILRES<-as.factor(mydata$MAILRES)
mydata$MAILFLAG<-as.factor(mydata$MAILFLAG)
mydata$TRAVEL<-as.factor(mydata$TRAVEL)
mydata$PCOWN<-as.factor(mydata$PCOWN)
mydata$CREDITCD<-as.factor(mydata$CREDITCD)
mydata$NEWCELLY<-as.factor(mydata$NEWCELLY)
mydata$NEWCELLN<-as.factor(mydata$NEWCELLN)
mydata$MCYCLE<-as.factor(mydata$MCYCLE)


#--------------------------------------------------------------
#----- Seperating continuous and categorical variables --------
#--------------------------------------------------------------

var_num<-sapply(mydata,is.factor)
cat_data<-mydata[,var_num]
cont_data<-mydata[,!var_num]
names(cont_data)

#--------------------------------------------------------------
#----------------- Corelation matrix --------------------------
#--------------------------------------------------------------

?cor
corrm <- cor(cont_data)
write.csv(corrm,"correlation_matrix.csv")


#--------------------------------------------------------------
#------------------- Factor Analysis --------------------------
#--------------------------------------------------------------

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "F:\\Business Analytics\\Case studies\\Proactive-Attrition-Management-Logistic Regression\\eigen.csv")  ### EXPORTING EIGEN VALUE SUMMARY
# 10 factors seem appropriate with an eigen value of 0.98 and variance explained-71% approx 

FA<-fa(r=corrm, 10, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(cont_data),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings, "F:\\Business Analytics\\Case studies\\Proactive-Attrition-Management-Logistic Regression\\loadings.csv") ### SAVING THE FILE



#--------------------------------------------------------------
#----- Performing Chi- Square for categorical variables  ------
#--------------------------------------------------------------

library(MASS)
names(cat_data)
tbl=table(cat_data$CHURN,cat_data$MCYCLE)
chisq.test(tbl)


#Dropping TRUCK, CREDITGY, CREDITZ, RV, OCCCLER, OCCSTUD, MAILFLAG, PCOWN, MCYCLE
mydata$TRUCK<-NULL
mydata$CREDITGY<-NULL
mydata$CREDITZ<-NULL
mydata$RV<-NULL
mydata$OCCCLER<-NULL
mydata$OCCSTUD<-NULL
mydata$MAILFLAG<-NULL
mydata$PCOWN<-NULL
mydata$MCYCLE<-NULL

#------------------------------------------------------------------
#Checking the distributions and tranforming certain independent variables to make them normal
#------------------------------------------------------------------

require(car)
library(caret)
install.packages("e1071")
library(e1071)


?preprocess
summary(mydata)
boxcoxtrans <- preProcess(mydata, method = "YeoJohnson")
boxcoxtrans

mydata<- predict(boxcoxtrans, mydata)
summary(mydata)





#Splitting data into Training, Validaton and Testing Dataset


training<-mydata[mydata$CALIBRAT==1,]
testing<-mydata[mydata$CALIBRAT==0,]
training$CALIBRAT<-NULL
testing$CALIBRAT<-NULL
training$CALLFWDV<-NULL
testing$CALLFWDV<-NULL

#Building Models for training dataset

names(training)
fit<-glm(CHURN~ .,data = training,
         family = binomial(logit))


#Output of Logistic Regression
summary(fit)
write.csv(coeff, "sum.csv")
ls(fit)
fit$model

options(max.print=999999)

coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

#Checking for concordance 
source("F:\\Business Analytics\\Linear and Logistic\\Linear and Logistic in R")
Concordance(fit)  #NOTE: To run these command, first run concordance function in Concordance.R 


#Stepwise regression
step1=step(fit)

 #Final Model
fit2<-glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
            CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + 
            MOUREC + OUTCALLS + INCALLS + PEAKVCE + CALLWAIT + 
            MONTHS + UNIQSUBS +  EQPDAYS + AGE1 + 
            CHILDREN + CREDITAA + CREDITB + CREDITC + CREDITDE + PRIZMUB + 
            REFURB + WEBCAP + OCCHMKR + MARRYUN + MAILRES + CREDITCD + 
            NEWCELLY + INCOME + CREDITAD + SETPRC,data = training,
          family = binomial(logit))
summary(fit2)
source(choose.files())
Concordance(fit2)

################################ VALIDATION ##############################
#Decile Scoring for 
##Training dataset
train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)

#Decile Analysis Reports

fit_test_DA21<- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                      , max(Prob) as max_prob 
                      , sum(CHURN) as default_cnt
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_test_DA1,"fit_test_DA2.csv",row.names = F)



##Testing dataset
test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)

fit_test_DA2 <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob 
                     , sum(CHURN) as default_cnt
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA2,"fit_test_DA2.csv",row.names = F)


