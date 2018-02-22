#--------------------------------------------------------------------
#------------ Cleaning the environment ----------------
#--------------------------------------------------------------------
rm(list=ls())

#---------------------------------------------------------------------------
#----------- Setting the working directory by browsing directory -----------
#---------------------------------------------------------------------------

dir<-choose.dir()
setwd(dir)
getwd()

#---------------------------------------------------------------
#----------- Importing files by browsing files -----------------
#---------------------------------------------------------------

file<-choose.files()
cc<-read.csv(file, header=TRUE)
cc$CUST_ID<-NULL

#-------------------------------------------------------------------------------
#----------- Checking the structure and column names of the dataset ------------
#-------------------------------------------------------------------------------

str(cc, list.len = 150)
names(cc)

#--------------------------------------------------------------------------
#------------------ Treating missing values -------------------------------
#--------------------------------------------------------------------------

sapply(cc, function(x) sum(is.na(x))) # Calculating missing values-313 missing values in Minimum_payments and 1 missing value in credit_limit
sapply(cc, function(x) mean(x,na.rm=TRUE)) # Calculating the mean of all the variables.

?mean()

cc$MINIMUM_PAYMENTS[is.na(cc$MINIMUM_PAYMENTS)]<-864.2065423 
cc$CREDIT_LIMIT[is.na(cc$CREDIT_LIMIT)]<-4494.4494504

#---------------------------------------------------------------------------
#------------------------ Creating new KYIs --------------------------------
#---------------------------------------------------------------------------


cc$monthly_avg_pur<-cc$PURCHASES/12
cc$monthly_cash_adv<-cc$CASH_ADVANCE/12
cc$avgamtperpur<-(cc$PURCHASES/cc$PURCHASES_FREQUENCY)*12
cc$avgamtpercashadv<-(cc$CASH_ADVANCE/cc$CASH_ADVANCE_FREQUENCY)*12
cc$limitusage<-cc$BALANCE/cc$CREDIT_LIMIT
cc$paymenttominpayment<-cc$PAYMENTS/cc$MINIMUM_PAYMENTS   
cc$purtype[cc$ONEOFF_PURCHASES==0 & cc$INSTALLMENTS_PURCHASES==0]<-"None"
cc$purtype[cc$ONEOFF_PURCHASES>0 & cc$INSTALLMENTS_PURCHASES==0]<-"One-off"
cc$purtype[cc$ONEOFF_PURCHASES==0 & cc$INSTALLMENTS_PURCHASES>0]<-"Installment Purchases"
cc$purtype[cc$ONEOFF_PURCHASES>0 & cc$INSTALLMENTS_PURCHASES>0]<-"Both"

#------------------------------------------------------------------------------
#---------------- Treating missing values in derived variables ----------------
#----------------------------------------------------------------------------

sapply(cc, function(x) sum(is.na(x))) # Calculating missing values-2043 missing in avgamtperpur and 4628 in avgamtpercashadv
                                      
sapply(cc, function(x) mean(x,na.rm=TRUE)) # Calculating the mean of all the variables.

cc$avgamtpercashadv[is.na(cc$avgamtpercashadv)]<-9.594949e+04
cc$avgamtperpur[is.na(cc$avgamtperpur)]<-2.561651e+04        # It makes more sense to drop these variables as imputation with mean causes a lot of 
                                                            # change in the orginal nature of the data,hence altering the distribution.



#----------------------------------------------------------------------------
#--------------------------- Factor Analysis --------------------------------
#----------------------------------------------------------------------------
install.packages("corrplot")
require(corrplot)

cc<-cc[,1:23] #removing pur_type variable as it is a character variable.
corrplot(cor(cc, method = "pearson", use = "complete.obs"),method="circle",cl.cex=0.8,tl.cex=0.5)


cc$monthly_avg_pur<-NULL  # Very high correlation with "purchases" variable.
cc$monthly_cash_adv<-NULL  # Very high correlation with "cash advance" variable.

corrm<-cor(cc)
install.packages('psych')
require(psych)
install.packages('GPArotation')
require(GPArotation)

#----------------------------------------------------------------------------
# SCREE PLOT-7 factors look appropriate in this case-with eigenvalue-0.991979273
#----------------------------------------------------------------------------


scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 

eigen(corrm)$values 


require(dplyr)

#----------------------------------------------------------------------------
#-------------- CALCULATING VARIANCE, CUMULATIVE VARIANCE -------------------
#----------------------------------------------------------------------------

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 

getwd()
write.csv(eigen_values,"F:/Business Analytics/Case studies/Segmentation Case Study/eigenvalues.csv")

?fa
FA<-fa(r=corrm, nfactors=7, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(cc),])


write.csv(Loadings,"F:/Business Analytics/Case studies/Segmentation Case Study/loadings.csv")


#----------------------------------------------------------------------------------
#------------ Treating outliers by imputation-mean + 3STD -------------------------
#----------------------------------------------------------------------------------

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}


vars<-c("ONEOFF_PURCHASES","PURCHASES","PAYMENTS","PURCHASES_INSTALLMENTS_FREQUENCY","PURCHASES_FREQUENCY",
        "CASH_ADVANCE_FREQUENCY","BALANCE","CREDIT_LIMIT","PRC_FULL_PAYMENT","CASH_ADVANCE","ONEOFF_PURCHASES_FREQUENCY",
        "INSTALLMENTS_PURCHASES")

diag_stats<-t(data.frame(apply(cc[vars], 2, mystats)))
write.csv(diag_stats,"F:/Business Analytics/Case studies/Segmentation Case Study/diag_stats.csv")

getwd()

#----------------------------------------------------------------------------------
#------------ Treating outliers by imputation-mean + 3STD -------------------------
#----------------------------------------------------------------------------------

ccc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES>5572.10112326315]<-5572.10112326315
cc$PURCHASES[cc$PURCHASES>7413.10917913822]<-7413.10917913822
cc$PAYMENTS[cc$PAYMENTS>10418.3351227385]<-10418.3351227385
cc$CASH_ADVANCE_FREQUENCY[cc$CASH_ADVANCE_FREQUENCY>0.735508364777687]<-0.735508364777687
cc$BALANCE[cc$BALANCE>7809.07046604775]<-7809.07046604775
cc$CREDIT_LIMIT[cc$CREDIT_LIMIT>15410.286684288]<-15410.286684288
cc$CASH_ADVANCE[cc$CASH_ADVANCE>7270.36274239517]<-7270.36274239517
cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES>3124.08199021888]<-3124.08199021888



inputdata_final <-cc[vars]


#----------------------------------------------------------------------------------
#------------------- Standarizing and creating clusters ---------------------------
#----------------------------------------------------------------------------------

inputdata_final<-scale(inputdata_final)

cluster_three<-kmeans(inputdata_final,3)
cluster_four<-kmeans(inputdata_final,4)
cluster_five<-kmeans(inputdata_final,5)
cluster_six<-kmeans(inputdata_final,6)

cluster_three$cluster


cc_new<-cbind(cc,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )

#---------------------------------------------------------------------------
#--------------------- Converting into factors -----------------------------
#---------------------------------------------------------------------------

cc_new$km_clust_3<-factor(cc_new$km_clust_3)
cc_new$km_clust_4<-factor(cc_new$km_clust_4)
cc_new$km_clust_5<-factor(cc_new$km_clust_5)
cc_new$km_clust_6<-factor(cc_new$km_clust_6)

install.packages("tables")
require(tables)

profile<-tabular(1+ONEOFF_PURCHASES+PURCHASES+PAYMENTS+ONEOFF_PURCHASES_FREQUENCY+CASH_ADVANCE+CASH_ADVANCE_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+PURCHASES_FREQUENCY+INSTALLMENTS_PURCHASES+BALANCE+PRC_FULL_PAYMENT+CREDIT_LIMIT 
                 ~mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),data=cc_new)


profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)


profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=cc_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)



write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)
