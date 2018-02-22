#--------------------------------------------------------------
#------------- Linear Regression Case Study -------------------
#--------------------------------------------------------------

rm(list=ls())

#----------------------------------------------------
#--------------- Setting the directory --------------
#----------------------------------------------------

setwd("F:\\Business Analytics\\Case studies\\Linear Regression Case Study\\Linear Reg in R")


#----------------------------------------------------
#-------------- Importing the data ------------------
#----------------------------------------------------


require(readxl)
mydata <-read_xlsx("Linear Regression Case.xlsx")


#----------------------------------------------------
#---------- Dealing with missing Values -------------
#----------------------------------------------------


numMissing <- apply(mydata, 2, function(x){sum(is.na(x))})
numMissing          # Number of Missing values



mydata$lninc[mydata$income==0]<-0
mydata$lncreddebt[mydata$creddebt==0]<-0
mydata$lnothdebt[mydata$othdebt==0]<-0
mydata$lnlongmon[mydata$longmon==0]<-0
mydata$lnlongten[mydata$longten==0]<-0
mydata$lntollmon[mydata$tollmon==0]<-0
mydata$lntollten[mydata$tollten==0]<-0
mydata$lnequipmon[mydata$equipmon==0]<-0
mydata$lnequipten[mydata$equipten==0]<-0
mydata$lncardmon[mydata$cardmon==0]<-0
mydata$lncardten[mydata$cardten==0]<-0
mydata$lnwiremon[mydata$wiremon==0]<-0
mydata$lnwireten[mydata$wireten==0]<-0

#--------------------------------------------------------------
#-------- Removing observations with missing values -----------
#--------------------------------------------------------------

mydata <- mydata[complete.cases(mydata),]


#--------------------------------------------------------------
#-------------- Defining dependent variable   -----------------
#--------------------------------------------------------------
mydata$totalspent<-mydata$cardspent+ mydata$card2spent
mydata$cardspent<-NULL
mydata$card2spent<-NULL


#--------------------------------------------------------------
#------ Separate out numeric and non-numeric Variables --------
#--------------------------------------------------------------

var_num <- sapply(mydata,is.numeric)
temp_num_data <- mydata[,var_num]
temp_cat_data <- mydata[,!var_num]


names(temp_cat_data)


#--------------------------------------------------------------
#------------- Removing usless variables further --------------
#--------------------------------------------------------------

mydata$custid<-NULL
mydata$birthmonth<-NULL
mydata$carditems<-NULL
mydata$card2items<-NULL

summary(mydata)
class(mydata)
names(mydata)
str(mydata,list.len=ncol(mydata))


#--------------------------------------------------------------
#------ Declaring categorical variables as factors ------------
#--------------------------------------------------------------


mydata$region<-factor(mydata$region)
mydata$townsize<-factor(mydata$townsize)
mydata$gender<-factor(mydata$gender)
mydata$agecat<-factor(mydata$agecat)
mydata$edcat<-factor(mydata$edcat)
mydata$jobcat<-factor(mydata$jobcat)
mydata$union<-factor(mydata$union)
mydata$empcat<-factor(mydata$empcat)
mydata$retire<-factor(mydata$retire)
mydata$inccat<-factor(mydata$inccat)
mydata$default<-factor(mydata$default)
mydata$jobsat<-factor(mydata$jobsat)
mydata$marital<-factor(mydata$marital)
mydata$spousedcat<-factor(mydata$spousedcat)
mydata$homeown<-factor(mydata$homeown)
mydata$hometype<-factor(mydata$hometype)
mydata$addresscat<-factor(mydata$addresscat)
mydata$carown<-factor(mydata$carown)
mydata$cartype<-factor(mydata$cartype)
mydata$carcatvalue<-factor(mydata$carcatvalue)
mydata$carbought<-factor(mydata$carbought)
mydata$carbuy<-factor(mydata$carbuy)
mydata$commutecat<-factor(mydata$commutecat)
mydata$commutebike<-factor(mydata$commutebike)
mydata$commutebus<-factor(mydata$commutebus)
mydata$commutecar<-factor(mydata$commutecar)
mydata$commutecarpool<-factor(mydata$commutecarpool)
mydata$commutemotorcycle<-factor(mydata$commutemotorcycle)
mydata$commutenonmotor<-factor(mydata$commutenonmotor)
mydata$commutepublic<-factor(mydata$commutepublic)
mydata$commuterail<-factor(mydata$commuterail)
mydata$commutewalk<-factor(mydata$commutewalk)
mydata$telecommute<-factor(mydata$telecommute)
mydata$reason<-factor(mydata$reason)
mydata$polview<-factor(mydata$polview)
mydata$polparty<-factor(mydata$polparty)
mydata$polcontrib<-factor(mydata$polcontrib)
mydata$vote<-factor(mydata$vote)
mydata$card<-factor(mydata$card)
mydata$cardtype<-factor(mydata$cardtype)
mydata$cardbenefit<-factor(mydata$cardbenefit)
mydata$cardfee<-factor(mydata$cardfee)
mydata$cardtenurecat<-factor(mydata$cardtenurecat)
mydata$card2<-factor(mydata$card2)
mydata$card2type<-factor(mydata$card2type)
mydata$card2benefit<-factor(mydata$card2benefit)
mydata$card2fee<-factor(mydata$card2fee)
mydata$card2tenurecat<-factor(mydata$card2tenurecat)
mydata$active<-factor(mydata$active)
mydata$churn<-factor(mydata$churn)
mydata$tollfree<-factor(mydata$tollfree)
mydata$equip<-factor(mydata$equip)
mydata$callcard<-factor(mydata$callcard)
mydata$wireless<-factor(mydata$wireless)
mydata$multline<-factor(mydata$multline)
mydata$voice<-factor(mydata$voice)
mydata$pager<-factor(mydata$pager)
mydata$internet<-factor(mydata$internet)
mydata$callid<-factor(mydata$callid)
mydata$callwait<-factor(mydata$callwait)
mydata$forward<-factor(mydata$forward)
mydata$confer<-factor(mydata$confer)
mydata$ebill<-factor(mydata$ebill)
mydata$owncd<-factor(mydata$owncd)
mydata$owndvd<-factor(mydata$owndvd)
mydata$owntv<-factor(mydata$owntv)
mydata$ownvcr<-factor(mydata$ownvcr)
mydata$ownpda<-factor(mydata$ownpda)
mydata$ownpc<-factor(mydata$ownpc)
mydata$ownipod<-factor(mydata$ownipod)
mydata$owngame<-factor(mydata$owngame)
mydata$ownfax<-factor(mydata$ownfax)
mydata$news<-factor(mydata$news)
mydata$response_01<-factor(mydata$response_01)
mydata$response_02<-factor(mydata$response_02)
mydata$response_03<-factor(mydata$response_03)
mydata$vote<-factor(mydata$vote)
mydata$bfast<-factor(mydata$bfast)


#--------------------------------------------------------------
#----- Seperating continuous and categorical variables --------
#--------------------------------------------------------------

var_num<-sapply(mydata,is.factor)
cat_data<-mydata[,var_num]
cont_data<-mydata[,!var_num]
names(cont_data)


#--------------------------------------------------------------
#-------- Function to calculate descriptive statistics --------
#--------------------------------------------------------------

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  c <- class(x)
  a <- x[!is.na(x)]
  m <- mean(a,na.rm = T)
  med=median(a,na.rm = T)
  n <- length(a)
  s <- sd(a,na.rm = T)
  min <- min(a,na.rm = T)
  q1<-quantile(a,0.25,na.rm = T)
  q2<-quantile(a,0.5,na.rm = T)
  q3<-quantile(a,0.75,na.rm = T)
  p99<-quantile(a,0.99,na.rm = T)
  max <- max(a,na.rm = T)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>1.5*(p99)
  return(c(class=c,n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,median=med, stdev=s,min = min,
           q1=q1,q2=q2,q3=q3,p99=p99,max=max, UC=UC, LC=LC ))
}
diag_stats <- t(data.frame(apply(cont_data,2,mystats))) 
write.csv(diag_stats,"mystats.csv")


#--------------------------------------------------------------
#------------------ Treating outliers -------------------------
#--------------------------------------------------------------
mydata$age[mydata$age>100.336613070311]<-100.336613070311
mydata$ed[mydata$ed>24.3862482904467]<-24.3862482904467
mydata$employ[mydata$employ>38.8031860367318]<-38.8031860367318
mydata$income[mydata$income>220.892133461375]<-220.892133461375
mydata$lninc[mydata$lninc>5.94112502191954]<-5.94112502191954
mydata$debtinc[mydata$debtinc>29.1535098654681]<-29.1535098654681
mydata$creddebt[mydata$creddebt>12.1045215564116]<-12.1045215564116
mydata$lncreddebt[mydata$lncreddebt>3.68836973278901]<-3.68836973278901
mydata$othdebt[mydata$othdebt>19.8399748432958]<-19.8399748432958
mydata$lnothdebt[mydata$lnothdebt>4.08230105863119]<-4.08230105863119
mydata$spoused[mydata$spoused>29.3433535268412]<-29.3433535268412
mydata$reside[mydata$reside>6.38593181564958]<-6.38593181564958
mydata$pets[mydata$pets>13.3108906638409]<-13.3108906638409
mydata$pets_cats[mydata$pets_cats>3.08275014404579]<-3.08275014404579
mydata$pets_dogs[mydata$pets_dogs>2.78065059576704]<-2.78065059576704
mydata$pets_birds[mydata$pets_birds>1.59308210660473]<-1.59308210660473
mydata$pets_reptiles[mydata$pets_reptiles>1.03292737286886]<-1.03292737286886
mydata$pets_small[mydata$pets_small>1.82099500608252]<-1.82099500608252
mydata$pets_saltfish[mydata$pets_saltfish>1.45523508779318]<-1.45523508779318
mydata$pets_freshfish[mydata$pets_freshfish>11.0718044299574]<-11.0718044299574
mydata$address[mydata$address>53.5948525120995]<-53.5948525120995
mydata$cars[mydata$cars>6.05301887538376]<-6.05301887538376
mydata$carvalue[mydata$carvalue>86.9274901043956]<-86.9274901043956
mydata$commute[mydata$commute>11.2267539572795]<-11.2267539572795
mydata$commutetime[mydata$commutetime>42.9829843912076]<-42.9829843912076
mydata$cardtenure[mydata$cardtenure>52.7186734768693]<-52.7186734768693
mydata$card2tenure[mydata$card2tenure>40.442608714695]<-40.442608714695
mydata$tenure[mydata$tenure>106.190464097253]<-106.190464097253
mydata$longmon[mydata$longmon>51.7915934790731]<-51.7915934790731
mydata$lnlongmon[mydata$lnlongmon>4.61431285621956]<-4.61431285621956
mydata$longten[mydata$longten>3646.7449698967]<-3646.7449698967
mydata$lnlongten[mydata$lnlongten>10.5592230154011]<-10.5592230154011
mydata$tollmon[mydata$tollmon>62.1945035285336]<-62.1945035285336
mydata$lntollmon[mydata$lntollmon>6.47358854627037]<-6.47358854627037
mydata$tollten[mydata$tollten>3425.28726891825]<-3425.28726891825
mydata$lntollten[mydata$lntollten>13.3168388895808]<-13.3168388895808
mydata$equipmon[mydata$equipmon>70.6301378051285]<-70.6301378051285
mydata$lnequipmon[mydata$lnequipmon>6.37076546092952]<-6.37076546092952
mydata$equipten[mydata$equipten>3206.83827243166]<-3206.83827243166
mydata$lnequipten[mydata$lnequipten>12.1217385850545]<-12.1217385850545
mydata$cardmon[mydata$cardmon>60.4665572812341]<-60.4665572812341
mydata$lncardmon[mydata$lncardmon>6.27293278338123]<-6.27293278338123
mydata$cardten[mydata$cardten>3487.1549711952]<-3487.1549711952
mydata$lncardten[mydata$lncardten>13.790810069111]<-13.790810069111
mydata$wiremon[mydata$wiremon>70.1006996032225]<-70.1006996032225
mydata$lnwiremon[mydata$lnwiremon>5.80239518959667]<-5.80239518959667
mydata$wireten[mydata$wireten>3424.99447023256]<-3424.99447023256
mydata$lnwireten[mydata$lnwireten>11.1032681715486]<-11.1032681715486
mydata$hourstv[mydata$hourstv>35.1418256530511]<-35.1418256530511
mydata$totalspent[mydata$totalspent>1552.66644022596]<-1552.66644022596
#--------------------------------------------------------------
#----------------- Corelation matrix --------------------------
#--------------------------------------------------------------

?cor
corrm <- cor(cont_data)
write.csv(corrm,"correlation_matrix.csv")
names(cat_data)
names(cont_data)


#----------------------------------------------------------------------
# Checking correlation of different variables with dependent variables 
#----------------------------------------------------------------------


# Dropping  spoused, longten,longmon,income,othdebt,creddebt,tollmon,tollten,equipmon,equipten,cardmon,cardten,wiremon,wireten,hourtv

names(cont_data)
cont_data<-subset(cont_data,select=-c(spoused, longten,longmon,income,othdebt,creddebt,
                                      tollmon,tollten,equipmon,equipten,cardmon,cardten,wiremon,wireten,hourstv))



#------------------------------------------------------------------
#---------- Transforming the independent variables ----------------
#------------------------------------------------------------------

cont_data$sqrt_cardtenure<-sqrt(cont_data$cardtenure)
cont_data$sqrt_card2tenure<-sqrt(cont_data$card2tenure)
cont_data$sqrt_tenure<-sqrt(cont_data$tenure)

cont_data$cardtenure<-NULL
cont_data$card2tenure<-NULL
cont_data$tenure<-NULL
names(cont_data)
#------------------------------------------------------------------
#------ Checking the distribution of my dependent variable --------
#------------------------------------------------------------------

hist(mydata$totalspent)
summary(mydata$totalspent)


#------------------------------------------------------------------
#----- Transforming the dependent variable to make it normal ------
#------------------------------------------------------------------

mydata$ln_totalspent <- log(mydata$totalspent)
hist(mydata$ln_totalspent)

numMissing <- apply(mydata, 2, function(x){sum(is.na(x))})
numMissing          # No missing value after transformation


cont_data$ln_totalspent<-log(cont_data$totalspent)

corrm<-cor(cont_data)
write.csv(corrm,"correlation_matrix.csv")


#------------------------------------------------------------------
#--------- Performing Anova fr categorical variables --------------
#------------------------------------------------------------------

ln_totalspent<-mydata$ln_totalspent
cat_data<-cbind(cat_data,ln_totalspent)


fit <- aov(ln_totalspent~.,data = cat_data)
summary(fit)    


# Dropping response_01, owngame, ownpda confer, callwait, callid, multline, tollfree, equip,
# card2type, cardtype, cardbenefit, vote, polparty, polcontrib,  commutewalk, 
# commutecat,cartype, addresscat,spousedcat, inccat, agecat,townsize, telecommute, commutebike

cat_data<-subset(cat_data, select=-c(response_01, owngame, ownpda, confer, callwait, callid, multline, tollfree, equip,
                                     card2type, cardtype, cardbenefit, vote, polparty, polcontrib,polview,union, commutewalk, 
                                     commutecat,cartype, addresscat,spousedcat, inccat, agecat,townsize, ln_totalspent,
                                     empcat,edcat,telecommute, commutebike))



names(cat_data)


mydata2<- cbind(cont_data,cat_data) # with transformations

names(mydata2)


#---------------------------------------------------------------
# Splitting data into Training, Validaton and Testing Dataset  
#---------------------------------------------------------------

set.seed(6552)
train_ind <- sample(1:nrow(mydata2), size = floor(0.70 * nrow(mydata2)))


training<-mydata2[train_ind,] 
testing<-mydata2[-train_ind,] 



#---------------------------------------------------------------
#----------------- Performing linear reg -----------------------
#---------------------------------------------------------------

mydata2$pets_freshfish<-NULL
mydata2$pets_birds<-NULL
mydata2$pets_saltfish<-NULL
mydata2$pets_reptiles<-NULL
mydata2$pets_cats<-NULL
mydata2$pets_dogs<-NULL
fit1<-lm(ln_totalspent~.-totalspent, data=mydata2)
summary(fit1)


training$pets_freshfish<-NULL
training$pets_birds<-NULL
training$pets_saltfish<-NULL
training$pets_reptiles<-NULL
training$pets_cats<-NULL
training$pets_dogs<-NULL


names(training)
fit2<-lm(ln_totalspent~.-totalspent, data=training)
summary(fit2)



#---------------------------------------------------------------
#---------- Performing stepwise linear regression --------------
#---------------------------------------------------------------

install.packages("MASS")
require(MASS)

step6<- stepAIC(fit2,direction="both") # on training sample
step7<- stepAIC(fit1,direction="both") # on entire dataset
?stepAIC()
ls(step6)
step6$anova



fit4<-lm(ln_totalspent ~ ed + lninc + carvalue + lnlongten + lntollten + 
           lncardmon + region + gender + jobcat + default + carown + 
           carcatvalue + carbuy + reason + card + cardtenurecat + card2 + 
           churn + callcard + pager + ownpc + response_02 + response_03,data=training) 


fit5<-lm(ln_totalspent ~ age + ed + lninc + carvalue + lncardmon + sqrt_tenure + 
           gender + default + reason + card + cardtenurecat + card2 + 
           card2benefit + voice + internet + forward + owntv + ownvcr + 
           response_03,data=mydata2) 

names(training)

summary(fit4)
summary(fit5)




w<- abs(rstudent(fit4)) > 3 | abs(cooks.distance(fit4)) > 4/nrow(fit4$model)
InfObs <- which(w)
InfObs
trainingupdated <- training[-InfObs,]

fit6<-lm(ln_totalspent ~ ed + lninc + carvalue + lnlongten + lntollten + 
           lncardmon + region + gender + jobcat + default + carown + 
           carcatvalue + carbuy + reason + card + cardtenurecat + card2 + 
           churn + callcard + pager + ownpc + response_02 + response_03,data=trainingupdated) 
# Refining
fit6<-lm(ln_totalspent ~ ed + lninc + carvalue + lnlongten + lntollten + 
           lncardmon + region + gender + jobcat + default + 
           carbuy + reason + card + cardtenurecat + card2 + 
           churn + callcard + pager + ownpc +  response_03,data=trainingupdated) 



summary(fit6) 



 #---------------------------------------------------------------
#---------- Predicting the total spending  ---------------------
#---------------------------------------------------------------


t1<-cbind(training, pred_spend = exp(predict(fit4)))
names(t1)
t1<- transform(t1, APE = abs(pred_spend - totalspent)/totalspent)
mean(t1$APE)
View(t1)

t2<-cbind(testing, pred_spend=exp(predict(fit4,testing)))
t2<- transform(t2, APE = abs(pred_spend - totalspent)/totalspent)
mean(t2$APE)
View(t2)



#---------------------------------------------------------------
#---------- Decile Analysis for training and testing  ----------
#---------------------------------------------------------------


# finding the decile locations 
decLocations <- quantile(t2$pred_spend, probs = seq(0.1,0.9,by=0.1))

# using findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_spend,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_spend) as avg_pre_spend,   
               avg(totalspent) as avg_Actual_spent
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"t2_DA.csv")


#------------------------------------------------------------------------------


decLocations1 <- quantile(t1$pred_spend, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_spend,c(-Inf,decLocations1, Inf))

t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_spend) as avg_pre_spend,   
               avg(totalspent) as avg_Actual_spent
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"t1_DA.csv")

#-------------------------------------------------------------------------
#----------------------- Regression diagnostics --------------------------
#-------------------------------------------------------------------------

influence(fit4)  #for unusual observations
coefficients(fit4) # model coefficients
confint(fit4, level=0.95) # CIs for model parameters 
fitted(fit4) # predicted values
residuals(fit4) # residuals
anova(fit2) # anova table 

#-------------------------------------------------------------------------
#--------------------- Creating diagnostic plots -------------------------
#-------------------------------------------------------------------------

layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(fit4)





