#Final
#Manan Shah

save(list=ls(all=T),file='final_shah398.RData')
wd<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)
load(paste0(wd, '/final_shah398.Rdata'))
rm(wd)


install.packages('caret')
install.packages('gam')
install.packages('rJava')
install.packages('bartMachine')
install.packages('ModelMetrics')
install.packages('stats')
install.packages('earth')
install.packages('rpart')
install.packages('e1071')
install.packages("VSURF")
library(ggplot2)
library(caret)
library(gam)
library(rJava)
library(bartMachine)
library(earth)
library(ModelMetrics)
library(stats)
library(corrplot)
library(Hmisc)
library(rpart)
library(e1071)
library(VSURF)

#Function----
crossValidate<-function(cvtype,folds,dataset,model,resp)
{
  df<-dataset
  l <- vector("list", 2)
  if (cvtype=="kfold")
  {
    df$knum<-sample(1:folds,nrow(df),replace = TRUE)
    rmse_kfold<-0
    for (i in 1:folds)
    {
      df.test<-df[df$knum==i,]
      df.train<-df[!df$knum==i,]
      pred<-predict(model,df.test)
      pred[is.na(pred)]<-mean(pred,na.rm = T)
      rmse_kfold<-cbind(rmse_kfold,rmse(df.test[,resp],pred))
    }
    l[[1]]<-rmse_kfold[,-1]
    l[[2]]<-mean(rmse_kfold[,-1])
    return (l)
  }
  else if (cvtype=="LOOCV"||cvtype=="loocv")
  {
    rmse_loocv<-0
    for (i in 1:nrow(df))
    {
      df.test<-df[i,]
      df.train<-df[-i,]
      pred<-predict(model,df.test)
      pred[is.na(pred)]<-mean(df.train[,resp])
      rmse_loocv<-cbind(rmse_loocv,rmse(df.test[,resp],pred))
    }
    l[[1]]<-rmse_loocv[,-1]
    l[[2]]<-mean(rmse_loocv[,-1])
    return(l)
  }
}

#Data Processing-------------------------------
data.df<-read.csv(file="recs2009_public.csv")
head(str(data.df))
length(data.df[which(data.df$REPORTABLE_DOMAIN==17),'REPORTABLE_DOMAIN'])
table(complete.cases(data.df))
data.df[which(data.df$REPORTABLE_DOMAIN!=17),'REPORTABLE_DOMAIN']<-NA
#Now, we only keep the survey responses in FL State in the final dataset. Now, all 948 observations are of Florida state.
final.df<-na.omit(data.df)
#final.df[which(final.df$CONDCOOP==-2),]<-0

#The total number of households in FL state is given as:
sum(final.df$NWEIGHT)
sum(data.df$NWEIGHT)
perc.homes.FL<-(sum(final.df$NWEIGHT)/sum(data.df$NWEIGHT))*100
perc.homes.FL
#So, we have 6.15% of households of the 113.6 million households in FL



#We build a dataset including only relevant variables
df<-final.df[,c(5:8,14,23:26,35,36,41,44,48,51,54,61,62,432,433,442,461,462,504,
                505,506,508,509,510,514,516,517,530,533,536,538,542,557,589,609,610,830,832,
                841:843,852)]

#Our response variable is:
resp<-df$KWHSPH+df$KWHCOL+df$KWHWTH
df<-df[,-c(44:46)]
df<-cbind(resp,df)

#DATA Cleaning--------------------------------
#NUMFLRS
#No of floors cannot be negative. So, for Not Applicable we code 0.
df[which(df$NUMFLRS==-2),'NUMFLRS']<-0

#NUMAPTS
#No of apartments cannot be negative. So, for Not Applicable we code 0.
df[which(df$NUMAPTS==-2),'NUMAPTS']<-0

#ROOFTYPE
#The roofing material cannot be -2. So, for Not Applicable we code 0.
df[which(df$ROOFTYPE==-2),'ROOFTYPE']<-0

#CELLAR
#If the house does not have a cellar, for this not applicable type we code again 0.This means the house does not contain a cellar.
df[which(df$CELLAR==-2),'CELLAR']<-0

#BASEHEAT
#If the basement heating is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$BASEHEAT==-2),'BASEHEAT']<-0

#BASECOOL
#If the basement cooling is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$BASECOOL==-2),'BASECOOL']<-0

#ATTIC
#If the house does not have a attic, for this not applicable type we code again 0.This means the house does not contain a attic.
df[which(df$ATTIC==-2),'ATTIC']<-0

#ATTCHEAT
#If the attic heating is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$ATTCHEAT==-2),'ATTCHEAT']<-0

#ATTCCOOL
#If the attic cooling is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$ATTCCOOL==-2),'ATTCCOOL']<-0

#GARGHEAT
#If the garage heating is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$GARGHEAT==-2),'GARGHEAT']<-0

#GARGCOOL
#If the garage cooling is not applicable, we can interpret it as not there. So, for Not Applicable we code 0.
df[which(df$GARGCOOL==-2),'GARGCOOL']<-0


#MAINTHT
#If there is no maintenance required, it means maintenance is not performed. So, for Not Applicable we code 0.
df[which(df$MAINTHT==-2),'MAINTHT']<-0


#EQUIPAGE
#If there is no space heating equipment, the age of the equipment can be taken as zero. So, for Not Applicable we code 0.
df[which(df$EQUIPAGE==-2),'EQUIPAGE']<-0
df[which(df$EQUIPAGE==41),'EQUIPAGE']<-4
df[which(df$EQUIPAGE==42),'EQUIPAGE']<-5
df[which(df$EQUIPAGE==5),'EQUIPAGE']<-6

#STEAMR
df[which(df$STEAMR==-2),'STEAMR']<-0


#EQMAMT
#Here, we can interpret not applicable as no portion of space heating is provided by main space heating equipment. So, for Not Applicable we code 0.
df[which(df$EQMAMT==-2),'EQMAMT']<-0


#HEATROOM
#Not applicable here can be interpreted as no rooms are heated. So, for Not Applicable we code 0.
df[which(df$HEATROOM==-2),'HEATROOM']<-0

#H2OTYPE1
#Not applicable here can be interpreted as no main water heater is needed.. So, for Not Applicable we code 0.
df[which(df$H2OTYPE1==-2),'H2OTYPE1']<-0

#WHEATOTH
df[which(df$WHEATOTH==-2),'WHEATOTH']<-0

#WHEATSIZ
df[which(df$WHEATSIZ==-2),'WHEATSIZ']<-0

#WHEATAGE
#Not applicable here can be interpreted as no main water heater used. So, for Not Applicable we code 0.
df[which(df$WHEATAGE==-2),'WHEATAGE']<-0
df[which(df$WHEATAGE==41),'WHEATAGE']<-4
df[which(df$WHEATAGE==42),'WHEATAGE']<-5
df[which(df$WHEATAGE==5),'WHEATAGE']<-6

#H2OTYPE2
#Not applicable here can be interpreted as no main water heater is needed.. So, for Not Applicable we code 0.
df[which(df$H2OTYPE2==-2),'H2OTYPE2']<-0

#WHEATSIZ2
df[which(df$WHEATSIZ2==-2),'WHEATSIZ2']<-0

#WHEATAGE2
#Not applicable here can be interpreted as no main water heater used. So, for Not Applicable we code 0.
df[which(df$WHEATAGE2==-2),'WHEATAGE2']<-0
df[which(df$WHEATAGE2==41),'WHEATAGE2']<-4
df[which(df$WHEATAGE2==42),'WHEATAGE2']<-5
df[which(df$WHEATAGE2==5),'WHEATAGE2']<-6

#COOLTYPE
df[which(df$COOLTYPE==-2),'COOLTYPE']<-0

#AGECENAC

df[which(df$AGECENAC==-2),'AGECENAC']<-0
df[which(df$AGECENAC==41),'AGECENAC']<-4
df[which(df$AGECENAC==42),'AGECENAC']<-5
df[which(df$AGECENAC==5),'AGECENAC']<-6

##ACOTHERS
df[which(df$ACOTHERS==-2),'ACOTHERS']<-0

#ACROOMS
#Not applicable can be interpretated as no rooms are cooled using AC. So, NA coded 0 here.
df[which(df$ACROOMS==-2),'ACROOMS']<-0

#USEWWAC
df[which(df$USEWWAC==-2),'USEWWAC']<-0

##HIGHCEIL
df[which(df$HIGHCEIL==-2),'HIGHCEIL']<-0

##DOOR1SUM
df[which(df$DOOR1SUM==-2),'DOOR1SUM']<-0

##WINDOWS


#EDA------------------------------------------


##Correlation Plot
df<-df[,-c(14)]
df<-lapply(df, as.numeric)
df<-df[,"resp"]
df<-data.frame(df)
str(df)
temp<-cor(df)
corrplot(temp, method="circle", tl.cex=0.5)

c1<-temp[which(cor(df)>=0.7 & cor(df) != 1)]
c1


##Density plot
d<-density(df$resp)
plot(d, xlim = c(-10,50000), main = 'Kernel Density of resp')
polygon(d, col="red", border="blue") 

##Histogram
x<-df$resp
h<-hist(x, col="red", xlab="resp",breaks = 200,xlim = c(0,40000),
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=80)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

##Violin plot
#Type of housing units
df[,1]<-as.factor(df[,1])
temp1<-df
levels(temp1$TYPEHUQ)<-c("Mobile Home",
                         "Single Family Detached",
                         "Single Family Attached",
                         "Apartment with 2-4 units",
                         "Apartment with 5+ units")
p<-ggplot(temp1, aes(TYPEHUQ,resp))
p+geom_violin(scale = "count",adjust = 0.8,aes(fill = TYPEHUQ), cex=0.1)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ylim(0,30000)

#UR
p<-ggplot(df, aes(UR,resp))
p+geom_violin(scale = "count",adjust = 0.8,aes(fill = UR))+ylim(0,30000)


#Models----------------------
set.seed(4)
folds <- 10
df$folds <- sample(seq(1:folds),size=nrow(df),replace=T)

error.df <- data.frame(glm=numeric(folds),gam=numeric(folds),tree=numeric(folds),
                       randomForest=numeric(folds),MARS=numeric(folds),bart=numeric(folds),svm=numeric(folds))
for(i in (1:folds)){
  # start a for loop
  set.seed(4)
  df.test<- df[which(df$folds==i),]
  df.train <-df[-which(df$folds==i),]

#GLM-----------------------------------

glm1<-glm(resp~.,data = df.train, family = gaussian)
summary(glm1)


glm2<-glm(resp ~ HDD65+TOTROOMS+CELLAR+GARGHEAT+GARGCOOL+WHEATSIZ2+ACROOMS+
            USEWWAC+WINDOWS+TOTCSQFT+DOLLAREL ,data = df.train)
summary(glm2)

glm2.pred.OS <- predict(glm2,df.test)
error.df$glm[i] <-rmse(actual = df.test$resp,predicted = glm2.pred.OS)

glm.diag=data.frame(glm2$residuals, glm2$fitted.values)
colnames(glm.diag)<-c('resid', 'pred')
par(mfrow=c(1,3))
plot(x=df.test$resp, y=glm2.pred.OS, xlab="Y", ylab="Y-hat", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=glm2$residuals, x=glm2$fitted.values, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat", xlim = c(0,30000))
qqnorm(glm2$residuals)
qqline(glm2$residuals)
  
#GAM-------------------------------------

resp<-"resp"
gam1<-gam(resp~.,data=df.train)
summary(gam1)
temp.gma.df<-df.train[,-c(1,5:7,12,17:19,21,23,24,27,29,33,38)]

form<-as.formula(paste0(resp,"~",paste0("s(",colnames(temp.gma.df[,c(1:7,17,21,23,24,26:30)]),
                                        ",d=4",")",collapse="+"),
                        "+",paste0(colnames(temp.gma.df[,c(8:16,18:20,22,25)]),collapse="+")
                                                    ,collapse=""))

gam2<-gam(formula = form, data = df.train)
summary(gam2)
rm(form)


temp.gma.df<-temp.gma.df[,c(1,3,15,19,20,22,28:30)]
form<-as.formula(paste0(resp,"~",paste0("s(",colnames(temp.gma.df[,c(1,2,7:9)]),
                                        ",d=4",")",collapse="+"),
                        "+",paste0(colnames(temp.gma.df[,c(3:6)]),collapse="+")
                        ,collapse=""))

gam3<-gam(formula = form, data = df.train)
summary(gam3)

temp.gma.df<-temp.gma.df[,-6]

form<-as.formula(paste0(resp,"~",paste0("s(",colnames(temp.gma.df[,c(1,2,6:8)]),
                                        ",d=4",")",collapse="+"),
                        "+",paste0(colnames(temp.gma.df[,c(3:5)]),collapse="+")
                        ,collapse=""))

gam4<-gam(formula = form, data = df.train)
summary(gam4)
rm(form)

# temp.gma.df<-temp.gma.df[,-c(8,9)]
# 
# form<-as.formula(paste0(resp,"~",paste0("s(",colnames(temp.gma.df[,c(1,2,8,9)]),
#                                         ",d=4",")",collapse="+"),
#                         "+",paste0(colnames(temp.gma.df[,c(3:7)]),collapse="+")
#                         ,collapse=""))
# gam.final<-gam(formula = form, data = df.train)
# summary(gam.final)
# rm(form)

form<-as.formula(paste0(resp,"~",paste0("s(",colnames(df.train[,c(2,4,43:45)]),
                                        ",d=4",")",collapse="+"),
                        "+",paste0(colnames(df.train[,c(26,32,34)]),collapse="+")
                        ,collapse=""))
gam.final<-gam(formula = form, data = df.train)
summary(gam.final)
par(mfrow=c(2,4))
plot(gam.final, se = T, col = "blue")

#Now, to decide the degree of freedom for the best gam model.
set.seed(4)
gam.obj<-gam(formula = form,data=df.train)
gam.step<-step.Gam(gam.obj,scope= list("TYPEHUQ"=~1+TYPEHUQ+s(TYPEHUQ,df=2)+s(TYPEHUQ,df=3)+s(TYPEHUQ,df=4)+s(TYPEHUQ,df=5),
                                       "HDD65"=~1+HDD65+s(HDD65,df=2)+s(HDD65,df=3)+s(HDD65,df=4)+s(HDD65,df=5),                                       
                                       "DOOR1SUM"=~1+DOOR1SUM+s(DOOR1SUM,df=2)+s(DOOR1SUM,df=3)+s(DOOR1SUM,df=4)+s(DOOR1SUM,df=5),
                                       "DOLLAREL"=~1+DOLLAREL+s(DOLLAREL,df=2)+s(DOLLAREL,df=3)+s(DOLLAREL,df=4)+s(DOLLAREL,df=5),
                                       "ROOFTYPE"=~1+ROOFTYPE,
                                       "TOTROOMS"=~1+TOTROOMS,
                                       "NUMH2OHTRS"=~1+NUMH2OHTRS,
                                       "WHEATSIZ2"=~1+WHEATSIZ2,
                                       "AIRCOND"=~1+AIRCOND,
                                       "TOTHSQFT"=~1+TOTHSQFT, "TOTCSQFT"=~1+TOTCSQFT), direction = "both",trace=2)

gam.bestmodel<-gam(resp ~ s(TYPEHUQ, d = 4) + s(HDD65, d = 4) + s(TOTCSQFT, d = 4) +s(TOTCSQFT, d = 4) 
                   + s(DOLLAREL, d = 4) + NUMH2OHTRS + WHEATSIZ2 + ACOTHERS, data = df.train)
gam.pred.OS <- predict(gam.bestmodel,df.test)
error.df$gam[i] <-rmse(actual = df.test$resp,predicted = gam.pred.OS)

gam.diag=data.frame(gam.bestmodel$residuals, gam.bestmodel$fitted.values)
colnames(gam.diag)<-c('resid', 'pred')
plot(y=df.test$resp, x=gam.pred.OS, ylab="Y", xlab="Y-hat", main="Y vs. Y-hat")# Y vs. Y-hat
plot(y=gam.bestmodel$residuals, x=gam.bestmodel$fitted.values, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat", xlim = c(0,30000))
qqnorm(gam.bestmodel$residuals)
qqline(gam.bestmodel$residuals)

#CART-----------------------------
set.seed(4)
#Unpruned CART
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[,-1]),collapse="+"))) 
tree.model1<-rpart(formula = form,data=df.train)
summary(tree.model1)
rpart.plot(tree.model1)  
tree.model1.cv<-crossValidate("kfold",10,df.train, tree.model1,"resp")
rm(form)
# tree.model1.predict.OS<-predict(tree.model1,df.test)
# error.df$tree[i] <-rmse(actual = df.test$resp,predicted = tree.model1.predict.OS)

#Pruned CART
plotcp(tree.model1,minline=T,lty=3,col=1,upper=c('size','splits','none'))
tree.model2<-prune(tree.model1, 0.27)
tree.model2.cv<-crossValidate("kfold",10,df.train, tree.model2,"resp")
# tree.model1.predict.OS<-predict(tree.model1,df.test)
# error.df$tree[i] <-rmse(actual = df.test$resp,predicted = tree.model1.predict.OS)

#Model Comparison
RMSEcompare.cart<-data.frame(tree.model1.cv[1],tree.model2.cv[1])
colnames(RMSEcompare.cart)<-c("CART_model1_rmse","CART_model2_rmse")
RMSEcompare.cart
par(mfrow=c(1,1))
boxplot(RMSEcompare.cart, col = c("blue","red"))
legend("bottomright",legend=c("CART_model1_rmse","CART_model2_rmse"),col=c("blue","red"),pch=c(19,19), cex = 0.6)

#The Unpruned model has quite lower out-of-sample rmse value. The variance in the rmse values is almost similar in both cases.
#So, we select the unpruned model.

tree.temp<-data.frame(tree.model1$variable.importance)
form<-as.formula(paste0(resp,"~",paste0(colnames(df.train[,c(2,4,5,7,8,10,11,24,26,38,41,43:45)]),collapse="+")
                        ,collapse=""))
tree.model1<-rpart(formula = form,data=df.train)
rm(form)
tree.model1.predict.OS<-predict(tree.model1, df.test)
error.df$tree[i] <-rmse(actual = df.test$resp,predicted = tree.model1.predict.OS)

tree.resid<-df.test$resp-tree.model1.predict.OS
rpart.diag=data.frame(tree.resid, tree.model1.predict.OS)
colnames(rpart.diag)<-c('resid', 'pred')
par(mfrow=c(1,3))
plot(x=df.test$resp, y=tree.model1.predict.OS, xlab="Y", ylab="Y-hat", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=tree.resid, x=tree.model1.predict.OS, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat")
qqnorm(tree.resid)
qqline(tree.resid) 

#Random Forest-----------------------
set.seed(4)
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[,-1]),collapse="+"))) 
rf.model<-randomForest(formula = form,data=df.train, importance = TRUE)
rm(form)

# #Using VSURF to do variable selection
# rf.model.vsurf<-VSURF(formula = form,data=df.train, parallel = TRUE, ncores = 20,mtry = 10)
# summary(rf.model.vsurf)  
# plot(rf.model.vsurf, step = "thres", imp.sd = FALSE, var.names = TRUE)


varImpPlot(rf.model,sort =TRUE, n.var=min(20, if(is.null(dim(rf.model$importance)))
  length(rf.model$importance) else nrow(rf.model$importance)))
#Building model using the important variables derived from %IncMSE graph
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[,c(2:5,7,8,10,11,21,24,34,35,37:41,43:45)])
                                                      ,collapse="+"))) 
rf.bestmodel<-randomForest(formula = form,data=df.train, importance = TRUE)
rm(form)

rf.bestmodel.predict.OS<-predict(rf.bestmodel, df.test)
error.df$randomForest[i] <-rmse(actual = df.test$resp,predicted = rf.bestmodel.predict.OS)

rf.resid<-df.train$resp-rf.bestmodel$predicted
resid.rf.OS<-df.test$resp-rf.bestmodel.predict.OS
par(mfrow=c(1,4))
plot(x=rf.bestmodel.predict.OS,y=df.test$resp,xlab="Y-hat",ylab="Y", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=df.train$resp, x=rf.bestmodel$predicted, xlab="Fitted Values", ylab="Y", main="Y vs. Fitted values")
plot(y=resid.rf.OS, x=rf.bestmodel.predict.OS, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat")
qqnorm(rf.resid)
qqline(rf.resid) 

#MARS-------------------------
#First we build an unpruned MARS model
set.seed(4)
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[-1]),collapse="+")))
mars.model1<-earth(formula=form,data=df.train,pmethod="none")
rm(form)
summary(mars.model1)
plotmo(mars.model1, bottom_margin = 1)

mars.model1.cv<-crossValidate("kfold",10,df.train,mars.model1,"resp")

#Now, we compare the model with a pruned MARS model
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[-1]),collapse="+")))
mars.model2<-earth(formula=form,data=df.train)
rm(form)
summary(mars.model2)
plotmo(mars.model2)
mars.model2.cv<-crossValidate("kfold",10,df.train,mars.model2,"resp")

#Comparing the 2 MARS models
RMSE<-data.frame(mars.model1.cv[1],mars.model2.cv[1])
colnames(RMSE)<-c("MARS_model1_rmse","MARS_model2_rmse")
RMSE
par(mfrow=c(1,1))
boxplot(RMSE, col = c("blue","red"))
legend("bottomright",legend=c("Model1_MARS","Model2_MARS"),col=c("blue","red"),pch=c(19,19), cex = 0.6)

#Since, the pruned MARS model has lower mean rmseOS value and also the standard deviation rmseOS values is lesser.
#Therefore, we select the pruned MARS model.

mars.finalmodel.predict.OS<-predict(mars.model2, df.test)
error.df$MARS[i] <-rmse(actual = df.test$resp,predicted = mars.finalmodel.predict.OS)

mars.resid<-mars.model2$residuals
mars.fitted<-mars.model2$fitted.values
par(mfrow=c(1,4))
plot(x=mars.finalmodel.predict.OS,y=df.test$resp,xlab="Y-hat",ylab="Y", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=df.train$resp, x=mars.fitted, xlab="Fitted Values", ylab="Y", main="Y vs. Fitted values")
plot(y=mars.resid, x=mars.fitted, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat",xlim = c(0,30000))
qqnorm(mars.resid)
qqline(mars.resid)   

  
#BART-------------------------
options(java.parameters="-Xmx5g")
library('bartMachine')
library('rJava')
set_bart_machine_num_cores(20)
  set.seed(4)
  df.train.covariates <- df.train[,-1]
  df.train.response <-df.train$resp
  bart.df<- bartMachine(X=df.train.covariates,y=df.train.response, serialize = TRUE)
  
  bart.pred.OS <- predict(bart.df,new_data = df.test[,-1])
  error.df$bart[i] <-rmse(actual=df.test$resp,predicted = bart.pred.OS)
  par(mfrow=c(1,1))
  investigate_var_importance(bart.df, num_replicates_for_avg = 20)
  plot_y_vs_yhat(bart.df, credible_intervals = TRUE)
# plot_y_vs_yhat(bart.df, prediction_intervals = TRUE)
  par(mfrow=c(2,2))
  pd_plot(bart.df,j='DOLLAREL')
  pd_plot(bart.df,j='TOTCSQFT')
  pd_plot(bart.df,j='TOTHSQFT')
  pd_plot(bart.df,j='HDD65')
  check_bart_error_assumptions(bart.df, hetero_plot = "yhats")
  # var_select_bartcv<-var_selection_by_permute_cv(bart.df, k_folds=10, 
  #                                                num_reps_for_avg=5,num_trees_for_permute=20, alpha=0.05, num_trees_pred_cv=50)
  # 
  # print(var_select_bartcv$best_method)
  # print(var_select_bartcv$important_vars_cv)
  # 
#SVM-----------------------------
set.seed(4)
form<-as.formula(paste0(names(df.train[1]),"~",paste0(names(df.train[-1]),collapse="+")))
svm.model<-svm(formula=form,data=df.train,cost = 1000, gamma = 0.0001)
rm(form)
summary(svm.model)
  
svm.model.predict.OS<-predict(svm.model, df.test)
error.df$svm[i] <-rmse(actual=df.test$resp,predicted = svm.model.predict.OS)


svm.resid<-svm.model$residuals
svm.fitted<-svm.model$fitted
plot(x=svm.model.predict.OS,y=df.test$resp,xlab="Y-hat",ylab="Y", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=df.train$resp, x=svm.fitted, xlab="Fitted Values", ylab="Y", main="Y vs. Fitted values")
plot(y=svm.resid, x=svm.fitted, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat",xlim = c(0,30000))
qqnorm(svm.resid)
qqline(svm.resid)
  
}

error.df<-data.frame(mean(error.df$glm[i]),mean(error.df$gam[i]),mean(error.df$tree[i]),
                     mean(error.df$randomForest[i]),mean(error.df$MARS[i]),mean(error.df$bart[i]),
                     mean(error.df$svm[i]))




finalmodel<-gam(resp ~ s(TYPEHUQ, d = 4) + s(HDD65, d = 4) + s(TOTCSQFT, d = 4) +s(TOTCSQFT, d = 4) 
                   + s(DOLLAREL, d = 4) + NUMH2OHTRS + WHEATSIZ2 + ACOTHERS, data = df.train)
















