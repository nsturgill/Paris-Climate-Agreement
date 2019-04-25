library(readxl)
library(dplyr)
library(data.table)
library(car)
library(emmeans)
library(MuMIn) 
library(colorspace)
library(grDevices)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(MASS)
library(mice)
library(RColorBrewer)
library(caTools)
library(scales)
options(digits=10)

#######################################
quant = read_excel(file.choose())               # quantified I/NDCs
econ = read_excel(file.choose())                # selected economic predictors
damage = read_excel(file.choose())
econ$Time = as.character(econ$Time)
econ = econ[econ$Time == "2015",]               # keep only year 2015

keep = quant$ISO
combo = econ[econ$'Country Code' %in% keep, ]
drops <- c("Time Code")                         # remove Time Code since no longer necessary
combo = combo[ , !(names(combo) %in% drops)]
# rename columns
colnam = c("Country", "CCode", "Time",          
           "GDP", "pgrowGDP", "GNI", "pgrowGNI", "totpop",
           "popgrow", "ManufValAdd", "medhighMVA",
           "Broadmoney", "Exchrate", "CPI", "Unemp", "TotGHG", "IntRate",
           "Literacy", "Deathrate", "BirthRate", "Nettrade",
           "MilitaryExp", "ConsumptExp")
colnames(combo) = colnam
id = c(4:23) 
combo[,id] = as.numeric(as.character(unlist(combo[,id])))

na_count = sapply(combo, function(y) sum(length(which(is.na(y)))))
na_count                                        # check NA's

drops <- c("TotGHG", "Literacy")                # remove TotGHG and Literacy since missing data > 80%
combo = combo[ , !(names(combo) %in% drops)]
combo$'CapMt' = quant$`Cap Mt`                  # add back in important data
combo$'Label' = quant$Label
combo$'CommitmentLevel' = quant$`Commitment level`

myOrder= c("Country", "CCode", "CapMt",         # re-order for sake of readability
           "Label", "CommitmentLevel", "Time",
           "GDP", "pgrowGDP", "GNI", "pgrowGNI", "totpop",
           "popgrow", "ManufValAdd", "medhighMVA",
           "Broadmoney", "Exchrate", "CPI", "Unemp", "IntRate", 
           "Deathrate", "BirthRate", "Nettrade",
           "MilitaryExp", "ConsumptExp")
setcolorder(combo, myOrder)

options(scipen=999)
id = c(3, 7:24)                                 # make numbers type "numeric"
combo[,id] = as.numeric(as.character(unlist(combo[,id])))
head(combo)                                     # check data looks correct

#########################################
# pull out predictor variables w/ 
preddat = subset(combo[,c(7:24)])  
preddat$damage = as.numeric(damage$`% Damage`)# missing data and impute  
tempData = mice(preddat,m=5,maxit=50,meth='cart',seed=500)

df = data.frame(preddat$GNI)                    # first time skips GNI, re-run on GNI
df$odd = 0
gnitemp <- mice(df,m=5,maxit=50,meth='cart',seed=500)

#########################################
completedData = complete(tempData,1)            # assign imputed data to data sets (no GNI yet)
completedData2 = complete(tempData,2)
completedData3 = complete(tempData,3)
completedData4 = complete(tempData,4)
completedData5 = complete(tempData,5)
# average data sets for higher accuracy
avgdat = data.frame(matrix(ncol = 19, nrow = 187))   
for(i in 1:nrow(completedData2)){
  for(j in 1:ncol(completedData3)){
    avgdat[i,j] = (completedData[i,j]+completedData2[i,j]+
                     completedData3[i,j]+completedData4[i,j]+completedData5[i,j])/5
  }
}

temp = complete(gnitemp, 1)                     # assign GNI imputation to data set
temp2 = complete(gnitemp, 2)                    # gni has high correlation with other var so MICE does not fully work
temp3 = complete(gnitemp, 3)
temp4 = complete(gnitemp, 4)
temp5 = complete(gnitemp, 5)
# average for higher accuracy
avggni = data.frame(matrix(ncol = 1, nrow = 187))    
for(i in 1:nrow(temp2)){
  for(j in 1:ncol(temp3)){
    avggni[i,j] = (temp[i,j]+temp2[i,j]+temp3[i,j]+temp4[i,j]+temp5[i,j])/5
  }
}
# add back into larger imputed set
# and assign column names
avgdat$X3 = avggni$matrix.ncol...1..nrow...187.      
colnames(avgdat) = colnames(preddat)

#################################################

regdat = data.frame(avgdat[,1:18])                     # move data to new data frame and convert CapMt
regdat$CapMt = combo$CapMt
regdat$`CapMt` = as.numeric(regdat$CapMt)/as.numeric(regdat$GDP)

summ = summary(regdat$`CapMt`)                  # to get cutoff values (1st and 3rd quartile)

regdat$comlevnum = 0                            # assign "High", "Medium", "Low" where low is 1st Q,
for(i in 1:nrow(combo)){                        # medium is middle 50%, and high is 4th Q
  if(is.na(regdat$`CapMt`[i])){
    combo$`CommitmentLevel`[i]= combo$`CommitmentLevel`[i]
  }else{
    if(regdat$`CapMt`[i] < (summ[2])){
      combo$`CommitmentLevel`[i] = "Low"
    }else{
      if(regdat$`CapMt`[i] > (summ[5])){
        combo$`CommitmentLevel`[i] = "High"
      }else{
        combo$`CommitmentLevel`[i] = "Medium"
      }
    }
  }
}

for(i in 1:nrow(combo)){                        # re-assign commitment level to numeric for analysis
  if(combo$`CommitmentLevel`[i] == "Low" ){
    regdat$comlevnum[i] = 0
  }else{
    if(combo$`CommitmentLevel`[i] == "Medium" ){
      regdat$comlevnum[i] = .5
    }else{
      regdat$comlevnum[i] = 1
    }
  }
  
}

#######################################

graphdat = regdat
graphdat$cat = ordered(combo$CommitmentLevel, levels = c("Low", "Medium", "High"))

quant$`Cap Mt`=as.numeric(quant$`Cap Mt`)
avgdat$GDP = as.numeric(avgdat$GDP)
sc = ggplot(avgdat, aes(x=quant$`Cap Mt`, y=avgdat$GDP, fill = graphdat$cat )) +
  geom_point(size=2, shape=22)+
  scale_fill_brewer()+
  labs(x= "Abatement Cap (MegaTonnes)", y="Gross Domestic Product (2010 USD)",
       fill= "Commitment Level")+
  ggtitle("Scatterplot of Abatement Cap by GDP")+
  theme(plot.title = element_text(family = "serif", 
                                  color="#666666", face="bold", size=20, hjust=0)) +
  theme(axis.title = element_text(family = "serif", 
                                  color="#666666", face="bold", size=14))+
  theme(legend.title = element_text(family = "serif", 
                                    color="#666666", face="bold", size=10))+
  scale_y_continuous(labels = scientific) #+geom_text(label=quant$Country) #(Used for labels if curious)
sc

p = ggplot(data=graphdat, aes(x=cat, fill = cat)) +
  geom_bar(stat="count")+
  labs(x= "Commitment Level", y="Quantity of Countries")+
  ggtitle("Quantity of Countries Per Commitment Level")+
  theme(plot.title = element_text(family = "serif", 
                                  color="#666666", face="bold", size=17, hjust=0)) +
  theme(axis.title = element_text(family = "serif", 
                                  color="#666666", face="bold", size=14))+
  scale_fill_brewer()

p 

#########################################
drops <- c("GDP", "CapMt")                      # remove GDP and CapMt (redundant)
regdat = regdat[ , !(names(regdat) %in% drops)]
fullmod = lm(comlevnum ~ ., data=regdat)        # full model

par(mfrow= c(2,2))                              # diagnostic plots (normality)
plot(fullmod)

summary(fullmod)                                # check coefficients

accvecfull = data.frame(matrix(ncol = 1, nrow = (.2*length(regdat))))
set.seed(101)                                   # set randomization
for(i in 1:100){                                # split 80/20 and test 100 times
  sample = sample.split(regdat$GNI, SplitRatio = .8)
  train = subset(regdat, sample == TRUE)
  test  = subset(regdat, sample == FALSE)
  modtest = lm(comlevnum ~ ., data = train)
  actual = test$comlevnum
  temp = predict.lm(modtest, test)
  for(j in 1:length(temp)){                     # recategorize results
    if(temp[j] > .75){
      temp[j] = 1
    }else{
      if(temp[j] < .25){
        temp[j] = 0
      }else{
        temp[j] = .5
      }
    }
  }
  predicted = temp
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  accuracy = sum(diag(cm)) / sum(cm)
  accvecfull[i,] = accuracy
}
mean(accvecfull[,1])                            # decimal accuracy

##########################################
cordat = cor(regdat)                            # find correlation values
symnum(cordat)                                  # B, *, +, , -> first rem ConsumptExp
subdat = regdat
subdat$ConsumptExp = NULL
cordat = cor(subdat)                            # check correlation again
symnum(cordat)
subdat$ManufValAdd = NULL                       # remove corr > .8
subdat$BirthRate = NULL
subdat$pgrowGDP = NULL
subdat$pgrowGNI = NULL
symnum(cor(subdat))                             # check again

##########################################
modselect1 = lm(comlevnum ~ ., data = subdat)   # partially selected model
par(mfrow= c(2,2))
plot(modselect1)                                # diagnostics and coefficients
summary(modselect1)

accvecpart = data.frame(matrix(ncol = 1, nrow = 100))
set.seed(101) 
for(i in 1:100){
  sample = sample.split(subdat$GNI, SplitRatio = .8)
  train = subset(subdat, sample == TRUE)
  test  = subset(subdat, sample == FALSE)
  modtest = lm(comlevnum ~ ., data = train)
  actual = test$comlevnum
  temp = predict.lm(modtest, test)
  for(j in 1:length(temp)){
    if(temp[j] > .75){
      temp[j] = 1
    }else{
      if(temp[j] < .25){
        temp[j] = 0
      }else{
        temp[j] = .5
      }
    }
  }
  predicted = temp
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  accuracy = sum(diag(cm)) / sum(cm)
  accvecpart[i,] = accuracy
}
mean(accvecpart[,1])

######################################3
outlierTest(modselect1)                         # check outliers - none at the .05 level
amod1 = Anova(modselect1, type = 2)
amod1

selectdat = data.frame(matrix(ncol = 13, nrow = 187)) 
nam = colnames(subdat)                          # reduce model based on sums of squares
coldat = data.frame(matrix(ncol = 1, nrow = 1))
count = 1
for(i in 1:length(amod1$`Sum Sq`)){             # if SSTrt > .1, keep 
  if(amod1$`Sum Sq`[i] > .01000){                # this allows for greater variability in model
    selectdat[,count] = subdat[,i]
    coldat[count] = nam[i]
    count= count+1
  }
}
selectdat = selectdat[,-c((length(coldat)+1):length(selectdat))]
colnames(selectdat) = coldat
head(selectdat)

####################################

modselect2 = lm(comlevnum ~ ., data = selectdat)
par(mfrow= c(2,2))
plot(modselect2)
summary(modselect2)


accvecselect = data.frame(matrix(ncol = 1, nrow = 100))
set.seed(101) 
for(i in 1:100){
  sample = sample.split(selectdat$GNI, SplitRatio = .8)
  train = subset(selectdat, sample == TRUE)
  test  = subset(selectdat, sample == FALSE)
  modtest = lm(comlevnum ~ ., data = train)
  actual = test$comlevnum
  temp = predict.lm(modtest, test)
  for(j in 1:length(temp)){
    if(temp[j] > .75){
      temp[j] = 1
    }else{
      if(temp[j] < .25){
        temp[j] = 0
      }else{
        temp[j] = .5
      }
    }
  }
  predicted = temp
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  accuracy = sum(diag(cm)) / sum(cm)
  accvecselect[i,] = accuracy
}
mean(accvecselect[,1])
###################################################################
damdat = selectdat
damdat$damage = avgdat$damage
modselect3 = lm(comlevnum ~ ., data = damdat)
par(mfrow= c(2,2))
plot(modselect3)
summary(modselect3)

accvecselect = data.frame(matrix(ncol = 1, nrow = 100))
set.seed(101) 
for(i in 1:100){
  sample = sample.split(damdat$GNI, SplitRatio = .8)
  train = subset(damdat, sample == TRUE)
  test  = subset(damdat, sample == FALSE)
  modtest = lm(comlevnum ~ ., data = train)
  actual = test$comlevnum
  temp = predict.lm(modtest, test)
  for(j in 1:length(temp)){
    if(temp[j] > .75){
      temp[j] = 1
    }else{
      if(temp[j] < .25){
        temp[j] = 0
      }else{
        temp[j] = .5
      }
    }
  }
  predicted = temp
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  accuracy = sum(diag(cm)) / sum(cm)
  accvecselect[i,] = accuracy
}
mean(accvecselect[,1])



###########################################################
min(avgdat$damage)
weight = avgdat$damage + abs(min(avgdat$damage))
modselect4 = lm(comlevnum ~ ., data = selectdat, weights = weight)
par(mfrow= c(2,2))
plot(modselect4)
summary(modselect4)

newdat = selectdat
newdat$damage = avgdat$damage
accvecselect = data.frame(matrix(ncol = 1, nrow = 100))
set.seed(101) 
for(i in 1:100){
  sample = sample.split(newdat$GNI, SplitRatio = .8)
  train = subset(newdat, sample == TRUE)
  x = train$damage + abs(min(train$damage))
  train$damage = NULL
  test  = subset(newdat, sample == FALSE)
  test$damage = NULL
  modtest = lm(comlevnum ~ ., data = train, weights = x )
  actual = test$comlevnum
  temp = predict.lm(modtest, test)
  for(j in 1:length(temp)){
    if(temp[j] > .75){
      temp[j] = 1
    }else{
      if(temp[j] < .25){
        temp[j] = 0
      }else{
        temp[j] = .5
      }
    }
  }
  predicted = temp
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  accuracy = sum(diag(cm)) / sum(cm)
  accvecselect[i,] = accuracy
}
mean(accvecselect[,1])


