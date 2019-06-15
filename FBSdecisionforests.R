#importing file and checking basic data quality
setwd("C:/Users/jstreeter/Desktop")
FBS<-read.csv("FBSrisk.csv") #saved this file in the CBCAFS/FBS Folder and noted that the CSV is the starting point for developing this code 
head(FBS)
summary(FBS)
str(FBS)

#changing variable types
FBS$MasterBK<-as.character(FBS$MasterBK)
FBS$Full_Name<-as.character(FBS$Full_Name)


FBS$AfterWrapMulti<-as.factor(FBS$AfterWrapMulti)
FBS$BeforeWrapMulti<-as.factor(FBS$BeforeWrapMulti)
FBS$DuringWrapMulti<-as.factor(FBS$DuringWrapMulti)

FBS$DHSBefore<-as.factor(FBS$DHSBefore)
FBS$DHSDuring<-as.factor(FBS$DHSDuring)
FBS$DHSAfter<-as.factor(FBS$DHSAfter)
levels(FBS$DHSBefore)<-c("not DHS", "DHS")
levels(FBS$DHSDuring)<-c("not DHS", "DHS")
levels(FBS$DHSAfter)<-c("not DHS", "DHS")

FBS$Sum.of.Risk<-as.factor(FBS$Sum.of.Risk)
FBS$RiskwoDHS<-as.factor(FBS$RiskwoDHS)
FBS$FinalRiskincDHS<-as.factor(FBS$FinalRiskincDHS)
levels(FBS$FinalRiskincDHS)
levels(FBS$FinalRiskincDHS)<-c("In Community", "Removed From Community")
levels(FBS$RiskwoDHS)<-c("In Community", "Removed From Community")

FBS$beforebegin<-as.Date(FBS$beforebegin,format="%m/%d/%Y")
FBS$beforeend<-as.Date(FBS$beforeend,format="%m/%d/%Y")
FBS$duringbegin<-as.Date(FBS$duringbegin,format="%m/%d/%Y")
FBS$duringend<-as.Date(FBS$duringend,format="%m/%d/%Y")
FBS$afterbegin<-as.Date(FBS$afterbegin,format="%m/%d/%Y")
FBS$afterend<-as.Date(FBS$afterend,format="%m/%d/%Y")



#figuring out age categories
summary(FBS$ageatservicebegin)
hist(FBS$ageatservicebegin)
sum(table(FBS$ageatservicebegin[FBS$ageatservicebegin<14]))
boxplot(FBS$ageatservicebegin)
boxplot(FBS$ageatservicebegin ~ FBS$FinalRiskincDHS)
#looks like the split around 13 divides the population pretty evenly
#folks removed from the community look like they're a bit older on average, but no real sig dif


#Other demos
boxplot(FBS$DuringFBS)
boxplot(FBS$DuringFBS ~ FBS$FinalRiskincDHS)
summary(FBS$RaceEthnic_Label[FBS$RaceEthnic_Label == 'ASIAN                                   '])
summary(FBS$RaceEthnic_Label)

table(FBS$Gender_Label,FBS$FinalRiskincDHS)
plot(table(FBS$Gender_Label,FBS$FinalRiskincDHS))
chisq.test(FBS$Gender_Label,FBS$FinalRiskincDHS)#sig dif odds of being removed from home based on gender

table(FBS$BeforeAIP==0)

#creating larger "other" category
#a number of categories that are broken out now are quite small.  BeforeOther1, DuringOther1, and AfterOther1
#all to include the base other category and the following categories collapsed in -> 
#Crisis, CRR, host home, preschool, residential.  
#Did not include CRR, Host Home, and Residentialin the after category as they are outcomes
#FamFFT (108 in before but v low in others) left as standalone category, wouldnt include in "during" but would include in before and after

FBS$BeforeOther1<-(FBS$BeforeCrisis+FBS$BeforeCRR+FBS$BeforeHH+FBS$BeforeOther+ FBS$BeforePreschool+ FBS$BeforeResidential)
summary(FBS$BeforeOther1)
hist(FBS$BeforeOther1[FBS$BeforeOther1>0])

FBS$DuringOther1<-(FBS$DuringCrisis+FBS$DuringCRR+FBS$DuringHH+FBS$DuringOther+ FBS$DuringPreschool+ FBS$DuringResidential)
summary(FBS$DuringOther1)
hist(FBS$DuringOther1[FBS$DuringOther1>0])

FBS$AfterOther1<-(FBS$AfterCrisis+FBS$AfterOther+ FBS$AfterPreschool)
summary(FBS$AfterOther1)
hist(FBS$AfterOther1[FBS$AfterOther1>0])

#looking at day distributions for those that use a given service

par(mfrow=c(1,1))
#AIP
boxplot(FBS$BeforeAIP[FBS$BeforeAIP>0], FBS$DuringAIP[FBS$DuringAIP>0],FBS$AfterAIP[FBS$AfterAIP>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of AIP")

#CRC
boxplot(FBS$BeforeCRC[FBS$BeforeCRC>0], FBS$DuringCRC[FBS$DuringCRC>0],FBS$AfterCRC[FBS$AfterCRC>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of CRC")

#Eval
boxplot(FBS$BeforeEval[FBS$BeforeEval>0], FBS$DuringEval[FBS$DuringEval>0],FBS$AfterEval[FBS$AfterEval>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of Eval")

#FamFFT
boxplot(FBS$BeforeFamFFT[FBS$BeforeFamFFT>0], FBS$DuringFamFFT[FBS$DuringFamFFT>0],FBS$AfterFamFFT[FBS$AfterFamFFT>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of FamFFT")

#FBS
boxplot(FBS$BeforeFBS[FBS$BeforeFBS>0], FBS$DuringFBS[FBS$DuringFBS>0],FBS$AfterFBS[FBS$AfterFBS>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of FBS")

#MedMgmt
boxplot(FBS$BeforeMedMgmt[FBS$BeforeMedMgmt>0], FBS$DuringMedMgmt[FBS$DuringMedMgmt>0],FBS$AfterMedMgmt[FBS$AfterMedMgmt>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of MedMgmt")

#OP
boxplot(FBS$BeforeOP[FBS$BeforeOP>0], FBS$DuringOP[FBS$DuringOP>0],FBS$AfterOP[FBS$AfterOP>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of OP")

#Other
boxplot(FBS$BeforeOther[FBS$BeforeOther>0], FBS$DuringOther[FBS$DuringOther>0],FBS$AfterOther[FBS$AfterOther>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of Other")

#Partial
boxplot(FBS$BeforePartial[FBS$BeforePartial>0], FBS$DuringPartial[FBS$DuringPartial>0],FBS$AfterPartial[FBS$AfterPartial>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of Partial")

#RTF
boxplot(FBS$BeforeRTF[FBS$BeforeRTF>0], FBS$DuringRTF[FBS$DuringRTF>0],FBS$AfterRTF[FBS$AfterRTF>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of RTF")

#STS
boxplot(FBS$BeforeSTS[FBS$BeforeSTS>0], FBS$DuringSTS[FBS$DuringSTS>0],FBS$AfterSTS[FBS$AfterSTS>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of STS")

#TCM
boxplot(FBS$BeforeTCM[FBS$BeforeTCM>0], FBS$DuringTCM[FBS$DuringTCM>0],FBS$AfterTCM[FBS$AfterTCM>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of TCM")

#WrapBSC
boxplot(FBS$BeforeWrapBS[FBS$BeforeWrapBS>0], FBS$DuringWrapBS[FBS$DuringWrapBS>0],FBS$AfterWrapBS[FBS$AfterWrapBS>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of WrapBS")

#WrapMT
boxplot(FBS$BeforeWrapMT[FBS$BeforeWrapMT>0], FBS$DuringWrapMT[FBS$DuringWrapMT>0],FBS$AfterWrapMT[FBS$AfterWrapMT>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of WrapMT")

#WrapOther
boxplot(FBS$BeforeWrapOther[FBS$BeforeWrapOther>0], FBS$DuringWrapOther[FBS$DuringWrapOther>0],FBS$AfterWrapOther[FBS$AfterWrapOther>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of WrapOther")

#WrapTSS
boxplot(FBS$BeforeWrapTSS[FBS$BeforeWrapTSS>0], FBS$DuringWrapTSS[FBS$DuringWrapTSS>0],FBS$AfterWrapTSS[FBS$AfterWrapTSS>0], names = c("6 Mo. Before", "During","6 Mo. After"), main = "Days of WrapTSS")

#create overall wrap categories and then do boxplots for them

FBS$BeforeWrapAny <- FBS$BeforeWrapBS + FBS$BeforeWrapMT + FBS$BeforeWrapOther + FBS$BeforeWrapTSS
FBS$DuringWrapAny <- FBS$DuringWrapBS + FBS$DuringWrapMT + FBS$DuringWrapOther + FBS$DuringWrapTSS
FBS$AfterWrapAny <- FBS$AfterWrapBS + FBS$AfterWrapMT + FBS$AfterWrapOther + FBS$AfterWrapTSS

#FBS LOS and Dose
FBS$LOS <- FBS$duringend-FBS$duringbegin + 1
FBS$LOS <-as.integer(FBS$LOS)
boxplot(FBS$LOS)

FBS$Dose<-FBS$DuringFBS/FBS$LOS * 100 # N days per 100 days in community
summary(FBS$Dose)

#FBS RaceCat
levels(FBS$RaceEthnic_Label)
FBS$RaceCat<- ifelse(FBS$RaceEthnic_Label == "ASIAN                                   " | FBS$RaceEthnic_Label == "OTHER                                   " | FBS$RaceEthnic_Label == "N.AMER.INDIAN/ALASKAN NATIVE            ","Other",
                    ifelse(FBS$RaceEthnic_Label == "HISPANIC                                ", "Hispanic",
                            ifelse(FBS$RaceEthnic_Label == "BLACK OR AFRICAN AMERICAN               ", "Black",
                                   ifelse(FBS$RaceEthnic_Label == "WHITE                                   ", "White", "Error"))))

table(FBS$RaceCat)
table(FBS$RaceEthnic_Label)
FBS$RaceCat<-as.factor(FBS$RaceCat)

#creating a subset that removes FBS cases ending after 5/31/2016
FBS.Subset<-FBS[FBS$duringend <= "2016-05-31",]
FBS.IncompCase<-FBS[FBS$duringend > "2016-05-31",]
summary(FBS.IncompCase)
summary(FBS$FinalRiskincDHS)



#limiting columns in subset to just potential test variables
colnames(FBS.Subset)
AllTestVariables<-c("FinalRiskincDHS","ageatservicebegin",
    "MasterBK", "Gender_Label", "RaceCat", "LOS", "Dose",
    "BeforeAIP", "DuringAIP",
    "BeforeCRC", "DuringCRC",
    "BeforeEval", "DuringEval",
    "BeforeFamFFT", "DuringFamFFT",
    "BeforeFBS", "DuringFBS",
    "BeforeOP", "DuringOP",
    "BeforePartial", "DuringPartial",
    "BeforeRTF", "DuringRTF",
    "BeforeSTS", "DuringSTS",
    "BeforeTCM", "DuringTCM",
    "BeforeWrapBS", "DuringWrapBS",
    "BeforeWrapMT", "DuringWrapMT",
    "BeforeWrapTSS", "DuringWrapTSS",
    "BeforeWrapOther", "DuringWrapOther",
    "BeforeWrapAny", "DuringWrapAny", #total days for ANY BHRS
    "BeforeOther1", "DuringOther1",
    "BeforeWrapMulti", "DuringWrapMulti",
    "DHSBefore", "DHSDuring", "DHSAfter") #r they using 0, 1, 2, or 3 Wrap Services

FBS.Subset1<-FBS.Subset[AllTestVariables] 

#####################################
# creating test variable subsets for quick reference
# pulled DHSduring and DHSAfter out of all sets bc DHS after is tied to risk, 
# as is DHSDuring(can't have DHSDuring if you're going to have DHSAfter)

#model1 - all detailed variables, raw WRAP info
Model1.rawwrap <-c( "Gender_Label", "RaceCat", "LOS", 
           "BeforeAIP", "DuringAIP",
           "BeforeCRC", "DuringCRC",
           "BeforeEval", "DuringEval",
           "BeforeFamFFT", "DuringFamFFT",
           "BeforeFBS", "DuringFBS",
           "BeforeOP", "DuringOP",
           "BeforePartial", "DuringPartial",
           "BeforeRTF", "DuringRTF",
           "BeforeSTS", "DuringSTS",
           "BeforeTCM", "DuringTCM",
           "BeforeWrapBS", "DuringWrapBS",
           "BeforeWrapMT", "DuringWrapMT",
           "BeforeWrapTSS", "DuringWrapTSS",
           "BeforeWrapOther", "DuringWrapOther",
           "BeforeOther1", "DuringOther1",
           "DHSBefore")

#model2 - all detailed variables, raw wrap info plus multiple levels of wrap.

Model2.rawwrapC <- c( "Gender_Label", "RaceCat", "LOS", 
            "BeforeAIP", "DuringAIP",
            "BeforeCRC", "DuringCRC",
            "BeforeEval", "DuringEval",
            "BeforeFamFFT", "DuringFamFFT",
            "BeforeFBS", "DuringFBS",
            "BeforeOP", "DuringOP",
            "BeforePartial", "DuringPartial",
            "BeforeRTF", "DuringRTF",
            "BeforeSTS", "DuringSTS",
            "BeforeTCM", "DuringTCM",
            "BeforeWrapBS", "DuringWrapBS",
            "BeforeWrapMT", "DuringWrapMT",
            "BeforeWrapTSS", "DuringWrapTSS",
            "BeforeWrapOther", "DuringWrapOther",
            "BeforeOther1", "DuringOther1",
            "BeforeWrapMulti", "DuringWrapMulti",
            "DHSBefore")

#model3 - all detailed variables, any wrap

Model3.combinedwrap <- c( "Gender_Label", "RaceCat", "LOS", 
            "BeforeAIP", "DuringAIP",
            "BeforeCRC", "DuringCRC",
            "BeforeEval", "DuringEval",
            "BeforeFamFFT", "DuringFamFFT",
            "BeforeFBS", "DuringFBS",
            "BeforeOP", "DuringOP",
            "BeforePartial", "DuringPartial",
            "BeforeRTF", "DuringRTF",
            "BeforeSTS", "DuringSTS",
            "BeforeTCM", "DuringTCM",
            "BeforeWrapAny", "DuringWrapAny", #total days for ANY BHRS
            "BeforeOther1", "DuringOther1",
            "DHSBefore")

#model4 - all detailed, any wrap + multiple levels of wrap

Model4.combinedwrapC<- c( "Gender_Label", "RaceCat", "LOS", 
            "BeforeAIP", "DuringAIP",
            "BeforeCRC", "DuringCRC",
            "BeforeEval", "DuringEval",
            "BeforeFamFFT", "DuringFamFFT",
            "BeforeFBS", "DuringFBS",
            "BeforeOP", "DuringOP",
            "BeforePartial", "DuringPartial",
            "BeforeRTF", "DuringRTF",
            "BeforeSTS", "DuringSTS",
            "BeforeTCM", "DuringTCM",
            "BeforeWrapAny", "DuringWrapAny", #total days for ANY BHRS
            "BeforeOther1", "DuringOther1",
            "BeforeWrapMulti", "DuringWrapMulti",
            "DHSBefore")


#################################################################################
#Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBS.trainingindex <- sample(1:nrow(FBS.Subset1), 0.7*nrow(FBS.Subset1), replace=F) #creates the indices for the sample
FBS.Train <- FBS.Subset1[FBS.trainingindex,] #pull vector where training indices are true
dim(FBS.Train) #1300 Cases
summary(FBS.Train$FinalRiskincDHS) #408 cases of high risk

## select the other 30% as the testing data
FBS.Test <- FBS.Subset1[-FBS.trainingindex,]
dim(FBS.Test) #535 Cases
summary(FBS.Test$FinalRiskincDHS) #198 cases of high risk



#################################################################################
####MODEL1#### 
#fitting model to training set
 
###Model 4 w/1000 trees: Accuracy : 82.1%, Precision: 85.3%, Recall: 40.9%
###Model 4 w/ 500 trees: Accuracy: 81.7%, Precision: 82.7%, Recall: 40.9%
###Model 4 w/250 trees: Accuracy: 82.1%, P: 86.0%, R: 40.4%
library(reprtree)



###########################################################
#Code i found for printing out tree rules in RF package

#**************************
#return the rules of a tree
#**************************
getConds<-function(tree){
#store all conditions into a list
conds<-list()
#start by the terminal nodes and find previous conditions
id.leafs<-which(tree$status==-1)
j<-0
for(i in id.leafs){
j<-j+1
prevConds<-prevCond(tree,i)
conds[[j]]<-prevConds$cond
while(prevConds$id>1){
prevConds<-prevCond(tree,prevConds$id)
conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
if(prevConds$id==1){
conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
break()
}
}

}

return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond<-function(tree,i){
if(i %in% tree$right_daughter){
id<-which(tree$right_daughter==i)
cond<-paste(tree$split_var[id],">",tree$split_point[id])
}
if(i %in% tree$left_daughter){
id<-which(tree$left_daughter==i)
cond<-paste(tree$split_var[id],"<",tree$split_point[id])
}

return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
x<-sub(" ","_",x)

return(x)
}

##Subbed in my variables to use this code
tree<-getTree(FBS.RFmodel4, k=300, labelVar=TRUE)
#rename the name of the column
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)

######
#Next steps
#go through the importance plots, consider new feature creation
#look at individual trees. 
#build individual trees using the most important variables
#once i've honed in on a good model, deal with cross validation of that model so I can tune parameters. 

#########################################################################################
#Changing days to binary cats

summary(FBS.Subset1)
summary(FBS.Subset1$BeforeAIP[FBS.Subset1$BeforeAIP > 0 & FBS.Subset1$DHSBefore == "not DHS"])
summary(FBS.Subset1$BeforeAIP[FBS.Subset1$BeforeAIP > 0 & FBS.Subset1$DHSBefore == "DHS"])
summary(FBS.Subset1$BeforeAIP[FBS.Subset1$DuringAIP > 0 & FBS.Subset1$DHSDuring == "not DHS"])
summary(FBS.Subset1$BeforeAIP[FBS.Subset1$DuringAIP > 0 & FBS.Subset1$DHSDuring == "DHS"])
names(FBS.Subset1)

FBS.SubsetCat<-FBS.Subset1 #FBS Subset Cat is just FBS subset 1 (all valid data) w days changed to categoricals



#recodes
FBS.SubsetCat$BeforeAIP <- as.factor(ifelse(FBS.SubsetCat$BeforeAIP == 0, 0, 1))
FBS.SubsetCat$DuringAIP <- as.factor(ifelse(FBS.SubsetCat$DuringAIP == 0, 0, 1))

FBS.SubsetCat$BeforeCRC <- as.factor(ifelse(FBS.SubsetCat$BeforeCRC == 0, 0, 1))
FBS.SubsetCat$DuringCRC <- as.factor(ifelse(FBS.SubsetCat$DuringCRC == 0, 0, 1))

FBS.SubsetCat$BeforeEval <- as.factor(ifelse(FBS.SubsetCat$BeforeEval == 0, 0, 1))
FBS.SubsetCat$DuringEval <- as.factor(ifelse(FBS.SubsetCat$DuringEval == 0, 0, 1))

FBS.SubsetCat$BeforeFamFFT <- as.factor(ifelse(FBS.SubsetCat$BeforeFamFFT == 0, 0, 1))
FBS.SubsetCat$DuringFamFFT <- as.factor(ifelse(FBS.SubsetCat$DuringFamFFT == 0, 0, 1))

FBS.SubsetCat$BeforeFBS <- as.factor(ifelse(FBS.SubsetCat$BeforeFBS == 0, 0, 1))
#FBS.SubsetCat$DuringFBS <- as.factor(ifelse(FBS.SubsetCat$DuringFBS == 0, 0, 1))
#Kept FBS as days bc i want to know how many days of intervention service they're using for during period, along w/LOS

FBS.SubsetCat$BeforeOP <- as.factor(ifelse(FBS.SubsetCat$BeforeOP == 0, 0, 1))
FBS.SubsetCat$DuringOP <- as.factor(ifelse(FBS.SubsetCat$DuringOP == 0, 0, 1))

FBS.SubsetCat$BeforePartial <- as.factor(ifelse(FBS.SubsetCat$BeforePartial == 0, 0, 1))
FBS.SubsetCat$DuringPartial <- as.factor(ifelse(FBS.SubsetCat$DuringPartial == 0, 0, 1))

FBS.SubsetCat$BeforeRTF <- as.factor(ifelse(FBS.SubsetCat$BeforeRTF == 0, 0, 1))
FBS.SubsetCat$DuringRTF <- as.factor(ifelse(FBS.SubsetCat$DuringRTF == 0, 0, 1))

FBS.SubsetCat$BeforeSTS <- as.factor(ifelse(FBS.SubsetCat$BeforeSTS == 0, 0, 1))
FBS.SubsetCat$DuringSTS <- as.factor(ifelse(FBS.SubsetCat$DuringSTS == 0, 0, 1))

FBS.SubsetCat$BeforeTCM <- as.factor(ifelse(FBS.SubsetCat$BeforeTCM == 0, 0, 1))
FBS.SubsetCat$DuringTCM <- as.factor(ifelse(FBS.SubsetCat$DuringTCM == 0, 0, 1))

FBS.SubsetCat$BeforeWrapBS <- as.factor(ifelse(FBS.SubsetCat$BeforeWrapBS == 0, 0, 1))
FBS.SubsetCat$DuringWrapBS <- as.factor(ifelse(FBS.SubsetCat$DuringWrapBS == 0, 0, 1))

FBS.SubsetCat$BeforeWrapMT <- as.factor(ifelse(FBS.SubsetCat$BeforeWrapMT == 0, 0, 1))
FBS.SubsetCat$DuringWrapMT <- as.factor(ifelse(FBS.SubsetCat$DuringWrapMT == 0, 0, 1))

FBS.SubsetCat$BeforeWrapTSS <- as.factor(ifelse(FBS.SubsetCat$BeforeWrapTSS == 0, 0, 1))
FBS.SubsetCat$DuringWrapTSS <- as.factor(ifelse(FBS.SubsetCat$DuringWrapTSS == 0, 0, 1))

FBS.SubsetCat$BeforeWrapOther <- as.factor(ifelse(FBS.SubsetCat$BeforeWrapOther == 0, 0, 1))
FBS.SubsetCat$DuringWrapOther <- as.factor(ifelse(FBS.SubsetCat$DuringWrapOther == 0, 0, 1))

FBS.SubsetCat$BeforeWrapAny <- as.factor(ifelse(FBS.SubsetCat$BeforeWrapAny == 0, 0, 1))
FBS.SubsetCat$DuringWrapAny <- as.factor(ifelse(FBS.SubsetCat$DuringWrapAny == 0, 0, 1))

FBS.SubsetCat$BeforeOther1 <- as.factor(ifelse(FBS.SubsetCat$BeforeOther1 == 0, 0, 1))
FBS.SubsetCat$DuringOther1 <- as.factor(ifelse(FBS.SubsetCat$DuringOther1 == 0, 0, 1))


#spot checking recodes
dim(FBS.Subset1[FBS.Subset1$BeforeCRC == 0,])
table(FBS.SubsetCat$BeforeCRC)

summary(FBS.SubsetCat)


################################################################################
#CATEGORICAL MODELS
#Testing Models 1 through 4 again

#################################################################################

#Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBSCat.trainingindex <- sample(1:nrow(FBS.SubsetCat), 0.7*nrow(FBS.SubsetCat), replace=F) #creates the indices for the sample
FBSCat.Train <- FBS.SubsetCat[FBSCat.trainingindex,] #pull vector where training indices are true
dim(FBSCat.Train) #1708 Cases
summary(FBSCat.Train$FinalRiskincDHS) #408 cases of high risk

## select the other 30% as the testing data
FBSCat.Test <- FBS.SubsetCat[-FBSCat.trainingindex,]
dim(FBSCat.Test) #733 Cases
summary(FBSCat.Test$FinalRiskincDHS) #198 cases of high risk

#################################################################################



####MODEL1 Cat#### 
#fitting model to training set
library(randomForest)
FBSCat.RFmodel1 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                               BeforeAIP + DuringAIP + 
                               BeforeCRC + DuringCRC + 
                               BeforeEval + DuringEval + 
                               BeforeFamFFT + DuringFamFFT + 
                               BeforeFBS + DuringFBS + 
                               BeforeOP + DuringOP + 
                               BeforePartial + DuringPartial + 
                               BeforeRTF + DuringRTF + 
                               BeforeSTS + DuringSTS + 
                               BeforeTCM + DuringTCM + 
                               BeforeWrapBS + DuringWrapBS + 
                               BeforeWrapMT + DuringWrapMT + 
                               BeforeWrapTSS + DuringWrapTSS + 
                               BeforeWrapOther + DuringWrapOther + 
                               BeforeOther1 + DuringOther1 + 
                               DHSBefore, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel1)
importance(FBSCat.RFmodel1)
varImpPlot(FBSCat.RFmodel1)

##predicting for model 1 on test set.
FBSCat.Model1Predictions <- predict(FBSCat.RFmodel1, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model1.TestConfusion <- table(FBSCat.Model1Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model1.TestConfusion)
##accuracy
FBSCat.Model1.TestAccuracy <- sum(diag(FBSCat.Model1.TestConfusion)) / sum(FBSCat.Model1.TestConfusion)
print(FBSCat.Model1.TestAccuracy) 
##precision
FBSCat.Model1.TestPrecision <- FBSCat.Model1.TestConfusion[2,2] / sum(FBSCat.Model1.TestConfusion[2,])
print(FBSCat.Model1.TestPrecision)
##recall
FBSCat.Model1.TestRecall <- FBSCat.Model1.TestConfusion[2,2]  / sum(FBSCat.Model1.TestConfusion[,2])
print(FBSCat.Model1.TestRecall)



#################################################################################
####MODEL 2 Cat#### 
#fitting model to training set
FBSCat.RFmodel2 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                               BeforeAIP + DuringAIP + 
                               BeforeCRC + DuringCRC + 
                               BeforeEval + DuringEval + 
                               BeforeFamFFT + DuringFamFFT + 
                               BeforeFBS + DuringFBS +
                               BeforeOP + DuringOP + 
                               BeforePartial + DuringPartial + 
                               BeforeRTF + DuringRTF + 
                               BeforeSTS + DuringSTS + 
                               BeforeTCM + DuringTCM + 
                               BeforeWrapBS + DuringWrapBS + 
                               BeforeWrapMT + DuringWrapMT + 
                               BeforeWrapTSS + DuringWrapTSS + 
                               BeforeWrapOther + DuringWrapOther + 
                               BeforeOther1 + DuringOther1 + 
                               BeforeWrapMulti + DuringWrapMulti +
                               DHSBefore, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel2)
importance(FBSCat.RFmodel2)
varImpPlot(FBSCat.RFmodel2)

##predicting for model 2 on test set.
FBSCat.Model2Predictions <- predict(FBSCat.RFmodel2, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model2.TestConfusion <- table(FBSCat.Model2Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model2.TestConfusion)
##accuracy
FBSCat.Model2.TestAccuracy <- sum(diag(FBSCat.Model2.TestConfusion)) / sum(FBSCat.Model2.TestConfusion)
print(FBSCat.Model2.TestAccuracy) 
##precision
FBSCat.Model2.TestPrecision <- FBSCat.Model2.TestConfusion[2,2] / sum(FBSCat.Model2.TestConfusion[2,])
print(FBSCat.Model2.TestPrecision)
##recall
FBSCat.Model2.TestRecall <- FBSCat.Model2.TestConfusion[2,2]  / sum(FBSCat.Model2.TestConfusion[,2])
print(FBSCat.Model2.TestRecall)



#################################################################################
####MODEL3 Cat#### 
#fitting model to training set
FBSCat.RFmodel3 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                               BeforeAIP + DuringAIP + 
                               BeforeCRC + DuringCRC + 
                               BeforeEval + DuringEval + 
                               BeforeFamFFT + DuringFamFFT + 
                               BeforeFBS +  DuringFBS +
                               BeforeOP + DuringOP + 
                               BeforePartial + DuringPartial + 
                               BeforeRTF + DuringRTF + 
                               BeforeSTS + DuringSTS + 
                               BeforeTCM + DuringTCM + 
                               BeforeWrapAny + DuringWrapAny +
                               BeforeOther1 + DuringOther1 + 
                               DHSBefore, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel3)
importance(FBSCat.RFmodel3)
varImpPlot(FBSCat.RFmodel3)

##predicting for model 3 on test set.
FBSCat.Model3Predictions <- predict(FBSCat.RFmodel3, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model3.TestConfusion <- table(FBSCat.Model3Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model3.TestConfusion)
##accuracy
FBSCat.Model3.TestAccuracy <- sum(diag(FBSCat.Model3.TestConfusion)) / sum(FBSCat.Model3.TestConfusion)
print(FBSCat.Model3.TestAccuracy) 
##precision
FBSCat.Model3.TestPrecision <- FBSCat.Model3.TestConfusion[2,2] / sum(FBSCat.Model3.TestConfusion[2,])
print(FBSCat.Model3.TestPrecision)
##recall
FBSCat.Model3.TestRecall <- FBSCat.Model3.TestConfusion[2,2]  / sum(FBSCat.Model3.TestConfusion[,2])
print(FBSCat.Model3.TestRecall)


#################################################################################
####MODEL 4 Cat#### 
#fitting model to training set
FBSCat.RFmodel4 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +
                               BeforeAIP + DuringAIP + 
                               BeforeCRC + DuringCRC + 
                               BeforeEval + DuringEval + 
                               BeforeFamFFT + DuringFamFFT + 
                               BeforeFBS + DuringFBS +
                               BeforeOP + DuringOP + 
                               BeforePartial + DuringPartial + 
                               BeforeRTF + DuringRTF + 
                               BeforeSTS + DuringSTS + 
                               BeforeTCM + DuringTCM + 
                               BeforeWrapAny + DuringWrapAny +
                               BeforeOther1 + DuringOther1 + 
                               BeforeWrapMulti + DuringWrapMulti +
                               DHSBefore, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel4)
importance(FBSCat.RFmodel4)
varImpPlot(FBSCat.RFmodel4)

##predicting for model 1 on test set.
FBSCat.Model4Predictions <- predict(FBSCat.RFmodel4, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model4.TestConfusion <- table(FBSCat.Model4Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model4.TestConfusion)
##accuracy
FBSCat.Model4.TestAccuracy <- sum(diag(FBSCat.Model4.TestConfusion)) / sum(FBSCat.Model4.TestConfusion)
print(FBSCat.Model4.TestAccuracy) 
##precision
FBSCat.Model4.TestPrecision <- FBSCat.Model4.TestConfusion[2,2] / sum(FBSCat.Model4.TestConfusion[2,])
print(FBSCat.Model4.TestPrecision)
##recall
FBSCat.Model4.TestRecall <- FBSCat.Model4.TestConfusion[2,2]  / sum(FBSCat.Model4.TestConfusion[,2])
print(FBSCat.Model4.TestRecall)

#############################################################################
#TRIMMING THE NUMBER OF VARIABLES IN MY TREES
#based on the variable importance plots in the last round, I want to see the effect of
#reducing the number of variables in my models. 

#################################################################################



####MODEL1 Cat Trimmed N Variables#### 
#fitting model to training set
library(randomForest)
FBSCat.RFmodel1 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                                  BeforeAIP + DuringAIP + 
                                  BeforeCRC + DuringCRC + 
                                  BeforeEval + DuringEval +
                                   DuringFBS + 
                                  BeforeOP + DuringOP + 
                                  BeforePartial + DuringPartial + 
                                  BeforeRTF + DuringRTF + 
                                  BeforeOther1  
                                  , data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel1)
importance(FBSCat.RFmodel1)
varImpPlot(FBSCat.RFmodel1)

##predicting for model 1 on test set.
FBSCat.Model1Predictions <- predict(FBSCat.RFmodel1, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model1.TestConfusion <- table(FBSCat.Model1Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model1.TestConfusion)
##accuracy
FBSCat.Model1.TestAccuracy <- sum(diag(FBSCat.Model1.TestConfusion)) / sum(FBSCat.Model1.TestConfusion)
print(FBSCat.Model1.TestAccuracy) 
##precision
FBSCat.Model1.TestPrecision <- FBSCat.Model1.TestConfusion[2,2] / sum(FBSCat.Model1.TestConfusion[2,])
print(FBSCat.Model1.TestPrecision)
##recall
FBSCat.Model1.TestRecall <- FBSCat.Model1.TestConfusion[2,2]  / sum(FBSCat.Model1.TestConfusion[,2])
print(FBSCat.Model1.TestRecall)



#################################################################################
####MODEL 2 Cat  Trimmed N Variables#### 
#fitting model to training set
FBSCat.RFmodel2 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                                  BeforeAIP + DuringAIP + 
                                  BeforeCRC + DuringCRC + 
                                  BeforeEval + DuringEval + 
                                   DuringFamFFT + 
                                  BeforeFBS + DuringFBS +
                                  BeforeOP + DuringOP + 
                                  BeforePartial + DuringPartial + 
                                  BeforeRTF + DuringRTF +  
                                   DuringWrapMT + 
                                   DuringWrapTSS + 
                                  BeforeWrapOther + DuringWrapOther + 
                                  BeforeWrapMulti + DuringWrapMulti
                                  , data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel2)
importance(FBSCat.RFmodel2)
varImpPlot(FBSCat.RFmodel2)

##predicting for model 2 on test set.
FBSCat.Model2Predictions <- predict(FBSCat.RFmodel2, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model2.TestConfusion <- table(FBSCat.Model2Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model2.TestConfusion)
##accuracy
FBSCat.Model2.TestAccuracy <- sum(diag(FBSCat.Model2.TestConfusion)) / sum(FBSCat.Model2.TestConfusion)
print(FBSCat.Model2.TestAccuracy) 
##precision
FBSCat.Model2.TestPrecision <- FBSCat.Model2.TestConfusion[2,2] / sum(FBSCat.Model2.TestConfusion[2,])
print(FBSCat.Model2.TestPrecision)
##recall
FBSCat.Model2.TestRecall <- FBSCat.Model2.TestConfusion[2,2]  / sum(FBSCat.Model2.TestConfusion[,2])
print(FBSCat.Model2.TestRecall)



#################################################################################
####MODEL3 Cat Trimmed N Variables#### 
#fitting model to training set
FBSCat.RFmodel3 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +  
                                  BeforeAIP + DuringAIP + 
                                  BeforeCRC + DuringCRC + 
                                  BeforeEval + DuringEval + 
                                    DuringFBS +
                                  BeforeOP + DuringOP + 
                                  BeforePartial + DuringPartial + 
                                  BeforeRTF + DuringRTF + 
                                  BeforeWrapAny, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel3)
importance(FBSCat.RFmodel3)
varImpPlot(FBSCat.RFmodel3)

##predicting for model 3 on test set.
FBSCat.Model3Predictions <- predict(FBSCat.RFmodel3, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model3.TestConfusion <- table(FBSCat.Model3Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model3.TestConfusion)
##accuracy
FBSCat.Model3.TestAccuracy <- sum(diag(FBSCat.Model3.TestConfusion)) / sum(FBSCat.Model3.TestConfusion)
print(FBSCat.Model3.TestAccuracy) 
##precision
FBSCat.Model3.TestPrecision <- FBSCat.Model3.TestConfusion[2,2] / sum(FBSCat.Model3.TestConfusion[2,])
print(FBSCat.Model3.TestPrecision)
##recall
FBSCat.Model3.TestRecall <- FBSCat.Model3.TestConfusion[2,2]  / sum(FBSCat.Model3.TestConfusion[,2])
print(FBSCat.Model3.TestRecall)


#################################################################################
####MODEL 4 Cat Trimmed N Variables#### 
#fitting model to training set
FBSCat.RFmodel4 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS +
                                  BeforeAIP + DuringAIP + 
                                  BeforeCRC + DuringCRC + 
                                  BeforeEval + DuringEval + 
                                  DuringFBS +
                                  BeforeOP + DuringOP + 
                                  BeforePartial + DuringPartial + 
                                  BeforeRTF + DuringRTF + 
                                  BeforeWrapMulti + DuringWrapMulti, data = FBSCat.Train, importance = TRUE, ntree = 500)
print(FBSCat.RFmodel4)
importance(FBSCat.RFmodel4)
varImpPlot(FBSCat.RFmodel4)

##predicting for model 1 on test set.
FBSCat.Model4Predictions <- predict(FBSCat.RFmodel4, FBSCat.Test, type = "response")
##confusion matrix
FBSCat.Model4.TestConfusion <- table(FBSCat.Model4Predictions, FBSCat.Test$FinalRiskincDHS)
print(FBSCat.Model4.TestConfusion)
##accuracy
FBSCat.Model4.TestAccuracy <- sum(diag(FBSCat.Model4.TestConfusion)) / sum(FBSCat.Model4.TestConfusion)
print(FBSCat.Model4.TestAccuracy) 
##precision
FBSCat.Model4.TestPrecision <- FBSCat.Model4.TestConfusion[2,2] / sum(FBSCat.Model4.TestConfusion[2,])
print(FBSCat.Model4.TestPrecision)
##recall
FBSCat.Model4.TestRecall <- FBSCat.Model4.TestConfusion[2,2]  / sum(FBSCat.Model4.TestConfusion[,2])
print(FBSCat.Model4.TestRecall)


####################################
#Creating a third subset so I can have both interval and categorical variables
#Using variables from my model that produced the highest recall (FBSCat.RFmodel4)


FBS.Comb<-FBS.Subset1

#recodes fpr variables that will remain factors

#Keeping AIP as count of days right now
boxplot(FBS.Comb$BeforeAIP[FBS.Comb$BeforeAIP>0])
boxplot.stats(FBS.Comb$BeforeAIP[FBS.Comb$BeforeAIP>0])
boxplot(FBS.Comb$DuringAIP[FBS.Comb$DuringAIP>0])
boxplot.stats(FBS.Comb$DuringAIP[FBS.Comb$DuringAIP>0])

#creating AIP Category
FBS.Comb$BeforeAIP.Cat <- as.factor(ifelse(FBS.Comb$BeforeAIP == 0, 0, ifelse(FBS.Comb$BeforeAIP <=30, 1, 2)))
levels(FBS.Comb$BeforeAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")
FBS.Comb$DuringAIP.Cat <- as.factor(ifelse(FBS.Comb$DuringAIP == 0, 0, ifelse(FBS.Comb$DuringAIP <=30, 1, 2)))
levels(FBS.Comb$DuringAIP.Cat) <- c("No AIP", "30 days or less", "More than 30 days")

summary(FBS.Comb$BeforeAIP.Cat)
summary(FBS.Comb$DuringAIP.Cat)

#checking if categories make sense
sum(table(FBS.Comb$BeforeCRC[FBS.Comb$BeforeCRC==1]))
sum(table(FBS.Comb$BeforeCRC[FBS.Comb$BeforeCRC==1]))
sum(table(FBS.Comb$BeforeCRC[FBS.Comb$DuringCRC==1]))
sum(table(FBS.Comb$BeforeCRC[FBS.Comb$DuringCRC>1]))

#Making CRC 3 categories
FBS.Comb$BeforeCRC <- as.factor(ifelse(FBS.Comb$BeforeCRC == 0, 0, ifelse(FBS.Comb$BeforeCRC == 1, 1, 2)))
levels(FBS.Comb$BeforeCRC)<-c("No days", "1 day", "More than 1 day")
FBS.Comb$DuringCRC <- as.factor(ifelse(FBS.Comb$DuringCRC == 0, 0, ifelse(FBS.Comb$DuringCRC==1, 1, 2)))
levels(FBS.Comb$DuringCRC)<-c("No days", "1 day", "More than 1 day")

FBS.Comb$BeforeEval <- as.factor(ifelse(FBS.Comb$BeforeEval == 0, 0, 1))
FBS.Comb$DuringEval <- as.factor(ifelse(FBS.Comb$DuringEval == 0, 0, 1))

FBS.Comb$BeforeFBS.Bin <- as.factor(ifelse(FBS.Comb$BeforeFBS == 0, 0, 1))
#FBS.Comb$DuringFBS <- as.factor(ifelse(FBS.Comb$DuringFBS == 0, 0, 1))
#Kept FBS as days bc i want to know how many days of intervention service they're using for during period, along w/LOS

#checking OP
sum(table(FBS.Comb$BeforeOP[FBS.Comb$BeforeOP > 0]))#979 ppl have OP before
summary(FBS.Comb$BeforeOP[FBS.Comb$BeforeOP > 0])
boxplot(FBS.Comb$BeforeOP[FBS.Comb$BeforeOP > 0])
boxplot.stats(FBS.Comb$BeforeOP[FBS.Comb$BeforeOP > 0]) # most people under 27 days, 21 outliers
sum(table(FBS.Comb$DuringOP[FBS.Comb$DuringOP > 0])) #561 people have OP during
summary(FBS.Comb$DuringOP[FBS.Comb$DuringOP > 0])
boxplot(FBS.Comb$DuringOP[FBS.Comb$DuringOP > 0]) # most people under 11 days, 63 over 11 days
boxplot.stats(FBS.Comb$DuringOP[FBS.Comb$DuringOP > 0])

#Creating 3 OP categories (None, Average, High)
FBS.Comb$BeforeOP.Cat <- as.factor(ifelse(FBS.Comb$BeforeOP == 0, 0, ifelse(FBS.Comb$BeforeOP > 0 & FBS.Comb$BeforeOP <= 12, 1, 2)))
levels(FBS.Comb$BeforeOP.Cat) <- c("None", "Average", "High")
FBS.Comb$DuringOP.Cat <- as.factor(ifelse(FBS.Comb$DuringOP == 0, 0, ifelse(FBS.Comb$DuringOP >0 & FBS.Comb$DuringOP <= 5, 1, 2)))
levels(FBS.Comb$DuringOP.Cat) <- c("None", "Average", "High")

#AlsoCreating OP Binary
FBS.Comb$BeforeOP.Bin <- as.factor(ifelse(FBS.Comb$BeforeOP == 0, 0, 1))
FBS.Comb$DuringOP.Bin <- as.factor(ifelse(FBS.Comb$DuringOP == 0, 0, 1))

#verifying recode
summary(FBS.Comb$BeforeOP.Cat)
summary(FBS.Comb$DuringOP.Cat)

#usually minimal variation in LOS, so making categorical
FBS.Comb$BeforePartial <- as.factor(ifelse(FBS.Comb$BeforePartial == 0, 0, 1))
FBS.Comb$DuringPartial <- as.factor(ifelse(FBS.Comb$DuringPartial == 0, 0, 1))

#seeing if i should have multiple RTF categories
sum(table(FBS.Comb$BeforeRTF[ FBS.Comb$BeforeRTF>=30])) #318
sum(table(FBS.Comb$BeforeRTF[FBS.Comb$BeforeRTF>0 & FBS.Comb$BeforeRTF<30])) #only 8 ppl with RTF Before have LT30 days
sum(table(FBS.Comb$DuringRTF[FBS.Comb$DuringRTF>0 & FBS.Comb$DuringRTF<30])) #213 folks in during have under 1 month LOS
sum(table(FBS.Comb$DuringRTF[FBS.Comb$DuringRTF>=30]) #only 48 have LOS > month, and they're all under 3 months
    
FBS.Comb$BeforeRTF <- as.factor(ifelse(FBS.Comb$BeforeRTF == 0, 0, 1)) #understand these are monstly long LOS
FBS.Comb$DuringRTF <- as.factor(ifelse(FBS.Comb$DuringRTF == 0, 0, 1)) #understand these are all under 90 days

#put WrapAny back in model with days, just to check
sum(table(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0]))#819 ppl have bhrs before
summary(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0])
boxplot(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0])
boxplot.stats(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0]) # most people under 104 days, 57 outliers
sum(table(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0])) #556 people have bhrs during
summary(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0])
boxplot.stats(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0]) #most people under 47 days,68 outliers )

#testing proposed cats
sum(table(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny == 0]))
sum(table(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 0 & FBS.Comb$BeforeWrapAny <= 50]))
sum(table(FBS.Comb$BeforeWrapAny[FBS.Comb$BeforeWrapAny > 50]))

sum(table(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny == 0]))
sum(table(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 0 & FBS.Comb$DuringWrapAny <= 20]))
sum(table(FBS.Comb$DuringWrapAny[FBS.Comb$DuringWrapAny > 20]))


#based on Wrapany boxplots, creating 3 categories for before and after - none, average, high
FBS.Comb$BeforeWrap.Cat <- as.factor(ifelse(FBS.Comb$BeforeWrapAny == 0, 0, ifelse(FBS.Comb$BeforeWrapAny > 0 & FBS.Comb$BeforeWrapAny <=50, 2, 3)))
levels(FBS.Comb$BeforeWrap.Cat) <- c("None", "Average", "High")
FBS.Comb$DuringWrap.Cat <- as.factor(ifelse(FBS.Comb$DuringWrapAny == 0, 0, ifelse(FBS.Comb$DuringWrapAny > 0 & FBS.Comb$DuringWrapAny <= 20, 2, 3)))
levels(FBS.Comb$DuringWrap.Cat) <- c("None", "Average", "High")

#checking coding
summary(FBS.Comb$BeforeWrap.Cat) 
summary(FBS.Comb$DuringWrap.Cat)

#checking test data
summary(FBS.Comb)
str(FBS.Comb)

#LOS data
boxplot(FBS.Comb$LOS)
boxplot.stats(FBS.Comb$LOS)
sum(table(FBS.Comb$LOS[FBS.Comb$LOS <= 43]))
sum(table(FBS.Comb$LOS[FBS.Comb$LOS > 43 & FBS.Comb$LOS <= 165]))
sum(table(FBS.Comb$LOS[FBS.Comb$LOS > 165 & FBS.Comb$LOS <= 235]))
sum(table(FBS.Comb$LOS[FBS.Comb$LOS > 235]))

#creatingLOSquartiles
FBS.Comb$LOSquart<-as.factor(ifelse(FBS.Comb$LOS <= 43, 1,ifelse(FBS.Comb$LOS > 43 & FBS.Comb$LOS <= 165, 2,ifelse(FBS.Comb$LOS > 165 & FBS.Comb$LOS <= 235, 3, 4))))
levels(FBS.Comb$LOSquart)<- c("43 Days or less", "44 to 165", "166 to 235", "GT 235")
summary(FBS.Comb$LOSquart)

#Creating LOScat based on descriptive statistics, orig logitreg with quartiles, and for ease of clinical use
FBS.Comb$LOScat<-as.factor(ifelse(FBS.Comb$LOS < 30, 1,ifelse(FBS.Comb$LOS >= 30 & FBS.Comb$LOS <= 120, 2,ifelse(FBS.Comb$LOS > 121 & FBS.Comb$LOS <= 240, 3, 4))))
levels(FBS.Comb$LOScat)<- c("LT 1 month", "30 days to 4 months", "121 days to 8 months", "GT 8 months")
summary(FBS.Comb$LOScat)



FBS.Comb$BeforeSTS.Bin <- as.factor(ifelse(FBS.Comb$BeforeSTS == 0, 0, 1))
  
FBS.Comb$BeforeTCM.Bin <- as.factor(ifelse(FBS.Comb$BeforeTCM == 0, 0, 1))

#Creating age cat
boxplot.stats(FBS.Comb$ageatservicebegin[FBS.Comb$FinalRiskincDHS == "Removed From Community"])
boxplot.stats(FBS.Comb$ageatservicebegin[FBS.Comb$FinalRiskincDHS == "In Community"])




##################################################################
#FBS.Comb is my working dataset at this point, has all the features I want
################################################################## 

#############################################
#Creating test/train sets

#Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBSComb.trainingindex <- sample(1:nrow(FBS.Comb), 0.7*nrow(FBS.Comb), replace=F) #creates the indices for the sample
FBSComb.Train <- FBS.Comb[FBSComb.trainingindex,] #pull vector where training indices are true
dim(FBSComb.Train) #1708 Cases
summary(FBSComb.Train$FinalRiskincDHS) #408 cases of high risk

## select the other 30% as the testing data
FBSComb.Test <- FBS.Comb[-FBSCat.trainingindex,]
dim(FBSComb.Test) #733 Cases
summary(FBSComb.Test$FinalRiskincDHS) #198 cases of high risk

#verifying code
summary(FBS.Comb$FinalRiskincDHS)



################################################
####MODEL 1 Cat Trimmed N Variables#### 
#fitting model to training set

FBSComb.RFmodel1 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOS + ageatservicebegin +
                                  BeforeAIP + DuringAIP + 
                                  BeforeCRC + DuringCRC + 
                                  BeforeEval + DuringEval + 
                                  DuringFBS +
                                  BeforeOP.Cat + DuringOP.Cat + 
                                  BeforePartial + DuringPartial + 
                                  BeforeRTF + DuringRTF + 
                                  BeforeWrap.Cat + DuringWrap.Cat +
                                   BeforeWrapMulti + DuringWrapMulti
                                 , data = FBSComb.Train, importance = TRUE, ntree = 500)
print(FBSComb.RFmodel1)
importance(FBSComb.RFmodel1)
varImpPlot(FBSComb.RFmodel1)

##predicting for model 1 on test set.
FBSComb.Model1Predictions <- predict(FBSComb.RFmodel1, FBSComb.Test, type = "response")
##confusion matrix
FBSComb.Model1.TestConfusion <- table(FBSComb.Model1Predictions, FBSComb.Test$FinalRiskincDHS)
print(FBSComb.Model1.TestConfusion)
##accuracy
FBSComb.Model1.TestAccuracy <- sum(diag(FBSComb.Model1.TestConfusion)) / sum(FBSComb.Model1.TestConfusion)
print(FBSComb.Model1.TestAccuracy) 
##precision
FBSComb.Model1.TestPrecision <- FBSComb.Model1.TestConfusion[2,2] / sum(FBSComb.Model1.TestConfusion[2,])
print(FBSComb.Model1.TestPrecision)
##recall
FBSComb.Model1.TestRecall <- FBSComb.Model1.TestConfusion[2,2]  / sum(FBSComb.Model1.TestConfusion[,2])
print(FBSComb.Model1.TestRecall)

################################################
####MODEL 2 Cat Trimmed N Variables#### 
#fitting model to training set

FBSComb.RFmodel2 <- randomForest(FinalRiskincDHS ~ Gender_Label+RaceCat+LOSquart + ageatservicebegin +
                                   BeforeAIP + DuringAIP + 
                                   BeforeCRC + DuringCRC + 
                                   BeforeEval + DuringEval + 
                                   DuringFBS +
                                   BeforeOP.Cat + DuringOP.Cat + 
                                   BeforePartial + DuringPartial + 
                                   BeforeRTF + DuringRTF + 
                                   BeforeWrap.Cat + DuringWrap.Cat +
                                   BeforeWrapMulti + DuringWrapMulti, data = FBSComb.Train, importance = TRUE, ntree = 500)
print(FBSComb.RFmodel2)
importance(FBSComb.RFmodel2)
varImpPlot(FBSComb.RFmodel2)

##predicting for model 1 on test set.
FBSComb.Model2Predictions <- predict(FBSComb.RFmodel2, FBSComb.Test, type = "response")
##confusion matrix
FBSComb.Model2.TestConfusion <- table(FBSComb.Model2Predictions, FBSComb.Test$FinalRiskincDHS)
print(FBSComb.Model2.TestConfusion)
##accuracy
FBSComb.Model2.TestAccuracy <- sum(diag(FBSComb.Model2.TestConfusion)) / sum(FBSComb.Model2.TestConfusion)
print(FBSComb.Model2.TestAccuracy) 
##precision
FBSComb.Model2.TestPrecision <- FBSComb.Model2.TestConfusion[2,2] / sum(FBSComb.Model2.TestConfusion[2,])
print(FBSComb.Model2.TestPrecision)
##recall
FBSComb.Model2.TestRecall <- FBSComb.Model2.TestConfusion[2,2]  / sum(FBSComb.Model2.TestConfusion[,2])
print(FBSComb.Model2.TestRecall)

#######################Logistic Regressions

#data exploration
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$LOS, main = "LOS in FBS")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeAIP, main = "AIP Before")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringAIP, main = "AIP During")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeCRC, main = "CRC before")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringCRC, main = "CRC during")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeOP.Cat, main = "OP before")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringOP.Cat, main = "OP during")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeWrapMulti, main = "Concurent BHRS services before")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringWrapMulti, main = "Concurrent BHRS services during")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeWrapAny, main = "BHRS before days")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringWrapAny, main = "BHRS during days")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeWrap.Cat, main = "BHRS before cat")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringWrap.Cat, main = "BHRS during cat")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeAIP.Cat, main = "AIP before cat")
plot(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringAIP.Cat, main = "AIP during cat")


table(FBS.Comb$FinalRiskincDHS, FBS.Comb$BeforeWrap.Cat)
table(FBS.Comb$FinalRiskincDHS, FBS.Comb$DuringWrap.Cat)

#####################################################
#####Creating test and training divisions

## randomly choose 70% of the data set as training data
set.seed(23)
FBSComb.trainingindex <- sample(1:nrow(FBS.Comb), 0.7*nrow(FBS.Comb), replace=F) #creates the indices for the sample
FBSComb.Train <- FBS.Comb[FBSComb.trainingindex,] #pull vector where training indices are true
dim(FBSComb.Train) #1708 Cases
summary(FBSComb.Train$FinalRiskincDHS) #408 cases of high risk

## select the other 30% as the testing data
FBSComb.Test <- FBS.Comb[-FBSCat.trainingindex,]
dim(FBSComb.Test) #733 Cases
summary(FBSComb.Test$FinalRiskincDHS) #198 cases of high risk

#verifying code
summary(FBS.Comb$FinalRiskincDHS)
str(FBS.Comb)
######Creating LogReg

#####################LR Model 1

FBSComb.LRmodel<-glm(FinalRiskincDHS ~ 
                       Gender_Label + RaceCat + LOS + #LOS Days
                       BeforeAIP + DuringAIP + #days
                       BeforeCRC + DuringCRC + #3 level cats
                       BeforeEval + DuringEval + #binary
                       DuringFBS + #days
                       BeforeOP.Cat + DuringOP.Cat + #3 level cats
                       BeforePartial + DuringPartial + #Binary
                       BeforeRTF + DuringRTF + #Binary
                       BeforeWrap.Cat + DuringWrap.Cat + #3 level cats
                       BeforeWrapMulti + DuringWrapMulti #3 level cat for concurrent services
                       , family = binomial(link = logit), data = FBSComb.Train)
summary(FBSComb.LRmodel)

anova(FBSComb.LRmodel, test = "Chisq")

#Predicting Risk of Removal
predicted.FBSComb.Model1 <- predict(FBSComb.LRmodel,newdata=FBSComb.Test,type='response') #predicts odds of being removed from community
predicted.FBSComb.Model1 <- as.factor(ifelse(predicted.FBSComb.Model1 > 0.4,1,0)) #makes the cut at .5 and codes 0/1
table(predicted.FBSComb.Model1) #checks the summary of 0/1, looks like 1 = kids
levels(predicted.FBSComb.Model1)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.Model1)

#Confusion Matrix
LR.FBSModel1.ConfusionMatrix <- table(predicted.FBSComb.Model1, FBSComb.Test$FinalRiskincDHS)
print(LR.FBSModel1.ConfusionMatrix)

#accuracy
LR.FBSModel1.TestAccuracy <- sum(diag(LR.FBSModel1.ConfusionMatrix)) / sum(LR.FBSModel1.ConfusionMatrix)
print(LR.FBSModel1.TestAccuracy) 

#Precision
LR.FBSModel1.TestPrecision <- LR.FBSModel1.ConfusionMatrix[2,2] / sum(LR.FBSModel1.ConfusionMatrix[2,])
print(LR.FBSModel1.TestPrecision)

#recall
LR.FBSModel1.TestRecall <- LR.FBSModel1.ConfusionMatrix[2,2]  / sum(LR.FBSModel1.ConfusionMatrix[,2])
print(LR.FBSModel1.TestRecall)

######################LR Model 1 - AIP Categories

FBSComb.LRmodel2<-glm(FinalRiskincDHS ~ 
                       Gender_Label + RaceCat + LOS + #LOS Days
                       BeforeAIP.Cat + DuringAIP.Cat + #days
                       BeforeCRC + DuringCRC + #3 level cats
                       BeforeEval + DuringEval + #binary
                       DuringFBS + #days
                       BeforeOP.Cat + DuringOP.Cat + #3 level cats
                       BeforePartial + DuringPartial + #Binary
                       BeforeRTF + DuringRTF + #Binary
                       BeforeWrap.Cat + DuringWrap.Cat + #3 level cats
                       BeforeWrapMulti + DuringWrapMulti #3 level cat for concurrent services
                     , family = binomial(link = logit), data = FBSComb.Train)
summary(FBSComb.LRmodel2)



anova(FBSComb.LRmodel2, test = "Chisq")

#Predicting Risk of Removal
predicted.FBSComb.Model2 <- predict(FBSComb.LRmodel2,newdata=FBSComb.Test,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.Model2[FBSComb.Test$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.Model2[FBSComb.Test$FinalRiskincDHS == "Removed From Community"])

predicted.FBSComb.Model2 <- as.factor(ifelse(predicted.FBSComb.Model2 > 0.2,1,0)) #makes the cut at .5 and codes 0/1
table(predicted.FBSComb.Model2) #checks the summary of 0/1, looks like 1 = kids
levels(predicted.FBSComb.Model2)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.Model2)

#Confusion Matrix
LR.FBSModel2.ConfusionMatrix <- table(predicted.FBSComb.Model2, FBSComb.Test$FinalRiskincDHS)
print(LR.FBSModel2.ConfusionMatrix)

#accuracy
LR.FBSModel2.TestAccuracy <- sum(diag(LR.FBSModel2.ConfusionMatrix)) / sum(LR.FBSModel2.ConfusionMatrix)
print(LR.FBSModel2.TestAccuracy) 

#Precision
LR.FBSModel2.TestPrecision <- LR.FBSModel2.ConfusionMatrix[2,2] / sum(LR.FBSModel2.ConfusionMatrix[2,])
print(LR.FBSModel2.TestPrecision)

#recall
LR.FBSModel2.TestRecall <- LR.FBSModel2.ConfusionMatrix[2,2]  / sum(LR.FBSModel2.ConfusionMatrix[,2])
print(LR.FBSModel2.TestRecall)

###Getting pseudo r2 for model above
library(pscl)
pR2(FBSComb.LRmodel2)

##################################
#LR Before Model
##################################

#Creating this model so clinical can run predictive model based only on information they know when a member starts FBS

str(FBS.Comb)

FBSComb.LRmodelBefore<-glm(FinalRiskincDHS ~ 
                        Gender_Label + RaceCat + ageatservicebegin +
                        BeforeAIP.Cat + #3 level cat
                        BeforeCRC + #3 level cat
                        BeforeEval + #Binary
                        BeforeFBS.Bin + #Binary
                        BeforeOP.Cat + #3 level cat
                        BeforePartial + #Bin
                        BeforeRTF + #Binary
                        BeforeSTS.Bin +
                        BeforeTCM.Bin +
                        BeforeWrap.Cat + #3 level cat
                        BeforeWrapMulti + #3 level cat for concurrent services
                        DHSBefore #Binary
                      , family = binomial(link = logit), data = FBSComb.Train)
summary(FBSComb.LRmodelBefore)

anova(FBSComb.LRmodelBefore, test = "Chisq")

###Getting pseudo r2 for model above
library(pscl)
pR2(FBSComb.LRmodelBefore)

#Predicting Risk of Removal
predicted.FBSComb.ModelBefore <- predict(FBSComb.LRmodelBefore,newdata=FBSComb.Test,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelBefore[FBSComb.Test$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelBefore[FBSComb.Test$FinalRiskincDHS == "Removed From Community"])

predicted.FBSComb.ModelBefore <- as.factor(ifelse(predicted.FBSComb.ModelBefore > 0.2,1,0)) #makes the cut at .5 and codes 0/1
table(predicted.FBSComb.ModelBefore) #checks the summary of 0/1, looks like 1 = kids
levels(predicted.FBSComb.ModelBefore)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.ModelBefore)

#Confusion Matrix
LR.FBSModelBefore.ConfusionMatrix <- table(predicted.FBSComb.ModelBefore, FBSComb.Test$FinalRiskincDHS)
print(LR.FBSModelBefore.ConfusionMatrix)

#accuracy
LR.FBSModelBefore.TestAccuracy <- sum(diag(LR.FBSModelBefore.ConfusionMatrix)) / sum(LR.FBSModelBefore.ConfusionMatrix)
print(LR.FBSModelBefore.TestAccuracy) 

#Precision
LR.FBSModelBefore.TestPrecision <- LR.FBSModelBefore.ConfusionMatrix[2,2] / sum(LR.FBSModelBefore.ConfusionMatrix[2,])
print(LR.FBSModelBefore.TestPrecision)

#recall
LR.FBSModelBefore.TestRecall <- LR.FBSModelBefore.ConfusionMatrix[2,2]  / sum(LR.FBSModelBefore.ConfusionMatrix[,2])
print(LR.FBSModelBefore.TestRecall)



####################################
#Skipping cross validation for the time being, and just running all the data to create the final model
#Creating this model so clinical can run predictive model based only on information they know when a member starts FBS
####################################

##FBSComb model for before they start FBS

FBSComb.LRmodelBefore2<-glm(FinalRiskincDHS ~ 
                             Gender_Label + RaceCat + ageservicebegin +
                             BeforeAIP.Cat + #3 level cat
                             BeforeCRC + #3 level cat
                             BeforeEval + #Binary
                             BeforeFBS.Bin + #Binary
                             BeforeOP.Cat + #3 level cat
                             BeforePartial + #Bin
                             BeforeRTF + #Binary
                             BeforeSTS.Bin +
                             BeforeTCM.Bin +
                             BeforeWrap.Cat + #3 level cat
                             BeforeWrapMulti + #3 level cat for concurrent services
                             DHSBefore #Binary
                           , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.LRmodelBefore2)

anova(FBSComb.LRmodelBefore2, test = "Chisq")

###Getting pseudo r2 for model above
pR2(FBSComb.LRmodelBefore2)



#Predicting Risk of Removal
predicted.FBSComb.ModelBefore2 <- predict(FBSComb.LRmodelBefore2,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelBefore2[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelBefore2[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column
FBS.Comb$Probability <- predicted.FBSComb.ModelBefore2
summary(FBS.Comb)
boxplot(FBS.Comb$Probability ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot.stats(FBS.Comb$Probability[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot.stats(FBS.Comb$Probability[FBS.Comb$FinalRiskincDHS== "Removed From Community"])


######################
predicted.FBSComb.ModelBefore2 <- as.factor(ifelse(predicted.FBSComb.ModelBefore2 > 0.2,1,0)) #makes the cut at .5 and codes 0/1
table(predicted.FBSComb.ModelBefore2) #checks the summary of 0/1, looks like 1 = kids
levels(predicted.FBSComb.ModelBefore2)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.ModelBefore2)

#Confusion Matrix
LR.FBSModelBefore2.ConfusionMatrix <- table(predicted.FBSComb.ModelBefore2, FBS.Comb$FinalRiskincDHS)
print(LR.FBSModelBefore2.ConfusionMatrix)

#accuracy
LR.FBSModelBefore2.TestAccuracy <- sum(diag(LR.FBSModelBefore2.ConfusionMatrix)) / sum(LR.FBSModelBefore2.ConfusionMatrix)
print(LR.FBSModelBefore2.TestAccuracy) 

#Precision
LR.FBSModelBefore2.TestPrecision <- LR.FBSModelBefore2.ConfusionMatrix[2,2] / sum(LR.FBSModelBefore2.ConfusionMatrix[2,])
print(LR.FBSModelBefore2.TestPrecision)

#recall
LR.FBSModelBefore2.TestRecall <- LR.FBSModelBefore2.ConfusionMatrix[2,2]  / sum(LR.FBSModelBefore2.ConfusionMatrix[,2])
print(LR.FBSModelBefore2.TestRecall)

###################


##FBSComb model for when they finish FBS
str(FBS.Comb)
#options(scipen = 999) - turns off scientific notation
FBSComb.LRmodelAfter<-glm(FinalRiskincDHS ~ 
                              Gender_Label + RaceCat + LOScat + ageatservicebegin +
                              BeforeAIP.Cat + DuringAIP.Cat + #3 level cat
                              BeforeCRC + DuringCRC + #3 level cat
                              BeforeEval + DuringEval + #Binary
                              BeforeFBS.Bin + DuringFBS + #Binary
                              BeforeOP.Cat + DuringOP.Cat + #3 level cat
                              BeforePartial + DuringPartial + #Bin
                              BeforeRTF + DuringRTF + #Binary
                              BeforeSTS.Bin + 
                              BeforeTCM.Bin + 
                              BeforeWrap.Cat + DuringWrap.Cat + #3 level cat
                              BeforeWrapMulti + DuringWrapMulti + #3 level cat for concurrent services
                              DHSBefore #Binary
                            , family = binomial(link = logit), data = FBS.Comb)
summary(FBSComb.LRmodelAfter)

anova(FBSComb.LRmodelAfter, test = "Chisq")

###Getting pseudo r2 for model above
pR2(FBSComb.LRmodelAfter)



#Predicting Risk of Removal
predicted.FBSComb.ModelAfter <- predict(FBSComb.LRmodelAfter,newdata=FBS.Comb,type='response') #predicts odds of being removed from community

#checking mean predicted value for those removed and those not removed
mean(predicted.FBSComb.ModelAfter[FBS.Comb$FinalRiskincDHS == "In Community"])
mean(predicted.FBSComb.ModelAfter[FBS.Comb$FinalRiskincDHS == "Removed From Community"])

#adding probability column
FBS.Comb$ProbabilityAfter <- predicted.FBSComb.ModelAfter
summary(FBS.Comb)
boxplot(FBS.Comb$ProbabilityAfter ~ FBS.Comb$FinalRiskincDHS, main = "Predicted probability vs reality")
boxplot.stats(FBS.Comb$ProbabilityAfter[FBS.Comb$FinalRiskincDHS== "In Community"])
boxplot.stats(FBS.Comb$ProbabilityAfter[FBS.Comb$FinalRiskincDHS== "Removed From Community"])


######################
predicted.FBSComb.ModelAfter <- as.factor(ifelse(predicted.FBSComb.ModelAfter > 0.2,1,0)) #makes the cut at .5 and codes 0/1
table(predicted.FBSComb.ModelAfter) #checks the summary of 0/1, looks like 1 = kids
levels(predicted.FBSComb.ModelAfter)<-c("In Community", "Removed From Community")
table(predicted.FBSComb.ModelAfter)

#Confusion Matrix
LR.FBSModelAfter.ConfusionMatrix <- table(predicted.FBSComb.ModelAfter, FBS.Comb$FinalRiskincDHS)
print(LR.FBSModelAfter.ConfusionMatrix)

#accuracy
LR.FBSModelAfter.TestAccuracy <- sum(diag(LR.FBSModelAfter.ConfusionMatrix)) / sum(LR.FBSModelAfter.ConfusionMatrix)
print(LR.FBSModelAfter.TestAccuracy) 

#Precision
LR.FBSModelAfter.TestPrecision <- LR.FBSModelAfter.ConfusionMatrix[2,2] / sum(LR.FBSModelAfter.ConfusionMatrix[2,])
print(LR.FBSModelAfter.TestPrecision)

#recall
LR.FBSModelAfter.TestRecall <- LR.FBSModelAfter.ConfusionMatrix[2,2]  / sum(LR.FBSModelAfter.ConfusionMatrix[,2])
print(LR.FBSModelAfter.TestRecall)


##############################################
##Figure Creation for report

#Distribution of LOS
boxplot(FBS.Comb$LOS, main = "Figure 1: Distribution of LOS in FBS", ylab = "Days")
mtext("2012 to 2015") #creates a subtitle. Can also use \n to start a new line in main
text(y=boxplot.stats(FBS.Comb$LOS)$stats, labels = boxplot.stats(FBS.Comb$LOS)$stats, x = 1.3)

boxplot(FBS.Comb$Dose, main = "Figure 2: Distribution of Dose in FBS", ylab = "Number of FBS Claims per 100 Days in FBS")
mtext("2012 to 2015") #creates a subtitle. Can also use \n to start a new line in main
text(y=boxplot.stats(FBS.Comb$Dose)$stats, labels = round(boxplot.stats(FBS.Comb$Dose)$stats, digits = 1), x = 1.3)

boxlab<-c("AIP", "Fam FFT", "OP", "Partial", "RTF", "STS", "TCM", "Wrap BSC", "Wrap MT", "Wrap TSS", "Wrap - Any")

#default par = 
par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mar = c(10, 5, 5, 2))
boxplot(
  FBS.Subset1$BeforeAIP[FBS.Subset1$BeforeAIP>0], 
  FBS.Subset1$BeforeFamFFT[FBS.Subset1$BeforeFamFFT>0],
  FBS.Subset1$BeforeOP[FBS.Subset1$BeforeOP>0], 
  FBS.Subset1$BeforePartial[FBS.Subset1$BeforePartial>0],
  FBS.Subset1$BeforeRTF[FBS.Subset1$BeforeRTF>0],
  FBS.Subset1$BeforeSTS[FBS.Subset1$BeforeSTS>0],
  FBS.Subset1$BeforeTCM[FBS.Subset1$BeforeTCM>0],
  FBS.Subset1$BeforeWrapBS[FBS.Subset1$BeforeWrapBS>0],
  FBS.Subset1$BeforeWrapMT[FBS.Subset1$BeforeWrapMT>0],
  FBS.Subset1$BeforeWrapTSS[FBS.Subset1$BeforeWrapTSS>0],
  FBS.Subset1$BeforeWrapAny[FBS.Subset1$BeforeWrapAny>0], 
  main = "Figure 3: Distribution of Members' Service Utilization 6 Months Prior to FBS", 
  #sub = "Only members that received these services are included in the distribution",
  ylab = "Days of Service Use",
  las = 2,
  names = boxlab)
mtext("Only members that used these services are included in the distribution", side = 1, line = 7)


boxplot(
  FBS.Subset1$DuringAIP[FBS.Subset1$DuringAIP>0], 
  FBS.Subset1$DuringFamFFT[FBS.Subset1$DuringFamFFT>0],
  FBS.Subset1$DuringOP[FBS.Subset1$DuringOP>0], 
  FBS.Subset1$DuringPartial[FBS.Subset1$DuringPartial>0],
  FBS.Subset1$DuringRTF[FBS.Subset1$DuringRTF>0],
  FBS.Subset1$DuringSTS[FBS.Subset1$DuringSTS>0],
  FBS.Subset1$DuringTCM[FBS.Subset1$DuringTCM>0],
  FBS.Subset1$DuringWrapBS[FBS.Subset1$DuringWrapBS>0],
  FBS.Subset1$DuringWrapMT[FBS.Subset1$DuringWrapMT>0],
  FBS.Subset1$DuringWrapTSS[FBS.Subset1$DuringWrapTSS>0],
  FBS.Subset1$DuringWrapAny[FBS.Subset1$DuringWrapAny>0], 
  main = "Figure 4: Distribution of Members' Service Utilization Concurrent with FBS", 
  #sub = "Only members that received these services are included in the distribution",
  ylab = "Days of Service Use",
  las = 2, #rotates x axis names 90 degrees
  names = boxlab, #sets x axis names
  ylim = c(0,150), #trims y axis from 0 to 250
  yaxp = c(0, 150, 15)) #y axis, from 0 to 250, 10 tick marks
mtext("Only members that used these services are included in the distribution", side = 1, line = 7)
mtext("Days of Service use beyond 150 days trimmed.  Partial, STS, TSS, and Wrap-Any are impacted.", side = 1, line = 8)

#Figure 5 - variable importance plot (remember these may change slightly depending on if I regenerated the model)
write.csv(importance(FBSComb.RFmodel1), file = "VarImpt.csv")
varImpPlot(FBSComb.RFmodel1)
  
#Figure of model performance
par(mfrow = c(1,2)) 

boxplot(FBS.Comb$Probability ~ FBS.Comb$FinalRiskincDHS, main = "FBS Before", names = c("In Community", "Removed"), ylab = "Probability of Removal")
mtext("Predicted probability vs reality", line = 0.5)
boxplot(FBS.Comb$ProbabilityAfter ~ FBS.Comb$FinalRiskincDHS, main = "FBS Complete", names = c("In Community", "Removed"))
mtext("Predicted probability vs reality", line = 0.5)

#two confusion matrices from entire models run with all data
print(LR.FBSModelBefore2.ConfusionMatrix)
print(LR.FBSModelAfter.ConfusionMatrix)


FBS.Comb[954,]
boxplot(FBS.Comb$BeforeAIP ~ FBS.Comb$BeforeAIP.Cat)
