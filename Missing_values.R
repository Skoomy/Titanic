##Titanic Missing values in Age
##Discretisation de Age et fare 
##Création de Title FamilySize FamilyID
#
library(ggplot2)
library(randomForest)
library(dplyr)
library(ISLR)
library(stats)
library(corrplot)
library(scales)
library(gridExtra)
library(lattice)
library(questionr)
library(mice)
library(rpart)



titanic <- read.csv("~/ensiie_S4/projet_MOST/train.csv")
test<-read.csv("~/ensiie_S4/projet_MOST/test.csv")
gendermodel<-read.csv("~/ensiie_S4/projet_MOST/gendermodel.csv")

names(titanic) #knowing the column name
str(titanic) #Know the type of your variables 
md.pattern(titanic) ## 177 missing values in Age

md.pattern(test) # missing 1 fare 86 age 

#Fusioning the two datasets 
test$Survived <-gendermodel$Survived
alldata_1 <- rbind(titanic,test)

options(digits=1)

md.pattern(alldata_1)

#  string
alldata_1$Name <- as.character(alldata_1$Name)
# variable: Title
alldata_1$Title <- sapply(alldata_1$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
alldata_1$Title <- sub(' ', '', alldata_1$Title)
# Aggreg
alldata_1$Title[alldata_1$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
alldata_1$Title[alldata_1$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
alldata_1$Title[alldata_1$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

alldata_1$Title <- factor(alldata_1$Title)
#variable: Family size
alldata_1$FamilySize <- alldata_1$SibSp + alldata_1$Parch + 1

# variable: Family
alldata_1$Surname <- sapply(alldata_1$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
alldata_1$FamilyID <- paste(as.character(alldata_1$FamilySize), alldata_1$Surname, sep="")
alldata_1$FamilyID[alldata_1$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(alldata_1$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
alldata_1$FamilyID[alldata_1$FamilyID %in% famIDs$Var1] <- 'Small'

alldata_1$FamilyID <- factor(alldata_1$FamilyID)


#Suppresion des variables CABIN ,TICKET Surname,et Name  no more usefull S
greping<--grep('Cabin|Surname|Name|Ticket',names(alldata_1))
alldata_1<-alldata_1[,greping]

####################################### Missing Value ########################################"
# Age valeur manquantes
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=alldata_1[!is.na(alldata_1$Age), ], method="anova")

alldata_1$Age[is.na(alldata_1$Age)] <- predict(Agefit, alldata_1[is.na(alldata_1$Age),])


###Fare valeur manquantes 
# Only one missing values in FAre remplace with the median
alldata_1$Fare[which(is.na(alldata_1$Fare))] <- median(alldata_1$Fare, na.rm=TRUE)
#############################################################################
md.pattern(alldata_1)


##Dicretisations des varaibles continues

#Discrétisation Age
q<-quantile(alldata_1$Age,seq(0,1,by=0.1))
qage<-cut(alldata_1$Fare,q)
 qage<-cut(alldata_1$Age,c(0,18,28,40,Inf))
tabAge<-table(qage,alldata_1$Survived)
prop.table(tabAge,1)
x<-as.data.frame((prop.table(tabAge,1)[,2]))
prop.table(tabAge,1)[,2]
y<-x$`(prop.table(tabAge, 1)[, 2])`
survie<-as.vector(y)
na<-as.vector(rownames(x))
df<-data.frame(na,survie)
df$na<-as.character(df$na)
df$na<-as.factor(df$na)

ggplot(df,aes(x=na,weight=survie))+
  geom_bar(fill="#0072B2")+
  geom_hline(yintercept=0.35,colour="orange",linetype="dashed",size=2)+
  labs(y="Taux de Survie",title="Discrétisation de Age",x="age")

Age<-cut(alldata_1$Age,c(0,18,28,40,Inf))
levels(Age)<-c("enfant","JeuneAdulte","Adulte","Senior")



##Fare
qplot(Survived,Fare,data=alldata_1,geom="boxplot")
q=quantile(alldata_1$Fare)
#q<-unique(quantile(alldata_1$Fare,seq(0,1,by=)))
qfare<-cut(alldata_1$Fare,q)
tab<-table(qfare,alldata_1$Survived)
prop.table(tab,1)
x<-as.data.frame((prop.table(tab,1)[,2]))
prop.table(tab,1)[,2]
y<-x$`(prop.table(tab, 1)[, 2])`
survie<-as.vector(y)
na<-as.vector(rownames(x))
df<-data.frame(na,survie)
df$na<-as.character(df$na)
df$na<-as.factor(df$na)

ggplot(df,aes(x=na,weight=survie))+
  geom_bar(fill="#CC79A7")+
  geom_hline(yintercept=0.3,colour="orange",linetype="dashed",size=2)+
  labs(y="Taux de Survie",title="Discrétisation de FARE",x="FARE")

Fare<-cut(alldata_1$Fare,c(-1,15,32,Inf))
levels(Fare)<-c("LowPrice","MediumPrice","HighPrice")

############Discretization FamilySize
table(alldata_1$FamilySize)
q<-quantile(alldata_1$Pa)
q<-cut(alldata_1$FamilySize,c(0,2,4,Inf))

tabFam<-table(q,alldata_1$Survived)
prop.table(tabFam,1)
x<-as.data.frame((prop.table(tabFam,1)[,2]))
prop.table(tabFam,1)[,2]
y<-x$`(prop.table(tabFam, 1)[, 2])`
survie<-as.vector(y)
na<-as.vector(rownames(x))
df<-data.frame(na,survie)
df$na<-as.character(df$na)
df$na<-as.factor(df$na)

ggplot(df,aes(x=na,weight=survie))+
  geom_bar(fill="#D55E00")+
  geom_hline(yintercept=0.3,colour="black",linetype="dashed",size=2)+
  labs(y="Taux de Survie",title="Discrétisation de Family Size",x="Size")+ 
  annotate("text", x = 3, y = 0.32, label = "line at 30%",size=10)

FamilySize<-cut(alldata_1$FamilySize,c(0,2,4,Inf))
levels(FamilySize)<-c("SoloBinome","Moyenne","Big")
#Convert in factor

alldata_1$Survived<-as.factor(alldata_1$Survived)
alldata_1$Pclass<-as.factor(alldata_1$Pclass)
str(alldata_1)




#continu<--grep('(PassengerId|Survived|Age|Fare|SibSp|Parch|FamilySize')


#########Normality Test 
##Fare
ggplot(alldata_1[complete.cases(alldata_1),],aes(Fare))+geom_density()+labs(title="Density of Fare")
Fare<-alldata_1$Fare
Fare<-Fare[complete.cases(Fare)]
shapiro.test(Fare)
##Age
ggplot(alldata_1[complete.cases(alldata_1),],aes(Age))+geom_density()+labs(title="Density of Age")

Age<-Fare[complete.cases(Age)]
shapiro.test(alldata_1$Age)
# library(discretization)
# all<-disc.Topdown(alldata_1[,"Age"],method=1)
# disc<-all$Disc.data
# fix(disc)
################################### Clustering #####################
library(cluster)
set.seed(321)
fit.pam<-pam(alldata_1[,c(-1,-2)],k=4,stand=TRUE)
fit.pam$medoids
fit.pam$medoids
clusplot(fit.pam,main="  ")
table(alldata_1$Survived,fit.pam$clustering)


alldata_3<-alldata_1
alldata_3$clustering<-factor(fit.pam$clustering)
names(alldata_3)
ggplot(data=alldata_3[,-1],aes(x=Age,y=,color=Sex,shape=Sex))+
  geom_point()

str(alldata_1)

library(NbClust)
wssplot(alldata_1[,-1])
nc<-NbClust(exprs(alldata_3[,c(-1,-2)]),min.nc=2,max.nc = 15,method="kmeans")

alldata_3<-as.data.frame(alldata_1)

#########################################
alldata_complete<-na.omit(alldata_1)
by(alldata_complete[,c("Age","Fare","FamilySize")],list(Survived=alldata_complete$Survived),summary)


Age
Fare
FamilySize
alldata_2<-alldata_1
#alldata_2$Age<-Age
alldata_2$Fare<-Fare
alldata_2$FamilySize<-FamilySize
sum(is.na(alldata_2))
str(alldata_2)

##############Correlations entre varailes explicatives V DE CRAMER et chi-2

alldata_cramer2<-alldata_2[,c(-1,-2)]
cramer<-matrix(NA,ncol(alldata_cramer2),ncol(alldata_cramer2)) 

for (i in ( 1:ncol(alldata_cramer2)))
{for (j in (1:ncol(alldata_cramer2))) {
  cramer[i,j]<-cramer.v(table(alldata_cramer2[,i],alldata_cramer2[,j]))
}
}

colnames(cramer)<-colnames(alldata_cramer2)
rownames(cramer)<-colnames(alldata_cramer2)
corrplot(cramer,type="upper",t1.srt=45,t1.col="black",diag=F,addCoef.col ="black",addCoefasPercent= T)

cramer<-matrix(NA,ncol(alldata_cramer2),3) 

for (i in ( 1:ncol(alldata_cramer2)))
  {cramer[i,1]<-names(alldata_cramer2[i])
  cramer[i,2]<-sqrt(chisq.test(table(alldata_cramer2[,i],alldata_2$Survived))$statistic/(length(alldata_cramer2[,i])))
  cramer[i,3]<-chisq.test(table(alldata_cramer2[,i],alldata_2$Survived))$p.value }

colnames(cramer)<-c("variable","V de Cramer","p-value Chi-2")

vcramer<-cramer[order(cramer[,2],decreasing = TRUE),]
a<-as.vector(vcramer[,1])
VofCram<-as.vector(vcramer[,2])
str(barCramer)
barCramer<-data.frame(a,VofCram)
barCramer$a<-as.character(barCramer$a)
barCramer$a<-as.factor(barCramer$a)
barCramer$VofCram<-as.numeric(vcramer[,2])
#V de Cramer Graphique 

barCramer <- transform(barCramer,a =reorder(a, order(VofCram,decreasing=TRUE)))

###Ploting the Variables Importance via Cramer 
ggplot(barCramer,aes(x=a,weight=VofCram,fill=a))+geom_bar()+scale_fill_brewer(palette="BrBG",direction=-1)+
  labs(title="V de Cramer variablesexplicatives \nVariables discretize",x="Variables",y="V de Cramer")+
  guides(fill = guide_legend(title = "Variables"))
  
######################

titanic_1 <- alldata_1[1:891,]
names(titanic_1)
test_1 <- alldata_1[892:1309,]
str(test_1)

titanic_2<-alldata_2[1:891,]
test_2<-alldata_2[892:1309,]
str(titanic_2)
str(test_2)
colnames(titanic_2)==colnames((test_2))
#######################


###SVM a noyau radial  
library(e1071)
library(pROC)
library(ROCR)
svmrad=tune.svm(Survived ~. , data=titanic_2, kernel="radial",
                cost=seq(0.1,5,by=0.1),gamma=c(0.001,0.5,by=0.01),
                validation.x=test_2[,-2],validation.y=test_2[,2],
                tunecontrol=tune.control(sampling="fix",fix=1))
summary(svmrad) ##Cost=3 gamma=0.001


svmrad<-svm(Survived ~ . ,data= titanic_2[,-1], kernel="radial",
            gamma=0.001,cost=3,probability=T)

solution_2<-data.frame(PassengerId=test_2$PassengerId,Survivded=test_2$Survived)
solution_2$svmrad<-predict(svmrad,test_2[,-2],probability=FALSE)
pred<-prediction(attr(solution_2$svmrad,"probabilities")[,1],
                 gendermodel$Survived,label.ordering=c(1,0))
solution_2$svmrad

performance(pred,"auc")@y.values[[1]]

mean(solution_2$svmrad != gendermodel$Survived) #7 soit 0.02

plot(ecdf(solution_2$svmrad[solution_2$Survivded==0]),
     main="Fonction de répartition du score",
     col="blue",pch=16)
plot(ecdf(solution_2$svmrad[solution_2$Survivded==1]),
     main="Fonction de répartition du score",
     col="red",pch=17,add=T)
legend("bottomright",c("score=0","score=1"),pch=c(16,17),col=c("blue","red"),lwd=1)
solution<-data.frame(PassengerId=test_2$PassengerId,Survived=solution_2$svmrad)
write.csv(solution,file="~/ensiie_S4/svmrad_titanic2_model.csv",row.names=F)


########Same on TITANIC 1
svmrad=tune.svm(Survived ~. , data=titanic_1, kernel="radial",
                cost=seq(0.1,5,by=0.1),gamma=c(0.001,0.5,by=0.01),
                validation.x=test_1[,-2],validation.y=test_1[,2],
                tunecontrol=tune.control(sampling="fix",fix=1))
summary(svmrad) ##Cost=3 gamma=0.001


svmrad<-svm(Survived ~ . ,data= titanic_1[,-1], kernel="radial",
            gamma=0.001,cost=3,probability=T)

solution_1<-data.frame(PassengerId=test_1$PassengerId,Survivded=test_1$Survived)
solution_1$svmrad<-predict(svmrad,test_1[,-2],probability=TRUE)
pred<-prediction(attr(solution_1$svmrad,"probabilities")[,1],
                 gendermodel$Survived,label.ordering=c(1,0))
solution_1$svmrad

performance(pred,"auc")@y.values[[1]]

sum(solution_1$svmrad != gendermodel$Survived) #10

plot(ecdf(solution_1$svmrad[solution_1$Survivded==0]),
     main="Fonction de répartition du score",
     col="blue",pch=16)
plot(ecdf(solution_1$svmrad[solution_1$Survivded==1]),
     main="Fonction de répartition du score",
     col="red",pch=17,add=T)
legend("bottomright",c("score=0","score=1"),pch=c(16,17),col=c("blue","red"),lwd=1)
solution<-data.frame(PassengerId=test_1$PassengerId,Survived=solution_1$svmrad)
write.csv(solution,file="~/ensiie_S4/svmrad_titanic1_model.csv",row.names=F)
