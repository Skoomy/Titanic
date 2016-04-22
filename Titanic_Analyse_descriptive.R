##Titanic Kaggle Analyse descriptive

library(ggplot2) #Beautifull plot
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
library(nnet)
library(ROCR)
library(tree)
library(rpart)
library(pROC) 
library(party)
library(e1071)


titanic <- read.csv("~/ensiie_S4/projet_MOST/train.csv")
test<-read.csv("~/ensiie_S4/projet_MOST/test.csv")
gendermodel<-read.csv("~/ensiie_S4/projet_MOST/gendermodel.csv")

names(titanic) #knowing the column name
str(titanic) #Know the type of your variables 
md.pattern(titanic) ## 177 missing values in Age

dim(titanic)

#Categorial variable



#Transfoorm in factor
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Survived<-as.factor(titanic$Survived) #Survived? yes = 1 no= 0
titanic$Embarked<-as.factor(titanic$Embarked)
titanic$Sex<-as.factor(titanic$Sex)


###PieSex


total=nrow(titanic)

dfSex <- data.frame(
  group = c("Male", "Female"),
  value = c(314, 577)
)
pieSex<- ggplot(dfSex, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pieSex <- pieSex + coord_polar("y", start=0)
pieSex=pieSex + scale_fill_brewer("Sex",palette = "Reds")+ theme_classic()+
  theme(axis.text.x=element_blank())+ggtitle("Gender")+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/total)), size=5)
######################PlotGender
plotSex=ggplot(titanic, aes(x=Sex, y=Survived,fill=Sex)) + geom_bar(stat="identity") +
  facet_grid(. ~ Survived) +scale_fill_brewer(palette="Reds")+theme_dark()+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))


grid.arrange(pieSex,plotSex, nrow=2, ncol=1) #to put them both on same page 



###############Pie Embarked
table(titanic$Embarked)

dfEmbarked <- data.frame(
  group = c("Cherbourg", "Queenstown","Southampton"),
  value = c(168,77,646)
)
pieEmbarked<- ggplot(dfEmbarked, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pieEmbarked <- pieEmbarked + coord_polar("y", start=0)
pieEmbarked=pieEmbarked + scale_fill_brewer("Embarked",palette = "Blues")+ theme_classic()+
  theme(axis.text.x=element_blank())+ggtitle("Embarkation Port")+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/total)), size=5)

#Embarked VS Survived
plotEmbarked=ggplot(titanic, aes(x=Embarked, y=Survived,fill=Embarked)) + geom_bar(stat="identity") +
  facet_grid(. ~ Survived) +scale_fill_brewer(palette="Blues")+theme_dark()+
  theme(strip.text = element_text(face="bold", size=rel(1.5),colour="black"),
        strip.background = element_rect(fill="lightblue",colour="black",
                                        size=1))

grid.arrange(pieEmbarked,plotEmbarked, nrow=2, ncol=1)



#####################################PiePclass
table(titanic$Pclass)

dfPclass <- data.frame(
  group = c("1st Class", "2nd Class","3rd Class"),
  value = c(216,184,491)
)
piePclass<- ggplot(dfPclass, aes(x="", y=value ,fill=group))+
  geom_bar(width = 1, stat = "identity")
piePclass <- piePclass + coord_polar("y", start=0)
piePclass=piePclass + scale_fill_brewer("Pclass",palette = "Greens")+ theme_classic()+
  theme(axis.text.x=element_blank())+ggtitle("Passenger Class")+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/total)), size=5)

#Survived vs Pclass
plotPclass=ggplot(titanic, aes(x=Pclass, y=Survived,fill=Pclass))+ geom_bar(stat="identity")+
  facet_grid(. ~ Survived) +scale_fill_brewer(palette="Greens")+theme_dark()+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))
piePclass
plotPclass
grid.arrange(piePclass,plotPclass, nrow=2, ncol=1)


### Fare 
FareDensity=ggplot(titanic, aes(titanic$Fare)) +
  geom_density(alpha=0.2)+labs(title= "Fare Density",x="Fare")

##Age density #177 missing_values
ggplot(titanic, aes(titanic$Age)) +
  geom_density(alpha=0.2)+labs(title= "Fare Density",x="Fare")
  

