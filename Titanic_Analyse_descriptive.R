##Titanic Kaggle

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

test$Survived<-NA
alldata_1 <- rbind(titanic,test)

dim(titanic)

###PieSex


table(titanic$Sex)

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
######################PlotSex
plotSex=ggplot(titanic_1, aes(x=Sex, y=Survived,fill=Sex)) + geom_bar(stat="identity") +
  facet_grid(. ~ Survived) +scale_fill_brewer(palette="Reds")+theme_dark()+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))

grid.arrange(pieSex,plotSex, nrow=2, ncol=1)
