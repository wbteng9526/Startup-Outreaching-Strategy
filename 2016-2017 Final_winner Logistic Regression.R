library(epiDisplay)
library(interplot)
library(ggplot2)
library(dplyr)
library(XLConnect)
library(broom)
library(caret)
library(ResourceSelection)
fin_win <- read.csv(file.choose(),stringsAsFactors = F)
attach(fin_win)

datedff <- NULL
for (i in 1:length(Founding.Date)) {
  yeardff <- (Class.Year[i]-(as.numeric(substr(Founding.Date[i],1,4))))*12
  if (substr(Founding.Date[i],7,7) == "/") {
    newdff <- yeardff + (10-as.numeric(substr(Founding.Date[i],6,6)))
  } else {
    newdff <- yeardff + (10-as.numeric(substr(Founding.Date[i],6,7)))
  }
  datedff <- c(datedff,newdff)
}
fin_win["Month.dff"] <- datedff

par(mfrow=c(2,3))
qqnorm(Month.dff,ylab = "Month Difference");qqline(Month.dff,col=2)
qqnorm(age,ylab = "Age");qqline(age,col=2)
qqnorm(Num.Founders,ylab = "Number of Founders");qqline(Num.Founders,col=2)
qqnorm(Number.Investors,ylab = "Number of Investors");qqline(Number.Investors,col=2)
qqnorm(Number.Funding.Rounds,ylab = "Number of Funding Rounds");qqline(Number.Funding.Rounds,col=2)

fin_win <- fin_win[-55,]
loginvestor <- log(Number.Investors)
logfunding <- log(Number.Funding.Rounds)
par(mfrow=c(2,2))
qqnorm(loginvestor,ylab = "Number of log Investors");qqline(loginvestor,col=2)
qqnorm(logfunding,ylab = "Number of log Funding Rounds");qqline(logfunding,col=2)

# Interaction Effects
ggplot(fin_win) +
  aes(x = age, y = Winner, color = factor(Graduate.Indicator)) +
  geom_point(color = "grey") +
  geom_smooth(method = "glm") +
  ggtitle("Interaction Effect of Graduate Indicator on Age") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "bottom")

ggplot(fin_win) +
  aes(x = Month.dff, y = Winner, color = factor(Graduate.Indicator)) +
  geom_point(color = "grey") +
  geom_smooth(method = "glm") +
  ggtitle("Interaction Effect of Graduate Indicator on Month Difference") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "bottom")

ggplot(fin_win) +
  aes(x = Past.Experience, y = Winner, color = factor(Graduate.Indicator)) +
  geom_point(color = "grey") +
  geom_smooth(method = "glm") +
  coord_cartesian(ylim = c(0,0.5)) +
  ggtitle("Interaction Effect of Graduate Indicator on Past Experience") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = "bottom")


winlogit <- glm(Winner ~ Graduate.Indicator + 
                  relevel(factor(Primary.Industry),ref = 'High Tech') + 
                  relevel(factor(Stateind),ref = 'N') + 
                  Month.dff +
                  age +
                  Num.Founders +
                  Past.Experience +
                  loginvestor +
                  logfunding +
                  Month.dff:age,data=fin_win,family = binomial("logit"))

summary(winlogit)
anlogit <- tidy(anova(winlogit,test = "Chisq"))
sumlogit <- tidy(winlogit)
explogit <- tidy(exp(coef(winlogit)))
expcilogit <- tidy(exp(confint(winlogit)))
wholetable <- cbind(sumlogit,explogit[,2],expcilogit[,c(2,3)])
AIC(winlogit)
logLik(winlogit)
write.csv(wholetable,"C:/Users/thinkpad/Desktop/BU/MassChallenge/regression/winlogitsum.csv")
write.csv(anlogit,"C:/Users/thinkpad/Desktop/BU/MassChallenge/regression/anlogit.csv")
# goodness of fit
hl <- hoslem.test(winlogit$y,fitted(winlogit),g=10)
hl
# Concordance Paired
bruteforce<-function(model){
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-0
  conc<-0
  disc<-0
  ties<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    for(j in 1:nrow(zeros))
    {
      pairs_tested<-pairs_tested+1
      if(ones[i,2]>zeros[j,2]) {conc<-conc+1}
      else if(ones[i,2]==zeros[j,2]){ties<-ties+1}
      else {disc<-disc+1}
    }
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  ties_perc<-ties/pairs_tested
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}
bruteforce(winlogit)
# Sensitivity & Specificity
threshold1=0.15
predicted_values1<-ifelse(predict(winlogit,type="response")>threshold1,1,0)
actual_values1<-winlogit$y
conf_matrix1 <-table(predicted_values1,actual_values1)
sensitivity(conf_matrix1);specificity(conf_matrix1)

threshold2=0.21
predicted_values2<-ifelse(predict(winlogit,type="response")>threshold2,1,0)
actual_values2<-winlogit$y
conf_matrix2 <-table(predicted_values2,actual_values2)
sensitivity(conf_matrix2);specificity(conf_matrix2)

threshold3=0.25
predicted_values3<-ifelse(predict(winlogit,type="response")>threshold3,1,0)
actual_values3<-winlogit$y
conf_matrix3 <-table(predicted_values3,actual_values3)
sensitivity(conf_matrix3);specificity(conf_matrix3)
par(mfrow=c(1,3))
interplot(winlogit, var1="age", var2="Graduate.Indicator",ci = 0.9, point = T,ercolor = "blue",esize = 1.5) +
  geom_point(size=2,color="red") +
  xlab("Graduate Indicator(1=Graduate, 0=Not Graduate)") +
  ylab("Estimated Coeffecient of Age") +
  ggtitle("Estimated Coeffecient of Age by Graduate Indicator") +
  theme(plot.title = element_text(face = "bold"))

interplot(winlogit, var1="Month.dff", var2="Graduate.Indicator",ci = 0.9,point = T,ercolor = "blue",esize = 1.5) +
  geom_point(size=2,color="red") +
  xlab("Graduate Indicator(1=Graduate, 0=Not Graduate)") +
  ylab("Estimated Coeffecient \nof Month Difference") +
  ggtitle("Estimated Coeffecient of Month Difference \nby Graduate Indicator") +
  theme(plot.title = element_text(face = "bold"))

interplot(winlogit, var1="Past.Experience", var2="Graduate.Indicator",ci = 0.9,point = T,ercolor = "blue",esize = 1.5) +
  geom_point(size=2,color="red") +
  xlab("Graduate Indicator(1=Graduate, 0=Not Graduate)") +
  ylab("Estimated Coeffecient of Past Experience \n(1=have started a company before, 0=not)") +
  ggtitle("Estimated Coeffecient of Past Experience \nby Graduate Indicator") +
  theme(plot.title = element_text(face = "bold"))

interplot(winlogit, var1="age", var2="Month.dff",ci=0.9) +
  xlab("Month Difference") +
  ylab("Estimated Coeffecient of Age \nby Month Difference") +
  theme_bw() +
  ggtitle("Estimated Coeffecient of Age \nby Month Difference") +
  theme(plot.title = element_text(face = "bold")) +
  geom_hline(yintercept = 0,linetype = "dashed")

interplot(winlogit, var1="loginvestor", var2="logfunding",ci=0.9) +
  xlab("log(Number of Funding Rounds)") +
  ylab("Estimated Coeffecient of \nlog(Number of Investors)") +
  theme_bw() +
  ggtitle("Estimated Coeffecient of log(Number of Investors) \nby log(Number of Funding Rounds)") +
  theme(plot.title = element_text(face = "bold")) +
  geom_hline(yintercept = 1,linetype = "dashed")


train <- fin_win[,c(5,14,15,17,18,19,21,24,25,26,27,28)][1:200,]
test <- fin_win[,c(5,14,15,17,18,19,21,24,25,26,27,28)][201:247,]

cl <- fin_win[,11][1:200]
cltest <- fin_win[,11][201:247]
knnresult <- knn(train,test,cl,k=5)
ctr <- 0
for (i in 1:47){
  if (knnresult[i]==cltest[i]){
    ctr <- ctr + 1
  } else {ctr <- ctr}
}
auc.coords <- lroc(winlogit)$auc
lroc(winlogit)+
  title("ROC Curve for Estimated Winners")


probabilities <- predict(winlogit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
# age
age <- NULL
for (i in 1:length(Date.of.Birth)) {
  newage <- 2018 - as.numeric(unlist(strsplit(Date.of.Birth[i],"/"))[3])
  age <- c(age,newage)
}
fin_win["age"] <- age
write.csv(fin_win,"C:/Users/thinkpad/Desktop/BU/MassChallenge/regression/2016-2017model.csv")


library(class)
