#############################################################################################################
#############################################################################################################
############################ Startup Outreaching Strategies #################################################

# Upload Relevant Packages
library(epiDisplay)
library(interplot)
library(ggplot2)
library(dplyr)
library(XLConnect)
library(broom)
library(caret)
library(ResourceSelection)

# Upload Datasets
fin_win <- read.csv(file.choose(),stringsAsFactors = F)
attach(fin_win)

# Calculate the number of month after the company was founded
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

# Derive qqplot for each variables
par(mfrow=c(2,3))
qqnorm(Month.dff,ylab = "Month Difference");qqline(Month.dff,col=2)
qqnorm(age,ylab = "Age");qqline(age,col=2)
qqnorm(Num.Founders,ylab = "Number of Founders");qqline(Num.Founders,col=2)
qqnorm(Number.Investors,ylab = "Number of Investors");qqline(Number.Investors,col=2)
qqnorm(Number.Funding.Rounds,ylab = "Number of Funding Rounds");qqline(Number.Funding.Rounds,col=2)

# Eliminate outlier and make transformations
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

# Establish logistic regression model
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
(hl <- hoslem.test(winlogit$y,fitted(winlogit),g=10))
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



# AUC
auc.coords <- lroc(winlogit)$auc
lroc(winlogit)+
  title("ROC Curve for Estimated Winners")


probabilities <- predict(winlogit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
