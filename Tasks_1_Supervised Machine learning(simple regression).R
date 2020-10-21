##                  My Name:-SURAJ RAVINDRA BHADANE.
###               Predicton Using Supervided Learning.

#Import data 
data<-read.csv(url("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"))
data
str(data)
names(data)
View(data)

###  Scores is dependand on Hours that means Scores is dependand(response) variable and  
###  Hours is independand variable.

# Firstly we check Assumptions of Regression analysis

## 1.Independancy (is data independant or not)
library(lawstat)
runs.test(data$Scores)
     #Interpretaton:- p-value(0.8315) > 0.05(alpha) tells us that,our response is independand  

##2.Normality
par(mfrow = c(2,2))
hist(data$Scores)
boxplot(data$Scores)
qqnorm(data$Scores)       #normal probability plot
qqline(data$Scores,col = 2)
      #interpretation:- from Plots Plot we can say that our response(Scores) dose not come from normal
shapiro.test(data$Scores) # normality test
      #interpretaion :- p-value = 0.04344 < 0.05,means our resonse(scores) not follow normality assumption.

#Fit the Rgression model
lm.fit<-lm(data$Scores~data$Hours)
anova(lm.fit)
summary(lm.fit)
      #Interpretation:- Residual standard error: 5.603(near to ZERO) shows model is better
      #                 R-squared:  0.9529 tell us that 95.29% variability explain by Hours in Scores
      #                 Hours = P-value < 0.001 means their is a significance diffrence


      
##Anothor Assumption is that error(residual) follows independancy and normality
res.lm.fit<-residuals(lm.fit)
res.lm.fit
runs.test(res.lm.fit)     #Check independancy of residuals
       #Interpritation :- p-value = 0.5447 > 0.05 shows Residuals are independant from each other

shapiro.test(res.lm.fit)  #Check normality of residual
       #interpretation :- p-value = 0.02246 < 0.05 , residual(error) not follow Normality


#Correlation between Hours and Score
cor(data$Hours,data$Scores)  # Correlation between study hours and scores
      #Interpretaton :- 0.90401(near to 1) means,Scores and Hours are Highly Correlated

lm.fit$coefficients
as.numeric(lm.fit$coefficients)
a<-as.matrix(fitted.values(lm.fit))
b<-as.matrix(data$Scores)
data.frame("Fitted Score"=a,"original Score"=b)

sum(a)
sum(b)

p<-par(mfrow=c(2,2))
plot(lm.fit,col=20)


#Predication
hours<-9.25
scorecap<-2.4837+9.7758*hours
scorecap
  #Interpretation:-If Hours=9.25 then Score=92.90985

#Graphical Representation
par(mfrow=c(1,1))
plot(data$Hours,data$Scores,pch=15,col ="blue",type = "p")
abline(lm.fit,col="black")
points(hours,scorecap,pch = 17,col = "red")

