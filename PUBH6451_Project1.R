################
### PUBH 6451 Biostatistics II
### Project 1: Do anxiety status, stress status and 
#   gender play a role in overall GPA among college aged students?
####
#### 2019Mar12
############### Mina Peyton

library("Rmisc")
sum1=summarySE(SleepStudy, measurevar="GPA", groupvar=c("AnxietyStatus"))
sum2=summarySE(SleepStudy, measurevar="GPA", groupvar=c("Stress"))
sum3=summarySE(SleepStudy, measurevar="GPA", groupvar=c("Gender"))

#how to reorder levels for graphs
str(SleepStudy$AnxietyStatus)
#Factor w/ 3 levels "moderate","normal",..: 2 2 3 2 3 1 2 2 3 1 ...
SleepStudy$AnxietyStatus = factor(SleepStudy$AnxietyStatus,levels(SleepStudy$AnxietyStatus)[c(2,1,3)])
print(levels(SleepStudy$AnxietyStatus))
#[1] "normal"   "moderate" "severe"

str(SleepStudy$Stress)
SleepStudy$Stress = factor(SleepStudy$Stress,levels(SleepStudy$Stress)[c(2,1)])
print(levels(SleepStudy$Stress))

#how to recode/rename data = need to create a new variable
#can use revalue or mapvalue
#remeber to check string and make sure it's a factor not an integar
library("plyr")
SleepStudy[,'Gender']<-factor(SleepStudy[,'Gender'])
SleepStudy$Gender2 <- revalue(SleepStudy$Gender, c("0"="Female", "1"="Male"))
print(levels(SleepStudy$Gender2))
str(SleepStudy$Gender2)

#check model assumptions for ANOVA:
# 1) independence, 2) normality, 3) equal variances (high sd/low sd < 3)
library(lattice)
histogram(~SleepStudy$GPA | SleepStudy$AnxietyStatus,
          main = "Histogram of GPA in Normal, Moderate, and Severe Anxiety Groups", 
          xlab="GPA", ylab = "Percent of Total", col = "orange", layout = c(3,1))

histogram(~SleepStudy$GPA | SleepStudy$Stress,
          main = "Histogram of GPA in Normal and High Stress Groups", 
          xlab="GPA", ylab = "Percent of Total", col = "violet", layout = c(2,1))

histogram(~SleepStudy$GPA | SleepStudy$Gender2,
          main = "Histogram of GPA in Female and Male Groups", 
          xlab="GPA", ylab = "Percent of Total", col = "blue", layout = c(2,1))

# Cook's distance = look for outliers >1 that may influence the model
cookd=cooks.distance(model)
which(cookd > 1)
#named integer(0) - this is the ouput from which(cookd >1)
plot(cookd, xlab = "Observation Number", ylab = "Cook's Distance")
abline(h=c(1), col="red")

par(mfrow=c(3,1)) #create 3 subplots in one plot
qqnorm(SleepStudy$GPA[SleepStudy$AnxietyStatus=="normal"], main = "Normal Anxiety Group")
qqline(SleepStudy$GPA)

qqnorm(SleepStudy$GPA[SleepStudy$AnxietyStatus=="moderate"], main = "Moderate Anxiety Group")
qqline(SleepStudy$GPA)

qqnorm(SleepStudy$GPA[SleepStudy$AnxietyStatus=="severe"], main = "Severe Anxiety Group")
qqline(SleepStudy$GPA)

par(mfrow=c(2,1)) #create 2 subplots in one plot
qqnorm(SleepStudy$GPA[SleepStudy$Stress=="normal"], main = "Normal Stress Group")
qqline(SleepStudy$GPA)

qqnorm(SleepStudy$GPA[SleepStudy$Stress=="high"], main = "High Stress Group")
qqline(SleepStudy$GPA)

par(mfrow=c(2,1)) #create 2 subplots in one plot
qqnorm(SleepStudy$GPA[SleepStudy$Gender2=="Female"], main = "Female Group")
qqline(SleepStudy$GPA)

qqnorm(SleepStudy$GPA[SleepStudy$Gender2=="Male"], main = "Male Group")
qqline(SleepStudy$GPA)

#Three-way ANOVA
model=lm(GPA~AnxietyStatus + Stress + Gender + AnxietyStatus:Stress +
           AnxietyStatus: Gender + Stress:Gender + AnxietyStatus:Stress:Gender, data = SleepStudy)
AnovaTypeIII <- function(model,SleepStudy) 
{library(car)
  options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  fit = lm(model,data=SleepStudy)
  anovatable = Anova(fit, type="III")
  options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))
  return(anovatable)}
#multiple linear regression output
summary(model)

#main effects model
model2=lm(GPA~Stress+ AnxietyStatus+ Gender2, data=SleepStudy)

AnovaTypeIII <- function(model2,SleepStudy) 
{library(car)
  options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  fit = lm(model2,data=SleepStudy)
  anovatable = Anova(fit, type="III")
  options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))
  return(anovatable)}

#post hoc analysis
library(multcompView)
library(lsmeans)
lsmeans = lsmeans::lsmeans ### Uses the lsmeans function
###  from the lsmeans package,
###  not from the lmerTest package
leastsquare = lsmeans(model2, 
                      "Gender2",
                      adjust="tukey")
cld(leastsquare, 
    alpha=.05,
    Letters=letters)

leastsquare2 = lsmeans(model2, 
                      "Stress",
                      adjust="tukey")
cld(leastsquare2, 
    alpha=.05,
    Letters=letters)

lsmeans(model2, pairwise~Gender2, data = SleepStudy, adjust="tukey")
lsmeans(model2, pairwise~Stress, data = SleepStudy, adjust="tukey")

#linear regression
summary(model2)

summary(model)
