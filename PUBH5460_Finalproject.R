######
##### PUBH6450 Biostatistics 1 Final Project
### National Health and Nutrition Examination Study:
## Modeling Predictors of Body Mass Index (BMI)
## 2018DEC16
############## Mina Peyton
########

#Section 1: Demographics
# Table 1. Demographics by Glucose Concentration 
# Summary Statistics for Average Glucose Concentration by Ethnicity
library(psych)

describeBy(nhanes$glucose, nhanes$ethnicity)

table.gluc.ethn=matrix(c(205, 456, 792, 204, 327, 205/1984*100, 456/1984*100, 792/1984*100, 204/1984*100, 327/1984*100, 110.77, 111.36, 106.07, 106.73, 105.19, 36.66, 41.59, 30.71, 27.87, 28.57), nrow=5, 
                         dimnames=list("Race/Ethnicity" = c("Mexican American", "Non-Hispanic Black", 
                                                            "Non-Hispanic White", "Other Hispanic", "Other/Multiracial"), 
                                       c("Count", "Percentage (of total)", 
                                         "Mean Glucose Concentration", "standard deviation")))
table.gluc.ethn

ANOVA.gluc.ethn=aov(nhanes$glucose~nhanes$ethnicity)
summary(ANOVA.gluc.ethn)

### Summary Statistics for Average Glucose Concentration by Education (College)
describeBy(nhanes$glucose, nhanes$College)

table.gluc.college=matrix(c(531, 583, 870, 531/1984*100, 583/1984*100, 870/1984*100, 103.28, 104.52, 112.52, 29.78, 27.89, 38.37), 
                            nrow=3, 
                            dimnames=list("College" = c("Bachelors", "Some/Associate", "None"), 
                                          c("Count", "Percentage (of total)", "Mean Glucose Concentration", "standard deviation")))
table.gluc.college

ANOVA.gluc.college=aov(nhanes$glucose~nhanes$college)
summary(ANOVA.gluc.college)

# Summary Statistics for Average Glucose Concentration by Income Level (Class)
describeBy(nhanes$glucose, nhanes$Class)

table.gluc.class=matrix(c(548, 777, 659, 548/1984*100, 777/1984*100, 659/1984*100, 102.56, 106.78, 113.05, 27.28, 30.52, 40.40), 
                          nrow=3, 
                          dimnames=list("Class" = c("Upper", "Middle", "Lower"), 
                                        c("Count", "Percentage (of total)", "Mean Glucose Concentration", "standard deviation")))
table.gluc.class

ANOVA.gluc.class=aov(nhanes$glucose~nhanes$Class)
summary(ANOVA.gluc.class)

#### Table 2. Demographics by Generation
## Summary Statistics for Generation by Ethnicity
table.boomer.ethn=table(nhanes$ethnicity, nhanes$BoomerPlus)
View(table.boomer.ethn)
table.boomer.ethn

100*prop.table(table.boomer.ethn, 1)
100*prop.table(table.boomer.ethn, 2)

table.boomer.ethn1=matrix(c(110, 300, 516, 136, 173, 95, 156, 276, 68, 154, 53.66, 65.79, 65.15, 66.67, 52.91, 46.34, 34.21, 34.85, 33.33, 47.09, 8.91, 24.29, 41.78, 11.01, 14.01, 12.68, 20.83, 36.85, 9.08, 20.56), 
                            nrow=5, 
                            dimnames=list("Race/Ethnicity" = 
                                            c("Mexican American", "Non-Hispanic Black", "Non-Hispanic White", "Other Hispanic", "Other/Multiracial"), 
                                          c("BoomerPlus Count", "GenXMillenial Count", "BoomerPlus % (row)", "GenXMillenial % (row)", "BoomerPlus % (column)", "GenXMillenial % (column)")))
table.boomer.ethn1						

# Chi-square test
mychi.boomer.ethn=chisq.test(table.boomer.ethn)
mychi.boomer.ethn

# Summary Statistics for Generation by Education (College)
table.boomer.college=table(nhanes$College, nhanes$BoomerPlus)
table.boomer.college

100*prop.table(table.boomer.college, 1)

100*prop.table(table.boomer.college, 2)

table.boomer.college1=matrix(c(302, 330, 603, 229, 253, 267, 56.87, 56.60, 69.31, 43.13, 43.40, 30.69, 24.45, 26.72, 48.83, 30.57, 33.78, 35.65), 
                              nrow=3, 
                              dimnames=list("College" = c("Bachelors", "Some/Associate", "None"), 
                                            c("BoomerPlus Count", "GenXMillenial Count", "BoomerPlus % (row)", "GenXMillenial % (row)", "BoomerPlus % (column)", "GenXMillenial % (column)")))
table.boomer.college1			

#### Chi-square test				
mychi.boomer.college=chisq.test(table.boomer.college)
mychi.boomer.college

# Summary Statistics for Generation by Income Level (Class)
table.boomer.class=table(nhanes$Class, nhanes$BoomerPlus)
table.boomer.class

100*prop.table(table.boomer.class, 1)
100*prop.table(table.boomer.class, 2)

table.boomer.class1=matrix(c(317,490,428,231,287,231,57.85,63.06,64.95,42.15,36.94,35.05,25.67,39.68,34.66,30.84,38.32,30.84), 
                            nrow=3, 
                            dimnames=list("Class" = c("Upper", "Middle", "Lower"), 
                                          c("BoomerPlus Count", "GenXMillenial Count", "BoomerPlus % (row)", "GenXMillenial % (row)", "BoomerPlus % (column)", "GenXMillenial % (column)")))
table.boomer.class1					

mychi.boomer.class=chisq.test(table.boomer.class)
mychi.boomer.class

###############
#####
#Section 2: Model BMI vs. Glucose Concentration
#####
##############
plot(nhanes$glucose, nhanes$bmi,
       +      xlab="Glucose Concentration",
       +      ylab="BMI",
       +      main= "BMI vs. Glucose Concentration")
cor(nhanes$glucose, nhanes$bmi, use="complete.obs")
model.glucose.bmi=lm(nhanes$bmi~nhanes$glucose)
abline(model.glucose.bmi)
par(mfrow=c(2,2))
plot(model.glucose.bmi)
par(mfrow=c(1,1))

cookd=cooks.distance(model.glucose.bmi)
which(cookd > 1)


plot(cookd, xlab = "Observation Number", ylab = "Cook's Distance")
abline(h=c(1), col="red")
summary(model.glucose.bmi)

###########
#####
### Section 3: Expanded Model BMI vs. Glucose Concentration + Generation
#####
###########
library(plyr)
nhanes$Generation=mapvalues(nhanes$BoomerPlus, from=c("BoomerPlus", "GenXMillenial"), to=c(0,1))
nhanes$Generation=as.integer(nhanes$Generation)
str(nhanes$Generation)

pairs(~bmi + glucose + Generation, 
        +       data=nhanes, 
        +       main="Scatterplot Matrix of BMI vs. Glucose Concentration + Generation", 
        +       lower.panel=NULL) 
plot(x=nhanes$glucose, y=nhanes$bmi,
       +      col=c("darkorange", "mediumturquoise")[nhanes$BoomerPlus],
       +      pch=20,
       +      ylab="BMI",
       +      xlab="Glucose Concentration", main= “BMI vs. Glucose Concentration + Generation”)
legend("topright", levels(nhanes$BoomerPlus), fill=c("darkorange", "mediumturquoise"))

library(ggplot2)
qplot(x=glucose, y=bmi, data=nhanes,
        +       colour=BoomerPlus,
        +       ylab="BMI", main = “BMI vs. Glucose Concentration + Generation”,
        +       xlab="Glucose Concentration") +
  +     labs(colour="Generation") +
  +     geom_smooth(method="lm", se=FALSE)

model.bmi.glucose.generation<-lm(bmi ~ glucose + Generation, data=nhanes)
par(mfrow=c(2,2))
plot(model.bmi.glucose.boomerplus)
par(mfrow=c(1,1))
summary(model.bmi.glucose.generation) 

#############
#######
## Section 4: Conclusion Model 1 versus Model 2
#######
############
model1=lm(bmi ~ glucose + Generation, data=nhanes)
model2=lm(bmi ~ glucose, data=nhanes)
anova(model1,model2)

###   Predictions
newdata.millenial=data.frame(glucose=90, Generation="1")
predict(model1, newdata.millenial)

newdata.boomer=data.frame(glucose=110, Generation="0")
predict(model1, newdata.boomer)


