########
#### PUBH6450 Biostatistics I
#### Midterm Project: 
# Framingham Study: Difference in Average MRW Between High Cholesterol Level
# and Low Cholesterol Level Groups 
### 2018OCT28
############### Mina Peyton


# New Binary and Categorical Variable
library(plyr)
framinghamA$HighChol<-mapvalues(framinghamA$Chol_Status, 
                                 from = c("High", "Borderline", "Desirable"), to = c(0, 1, 1))

table(framinghamA$HighChol, framinghamA$Chol_Status)

# Borderline   Desirable       High
# 1        206       157         0
# 0          0         0         137


framinghamA$HighChol3<-mapvalues(framinghamA$Chol_Status, from = c("High", "Borderline", "Desirable"), to = c("High", "Low", "Low"))

table(framinghamA$HighChol3, framinghamA$Chol_Status)

# Exploratory Data Analysis
plot(framinghamA$HighChol3, framinghamA$MRW, main="MRW in High and Low Cholesterol Groups", xlab="Cholesterol Group", ylab="MRW")

library(lattice)
histogram(~framinghamA$MRW | framinghamA$HighChol3, main = "Histogram of MRW in High and Low Cholesterol Groups", xlab="MRW")

library(psych)
describeBy(framinghamA$MRW, framinghamA$HighChol3)

# Descriptive statistics by group 
# group: Low
# vars   n   mean   sd      median trimmed   mad min max range skew kurtosis   se
# X1    1 363 116.32 18.3    114      114.9     16.31  82 200   118      0.93     1.65 0.96
--------------------------------------------------------------------------- 
#  group: High
# vars   n     mean    sd    median trimmed   mad min max range skew kurtosis   se
# X1    1 137 120.9  20.75    119      118.71    17.79  89 216   127  1.8     5.56 1.77

qqnorm(framinghamA$MRW[framinghamA$HighChol==1], main = "Low Cholesterol Group")
qqline(framinghamA$MRW)


qqnorm(framinghamA$MRW[framinghamA$HighChol==0], main = "High Cholesterol Group")
qqline(framinghamA$MRW)

# Inference
t.test(framinghamA$MRW~framinghamA$HighChol3, conf.level=0.95,
        mu=0, alternative="two.sided", var.equal=TRUE)

# Two Sample t-test

# data:  framinghamA$MRW by framinghamA$HighChol3
# t = -2.4021, df = 498, p-value = 0.01667
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -8.3178941 -0.8330982
# sample estimates:
#   mean in group Low mean in group High 
# 116.3223           120.8978
