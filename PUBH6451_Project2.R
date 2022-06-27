################
### PUBH 6451 Biostatistics II
### Project 2: Impact of sex and total cholesterol on the odds of 
# hospitalized myocardial infarction or fatal coronary heart disease (MI_FCHD) 
# among subjects in the community of Framingham, MA
#### 2019May03
############### Mina Peyton

#change sex to a factor
str(frmgham2)
frmgham2[,'SEX']<-factor(frmgham2[,'SEX'])
frmgham2[,'MI_FCHD']<-factor(frmgham2[,'MI_FCHD'])
str(frmgham2)

#calculate mean TOTCHOl by individual

# an optional code with plyr, but only one variable and unable to inclue SEX
#library(plyr)
#res.plyr <- ddply( frmgham2, .(RANDID), function(x) mean(x$TOTCHOL) )
#res.plyr

library("Rmisc")
sum=summarySE(frmgham2, measurevar="TOTCHOL",na.rm=TRUE, groupvar=c("RANDID","SEX", "MI_FCHD"))
sum

str(sum)
sum[,'SEX']<-factor(sum[,'SEX'])
sum[,'MI_FCHD']<-factor(sum[,'MI_FCHD'])
str(sum)

# did not use this code for analysis, could have as an option
# frmgham2 <- frmgham2[order(frmgham2$RANDID,frmgham2$PERIOD),]  	# Sort by ID and visit
# frmgham2 <- frmgham2[!duplicated(frmgham2$RANDID, fromLast=T),] 	# Keep last observation per ID

#scatterplot of MI_FCHD by total cholesterol and sex

#qplot(x = TOTCHOL, y = factor(MI_FCHD), color = SEX, data = sum, geom = "point")
# above code works, below code is more flexible

p = ggplot(sum, aes(x=TOTCHOL, 
                y=MI_FCHD, 
                color=SEX)) + geom_point(aes(colour=SEX)) +
  #geom_errorbar(aes(ymin=TOTCHOL-se, 
                   # ymax=TOTCHOL+se), 
                #width=.1, size=0.7) +
  # geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("blue", "orange"))

p + ggtitle("Plot of MI_FCHD by Total Cholesterol and Sex") +
  xlab("Total Cholesterol (mg/dL)") + ylab("MI_FCHD")

# Boxplot of Total cholesterol by MI_FCHD and sex
# Default plot
p2 <- ggplot(sum, aes(x=MI_FCHD, y=TOTCHOL, color = SEX))+
  geom_boxplot() +
  scale_color_manual(values = c("blue", "orange"))
p2
# Modify legend titles
p2 +labs(title="Boxplot of Total Cholesterol by MI_FCHD and Sex",
        x ="MI_FCHD", y = "Total Cholesterol (md/dL)")

#multiple logistic regression model
P1_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL + SEX*TOTCHOL, 
                   family = binomial, data = frmgham2)

P1_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL + SEX*TOTCHOL, 
                   family = binomial, data = sum)
summary(P1_mult_glm)
summary(P1_mult_glm)$coefficients
confint(P1_mult_glm)  #ofile-Likelihood CI on log scale
exp(confint(P1_mult_glm))  # Profile-Likelihood CI on OR scale

# Wald CI’s for beta parameters (if desired)
confint.default(P1_mult_glm)
exp(confint.default(P1_mult_glm))  # on OR scale

library("rms")
fit.lrm1 <- lrm(MI_FCHD ~ SEX + TOTCHOL + SEX*TOTCHOL, data= sum)
fit.lrm1
fit.lrm1$coefficients

# create total cholesteral categories
# another example: create 3 age categories 
attach(sum)
sum$TOTCHOL2[TOTCHOL <= 200] <- "1.Normal"
sum$TOTCHOL2[TOTCHOL >= 201 & TOTCHOL <= 240] <- "2.Borderline High"
sum$TOTCHOL2[TOTCHOL >= 241] <- "3.High"
detach(sum)

str(sum)
sum[,'TOTCHOL2']<-factor(sum[,'TOTCHOL2'])
str(sum)

sum2=summarySE(sum, measurevar="TOTCHOL",na.rm=TRUE, groupvar=c("SEX", "MI_FCHD", "TOTCHOL2"))
sum2

P2_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL2 + SEX*TOTCHOL2, 
                   family = binomial, data = sum)
summary(P2_mult_glm)
summary(P2_mult_glm)$coefficients
confint(P2_mult_glm)  #ofile-Likelihood CI on log scale
exp(confint(P2_mult_glm))  # Profile-Likelihood CI on OR scale

# Wald CI’s for beta parameters (if desired)
confint.default(P2_mult_glm)
exp(confint.default(P2_mult_glm))  # on OR scale

P3_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL2, 
                   family = binomial, data = sum)
summary(P3_mult_glm)
summary(P3_mult_glm)$coefficients
confint(P3_mult_glm)  #ofile-Likelihood CI on log scale
exp(confint(P3_mult_glm))  # Profile-Likelihood CI on OR scale

# Wald CI’s for beta parameters (if desired)
confint.default(P3_mult_glm)
exp(confint.default(P3_mult_glm))  # on OR scale

# Grouped Bar Plot
counts <- table(sum$MI_FCHD, sum$TOTCHOL2, sum$SEX)
counts
count2 = table(sum$MI_FCHD, sum$TOTCHOL2)
count2
barplot(count2, main="MI_FCHD by Total Cholesterol Group",
        xlab="Total Cholesterol Group",  col=c("dark blue","dark red"),
        legend = rownames(counts), beside=TRUE)

mytable = table(sum$SEX, sum$TOTCHOL2, sum$MI_FCHD)
mytable

str(project2.counts)
project2.counts[,'SEX']<-factor(project2.counts[,'SEX'])

p6 <- ggplot(project2.counts2, aes(x=smoke, y=count, color = MI_FCHD))+
  geom_point() +
  scale_color_manual(values = c("red", "dark red", "green", "dark green"))
p6
# Modify legend titles
p6 +labs(title="MI_FCHD Counts by Cholesterol Groups & Sex",
         x ="Cholesterol Groups", y = "MI_FCHD Counts")


# plots of logistic regressions 
P1_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL + SEX*TOTCHOL, 
                   family = binomial, data = sum)

newdata <- expand.grid(SEX = sum$SEX, TOTCHOL = pretty(sum$TOTCHOL, 100))
newdata$MI_FCHD <- predict(P1_mult_glm, newdata = newdata, type = "response")
library(ggplot2)
p4 = ggplot(newdata, aes(x = TOTCHOL, y = MI_FCHD, colour = SEX, group = SEX)) + 
  geom_line() + scale_color_manual(values = c("orange", "blue"))
p4 
p4 +labs(title="Probability of MI_FCHD by Total Cholesterol and Sex",
            x ="Total Cholesterol", y = "Probability of MI_FCDH")

pcts <- predict(P1_mult_glm, 
                newdata=data.frame(SEX = c("1","1","1","2","2","2"),
                                   TOTCHOL = c(150,225,300,150,225,300),
                                   type = "response", se.fit=TRUE))
pcts




P2_mult_glm <- glm(MI_FCHD ~ SEX + TOTCHOL2 + SEX*TOTCHOL2, 
                   family = binomial, data = sum)
P2_mult_glm
newdata2 <- expand.grid(SEX = sum$SEX, TOTCHOL2 = c("1.Normal", "2.Borderline High", "3.High"))
newdata2$MI_FCHD <- predict(P2_mult_glm, newdata2 = newdata2, type = "response")
library(ggplot2)
p5 = ggplot(newdata2, aes(x = TOTCHOL2, y = MI_FCHD, colour = SEX, group = SEX)) + 
  geom_line() + scale_color_manual(values = c("blue", "orange"))
p5 
p5 +labs(title="Probability of MI_FCHD by Total Cholesterol and Sex",
         x ="Total Cholesterol", y = "Probability of MI_FCDH")

#str(sum)
#sum$SEX<-mapvalues(sum$SEX, from = c(1, 2), to = c(0, 1))
#sum$SEX<-mapvalues(sum$SEX, from = c(0, 1), to = c(1, 2))

library(lattice)
histogram(~sum$MI_FCHD | sum$TOTCHOL2, main = "Histogram of MI_FCHD in Cholesterol Groups", 
          xlab="MI_FCHD")

require(graphics)
with(sum,scatter.smooth(TOTCHOL,SEX)  )

require(graphics)
with(sum,scatter.smooth(TOTCHOL,MI_FCHD)  )

sum$MI_FCHD<-mapvalues(sum$MI_FCHD, 
                             from = c(1, 2), to = c(0, 1))
str(sum)                       
                       