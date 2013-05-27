### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_ANOVA_GLM/CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw'

###################################################
### code chunk number 1: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:8-11
###################################################
# changes some default display options
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:20-21
###################################################
require(car)


###################################################
### code chunk number 3: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:26-27
###################################################
parasite.data <- read.csv('http://beaconcourse.pbworks.com/f/sexAsex.csv', h=T)


###################################################
### code chunk number 4: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:31-33
###################################################
str(parasite.data)
summary(parasite.data)


###################################################
### code chunk number 5: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:38-42
###################################################
parasite.data$Sex <- factor(parasite.data$Sex, 
  levels=c(0,1), labels=c("NoSex", "Sex"))

str(parasite.data$Sex)  


###################################################
### code chunk number 6: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:47-50
###################################################
head(parasite.data$Sex) # First six observations

tail(parasite.data$Sex) # Final six observations


###################################################
### code chunk number 7: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:54-56
###################################################
parasite.data$Parasites <- factor(parasite.data$Parasites, 
  levels=c(0,1), labels=c("NoParasites", "Parasites"))  


###################################################
### code chunk number 8: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:64-74
###################################################

par(mfrow=c(2,1))
plot(FinalDiversityIndex ~ Sex, 
  data=parasite.data, 
  main="Relationship between diversity and recombination" )

plot(FinalDiversityIndex ~ Parasites, 
  data=parasite.data, 
  main="Relationship between diversity and Presence of Parasites" )
  


###################################################
### code chunk number 9: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:96-101
###################################################
model_1 <- lm(FinalDiversityIndex ~ Parasites, 
  data=parasite.data)

summary(model_1)
confint(model_1)


###################################################
### code chunk number 10: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:106-109
###################################################
div_ParN_mean <- mean(parasite.data$FinalDiversityIndex[parasite.data$Parasites=="NoParasites"])
div_ParN_mean
coef(model_1)[1] #Compare this to divParN_mean


###################################################
### code chunk number 11: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:118-120
###################################################
div_ParY_mean <- mean(parasite.data$FinalDiversityIndex[parasite.data$Parasites=="Parasites"])
div_ParY_mean


###################################################
### code chunk number 12: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:125-129
###################################################
coef(model_1)[1] + coef(model_1)[2] 
  # adding the two parameters together

sum(coef(model_1)) # An easier way of coding it  


###################################################
### code chunk number 13: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:134-136
###################################################
div_ParY_mean - div_ParN_mean # difference in means
coef(model_1)[2]  # same as the co-efficient


###################################################
### code chunk number 14: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:141-143
###################################################
head(model.matrix(model_1)) # First 6 rows of the design matrix
tail(model.matrix(model_1)) # Last 6 rows of the design matrix


###################################################
### code chunk number 15: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:151-154
###################################################
model_cell_means <- lm(FinalDiversityIndex ~ 0 + Parasites, 
  data=parasite.data) 
  # This forces a zero intercept


###################################################
### code chunk number 16: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:159-162
###################################################
coef(model_cell_means)
div_ParY_mean
div_ParN_mean


###################################################
### code chunk number 17: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:167-169
###################################################
head(model.matrix(model_cell_means)) 
tail(model.matrix(model_cell_means)) 


###################################################
### code chunk number 18: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:176-177
###################################################
summary(model_cell_means)


###################################################
### code chunk number 19: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:184-185
###################################################
confint(model_cell_means)


###################################################
### code chunk number 20: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:191-195
###################################################
par(mfrow=c(1,1))
arm::coefplot(model_1, int=T, var.las=0, 
  h.axis=T, cex.pts=2, vertical=F, 
  main= " Estimates treatment contrasts", lwd=3)


###################################################
### code chunk number 21: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:204-207
###################################################
arm::coefplot(model_cell_means, int=T, var.las=0, 
  h.axis=T, cex.pts=2, vertical=F, 
  main= " Estimates cell means", lwd=3)


###################################################
### code chunk number 22: CSE891_2012_ANOVA_tutorial_April16_2012_inClass.Rnw:216-217
###################################################
anova(model_1)


