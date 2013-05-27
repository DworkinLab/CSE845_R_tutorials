### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE845_InterpretingMultipleRegression/CSE845_MLR_part2/FitzpatrickData.Rnw'

###################################################
### code chunk number 1: FitzpatrickData.Rnw:47-49
###################################################
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: FitzpatrickData.Rnw:52-56
###################################################
require(car)
require(arm)
require(effects)
require(MASS)


###################################################
### code chunk number 3: FitzpatrickData.Rnw:59-110
###################################################
# condition number for multi-colinearity
ConditionNumber <- function(X){
    mod.X <- model.matrix(X)
    eigen.x <- eigen(t(mod.X) %*%mod.X)
    eigen.x$val # eigenvalues from the design matrix
    sqrt(max(eigen.x$val)/min(eigen.x$val))
}


Rsq <- function( model ){
	fitted.variance <- var(model$fitted)
	total.variance	<- var(model$fitted) + var(model$resid)
	fitted.variance / total.variance
}

# You could also just use the following to give you the unadjusted R2
# summary(your.model.object)$r.sq

# Or for the adjusted R2
#summary(model.crap)$adj.r


# Here is the function to compute all of the partial R2, the overall model R2 (unadjusted and adjusted). This should work for any lm model object.
PRsq <- function( model ){
	residual.variance <- var(model$resid)
	variables <- attr(terms(model), "term.labels")
		model.length <- length(variables)
		variable.name <- rep(NA, model.length )
		partial.Rsq <- rep(NA, model.length )
		univariate.Model.Rsq <- rep(NA, model.length )
			
	for (i in 1:model.length){
		variable.name[i] <- variables[i]
		drop <- parse( text=variables[i] )
		new.formula <- as.formula( paste( ".~.-", variables[i], sep=""))
		new.model <- update(model, new.formula )
		partial.Rsq[i] <- (var(new.model$resid) - residual.variance)/ var(new.model$resid)
		
		new.formula.univariate <- as.formula( paste( ".~", variables[i], sep=""))
		univariate.model <- update(model, new.formula.univariate)
		univariate.Model.Rsq[i] <- summary(univariate.model)$r.sq
		}
	
	R2 <- Rsq( model )
	adj.R2 <- summary(model)$adj.r
	
	partials <- data.frame(partial.Rsq, univariate.Model.Rsq )
	row.names(partials) <- variable.name
	
	list(FullModelRsquared=R2, FullModelAdjustedR2 = adj.R2, partials=partials	)
}


###################################################
### code chunk number 4: FitzpatrickData.Rnw:125-127
###################################################
# Normally read it in like this
# spawn_data <- read.table("http://datadryad.org/bitstream/handle/10255/dryad.37940/Fitzpatrick_et_al_Selection_Data.txt", h=T, skip=1) # skipping first line of labels but there were a couple of minor issues that needed to be fixed.


###################################################
### code chunk number 5: FitzpatrickData.Rnw:130-131
###################################################
spawn_data <- read.table("http://beaconcourse.pbworks.com/f/Fitzpatrick_et_al_Selection_Data.txt", h=T)


###################################################
### code chunk number 6: FitzpatrickData.Rnw:135-138
###################################################
dim(spawn_data)
str(spawn_data)
cov(spawn_data)


###################################################
### code chunk number 7: FitzpatrickData.Rnw:141-142
###################################################
cor(spawn_data)


###################################################
### code chunk number 8: FitzpatrickData.Rnw:148-150
###################################################
spawn_data$ProportionFertilized <- with(spawn_data, 
    ProportionFertilized/mean(ProportionFertilized))


###################################################
### code chunk number 9: FitzpatrickData.Rnw:153-154
###################################################
mean(spawn_data$ProportionFertilized)


###################################################
### code chunk number 10: FitzpatrickData.Rnw:160-165
###################################################
spawn_data_scaled <- data.frame(spawn_data[,1], scale(spawn_data[,-1]))
colnames(spawn_data_scaled)[1] <- "ProportionFertilized" # add name of column back on.

summary(spawn_data_scaled)
str(spawn_data_scaled)


###################################################
### code chunk number 11: FitzpatrickData.Rnw:168-170
###################################################
#This just shows that all the elements are equal.
identical(cor(spawn_data_scaled[,2:8]), cov(spawn_data_scaled[,2:8]))


###################################################
### code chunk number 12: FitzpatrickData.Rnw:176-185
###################################################
par(mfrow=c(2,1))
plot(ProportionFertilized ~ MeanFlagellumLength_uM, 
    data=spawn_data_scaled, pch=16)
with(spawn_data_scaled,
    lines(lowess(x=MeanFlagellumLength_uM, y =ProportionFertilized), lwd=2, col="red"))
plot(ProportionFertilized ~ SpermMotilityPC1, 
    data=spawn_data_scaled, pch=16)
with(spawn_data_scaled,
    lines(lowess(x=SpermMotilityPC1, y =ProportionFertilized), lwd=2, col="red"))


###################################################
### code chunk number 13: FitzpatrickData.Rnw:192-197
###################################################
par(mfrow=c(1,1))
plot(MeanFlagellumLength_uM ~ SpermMotilityPC1, 
    data=spawn_data_scaled, pch=16)
with(spawn_data_scaled,
    lines(lowess(x=SpermMotilityPC1, y =MeanFlagellumLength_uM), lwd=2, col="red"))


###################################################
### code chunk number 14: FitzpatrickData.Rnw:205-209
###################################################
model_1 <- lm(ProportionFertilized ~ MeanFlagellumLength_uM + SpermMotilityPC1 +
     MeanFlagellumLength_uM:SpermMotilityPC1 + I(SpermMotilityPC1^2),
     data= spawn_data_scaled)
summary(model_1)


###################################################
### code chunk number 15: FitzpatrickData.Rnw:215-216
###################################################
print(cbind(summary(model_1)$coef[,1:2], confint(model_1)), digits=2)


###################################################
### code chunk number 16: FitzpatrickData.Rnw:226-227
###################################################
avPlots(model_1)


###################################################
### code chunk number 17: FitzpatrickData.Rnw:232-233
###################################################
eff <- allEffects(mod=model_1)


###################################################
### code chunk number 18: FitzpatrickData.Rnw:237-239
###################################################
plot(eff[1], ylab="Relative Fitness")
#plot(eff[2], ylab="Relative Fitness") # for interaction term.


###################################################
### code chunk number 19: FitzpatrickData.Rnw:251-254
###################################################
n <- 50 # # of points we want
MF <- with(spawn_data_scaled, seq(min(MeanFlagellumLength_uM), max(MeanFlagellumLength_uM), length.out=n))
SM <- with(spawn_data_scaled, seq(min(+ SpermMotilityPC1), max(SpermMotilityPC1), length.out=n))


###################################################
### code chunk number 20: FitzpatrickData.Rnw:258-259
###################################################
params <- coef(model_1) # parameters from the model


###################################################
### code chunk number 21: FitzpatrickData.Rnw:264-266
###################################################
f <- function(x=MF,y=SM){
	z <- params[1] + params[2]*x + params[3]*y + params[4]*(y^2) + params[5]*x*y}


###################################################
### code chunk number 22: FitzpatrickData.Rnw:270-271
###################################################
z <- outer(X=MF, Y=SM, f)


###################################################
### code chunk number 23: FitzpatrickData.Rnw:277-278
###################################################
res = persp(x=MF,y=SM,z, theta=75, phi=0,, zlim=c(0.7, 1.4))


###################################################
### code chunk number 24: FitzpatrickData.Rnw:282-298
###################################################
# look at ?persp for more information
persp(x = MF,y = SM, z,
    theta=70, phi=0, box=T, col="#FF000075", shade=0.05, ticktype="detailed", lphi=180,
    xlab= "MF", ylab = "SM", zlab="predicted fitness", axes=T, 
    xlim=range(MF), ylim=range(SM), zlim=c(0.7, 1.4), border=F,
    main="influence of sperm motility and length on fitness")
    
# uses the trans3d function to project into the right perspective based on pmat=res     
mypoints <- with(spawn_data_scaled, 
    trans3d(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, z=ProportionFertilized, 
    pmat=res))   

#adds points back on in the correct perspective
points(mypoints, pch=16, cex=1, 
    col=densCols(x=spawn_data_scaled$MeanFlagellumLength_uM, 
        y=spawn_data_scaled$SpermMotilityPC1, nbin=20))


###################################################
### code chunk number 25: FitzpatrickData.Rnw:306-313
###################################################
#scatter3d(ProportionFertilized ~ MeanFlagellumLength_uM + SpermMotilityPC1, fit="linear", data=spawn_data_scaled)

#scatter3d(ProportionFertilized ~ MeanFlagellumLength_uM + SpermMotilityPC1, fit="additive", data=spawn_data_scaled)

#scatter3d(ProportionFertilized ~ MeanFlagellumLength_uM + SpermMotilityPC1, fit=c("quadratic", "additive"), data=spawn_data_scaled)

scatter3d(ProportionFertilized ~ MeanFlagellumLength_uM + SpermMotilityPC1, fit="quadratic", data=spawn_data_scaled)


###################################################
### code chunk number 26: FitzpatrickData.Rnw:319-325
###################################################
contour(z=z, x=MF, y=SM, col="black",  ylab="motility", xlab="sperm length")

with(spawn_data_scaled, 
    points(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, 
    col=densCols(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, nbin=20), 
    pch=16, cex=0.9 ))


###################################################
### code chunk number 27: FitzpatrickData.Rnw:331-343
###################################################
hc <- topo.colors(10)         
spawn_colors <- as.numeric(cut(spawn_data_scaled[,1], 10))# breaks it into 10 groups based on values, assigns #1-10

contour(z=z, x=MF, y=SM, col="black", xlim = c(-2.75, 2.55), 
 ylab="motility", xlab="sperm length", lwd=2)

with(spawn_data_scaled, 
    points(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, 
    col=hc[spawn_colors], 
    pch=16, cex=1.1 ))
legend(x=2.0,y=-0.25, fill=hc[1:10], 
    legend=c(0.75,0.8,0.86,0.92,0.97, 1.03, 1.09,1.14,1.2,1.26), border=F, bty="n")


###################################################
### code chunk number 28: FitzpatrickData.Rnw:348-360
###################################################
tc <- gray(1:10/10) # 10 shades of gray.
par(mfrow=c(1,1))
with(spawn_data_scaled, filled.contour(z=z, x=MF, y=SM, 
    ylim=c(min(SM), max(SM)), xlim=c(min(MF), max(MF)), 
    xlab="sperm length", ylab="motility", color = heat.colors,
    # to plot points in the squished frame set up by filled.contour use...
    plot.axes={axis(1); axis(2); # puts back axis
    	points(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, pch=16, cex=0.9, # call points like you would externally
        #col=densCols(x=MeanFlagellumLength_uM, y=SpermMotilityPC1, nbin=20))
        col=tc[spawn_colors])
        },
    main="influence of sperm length and motility on fitness"))


###################################################
### code chunk number 29: FitzpatrickData.Rnw:367-369
###################################################
coefplot(model_1, int=F, h.axis=T, vertical=F, var.las=2, 
  mar= c(10,3,5.1,2), main= "estimated parameters")


###################################################
### code chunk number 30: FitzpatrickData.Rnw:377-379
###################################################
par(mfrow=c(2,2))
plot(model_1)


###################################################
### code chunk number 31: FitzpatrickData.Rnw:385-390
###################################################
par(mfrow=c(2,1))
plot(model_1$resid ~ spawn_data_scaled$MeanFlagellumLength_uM,
    pch=16, xlab = " Mean Flagellum Length", ylab = "residuals")
plot(model_1$resid ~ spawn_data_scaled$SpermMotilityPC1,
    pch=16, xlab = " Sperm Motility PC1", ylab = "residuals")


###################################################
### code chunk number 32: FitzpatrickData.Rnw:397-400
###################################################
#print(cov2cor(vcov(model_1)), 1) # run yourself
vif(model_1) 
ConditionNumber(model_1)


###################################################
### code chunk number 33: FitzpatrickData.Rnw:406-408
###################################################
summary(model_1)$r.sq
summary(model_1)$adj.r.sq


###################################################
### code chunk number 34: FitzpatrickData.Rnw:413-420
###################################################
with(spawn_data_scaled, 
  plot(ProportionFertilized ~ fitted(model_1), 
      xlim=c(min(ProportionFertilized), max(ProportionFertilized)),
      ylim=c(min(ProportionFertilized), max(ProportionFertilized)),
      ylab= "observed ProportionFertilized",
      col=densCols(fitted(model_1))))
 abline(a=0, b=1,lty=3)  


###################################################
### code chunk number 35: FitzpatrickData.Rnw:427-428
###################################################
PRsq(model_1)


