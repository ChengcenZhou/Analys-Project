# Analys-Project
View(Boston)
library(MASS)
?Boston
# fullmodel
class(Boston$rad)
fullmodel=lm(medv~.,data=Boston)
summary(fullmodel)
par(mfrow=c(2,2))
plot(fullmodel)
#We can see the Multiple R-squared is 0.7406 
#Residuals VS Fitted plot indicates that model violates the costant variance assumption
#Normal QQ indicates that normality assumption is not satistfied. 
histNorm <- function(x, densCol = "darkblue"){
  m <- mean(x)
  std <- sqrt(var(x))
  h <- max(hist(x,plot=FALSE)$density)
  d <- dnorm(x, mean=m, sd=std)
  maxY <- max(h,d)
  hist(x, prob=TRUE,
       xlab="x", ylim=c(0, maxY),
       main="(Probability) Histogram with Normal Density")
  curve(dnorm(x, mean=m, sd=std),
        col=densCol, lwd=2, add=TRUE)
}
hist(fullmodel$residuals)
histNorm(fullmodel$residuals, "orange")
shapiro.test(fullmodel$residuals)
boxcox(fullmodel)
#When lamda is equal 0, we log response.

mod=lm(log(medv)~.,Boston)
rstudent(mod)
sresid <- rstudent(mod)
n <- length(sresid)
p <- length(mod$coefficients)
df <- n - p - 1
alpha <- 0.05
crit <- qt(1 - (0.05/2)/n,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

Boston1=Boston[-c(372,373,402,413),]
mod1=lm(log(medv)~.,Boston1)
rstudent(mod1)
sresid <- rstudent(mod1)
n <- length(sresid)
p <- length(mod1$coefficients)
df <- n - p - 1
alpha <- 0.05
crit <- qt(1 - (0.05/2)/n,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

Boston2=Boston1[-c(369),]
mod2=lm(log(medv)~.,Boston2)
rstudent(mod2)
sresid <- rstudent(mod2)
n <- length(sresid)
p <- length(mod2$coefficients)
df <- n - p - 1
alpha <- 0.05
crit <- qt(1 - (0.05/2)/n,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

Boston3=Boston2[-c(401),]
mod3=lm(log(medv)~.,Boston3)
rstudent(mod3)
sresid <- rstudent(mod3)
n <- length(sresid)
p <- length(mod3$coefficients)
df <- n - p - 1
alpha <- 0.05
crit <- qt(1 - (0.05/2)/n,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

summary(mod3)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod1)$adj.r.squared
summary(mod)$adj.r.squared
#When we do the third step, we find that adjusted R decreases. So we choose the previous model(mod2).
?Boston
#Model seleciton begins!
library(broom)
fit_back_aic1=step(mod2,direction="backward")
glance(fit_back_aic1)$adj.r.square
fit_back_aic2=step(mod,direction="backward")
glance(fit_back_aic2)$adj.r.square


fit_start=lm(log(medv)~1,data=Boston)
fit_forw_aic1<-step(fit_start,medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,direction="forward")
glance(fit_forw_aic1)$adj.r.square
fit_start=lm(log(medv)~1,data=Boston2)
fit_forw_aic2<-step(fit_start,medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,direction="forward")
glance(fit_forw_aic2)$adj.r.square

fit_both_aic2 <- step(mod2, direction = "both")
glance(fit_both_aic2)$adj.r.square
fit_both_aic1 <- step(mod, direction = "both")
glance(fit_both_aic1)$adj.r.square

n=length(resid(mod2))
fit_back_bic2 <- step(mod2, direction = "backward", k = log(n))
glance(fit_back_bic2)$adj.r.square
n=length(resid(mod))
fit_back_bic1 <- step(mod, direction = "backward", k = log(n))
glance(fit_back_bic1)$adj.r.square

n=length(resid(mod2))
fit_forw_bic2 <- step(mod2, direction = "forward", k = log(n))
glance(fit_forw_bic2)$adj.r.square
n=length(resid(mod))
fit_forw_bic1 <- step(mod, direction = "forward", k = log(n))
glance(fit_forw_bic1)$adj.r.square

glance(fit_forw_bic2)$adj.r.square
glance(fit_back_bic2)$adj.r.square
glance(fit_both_aic2)$adj.r.square
glance(fit_forw_aic2)$adj.r.square
glance(fit_back_aic1)$adj.r.square

hist(mod2$residuals)

par(mfrow=c(1,1))
plot(fit_back_aic1)
anova(fit_back_aic1,mod2)
hist(fit_back_aic1$residuals)
histNorm(fit_back_aic1$residuals, "orange")
shapiro.test(fit_back_aic1$residuals)
library(lmtest)
bptest(fit_back_aic1)

#influential points
cookd=cooks.distance(fullmodel)
cutoff <- 4/((nrow(mtcars)-length(fullmodel$coefficients)-2)) 
par(mfrow=c(1,1))
plot(fullmodel, which=4, cook.levels=cutoff)
dffinalmod1=dfbetas(fullmodel)

