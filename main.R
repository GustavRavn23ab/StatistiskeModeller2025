#------------------------------------------------ Opgave 1 ------------------------------------------------

## 1) ----
# Vi henter data
Bwages <- read.csv("data/bwages.csv")

#Vi laver et standard qq plot
qqnorm(Bwages$wage,xlab ="Teoretiske fraktile", ylab="Stikprøve fraktiler" )
qqline(Bwages$wage,col="firebrick", lwd =2)

# Transformerer vi istedet dataet med log
Bwages$logwage <- log(Bwages$wage)

#plotter igen
qqnorm(Bwages$logwage,xlab = "Teoretiske fraktile", ylab = "Stikprøve fraktiler" )
qqline(Bwages$logwage,col="firebrick", lwd =2)

## 2) ----
#vi fitter en linje 
fit.1 <- lm(logwage ~ exper,data=Bwages)
#plotter
plot(logwage ~ exper,data=Bwages, xlab ="Erfaring (antal år)",ylab = "log(Timeløn)")
abline(fit.1,col="firebrick", lwd =2)

## 3) ----
# vi plotter det første plot (which = 1)
plot(fit.1,which = 1)

## 4) ----
# Beregn ksmooth først
s.under <- ksmooth(Bwages$exper, Bwages$logwage, kernel = "normal", bandwidth = 1)
s.mellem <- ksmooth(Bwages$exper, Bwages$logwage, kernel = "normal", bandwidth = 5)
s.over <- ksmooth(Bwages$exper, Bwages$logwage, kernel = "normal", bandwidth = 50)

#plot dem sammen
plot(logwage ~ exper,data=Bwages, xlab ="Erfaring (antal år)",ylab = "log(Timeløn)",main="Ksmooth med forskellige båndbredder")
lines(s.under,col="chartreuse4", lwd =2)
lines(s.mellem,col="dodgerblue", lwd =2)
lines(s.over,col="firebrick", lwd =2)

## 5) ----
#fit en regressionsmodel af orden 3 
fit.3 <- lm(logwage ~ exper + I(exper^2) + I(exper^3),data=Bwages)
#plot dem sammen
# Vi sætter plotting area: med 1 række og 2 cols
# plot fra 4)
plot(logwage ~ exper, data = Bwages, 
     xlab = "Erfaring (antal år)", ylab = "log(Timeløn)", 
     main="Ksmooth med forskellige båndbredder og poly fit")
lines(s.under, col = "chartreuse4", lwd = 2)
lines(s.mellem, col = "dodgerblue", lwd = 2)
lines(s.over, col = "firebrick", lwd = 2)
curve(predict(fit.3, newdata = data.frame(exper = x)), 
      add = TRUE, col = "deeppink2", lwd = 3)

legend("topleft",inset = .02,y.intersp = 1.5,  
       legend = c("Underudglatning (h=1)", "Mellem (h=5)", "Overudglatning (h=50)", "3. ordens polynomium"), 
       col = c("chartreuse4", "dodgerblue", "firebrick", "deeppink2"), 
       lwd = c(2, 2, 2, 3), 
       bty = "n")

#Vi undersøger om vi kunne have nøjes medn simpel lineær
anova(fit.1,fit.3)
# og nu for anden orden
fit.2 <- lm(logwage ~ exper + I(exper^2) , data = Bwages)
anova(fit.2,fit.3)


## 6) ----
# vi fitter den fulde (antager vi går videre med 2. orden)
fit.full <- lm(logwage~ exper + I(exper^2) + educ + male, data = Bwages)
fit.full

## 7)
summary(fit.full)
#vi kan regne det som:
t <- qt(0.025, df=1467)
c(1.145e-01 + t*1.528e-02,1.145e-01 - t*1.528e-02)  
confint(fit.full, "male")

#------------------------------------------------ Opgave 2 ------------------------------------------------
## 2) ----
p <- 4
r <- 5
rep1 <- cbind(1,contr.sum(p))
X <- rep1[rep(1:4, each=5),]
solve(crossprod(X))


#------------------------------------------------ Opgave 3 ------------------------------------------------
# Vi henter data på ny
Bwages <- read.csv("data/bwages.csv")
Bwages$logwage <- log(Bwages$wage)
#libraries
library(MASS)
library(car)

## 1) ----
# fit linear model
fit <- lm(wage~exper+male+educ,data=Bwages)
#plot box cox
bc <- MASS::boxcox(fit)
bc$x[which.max(bc$y)]

## 2) ----
#fit full uden poly
fit.full <- lm(logwage~ exper + educ + male, data = Bwages)
car::avPlots(fit.full,layout = c(1,3))
car::crPlots(fit.full,layout = c(1,3))

## 3) ----
#fjern observationer med exper <= 0
Bwages.ny <- Bwages[Bwages$exper>0,]
#udfør box tidwell
bt<-car::boxTidwell(formula = logwage ~ exper, other.x = ~educ, data = Bwages.ny)
(gamma <- bt$result[1])


## 4) ----
lm.r <- lm(logwage~educ+male+I(exper^gamma),data=Bwages)
lm.f <-lm(logwage~factor(educ)+male+I(exper^gamma),data=Bwages)
anova(lm.r,lm.f)
summary(lm.r)

## 5)----
lm.vv <- lm(logwage~I(exper^gamma)*male+educ*male, data = Bwages)
summary(lm.vv)


#------------------------------------------------ Opgave 4 ------------------------------------------------
#library 
library(Ecdat)
library(car) 

## 1) ----
#fitter
fit <-lm(mv~.-townid,data = Hedonic)
summary(fit)

## 2)----
drop1(fit, test="F")
drop1(update(fit,~.-indus), test="F")
drop1(update(fit,~.-indus-zn), test="F")
drop1(update(fit,~.-indus-zn-age), test="F")

## 3) ----
#fjerner zn, indus og age fra fit
fit.reduced <- update(fit,~.-zn-indus-age)
car::vif(fit.reduced)

## 4) ----
#tilføjer townid som factor
fit.factor <- update(fit.reduced,~factor(townid)+.)
summary(fit.factor)
#fjerner rad,tax og pratio
fit.factor.clean <- update(fit.factor,~.-rad-tax-ptratio)

## 5) ----
#GVIF pga høj df
vif(fit.factor.clean)

## 6) ----
plot(fit.factor.clean)

#------------------------------------------------ Opgave 5 ------------------------------------------------
## 4)----
#vi bruger opgivet kode
Z <- model.matrix(~ factor(townid) - 1 + crim + nox + rm + age + blacks + lstat, data = Hedonic)
G <- Z[,1:92]
X <- Z[,-(1:92)]
gr <- Hedonic$townid
Y <- Hedonic$mv
X <- Z[,-(1:92)]
Ytilde <- Y - ave(Y, gr)
Xtilde <- X
for ( j in 1:ncol(X) ) Xtilde[,j] <- X[,j] - ave(X[,j], gr)

#vi kan nu se på det nederste hjørne af (Z'Z)^-1 og sammenligne med (X\tilde'X\tilde)^-1
solve(crossprod(Z))[93:98,93:98]
solve(crossprod(Xtilde))

## 5)----

lm.fit <-lm(mv ~ factor(townid) - 1 + crim + nox + rm + age +
     blacks + lstat, data = Hedonic)
tail(lm.fit$coefficients, 6)
lm(Ytilde ~ Xtilde-1)

#------------------------------------------------ Opgave 6 ------------------------------------------------
#Library
library(Ecdat)
#data
Ysub <- transform(subset(Yogurt, choice %in% c("yoplait", "dannon")), choice=factor(choice))

## 1)----
#fit model
fit<-glm(choice~price.yoplait+price.dannon,Ysub,family=binomial)
#undersøg levels
levels(Ysub$choice)
#summary og KI
summary(fit)
confint(fit)

## 2)----
#forskel varible (pris.yoplait- pris.dannon)
Ysub$forskel <- Ysub$price.yoplait-Ysub$price.dannon
#fit model
fit.dif <- glm(choice~forskel,family = binomial,data=Ysub) #link = logit
#summary 
summary(fit.dif)
# lrt 
anova(fit.dif,fit,"LRT")


## 3)----
tbl <- xtabs(~ id + choice, Ysub)
Y <- tbl[,1]
n <- rowSums(tbl)
uninf <- names(Y)[Y==0 | Y==n]
Ysub2 <- subset(Ysub, !(id %in% uninf))
fit <- glm(choice ~ factor(id) + price.yoplait+price.dannon, binomial, Ysub2)
summary(fit)
tail(confint(fit),2)

#lav den reducerede
fit.r <- glm(choice ~ factor(id) + forskel, binomial, Ysub2)
#udfør anova
anova(fit.r,fit)


#------------------------------------------------ Opgave 7 ------------------------------------------------
#library
library(stats4)
## 4)----
tbl <- xtabs(~ id + choice, Ysub)
Y <- tbl[,1]
n <- rowSums(tbl)

mll <- function(alpha=0.3, beta=0.3){
  -sum(log(choose(n,Y)*(beta(Y+alpha,n-Y+beta))/(beta(alpha,beta))))
}
mle.fit <-mle(mll)
summary(mle.fit)

## 5) ----
#kovarians matrix
V <- vcov(mle.fit)
V
#alpha og beta hat
(alpha.hat <-coef(mle.fit)[1])
(beta.hat <-coef(mle.fit)[2])

#gradient
(grad <- c(beta.hat/((alpha.hat+beta.hat)^2),-alpha.hat/((alpha.hat+beta.hat)^2)))

#standard fejl
(s.e <-sqrt( t(grad)%*%V%*%grad))

#tau: delta metode
p.est <- alpha.hat/(alpha.hat+beta.hat)
#KI
p.est+c(qnorm(0.025)*s.e,-qnorm(0.025)*s.e)

## 6) ----
#mll - negativ log-likelihood
mll.ny <- function(p=0.3, r=0.3){
  -sum(log(choose(n,Y)*(beta(Y+p*(1/r-1),n-Y+(1-p)*(1/r-1)))/(beta(p*(1/r-1),(1-p)*(1/r-1)))))
}
mle.fit.ny <-mle(mll.ny)
summary(mle.fit.ny)
confint(mle.fit.ny)

## 7)----
# vi gætter
fit.sidst <-glm(Y/n ~ 1, family=quasibinomial, weights=n/(1+(n-1)*0.5972883))
summary(fit.sidst)$dispersion

