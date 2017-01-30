
set.seed(123)

#create data:

n <- 10000

e <- rnorm(n)

x1 = rnorm(n)           # some continuous variables 

x2 = rnorm(n)

# z = 1 + 2*x1 + 3*x2 + e       # linear combination with a bias
# z = 1 + 2*x1 + e
z = 1 + 2*x1

pr = 1/(1+exp(-z))         # pass through an inv-logit function

y = rbinom(n,1,pr)      # bernoulli response variable

plot(x1, z)
plot(x1, pr)
plot(x1, y)

#now feed it to glm:

df = data.frame(y=y,x1=x1,x2=x2)

glm =glm(y~x1, data=df, family="binomial")

summary(glm)

y <- rmultinom(n = 2, size = 600, prob = c(0.333,0.333,0.333))



##############################
# MULTINOMIAL LOGIT SIMULATION
# setwd("C:/Privat/R_Code/LOGIT")
setwd("C:/GIM_JB/Advanced_Analytics/LOGIT")

source("C:/GIM_JB/Advanced_Analytics/LOGIT/170130_MAKE.DUMMY.VARS.R")

library(nnet)
library(mlogit)

n <- 100000
size <- 5

# covariate matrix
mX = matrix(rnorm(n), n/size, size)

mD <- matrix(rep(c(1,2,3,4), n/size/length(c(1,2,3,4))),n/size,1)

k <- as.data.frame(mD); k$V1 <- factor(k$V1)
new <- MAKE.DUMMY.VARS(k, categorical.vars = c("V1"), redundant = F)
new[,1] <- NULL
new <- data.matrix(new)-1

mX <- cbind(mX, new)

# coefficients for each choice
vCoef1 = rep(0, 8)
vCoef2 = c(3, 4, 5, 6, 7, 8, 9, 10)
vCoef3 = rep(5, 8)

#vCoef2 = rnorm(5)
#vCoef3 = rnorm(5)

# vector of probabilities
vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))
vProb <- vProb/rowSums(vProb) 

# multinomial draws
#mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
mChoices = t(apply(vProb, 1, function(x) rmultinom(n = 1, size = 1, prob = x)))
l <- as.data.frame(mX)
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), l)

#test <- multinom(y ~ V1, data = dfM)
test <- multinom(y ~ V1 + V2 + V3 + V4 + V5 + V11 + V12 + V13, data = dfM)
summary(test)

e <- rep(rnorm((n/size)))

y <- mX%*%vCoef2

dfM.l = cbind.data.frame(y, l)

test.lin <- lm(y ~ V1 + V2 + V3 + V4 + V5 + V11 + V12 + V13 + V14 -1, data = dfM.l)
summary(test.lin)

################################################################################
################################################################################
################################################################################

library(mgcv)
set.seed(6)
## simulate some data from a three class model
n <- 1000
f1 <- function(x) sin(3*pi*x)*exp(-x)
f2 <- function(x) x^3
f3 <- function(x) .5*exp(-x^2)-.2
f4 <- function(x) 1
x1 <- runif(n);x2 <- runif(n)
eta1 <- 2*(f1(x1) + f2(x2))-.5
eta2 <- 2*(f3(x1) + f4(x2))-1
p <- exp(cbind(0,eta1,eta2))
p <- p/rowSums(p) ## prob. of each category 
cp <- t(apply(p,1,cumsum)) ## cumulative prob.
## simulate multinomial response with these probabilities
## see also ?rmultinom
y <- apply(cp,1,function(x) min(which(x>runif(1))))-1

y = t(apply(cp, 1, function(x) rmultinom(n = 1, size = 1, prob = x)))
y = apply(y, 1, function(x) which(x==1))-1

## plot simulated data...
plot(x1,x2,col=y+3)

## now fit the model...
b <- gam(list(y~s(x1)+s(x2),~s(x1)+s(x2)),family=multinom(K=2))
plot(b,pages=1)
gam.check(b)

## now a simple classification plot...
expand.grid(x1=seq(0,1,length=40),x2=seq(0,1,length=40)) -> gr
pp <- predict(b,newdata=gr,type="response")
pc <- apply(pp,1,function(x) which(max(x)==x)[1])-1
plot(gr,col=pc+3,pch=19)










































