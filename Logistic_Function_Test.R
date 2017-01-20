

set.seed(123)
size <- 1000
x <- runif(size, min=0, max=5)
x <- rnorm(size)

a <- 2
beta <- -5

e <- rnorm(size, 0, 0.5)
#e <- dlogis(e)

logit <- exp(beta+a*x)/(1+exp(beta+a*x))

y.prop <- exp(beta+a*x+e)/(1+exp(beta+a*x+e))


#logit <- sort(logit)
plot(x, logit)
plot(x, y.prop)

mydata <- data.frame(y.prop,x)
#mydata$y <- 0; mydata$y[mydata$y.prop >= .5] <- 1
mydata$y = rbinom(size,1,mydata$y.prop) 

plot(mydata$x, mydata$y)

mylogit <- glm(y ~ x, data = mydata, family = binomial(link="logit"))
summary(mylogit)




set.seed(123)
#create data:
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(1000,1,pr)      # bernoulli response variable

#now feed it to glm:
df = data.frame(y=y,x1=x1,x2=x2)
glm =glm(y~x1+x2, data=df, family="binomial")
summary(glm)
