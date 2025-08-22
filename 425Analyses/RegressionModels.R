# Class Activity Expanding the Regression Model (QUADRATIC MODEL) : 

set.seed(12192021) 
n <- 89
X_i <- runif(n, 1.5, 6.5)


# Betas and such
beta0 <- -1  
beta1 <- 6.6  
beta2 <- -0.7 

sigma <- 0.59 
epsilon_i <- rnorm(n, -0.011, sigma) 


Y_i <- beta0 + beta1*X_i + beta2*X_i^2 + epsilon_i 


fabData <- data.frame(y=Y_i, x=X_i) 
fab.lm <- lm(y ~ x + I(x^2), data=fabData) 

  
summary(fab.lm) 


plot(y ~ x, data=fabData, pch=19, size = .3,  main="Quadratic Regression Relation Diagram")
curve(predict(fab.lm, newdata = data.frame(x=x)), add = TRUE, lwd = 1)
curve(beta0 +beta1*x+beta2*x^2, lwd = 1, lty = 2, add = TRUE) 


---
  

# Class Activity - Different Types of Models (CUBIC MODEL):

set.seed(3793) 

n <- 30


X_i <- runif(n, -2, 3.8) 



beta0 <- 2.7 
beta1 <- -4.8 
beta2 <- -1.9 
beta3 <- 1.1 

sigma <- 2 
epsilon_i <- rnorm(n, -0.01, sigma) 


Y_i <- beta0 + beta1*X_i + beta2*X_i^2 + beta3*X_i^3 + epsilon_i 


fabData <- data.frame(y=Y_i, x=X_i) 

fab.lm <- lm(y ~ x + I(x^2) + I(x^3), data=fabData) 
summary(fab.lm) 

plot(y ~ x, data=fabData, pch=19, size = .3,  main="Cubic Regression Relation Diagram")
curve(predict(fab.lm, newdata = data.frame(x=x)), add = TRUE, lwd = 1)
curve(beta0 +beta1*x+beta2*x^2 + beta3*x^3, lwd = 1, lty = 2, add = TRUE)


---


# Class Activity - Different Types of Models (TWO LINES MODEL) : 

set.seed(11833) 

n <- 40


X_i <- runif(n, 30, 52) 

x2 <- sample(c(0,1), n, replace = TRUE)  


beta0 <- 58 
beta1 <- -0.22 
beta2 <- -65 
beta3 <- 1.45 

sigma <- 2.4 
epsilon_i <- rnorm(n, 0, sigma) 

Y0 <- beta0 + beta1*X_i + epsilon_i
Y1 <- (beta0 +beta2) + (beta1 + beta3)*X_i + epsilon_i

Y_i <- ifelse(x2 == 0, Y0, Y1)


fabData <- data.frame(y=Y_i, x=X_i, x2=x2) 

fab.lm <- lm(y ~ x + x2 + x:x2, data=fabData) 


summary(fab.lm) 

plot(y ~ x, data=fabData, col=as.factor(x2), pch = 19, size = 1.5, main = "Two Lines Model") 

b <- coef(fab.lm)
x2 = 0
curve(b[1] + b[2]*x + b[3]*x2 + b[4]*x*x2, add=TRUE, col= "black")
x2 = 1
curve(b[1] + b[2]*x + b[3]*x2 + b[4]*x*x2, add=TRUE, col="red")

x2=0
curve(beta0 + beta1*x + beta2*x2 +beta3*x*x2, add=TRUE, lty=2, col="black")
x2=1
curve(beta0 + beta1*x + beta2*x2 +beta3*x*x2, add=TRUE, lty=2, col="red")


---

  
# Skills Quiz - Different Types of Models (Problem 3 Part a)


mtcars$disp2 <- mtcars$disp^2

drivin.lm <- lm(qsec ~ disp + disp2 + am + disp:am + disp2:am, data = mtcars)

summary(drivin.lm)  

ggplot(mtcars, aes(x = disp, y = qsec, color = factor(am))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
  scale_color_manual(name = "Transmission Type", values = c("skyblue", "orange"),
                     labels = c("Automatic", "Manual")) +
  theme_classic() +
  labs(title = "Regression of qsec on disp and Transmission Type",
       x = "Displacement (disp)",
       y = "Quarter Mile Time (qsec)")


---


# Skills Quiz - Different Types of Models (Problem 4)

set.seed(101) 

n <- 100 

X_i <- runif(n, -2, 2)


beta0.1 <- -2 
beta1.1 <- 3
beta2.1 <- 4

beta0.2 <- 2
beta1.2 <- 5
beta2.2 <- -3

sigma <- 2

epsilon_i <- rnorm(n, 1, sigma)  


Y_i.1 <- beta0.1 + beta1.1*X_i + beta2.1*X_i^2 + epsilon_i
Y_i.2 <- beta0.2 + beta1.2*X_i + beta2.2*X_i^2 + epsilon_i


truth.1 <- function(x) -2 + 3*x + 4*x^2
truth.2 <- function(x) 2 + 5*x - 3*x^2

fabData <- data.frame(x = X_i, y = c(Y_i.1, Y_i.2), 
                      equation = rep(c("y.1", "y.2"), each = n))


fab.lm.1 <- lm(y ~ x + I(x^2), data = fabData, subset = equation == "y.1")
fab.lm.2 <- lm(y ~ x + I(x^2), data = fabData, subset = equation == "y.2")

summary(fab.lm.1)
summary(fab.lm.2)

ggplot(fabData, aes(x = x, y = y, color = equation)) +
  geom_point(size = 1, pch=19) +
  stat_function(fun = truth.1, color = "firebrick", linetype = "dashed", size = 1) + 
  stat_function(fun = truth.2, color = "midnightblue", linetype = "dashed", size = 1) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) + 
  scale_color_manual(name = "Equation", values = c("firebrick", "midnightblue")) + 
  theme_minimal() +
  labs(title = "Quadratic Regression for Two Models",
       x = "X Values",
       y = "Y Values")