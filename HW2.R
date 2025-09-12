set.seed(123)
y <- rnorm(50)
x <- rnorm(50)

y2 <- rep(y, 100)
x2 <- rep(x, 100)

m1 <- lm(y ~ x)
m2 <- lm(y2 ~ x2)

summary(m1)

summary(m2)

plot(m2$fitted.values, 
     resid(m2),
     xlab = "Fitted values", 
     ylab = "Residuals",
     main = "Residual Plot for m2",
     pch = 20)
par(new = TRUE)

abline(h=0, col = "red")
