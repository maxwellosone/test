d <- read.csv("/Users/maxosone/Downloads/mlb.csv")

d$AVG <- 100 * d$H / d$AB

# linear model
model1 <- lm(salary ~ POS, data = d)
summary(model1)
# residual plot
plot(model1$fitted.values, 
     resid(model1),
     xlab = "Fitted values", 
     ylab = "Residuals",
     main = "Residual Plot for Model 1",
     pch = 20)
par(new = TRUE) 

abline(h=0, col = "red")

## Exercise 3

d$yearID <- as.factor(d$yearID)
model2 <- lm(salary ~ AVG + yearID + teamID + bats + POS,
             data = d)
summary(model2)

## Exercise 4
plot(model2$fitted.values, 
     resid(model2),
     xlab = "Fitted values", 
     ylab = "Residuals",
     main = "Residual Plot for Model 2",
     pch = 20)
par(new = TRUE)

abline(h=0, col = "red")

hist(d$salary)
#hist(d$salary[d$POS=="1B"])
#mean(d$salary[d$POS=="1B"])