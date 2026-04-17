#KALYN WOLTERS 
#MT CARS REGRESSION ANALYSIS
#Exploring the relationship between car weight and fuel efficiency

#Load buikt in dataset (Motor Trend Car Road Tests, 1974)
data("mtcars")

#quick look at the data
head(mtcars)

#Make the scatterplot to visulaize the relationship we are exploring
plot(mtcars$wt, mtcars$mpg, 
     main = "Fuel efficiency vs Car weight",
     xlab = "Weight (1000 lbs)",
     ylab = "Miles Per Gallon (MPG)")

#Fit a simple liner regression (mpg predicted by weight)
ml <- lm(mpg ~ wt, data = mtcars) #mpg ~ wt is read as mpg is modeled by weight or y is modeled by x
summary(ml)

#plot fitted values vs residuals to check model assumptions
plot(ml$fitted.values, resid(ml), 
     main = "residuals: Linear Model",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")
#random scatter around 0 means good fit, patterns or curves means the model may be missing soemthing

#Log transformations to adress unequal spread seen in the residuals model
m2 <- lm(log(mpg) ~ wt, data = mtcars)
summary(m2)

#check if log transformation improved model behavior
plot(m2$fitted.values, resid(m2),
     main = "Residuals vs Fitted values: Log Model",
     xlab = "Fitted Values",
     ylab = "Residul values")
abline(h = 0, col = "red")

#Returns slope and intercept from log model
#( 3.83191,-0.27178)
#The is no such thing as a car in real life that weighs zero lbs so the 
#intercept helps anchor the regression line mathematically
#This serves as a baseline for the regression equation to estimate
#the mpg of cars that weigh specific lbs

coef(m2)

#R^2 for the linear model and log transformed model:
summary(ml)$r.squared
summary(m2)$r.squared
#In the log transformed model wt can explain a higher percentage of variation for mpg than in the linear model
