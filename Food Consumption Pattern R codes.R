rm (list = ls())
daily_caloric_intake_dataset <- read.csv(file.choose() , header = TRUE)
View(daily_caloric_intake_dataset)
#Question 1 How does weight depend upon the calorie intake based upon age?
#Techniques: Linear Regression Analysis with Interaction term(ageweight)
daily_caloric_intake_dataset $ ageweight <-daily_caloric_intake_dataset$Age*daily_caloric_intake_dataset$Weight
mod1 <- lm(Daily.Caloric.Intake~ageweight, data = daily_caloric_intake_dataset)
summary(mod1)
#plot between interaction term and response variable-
plot(daily_caloric_intake_dataset$Daily.Caloric.Intake, daily_caloric_intake_dataset$ageweight)
#residual analysis
predictval = predict(mod1)
residuals <- mod1$residuals
plot(predictval, residuals, xlab = "Predicted Values",ylab = "Residuals", main = "Residuals vs Predicted Values")
#Multicollinearity check-
mod4 <- lm(Daily.Caloric.Intake~ Age+ Weight+ ageweight, data = daily_caloric_intake_dataset)
summary(mod4)
vif_val <-vif(mod4)
print(vif_val)
#Question 2 Is there a significant difference in daily calorie intake between different age groups?
#Techniques: ANOVA-Analysis of Variance. 
anova_model <- aov(Daily.Caloric.Intake ~ Age, data = daily_caloric_intake_dataset)
summary(anova_model)
#Residual analysis
model2 <- lm(Daily.Caloric.Intake ~ Age, data = daily_caloric_intake_dataset)
summary(model2)
residuals <- model2$residuals
predicted_val = predict(model2)
plot(predicted_val, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Predicted Values")
#Q3 Considering potential variations in the personalized food choices, how does gender affect the daily calorie intake?
#Techniques:Simple Regression Analysis
modl <- lm (Daily.Caloric.Intake ~ Gender, data = daily_caloric_intake_dataset)
summary(modl)
#Residual Analysis
predictval <- predict(mod1)
residuals <- mod1$residuals
hist(model$residuals, breaks = 100)
#Q4 Are there interaction effects between certain independent variables (e.g., dietary preference, exercise frequency, location, eating disorder habits) on daily caloric intake?
#Techniques:Backward Selection
library(MASS)
mod3 <- lm(Daily.Caloric.Intake Dietary.Preference + Exercise.Frequency + Location+  Eating.Disorder.Habit, data= daily_caloric_intake_dataset)
summary(mod3)
step <- stepAIC(mod3, direction = "backward")
summary(step)
step$anova
#Multicollinearity check
vif_values <- vif(mod3)
print(vif_values)