# Regression Extension Homework

### 1. Wages data ---------------------------------------------------------

# load Data
wages <- readxl::read_excel("/Users/drydenseeley1/Documents/wages.xlsx")
str(wages)

# Plot Wage vs Age 
plot(w$Age, w$Wage, xlab="Age", ylab="Wage", pch=19)

# Linear and quadratic models 
lm1 <- lm(Wage ~ Age + Educ, data=w) 
lm2 <- lm(Wage ~ Age + I(Age^2) + Educ, data=w)

# Compare AIC 
AIC(lm1) 
AIC(lm2)

# Residual diagnostics 
res1 <- residuals(lm1) 
res2 <- residuals(lm2) 
shapiro.test(res1) 
shapiro.test(res2)

# Predictions for Educ = 16 at ages 30, 50, 70 
predict(lm2, newdata=data.frame(Age=c(30,50,70), Educ=c(16,16,16)))

# Age that maximizes wage 
b <- coef(lm2) 
age_star <- -b["Age"] / (2 * b["I(Age^2)"]) 
age_star 
predict(lm2, newdata=data.frame(Age=age_star, Educ=16))

# --- ANN ARBOR DATA --- 
a <- read_excel("AnnArbor.xlsx")

# Correlations 
cor(a$Rent, a$Beds) 
cor(a$Rent, a$Baths) 
cor(a$Rent, a$Sqft)

# Simple regressions 
summary(lm(Rent ~ Beds, data=a)) 
summary(lm(Rent ~ Baths, data=a)) 
summary(lm(Rent ~ Sqft, data=a))

# Multiple models 
m1 <- lm(Rent ~ Beds + Baths + Sqft, data=a) 
m2 <- lm(log(Rent) ~ Beds + Baths + Sqft, data=a) 
m3 <- lm(Rent ~ Beds + Baths + log(Sqft), data=a) 
m4 <- lm(log(Rent) ~ Beds + Baths + log(Sqft), data=a)

# Compare models 
AIC(m1, m2, m3, m4)

# Prediction for 1600 sqft, 3 beds, 2 baths 
predict(m4, newdata=data.frame(Beds=3, Baths=2, Sqft=1600)) 
exp(predict(m4, newdata=data.frame(Beds=3, Baths=2, Sqft=1600)))











