### STEP 0: load libraries, etc

### STEP 1: Descriptive Statistics

### STEP 2: Univariate analysis (Cox model) of the association of each independent variable with the outcome under study

# Load necessary libraries
library(survival)
library(survminer)
library(utils)

# Read the dataset
url <- "https://raw.githubusercontent.com/darthgween/BigDataInHealthCareProject/main/heart.valve.txt?token=GHSAT0AAAAAACR3GYSWOK6GYZRGKSSK3Z2CZRXTKPA"
data <- read.table(url, header = TRUE)
head(data)

# Convert factors to categorical variables if necessary
data$sex <- factor(data$sex, levels = c(0, 1), labels = c("Male", "Female"))
data$con.cabg <- factor(data$con.cabg, levels = c(0, 1), labels = c("No", "Yes"))
data$lv <- factor(data$lv, levels = c(1, 2, 3), labels = c("High", "Moderate", "Low"))
data$sten.reg.mix <- factor(data$sten.reg.mix, levels = c(1, 2, 3), labels = c("Stenosis", "Regurgitation", "Mixed"))

# Fit Cox models for each predictor
# Outcome is 'status' and follow-up time is 'fuyrs'

# Model for log.lvmi
cox_log_lvmi <- coxph(Surv(fuyrs, status) ~ log.lvmi, data = data)
summary(cox_log_lvmi)

# Model for age
cox_age <- coxph(Surv(fuyrs, status) ~ age, data = data)
summary(cox_age)

# Model for sex
cox_sex <- coxph(Surv(fuyrs, status) ~ sex, data = data)
summary(cox_sex)

# Model for con.cabg
cox_cabg <- coxph(Surv(fuyrs, status) ~ con.cabg, data = data)
summary(cox_cabg)

# Model for creatinine levels
cox_creat <- coxph(Surv(fuyrs, status) ~ creat, data = data)
summary(cox_creat)

# Model for left ventricular ejection fraction
cox_lv <- coxph(Surv(fuyrs, status) ~ lv, data = data)
summary(cox_lv)

# Model for stenosis/regurgitation/mixed
cox_sten_reg_mix <- coxph(Surv(fuyrs, status) ~ sten.reg.mix, data = data)
summary(cox_sten_reg_mix)

### STEP 3: Development of the "basic" predictive model (Cox model) with covariates: sex, age, con.cabg, creat, lv, sten.reg.mix. without including log.lvmi.

# Fit the Cox proportional hazards model
# Include sex, age, con.cabg, creat, lv, and sten.reg.mix as predictors
basic_model <- coxph(Surv(fuyrs, status) ~ sex + age + con.cabg + creat + lv + sten.reg.mix, data = data)

# Summarize the model results
summary(basic_model)

### STEP 4: Evaluate functional form of continuous variables and assumption "Proportional Hazards" for all covariates 

# Check the functional form of 'age' using Martingale residuals
mart_resid_age <- resid(basic_model, type = "martingale")
plot(data$age, mart_resid_age, xlab = "Age", ylab = "Martingale residuals", main = "Martingale Residuals vs Age")
abline(h = 0, col = "red")

# Check the functional form of 'creat' using Martingale residuals
mart_resid_creat <- resid(basic_model, type = "martingale")
plot(data$creat, mart_resid_creat, xlab = "Creatinine Levels", ylab = "Martingale residuals", main = "Martingale Residuals vs Creatinine Levels")
abline(h = 0, col = "red")

# Checking Proportional Hazards assumption
# We use cox.zph function to perform the test
ph_test <- cox.zph(basic_model)
print(ph_test)  # This will print the test results
plot(ph_test)   # This plots the Schoenfeld residuals

### STEP 5: Development of an "augmented" predictive model (Cox model) with covariates: sex, age, con.cabg, creat, lv, sten.reg.mix and including log.lvmi.
# Fit the Cox proportional hazards model including log.lvmi
augmented_model <- coxph(Surv(fuyrs, status) ~ sex + age + con.cabg + creat + lv + sten.reg.mix + log.lvmi, data = data)

# Summarize the model results
summary(augmented_model)

### STEP 6:  Evaluate functional form of continuous variables and assumption "Proportional Hazards" for all covariates 


# Evaluate functional form of continuous variables using Martingale residuals
# Check for 'log.lvmi'
plot(resid(augmented_model, type = "martingale") ~ data$log.lvmi, 
     xlab = "Log LVMi", ylab = "Martingale Residuals", main = "Martingale Residuals vs Log LVMi")
abline(h = 0, col = "red")

# Check for 'age'
plot(resid(augmented_model, type = "martingale") ~ data$age, 
     xlab = "Age", ylab = "Martingale Residuals", main = "Martingale Residuals vs Age")
abline(h = 0, col = "red")

# Check for 'creat'
plot(resid(augmented_model, type = "martingale") ~ data$creat, 
     xlab = "Creatinine Levels", ylab = "Martingale Residuals", main = "Martingale Residuals vs Creatinine Levels")
abline(h = 0, col = "red")

# Assess proportional hazards assumption using Schoenfeld residuals
ph_test <- cox.zph(augmented_model)
plot(ph_test)  # This plots the Schoenfeld residuals for each covariate
