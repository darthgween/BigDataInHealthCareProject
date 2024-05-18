# Library
library(ggplot2)
library(dplyr)
library(readr)
library(survival)
library(survminer)
library(randomForestSRC)
library(gbm)
library(utils) #For not having to upload the txt each time

url <- "https://raw.githubusercontent.com/darthgween/BigDataInHealthCareProject/main/heart.valve.txt?token=GHSAT0AAAAAACR3GYSXO5Z3ARKVQY2S6SYSZRYZQWA"
data <- read.table(url, header = TRUE)
head(data)

# data$sex <- factor(data$sex, levels = c(0, 1), labels = c("Male", "Female"))
# data$con.cabg <- factor(data$con.cabg, levels = c(0, 1), labels = c("No", "Yes"))
# data$lv <- factor(data$lv, levels = c(1, 2, 3), labels = c("High", "Moderate", "Low"))
# data$sten.reg.mix <- factor(data$sten.reg.mix, levels = c(1, 2, 3), labels = c("Stenosis", "Regurgitation", "Mixed"))

### EDA

# Descriptive Statistics
dim(data)
str(data)
summary(data)

# Age distribution and Correlation Matrix
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Age Distribution") +
  xlab("Age") +
  ylab("Frequency")

if (!require(corrplot)) {
    install.packages("corrplot")
    library(corrplot)
}
corrplot(cor_matrix, method = "circle")

# Visualization of the relationship between age and log.lvmi
ggplot(data, aes(x = factor(status), y = log.lvmi, fill = factor(status))) +
  geom_boxplot() +
  ggtitle("Boxplot di log.lvmi per Status") +
  xlab("Status") +
  ylab("log.lvmi") +
  scale_fill_discrete(name = "Status")

ggplot(data, aes(x = age, y = log.lvmi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Scatter Plot: età vs log.lvmi") +
  xlab("Età") +
  ylab("log.lvmi")