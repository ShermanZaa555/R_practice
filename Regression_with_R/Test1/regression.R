#HAPPINESS WITH LINEAR REGRESSION

#install packages to make data analysis and linear regression
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

#Load the packages
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#1.Load the data
summary(income.data)

#2.Check Independence of observations
#    Normality
hist(income.data$happiness)
#    Linearity
plot(happiness ~ income, data = income.data)

#3.Simple regression
income.happiness.lm <- lm(happiness ~ income, data = income.data)
summary(income.happiness.lm)

#4.Homoscedasticity
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))

#5.Visualization
income.graph <- ggplot(income.data, aes(x=income, y=happiness)) + geom_point()
income.graph <- income.graph + geom_smooth(method = "lm", col = "black")
income.graph <- income.graph +
  stat_regline_equation(label.x = 3, label.y = 7)
income.graph <- income.graph + 
  theme_classic() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10000)",
       y = "Happiness score (0 to 10)")

#HEART DISEASE WITH LINEAR REGRESSION

#1.Load the data
summary(heart.data)

#2.Check Independence of observations
cor(heart.data$biking, heart.data$smoking)
#    Normality
hist(heart.data$heart.disease)
#    Linearity
plot(heart.disease ~ biking, data = heart.data)
plot(heart.disease ~ smoking, data = heart.data)

#3.Simple regression
heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)

#4.Homoscedasticity
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))

#5.Visualization
plotting.data <- expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out=30),
  smoking = c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking)))

plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata = plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
plotting.data$smoking <- as.factor(plotting.data$smoking)

heart.plot <- ggplot(heart.data, aes(x=biking, y=heart.disease)) + 
  geom_point()
heart.plot <- heart.plot +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)
heart.plot <- heart.plot +
  theme_classic() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")
heart.plot + annotate(geom="text", x=30, y=1.75, label="= 15 + (-0.2*biking) + (0.178*smoking)")

