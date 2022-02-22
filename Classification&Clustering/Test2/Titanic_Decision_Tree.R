# Install packages to make Machine Learning.
install.packages("caret")
install.packages("rpart.plot")

# Load the packages.
library(caret)
library(rpart.plot)

# Load the dataset.
Titanic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Titanic.csv")
summary(Titanic)

# Choose 'Survived' as a target, and PClass, Age, and Sex as features.
Titanic <- Titanic[,c("PClass","Age","Sex","Survived")]
# Convert Survived to factor and set if value 0 = Died, otherwise = Survived.
Titanic$Survived <- as.factor(ifelse(Titanic$Survived==0,"Died","Survived"))
# Convert PClass to factor.
Titanic$PClass <- as.factor(Titanic$PClass)
# Convert Sex to factor.
Titanic$Sex <- as.factor(Titanic$Sex)

# Show description and summary data of data frame.
str(Titanic)
summary(Titanic)

# Remove NA value.
Titanic <- na.omit(Titanic)

# Generate random seed.
set.seed(9999)

# Cross validation. Set Train set (80%) and Test set (20%).
train <- createDataPartition(Titanic[,"Survived"],p=0.8,list=FALSE)
Titanic.trn <- Titanic[train,]
Titanic.tst <- Titanic[-train,]

ctrl <- trainControl(method = "cv", number = 10)

# Fit model and using rpart algorithm of decision tree
# Use trControl for cross-validation. Test 30 different values of parameters.
fit.cv <- train(Survived ~ ., data = Titanic.trn, method = "rpart",
                trControl = ctrl,
                tuneLength = 30)

# Predict
pred <- predict(fit.cv,Titanic.tst)

# Evaluate model with confusion matrix.
confusionMatrix(table(Titanic.tst[,"Survived"],pred))

# Show fit value. (Cp = complexity parameter)
print(fit.cv)
#plot accuracy chart.
plot(fit.cv)

# Plot with values.
plot(fit.cv$finalModel)
# Add text to plot
text(fit.cv$finalModel)

# Use rpart.plot
library(rpart)
rpart.plot(fit.cv$finalModel)
rpart.plot(fit.cv$finalModel,fallen.leaves = FALSE)

# Another method
install.packages("RColorBrewer")
install.packages("rattle")
library(RColorBrewer)
library(rattle)

tree <- rpart(Survived ~., data=Titanic.trn,
                method="class")
fancyRpartPlot(tree)